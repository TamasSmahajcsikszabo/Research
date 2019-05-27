library(data.table)
library(ggthemes)

setwd("C:/Users/tamas/OneDrive/Dokumentumok/Kutat?s")

summary2 <- function (data) {
  data %>%
    group_by(Subject, Emotion, bin) %>%
    summarise(ABV = mean(ABV))
}

SD_filtering_abv <- SD_filtering_abv %>% 
  mutate(method = "SD filtering") %>% 
  as_tibble() %>% 
  summary2() %>%
  rename(SD_filtered_ABV = ABV)

thresfhold_abv <- thresfhold_abv %>% 
  mutate(method = "thresholded") %>% 
  as_tibble() %>% 
  summary2()%>%
  rename(thresholded_ABV = ABV)

winsorizing_abv <- winsorizing_abv %>% 
  mutate(method = "winsorized") %>% 
  as_tibble() %>% 
  summary2()%>%
  rename(winsorized_ABV = ABV)

summary(SD_filtering_abv)


merge <- SD_filtering_abv %>%
  left_join(thresfhold_abv) %>%
  left_join(winsorizing_abv)

merge <- merge %>%
  gather(4:6, key="ABV_method", value="ABV")

#trendlines with points

merge %>%
  filter(!Subject == "7552") %>%
  mutate(ABV_method = factor(ABV_method, levels=rev(unique(ABV_method)))) %>%
  group_by(bin, Subject, ABV_method) %>%
  summarise(ABV = mean(ABV, na.rm = TRUE)) %>%
  ggplot(aes(Subject, ABV, fill=ABV_method, colour=ABV_method)) +
  geom_smooth(size=0.7, alpha=1/3, colour="black")+
  theme_igray()+ 
  scale_colour_tableau() + 
  scale_fill_tableau()

merge %>%
  filter(!Subject == "7552") %>%
  mutate(ABV_method = factor(ABV_method, levels=rev(unique(ABV_method)))) %>%
  group_by(bin, Subject, ABV_method) %>%
  summarise(ABV = mean(ABV, na.rm = TRUE)) %>%
  ggplot(aes(Subject, ABV, fill=ABV_method, colour=ABV_method)) +
  geom_point(alpha=1/5, size=2, show.legend = FALSE) +
  geom_smooth(size=0.7, alpha=1/3, colour="black", show.legend = FALSE)+
  facet_grid(ABV_method~bin, scales="free") +
  theme_igray()+ 
  scale_colour_tableau() + 
  scale_fill_tableau()

#column_charts ~ bins per emotions
merge %>%
  filter(!Subject == "7552") %>%
  mutate(ABV_method = factor(ABV_method, levels=rev(unique(ABV_method)))) %>%
  group_by(bin, Emotion,ABV_method) %>%
  summarise(ABV = mean(ABV, na.rm = TRUE)) %>%
  ggplot(aes(bin, ABV, group=bin, fill=ABV_method, colour=ABV_method, label=round(ABV, digit=3))) +
  geom_bar(stat="identity", position="dodge", show.legend = FALSE, alpha=1/1.3) +
  facet_grid(~ABV_method ~ Emotion, scales="free_x") +
  theme_igray()+ 
  scale_colour_tableau() + 
  scale_fill_tableau() +
  geom_label(colour="black",position = position_dodge(0.9), show.legend = FALSE) +
  ylab("Mean of ABV") +
  xlab("ABV Method ~ bin ~ emotion")

#column_charts ~ emotions per bins
merge %>%
  filter(!Subject == "7552") %>%
  mutate(ABV_method = factor(ABV_method, levels=rev(unique(ABV_method)))) %>%
  group_by(bin, Emotion,ABV_method) %>%
  summarise(ABV = mean(ABV, na.rm = TRUE)) %>%
  ggplot(aes(Emotion, ABV, group=Emotion, fill=ABV_method, colour=ABV_method, label=round(ABV, digit=3))) +
  geom_bar(stat="identity", position="dodge", show.legend = FALSE, alpha=1/1.3) +
  facet_grid(~ABV_method ~ bin, scales="free_x") +
  theme_igray()+ 
  scale_colour_tableau() + 
  scale_fill_tableau() +
  geom_label(colour="black",position = position_dodge(0.9), show.legend = FALSE) +
  ylab("Mean of ABV") +
  xlab("ABV Method ~ bin ~ emotion")

#column_charts ~ only emotions
merge %>%
  filter(!Subject == "7552") %>%
  mutate(ABV_method = factor(ABV_method, levels=rev(unique(ABV_method)))) %>%
  group_by(Emotion,ABV_method) %>%
  summarise(ABV = mean(ABV, na.rm = TRUE)) %>%
  ggplot(aes(ABV_method, ABV, group=ABV_method, fill=ABV_method, colour=ABV_method, label=round(ABV, digit=3))) +
  geom_bar(stat="identity", position="dodge", show.legend = FALSE, alpha=1/1.3) +
  facet_grid(~Emotion,scales="free_x") +
  theme_igray()+ 
  scale_colour_tableau() + 
  scale_fill_tableau() +
  geom_label(colour="black",position = position_dodge(0.9), show.legend = FALSE) +
  ylab("Mean of ABV") +
  xlab("ABV Method ~ emotion")

# trend charts
merge %>%
  filter(!Subject == "7552") %>%
  mutate(ABV_method = factor(ABV_method, levels=rev(unique(ABV_method)))) %>% 
  group_by(Subject, ABV_method) %>% 
  summarise(ABV = mean(ABV, na.rm = TRUE)) %>%
  ggplot(aes(Subject, ABV, fill = ABV_method, colour = ABV_method)) + 
  geom_smooth(alpha = 1/10, show.legend =  FALSE) +
  theme_igray()+ 
  scale_colour_colorblind() + 
  scale_fill_colorblind()

merge2 <- merge %>%
  group_by(ABV_method) %>% 
  summarise(ABV = mean(ABV, na.rm = TRUE))

animate_xy(merge2, fps = 19)


# Histograms --------------------------------------------------------------

merged_data <- bind_rows(winsorized_for_analysis, SD_filtered, thresholded)

merged_data %>% 
  ggplot(aes(RT, fill = method)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~method) + 
  theme_minimal()

merged_data %>% 
  ggplot(aes(Subject, trial))+
  geom_hex()

x <- merged_data %>% filter(method == "winsorizing") %>% select(RT) %>%  na.omit() %>% filter(RT < 2000)
x <- as.double(x)
hist(x,
     freq=FALSE,
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main="Histogram, rug plot, density curve")
rug(jitter(x))
lines(density(x), col="blue", lwd=2)

merged_data %>% 
  filter(RT <1000) %>% 
  ggplot(aes(RT)) +
  geom_histogram(aes(fill = method), colour = "grey30", show.legend = FALSE) + 
  scale_fill_manual(values = c(c("#8A5ED6", "#37BA84", "#E0C143"))) + 
  geom_freqpoly(show.legend = FALSE, size = 1, alpha = 1/3) +
  theme_base() +
  facet_wrap(~ method)



