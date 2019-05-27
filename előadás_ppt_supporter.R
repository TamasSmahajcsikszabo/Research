library(flexdashboard)
library(xlsx)
library(tidyverse)
library(forcats)
library(lazyeval)
library(ggthemes)
library(plotly)
library(rbokeh)
library(viridis)
library(e1071)

# Data Import -------------------------------------------------------------
### reading data:
thresholded <- read.csv("thresholded.csv")
sd_filtered <- read.csv("SD_filtered.csv")
winsorized <- read.csv("winsorized_for_analysis.csv")
tidied <- read.csv("original_dataset.csv")

tidy_data <- function(data, ...){
  require(tidyverse)
  col = c(...)
  indices <- vector("integer", length(col))
  names <-  as_tibble(names(data)) %>% 
    mutate(index = row_number())
  
  for (i in seq_along(col)) {
    indices[[i]] <- names$index[[i]]
  }
  
  selection <- colnames(data[-indices])
  
  tidied <- data %>%
    gather(selection, key="trial", value="RT") %>% 
    as_data_frame()
  
  tidied = as.data.frame(tidied) %>% distinct()
  tidied
  
}
ABV <- function (data, x) {
  ICVC <- data %>%
    mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
    mutate(trial = as.integer(trial)) %>%
    arrange(felvetel.eve, Subject, trial) %>%
    group_by(felvetel.eve, Subject) %>%
    mutate(index = row_number()) %>%
    mutate(bin = (index %/% x)+1) %>%
    ungroup() %>%
    group_by(Subject, bin, Emotion, label) %>%
    summarise(mean_RT = mean(RT, na.rm=TRUE)) %>%
    spread(4:5, key=label, value=mean_RT) %>%
    mutate(ICVC = Incongruent - Congruent)
  
  SD <- ICVC %>%
    group_by(Subject, Emotion) %>%   
    summarise(SD = sd(ICVC))
  
  ICVC <- 
    ICVC %>% left_join(SD, by=c(Subject = "Subject", Emotion = "Emotion"))  
  
  mean_rt <- data %>%
    mutate(Subject = as.character(Subject)) %>%
    mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
    mutate(trial = as.integer(trial)) %>%
    arrange(felvetel.eve, Subject, trial) %>%
    group_by(felvetel.eve, Subject) %>%
    mutate(index = row_number()) %>%
    mutate(bin = (index %/% x)+1) %>%
    ungroup() %>% 
    group_by(Subject, bin, Emotion) %>%
    summarise(mean_RT = mean(RT, na.rm=TRUE))
  
  mean_rt <- mean_rt %>% ungroup() %>% 
    mutate(Subject = as.integer(Subject))
  
  ICVC <- ICVC %>%
    left_join(mean_rt, by=c(Subject = "Subject", bin = "bin", Emotion = "Emotion")) %>%
    mutate(SD = as.double(SD)) %>%
    mutate(ABV = SD / mean_RT)
  
  ICVC
}
winsorize <- function(data, x, y, z, ...){
  gc()
  grouping = c(...)
  winsorized <- data %>%
    group_by_(grouping) %>%
    mutate("25th" = quantile(RT, c(x), na.rm=TRUE),
           "75th" = quantile(RT, c(y), na.rm=TRUE),
           IQR = z * IQR(RT, na.rm=TRUE)) %>%
    mutate(
      upper = `75th` + IQR,
      lower = `25th` - IQR
    ) %>%
    mutate(
      above_upper = if_else(RT > upper, "above", "normal"),
      below_lower = if_else(RT < lower, "below", "normal")) %>%
    ungroup()
  
  ranked_upper <- winsorized %>%
    filter(above_upper == "normal") %>%
    group_by_(grouping) %>%
    arrange(desc(RT)) %>%
    top_n(1, RT) %>%
    rename(upper_replacer = RT) %>%
    select(grouping, upper_replacer) %>%
    ungroup()
  
  ranked_lower <- winsorized %>%
    filter(below_lower == "normal") %>%
    group_by_(grouping) %>%
    arrange((RT)) %>%
    top_n(-1, RT) %>%
    rename(lower_replacer = RT) %>%
    select(grouping, lower_replacer) %>%
    ungroup()
  
  winsorized_for_analysis <- 
    winsorized %>%
    select(grouping, trial, RT) %>%
    spread(key=trial, value=RT ) %>%
    left_join(ranked_upper) %>%
    left_join(ranked_lower)
  
  winsorized_for_analysis <- winsorized_for_analysis %>%
    distinct() %>%
    gather(3:116, key=trial, value=RT) %>%
    mutate(RT = if_else(RT>upper_replacer, upper_replacer, RT),
           RT = if_else(RT<lower_replacer, lower_replacer, RT)) %>%
    select(grouping, trial, RT) %>%
    spread(key=trial, value=RT) 
}
threshold <- function(data, ...) {
  lower_threshold <- c(...)
  upper_threshold <- c(...)
  
  thresholded <- data %>%
    filter(!RT %in% lower_threshold) %>%
    filter(!RT %in% upper_threshold)
  
  thresholded <- thresholded %>%
    spread(key=trial, value=RT )
}
SD_filter <- function(data, x, ...) {
  grouping = c(...)
  
  SD <- data %>%
    group_by_(grouping) %>%
    mutate(SD = sd(RT, na.rm=TRUE)) %>%
    ungroup() %>%
    spread(key=trial, value=RT )
  
  data_for_analysis <- data %>%
    spread(key=trial, value=RT) %>% 
    left_join(SD) %>% as_tibble()
  
  SD_filtered <- data_for_analysis %>%
    gather(3:116, key="trial", value="RT") %>%
    filter(!RT > x*SD | !RT < -x*SD) %>% 
    spread(key=trial, value=RT ) %>%
    as_tibble() %>% 
    select(-SD)
}
export <- function(data, filename, path, format) {
  if (format == "excel") {
    write.xlsx(data, filename, path, col.names =TRUE, row.names=FALSE)
  } else if (format == "csv") {
    write.csv(data, filename, path)
  } else {}
}

wins_sum <- summary(winsorized$RT) %>% as.matrix() %>% as.data.frame()
wins_sum <- wins_sum %>% mutate(variable = row.names(wins_sum)) %>% select(variable, V1) %>% spread(variable, V1)

SD_sum <- summary(sd_filtered$RT) %>% as.matrix()%>% as.data.frame()
SD_sum <- SD_sum %>% mutate(variable = row.names(SD_sum)) %>% select(variable, V1) %>% spread(variable, V1)

thres_sum <- summary(thresholded$RT) %>% as.matrix() %>% as.data.frame()
thres_sum <- thres_sum %>% mutate(variable = row.names(thres_sum)) %>% select(variable, V1) %>% spread(variable, V1)

trial_types <- read.delim("trial_types.txt") %>% 
  rename(trial = Trial)
type_names <- tribble(
  ~type, ~name,
  "RpLe","Incongruent",
  "RpRe","Congruent",
  "LpRe","Incongruent",
  "LpLe","Congruent"
)

emotions <- read.xlsx("probak_arcos_dotprobe.xlsx", sheetIndex = 1)
emotions_adjusted <- emotions %>% select(TRIAL, Emotion) %>% 
  mutate(TRIAL = paste("X", row_number(), sep="")) %>% 
  rename("trial" = "TRIAL") %>% 
  mutate(Emotion = fct_recode(Emotion,
                              "sad" = "Sad"))%>% 
  mutate(trial = as.numeric(str_sub(trial, start = 2, end = length(trial))))

### original data histogram
original <- tidied %>% filter(!is.na(RT))
library(extrafont)
loadfonts(device = "win")

par <- retimes::mexgauss(original$RT) %>% as.numeric()
mean = round(mean(original$RT, na.rm = TRUE),digits = 2)
SD = round(sd(original$RT, na.rm = TRUE), digits = 2)
tau = round(par[[3]], digits=2)

plot <- original %>% 
  filter(RT < 2500) %>% 
  ggplot(aes(RT)) +
  geom_histogram(fill = "white", color = "grey30", binwidth = 10) + 
  theme_minimal() +
  labs(
    title = paste("A reakcióidő a mintán tipikus ex-Gaussian eloszlást mutat"),
    x = "Reakcióidő (ms)",
    y = "Gyakoriság (n)",
    caption = "A jobb láthatóság végett a reakcióidő 2500 ms alá szűrve értendő"
  ) + 
  geom_segment(x = 200, y = 2200, xend = 2500, yend=2200, linetype = 3, colour = "navyblue", size = 1) + 
  geom_segment(x = mean, y = 2150, yend = 2250, xend = mean, colour = "navyblue", size = 2) +
  geom_text(aes(x = mean + 200, y = 2100, label = paste0("µ = ", mean, " ms")), colour = "navyblue") +
  geom_segment(x =200, y = 2150, yend = 2250, xend = 200, colour = "navyblue", size = 2) +
  geom_segment(x =2500, y = 2150, yend = 2250, xend = 2500, colour = "navyblue", size = 2) +
  geom_text(x = mean + 200, y = 1900, label = paste0("σ = ", SD, " ms"), color = "navyblue") +
  geom_text(x = 1500, y = 2100, label = paste0("τ = ", tau," ms"), colour = "navyblue") +
  theme(text = element_text(family = "Garamond", size = 14))
ggsave("original_histo.png", plot, device = "png", dpi = 400, width = 8, height = 6)

 
thresholded_prepared <- thresholded %>% mutate(method = "Köszöbértékkel szűrt adatok")
SD_filtered_prepared <- sd_filtered %>% mutate(method = "+/- 3 szórással szűrt adatok")
winsorized_prepared <- winsorized %>% mutate(method = "Winsorizált adatok")

all_data_prepared <- bind_rows(thresholded_prepared, SD_filtered_prepared, winsorized_prepared)
plot_2 <- all_data_prepared %>%
  filter(RT<2000) %>% 
  ggplot(aes(RT)) +
  geom_histogram(aes(fill = method), color = "grey40", binwidth = 10, show.legend = FALSE) + 
  theme_minimal() +
  labs(
    title = paste("Az eloszlás jellegét leginkább a winsorizálás változtatja meg"),
    x = "Reakcióidő (ms)",
    y = "Gyakoriság (n)",
    caption = "A jobb láthatóság végett a reakcióidő 2000 ms alá szűrve értendő"
  ) + 
  viridis::scale_fill_viridis(option = "D", discrete = TRUE) +
  facet_wrap(~ method, ncol = 3, scales = "free_x")+
  theme(text = element_text(family = "Garamond", size = 18))
ggsave("modified_histo.png", plot_2, device = "png", dpi = 600, width = 18, height = 6)

datasets <- c("original", "winsorized_data", "SD_filtered_data", "thresholded_data") %>% as_tibble() %>% 
  rename(names = value)

original_sum <- summary(tidied$RT)
thres_sum <- summary(thresholded$RT)
SD_sum <- summary(sd_filtered$RT)
wins_sum <- summary(winsorized$RT)

original_n <- tidied %>% filter(!is.na(RT)) %>% summarise(n = n()) %>% mutate(names = "original")
winsorized_data_n <- winsorized %>% filter(!is.na(RT)) %>% summarise(n = n(),
                                                                     delta = original_n$n - n,
                                                                     data_loss_ratio = paste0(round((delta/n)*100, digits=2),"%")) %>% mutate(names = "winsorized_data")
SD_filtered_data_n <- sd_filtered %>% filter(!is.na(RT)) %>% summarise(n = n(), 
                                                                       delta = original_n$n - n,
                                                                       data_loss_ratio = paste0(round((delta/n)*100, digits=2),"%")) %>% mutate(names = "SD_filtered_data")
thresholded_data_n <- thresholded %>% filter(!is.na(RT)) %>% summarise(n = n(),
                                                                       delta = original_n$n - n,
                                                                       data_loss_ratio = paste0(round((delta/n)*100, digits=2),"%")) %>% mutate(names = "thresholded_data")

n_summary <- bind_rows(original_n, winsorized_data_n, SD_filtered_data_n, thresholded_data_n)

summary <- bind_rows(original_sum, thres_sum, SD_sum, wins_sum)
summary <- bind_cols(summary,datasets) %>% 
  select(names, names(summary))

summary_detail <- summary %>% left_join(n_summary) %>% 
  mutate(Mean = round(Mean, digits = 2)) %>% 
  rename("Különbség" = delta,
         "Elemszám" = n,
         "Adatveszteség (%)" = data_loss_ratio)
write.xlsx(summary_detail, "summary_detail.xlsx")


library(caret)
library(AppliedPredictiveModeling)
RT_tidied <- tidied$RT
original_skew <- round(skewness(RT_tidied, na.rm = TRUE), digits = 2) %>% as_tibble() %>% mutate(adat = "eredeti")

RT_thresh <- thresholded$RT
thresh_skew <- round(skewness(RT_thresh, na.rm = TRUE), digits = 2) %>% as_tibble() %>% mutate(adat = "küszöbbel szűrt")

RT_SD <- sd_filtered$RT
SD_skew <- round(skewness(RT_SD, na.rm = TRUE), digits = 2) %>% as_tibble() %>% mutate(adat = "szórással szűrt")

RT_win <- winsorized$RT
win_skew <- round(skewness(RT_win, na.rm = TRUE), digits = 2) %>% as_tibble() %>% mutate(adat = "winsorizált")

skewness_summary <- tribble(
  ~"adat", ~"ferdeség",
  "eredeti", round(skewness(RT_tidied, na.rm = TRUE), digits = 2),
  "küszöbbel szűrt",round(skewness(RT_thresh, na.rm = TRUE), digits = 2),
  "szórással szűrt",round(skewness(RT_SD, na.rm = TRUE), digits = 2) ,
  "winsorizált",round(skewness(RT_win, na.rm = TRUE), digits = 2)
)

DT::datatable(skewness_summary)
write.xlsx(skewness_summary, "skewness_summary.xlsx")


library(viridis)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")

# Original data -----------------------------------------------------------


x = 6
ABV_data <- read.xlsx("results.xlsx", sheetIndex = 1)
original_dataset <- tidied %>% 
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted) %>% 
  left_join(ABV_data, by = c(felvetel.eve = "felvetel.eve", Subject = "Subject")) %>% 
  #select(-c(11:19))%>% 
  rename(NegA = 'NA.',
         PosA = PA)
original_dataset <- original_dataset %>% 
  filter(!is.na(Ruminacio)) %>% 
  mutate(group = if_else(Ruminacio <= (WRS::win(original_dataset$Ruminacio, tr = .25) - sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))), "Alacsony",
                         if_else(Ruminacio > (WRS::win(original_dataset$Ruminacio, tr = .25) + sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))), "Magas", 
                                 if_else(Ruminacio <= WRS::win(original_dataset$Ruminacio, tr = .25) & Ruminacio > (WRS::win(original_dataset$Ruminacio, tr = .25) - sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))), "Win. Átlag alatt",
                                         if_else(Ruminacio <= (WRS::win(original_dataset$Ruminacio, tr = .25) + sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))) & Ruminacio > WRS::win(original_dataset$Ruminacio, tr = .25), "Win. Átlag felett","")))),
         groupN = if_else(NegA <= summary(NegA)[[2]], "Alacsony NegA",
                          if_else(NegA >= summary(NegA)[[5]], "Magas NegA", "Átlagos NegA"))) %>% 
  mutate(group = factor(group, levels = unique(rev(group)))) %>% 
  mutate(group = fct_relevel(group, "Alacsony", "Win. Átlag alatt", "Win. Átlag felett", "Magas"))


ICVC <- original_dataset %>%
  mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(felvetel.eve, Subject, group, groupN, trial) %>%
  group_by(felvetel.eve, Subject) %>%
  mutate(index = row_number()) %>%
  mutate(bin = (index %/% x)+1) %>%
  ungroup() %>%
  group_by(felvetel.eve, Subject, bin, Emotion, group,groupN, label) %>%
  summarise(mean_RT = mean(RT, na.rm=TRUE)) %>%
  spread(4:5, key=label, value=mean_RT) %>%
  mutate(ICVC = Incongruent - Congruent)

SD <- ICVC %>%
  group_by(felvetel.eve, Subject, Emotion, group, groupN) %>%   
  summarise(SD = sd(ICVC))

ICVC <- 
  ICVC %>% left_join(SD, by=c(felvetel.eve = "felvetel.eve", Subject = "Subject", Emotion = "Emotion", group = "group", groupN ="groupN"))  

mean_rt <- original_dataset  %>%
  mutate(Subject = as.character(Subject)) %>%
  mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(felvetel.eve, Subject, trial) %>%
  group_by(felvetel.eve, Subject, group, groupN) %>%
  mutate(index = row_number()) %>%
  mutate(bin = (index %/% x)+1) %>%
  ungroup() %>% 
  group_by(felvetel.eve, Subject, group,groupN, bin) %>%
  summarise(mean_RT = mean(RT, na.rm=TRUE))

mean_rt <- mean_rt %>% ungroup() %>% 
  mutate(Subject = as.integer(Subject))

ICVC <- ICVC %>%
  left_join(mean_rt, by=c(felvetel.eve = "felvetel.eve", Subject = "Subject", bin = "bin",  group = "group", groupN ="groupN")) %>%
  mutate(SD = as.double(SD)) %>%
  mutate(ABV = SD / mean_RT)


plot_data <- ICVC %>%
  filter(!is.na(group),
         !is.na(ABV)) %>%
  ungroup() %>% 
  mutate(sub = factor(Subject, levels=unique(rev(Subject)))) %>%
  mutate(Emotion = fct_recode(Emotion,
                              "boldog" = "happy",
                              "haragvó" = "angry",
                              "szomorú" = "sad")) %>% 
  arrange(sub, bin, Emotion, group, groupN)

PANAS <- original_dataset %>% select(felvetel.eve, Subject, PosA, NegA) %>% ungroup() %>% 
  group_by(felvetel.eve, Subject) %>% 
  summarise(NP = sum(NegA)/sum(PosA))
plot_data_PANAS <- plot_data %>% left_join(PANAS, by = c(felvetel.eve = "felvetel.eve", Subject = "Subject")) %>% 
  arrange(Subject, bin)




plot_NP <- plot_data_PANAS %>% 
  arrange(bin) %>% 
  ggplot(aes(bin, ABV, group = interaction(sub, Emotion, group))) + 
  geom_point(aes(color = ABV, size = NP, alpha = ABV)) +
  geom_path(aes(color = ABV, alpha = ABV)) +
  theme_minimal() +
  viridis::scale_color_viridis(option = "D", direction = -1) +
  facet_grid(~group~Emotion)+
  labs(
    title = "A magasan ruminálók figyelmi varianciája alacsonyabb szomorú érzelmi ingerekre",
    x = "Bin sorszáma (1 bin = 6 egymást követő próba)",
    y = "ABV-index",
    caption = "A grafikon az eredeti adatok felhasználásával készült; a ruminálók csoportjait a winsorizált átlag és szórás értékei alapján képeztük"
  )+
  theme(text = element_text(family = "Garamond", size = 18))

ggsave("rumi_np.png", plot_NP, device = "png", dpi = 400, width = 14, height =8)


### histrogram for ABV

x = 6
ABV_data <- read.xlsx("results.xlsx", sheetIndex = 1)
original_dataset <- tidied %>% 
  mutate(trial = as.numeric(str_sub(trial, start = 2, end = length(trial)))) %>% 
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted) %>% 
  left_join(ABV_data, by = c(felvetel.eve = "felvetel.eve", Subject = "Subject"))
original_dataset <- original_dataset %>% 
  filter(!is.na(Ruminacio)) %>% 
  mutate(group = if_else(Ruminacio <= (WRS::win(original_dataset$Ruminacio, tr = .25) - sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))), "Alacsony",
                         if_else(Ruminacio > (WRS::win(original_dataset$Ruminacio, tr = .25) + sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))), "Magas", 
                                 if_else(Ruminacio <= WRS::win(original_dataset$Ruminacio, tr = .25) & Ruminacio > (WRS::win(original_dataset$Ruminacio, tr = .25) - sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))), "Win. Átlag alatt",
                                         if_else(Ruminacio <= (WRS::win(original_dataset$Ruminacio, tr = .25) + sqrt(WRS::winvar(original_dataset$Ruminacio, tr = .25))) & Ruminacio > WRS::win(original_dataset$Ruminacio, tr = .25), "Win. Átlag felett",""))))) %>% 
  mutate(group = factor(group, levels = unique(rev(group)))) %>% 
  mutate(group = fct_relevel(group, "Alacsony", "Win. Átlag alatt", "Win. Átlag felett", "Magas"))


ICVC <- original_dataset %>%
  mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(felvetel.eve, Subject, group,trial) %>%
  group_by(felvetel.eve, Subject) %>%
  mutate(index = row_number()) %>%
  mutate(bin = (index %/% x)+1) %>%
  ungroup() %>%
  group_by(felvetel.eve, Subject, bin, Emotion, group, label) %>%
  summarise(mean_RT = mean(RT, na.rm=TRUE)) %>%
  spread(4:5, key=label, value=mean_RT) %>%
  mutate(ICVC = Incongruent - Congruent)

SD <- ICVC %>%
  group_by(felvetel.eve, Subject, Emotion, group) %>%   
  summarise(SD = sd(ICVC))

ICVC <- 
  ICVC %>% left_join(SD, by=c(felvetel.eve = "felvetel.eve", Subject = "Subject", Emotion = "Emotion", group = "group"))  

mean_rt <- original_dataset  %>%
  mutate(Subject = as.character(Subject)) %>%
  mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(felvetel.eve, Subject, trial) %>%
  group_by(felvetel.eve, Subject, group) %>%
  mutate(index = row_number()) %>%
  mutate(bin = (index %/% x)+1) %>%
  ungroup() %>% 
  group_by(felvetel.eve, Subject, group, bin) %>%
  summarise(mean_RT = mean(RT, na.rm=TRUE))

mean_rt <- mean_rt %>% ungroup() %>% 
  mutate(Subject = as.integer(Subject))

ICVC2 <- ICVC %>%
  left_join(mean_rt, by=c(felvetel.eve = "felvetel.eve", Subject = "Subject", bin = "bin",  group = "group")) %>%
  mutate(SD = as.double(SD)) %>%
  mutate(ABV = SD / mean_RT)

plot_3 <- ICVC2  %>% 
  ggplot(aes(ABV)) +
  geom_freqpoly(aes(color = interaction(Emotion, group), group = interaction(Emotion, group)), show.legend = FALSE, size = 1) + 
  geom_segment(x = round(WRS::win(ICVC2$ABV, tr = .25),  digits = 2), xend= round(WRS::win(ICVC2$ABV, tr = 0.25), digits=2), y = -10, yend=750, color = "navyblue", size = 1, linetype = 3) + 
  theme_minimal() +
  labs(
    title = paste("A rumináció skálán legmagasabb csoportba esők csökkent ABV-indexe figyelhető meg"),
    x = "ABV-index",
    y = "Gyakoriság (n)",
    caption = "Pontos kék vonallal a winsorizált átlagot jelöltük 25%-os gamma mellett"
  ) + 
  viridis::scale_color_viridis(option = "D", discrete = TRUE) +
  facet_grid(~ Emotion~group, scales = "free_x")+
  theme(text = element_text(family = "Garamond", size = 18))
ggsave("rumi_freqploy.png", plot_3, device = "png", dpi = 400, width = 14, height =8)
