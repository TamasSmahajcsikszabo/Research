x = 6
ABV_data <- read.xlsx("results.xlsx", sheetIndex = 1)
original_dataset <- tidied %>% 
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted) %>% 
  left_join(ABV_data, by = c(felvetel.eve = "felvetel.eve", Subject = "Subject")) %>% 
  select(-c(11:19))%>% 
  rename(NegA = 'NA.',
         PosA = PA) %>% 
  mutate(group = if_else(Ruminacio <= summary(Ruminacio)[[2]], "Alacsonyan ruminál",
                         if_else(Ruminacio >= summary(Ruminacio)[[5]], "Magasan ruminál", "Átlagos rumináló")),
         groupN = if_else(NegA <= summary(NegA)[[2]], "Alacsony NegA",
                         if_else(NegA >= summary(NegA)[[5]], "Magas NegA", "Átlagos NegA"))) 

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

library(viridis)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")

plot_data <- ICVC %>%
  filter(!is.na(group),
         !is.na(ABV)) %>%
  ungroup() %>% 
  mutate(sub = factor(Subject, levels=unique(rev(Subject)))) %>%
  mutate(Emotion = fct_recode(Emotion,
                              "boldog" = "happy",
                              "harag" = "angry",
                              "szomorú" = "sad")) %>% 
  arrange(sub, bin, Emotion, group, groupN)

plot <- ggplot(plot_data, aes(bin, ABV, group = interaction(sub, Emotion, group))) +
  geom_path(aes(color = ABV), alpha = 1/3, size = 1, na.rm = TRUE) +
  geom_point(aes(color = ABV), alpha = 1/2, size = 4) +
  facet_grid(~group~Emotion)+
  theme_minimal()+
  viridis::scale_color_viridis(option = "D") +
  labs(
    title = "A magasan ruminálók figyelmi varianciája alacsonyabb szomorú érzelmi ingerekre",
    x = "Bin sorszáma (1 bin = 6 egymást követő próba)",
    y = "ABV-index",
    caption = "A grafikon az eredeti adatok felhasználásával készült; a ruminálók csoportjait a 25. és 75. quartilisek értékei alapján képeztük"
  )+
  theme(text = element_text(family = "Garamond", size = 18))
ggsave("rumi.png", plot, device = "png", dpi = 400, width = 15, height = 12)
img_path = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/rumi.png"

plot2 <- ggplot(plot_data, aes(bin, ABV, group = Emotion)) +
  geom_smooth(aes(color = Emotion, fill = Emotion), size = 1) +
  facet_wrap(~group, ncol = 3)+
  theme_minimal()+
  viridis::scale_color_viridis(option = "D", discrete = TRUE) +
  viridis::scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(
    title = "A magasan ruminálók kezdetben magas figyelmi varianciát mutatnak",
    subtitle = "A szomorú ingerekre a vizsgálat utolsó próbáiban ismét megemelkedik a figyelmi variancia",
    x = "Bin sorszáma (1 bin = 6 egymást követő próba)",
    y = "ABV-index",
    caption = "A grafikon az eredeti adatok felhasználásával készült; a ruminálók csoportjait a 25. és 75. quartilisek értékei alapján képeztük"
  )+
  theme(text = element_text(family = "Garamond", size = 22))
ggsave("rumi_gam.png", plot2, device = "png", dpi = 400, width =24, height = 8)
img_path_2 = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/rumi_gam.png"

plot_data_mod <- plot_data %>% 
  group_by(bin, Emotion, group) %>% 
  summarise(ABV_win = WRS::winmean(ABV, tr = 0.25))
plot3 <- ggplot(plot_data_mod, aes(bin, ABV_win, group = interaction(Emotion, group))) +
  geom_path(aes(linetype = group, color = Emotion),size = 1.2) +
  theme_minimal()+
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "A magasan ruminálók kezdetben magas figyelmi varianciát mutatnak",
    subtitle = "A szomorú ingerekre a vizsgálat utolsó próbáiban ismét megemelkedik a figyelmi variancia",
    x = "Bin sorszáma (1 bin = 6 egymást követő próba)",
    y = "ABV-index",
    caption = "A grafikon az eredeti adatok felhasználásával készült; a ruminálók csoportjait a 25. és 75. quartilisek értékei alapján képeztük"
  )+
  theme(text = element_text(family = "Garamond", size = 22))
ggsave("rumi_win.png", plot3, device = "png", dpi = 400, width =24, height = 8)
img_path_3 = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/rumi_win.png"


PANAS <- original_dataset %>% select(felvetel.eve, Subject, PosA, NegA) %>% ungroup() %>% 
  group_by(felvetel.eve, Subject) %>% 
  summarise(NP = sum(NegA)/sum(PosA))
plot_data_PANAS <- plot_data %>% left_join(PANAS, by = c(felvetel.eve = "felvetel.eve", Subject = "Subject"))

plot_NP <- plot_data_PANAS %>% 
  ggplot(aes(bin, ABV, group = interaction(sub, Emotion, group))) + 
  geom_point(aes(color = ABV, size = NP, alpha = ABV)) +
  geom_path(aes(color = ABV, alpha = ABV)) +
  theme_minimal() +
  viridis::scale_color_viridis(option = "D") +
  facet_grid(~group~Emotion)+
  labs(
    title = "A magasan ruminálók figyelmi varianciája alacsonyabb szomorú érzelmi ingerekre",
    x = "Bin sorszáma (1 bin = 6 egymást követő próba)",
    y = "ABV-index",
    caption = "A grafikon az eredeti adatok felhasználásával készült; a ruminálók csoportjait a 25. és 75. quartilisek értékei alapján képeztük"
  )+
  theme(text = element_text(family = "Garamond", size = 18))
ggsave("rumi_np.png", plot_NP, device = "png", dpi = 400, width = 15, height = 12)
img_path = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/rumi_np.png"
                                           