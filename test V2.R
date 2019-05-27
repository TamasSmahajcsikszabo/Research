
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
ggsave("rumi.png", plot, device = "png", dpi = 400, width = 10, height = 6)
img_path = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/rumi.png"


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
  theme(text = element_text(family = "Garamond", size = 16))
ggsave("rumi_np.png", plot_NP, device = "png", dpi = 400, width = 10, height =8)
img_path = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/rumi_np.png"
 
 
 original_dataset %>% 
   mutate(trial = as.numeric(str_sub(trial, start = 2, end = length(trial)))) %>% 
   group_by(trial, label, Emotion, group) %>% 
   summarise(RT = WRS::winmean(RT, tr=0.25)) %>% 
   filter(!is.na(group)) %>% 
   ggplot(aes(trial, RT, group = interaction(Emotion))) +
   geom_smooth(aes(color = Emotion), se = FALSE, size = 2) + 
   facet_grid(~label~group, scales = "free_x") +
   theme_minimal() +
   viridis::scale_colour_viridis(option = "D", discrete = TRUE) +
   modelr::geom_ref_line(h = 400, colour = "red", size = 1)


# Threshold + Winsorizing -------------------------------------------------
 x = 6
 ABV_data <- read.xlsx("results.xlsx", sheetIndex = 1)
 winsorized <-  SD_filtered %>% 
   #group_by(felvetel.eve, Subject) %>%
   mutate("25th" = quantile(RT, c(.25), na.rm=TRUE),
          "75th" = quantile(RT, c(.75), na.rm=TRUE),
          IQR = 1.5 * IQR(RT, na.rm=TRUE)) %>%
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
   group_by(felvetel.eve, Subject) %>%
   arrange(desc(RT)) %>%
   top_n(1, RT) %>%
   rename(upper_replacer = RT) %>%
   select(felvetel.eve, Subject, upper_replacer) %>%
   ungroup()
 
 ranked_lower <- winsorized %>%
   filter(below_lower == "normal") %>%
   group_by(felvetel.eve, Subject) %>%
   arrange((RT)) %>%
   top_n(-1, RT) %>%
   rename(lower_replacer = RT) %>%
   select(felvetel.eve, Subject, lower_replacer) %>%
   ungroup()
 
 winsorized_for_analysis <- 
   winsorized %>%
   select(felvetel.eve, Subject, trial, RT) %>%
   spread(key=trial, value=RT ) %>%
   left_join(ranked_upper) %>%
   left_join(ranked_lower)
 
 winsorized_for_analysis <- winsorized_for_analysis %>%
   distinct() %>%
   gather(3:116, key=trial, value=RT) %>%
   mutate(RT = if_else(RT>upper_replacer, upper_replacer, RT),
          RT = if_else(RT<lower_replacer, lower_replacer, RT)) %>%
   select(felvetel.eve, Subject, trial, RT)
   
 original_dataset <- SD_filtered %>% 
   #left_join(trial_types, by=c(trial = "trial")) %>% 
   #left_join(emotions_adjusted) %>% 
   left_join(ABV_data, by = c(felvetel.eve = "felvetel.eve", Subject = "Subject")) %>% 
   rename(NegA = 'NA.',
          PosA = PA) %>% 
   mutate(group = if_else(Ruminacio <= summary(Ruminacio)[[2]], "Alacsony",
                          if_else(Ruminacio > summary(Ruminacio)[[5]], "Magas", 
                                  if_else(Ruminacio <= summary(Ruminacio)[[3]] & Ruminacio > summary(Ruminacio)[[2]], "Átlag alatt",
                                          if_else(Ruminacio <= summary(Ruminacio)[[5]] & Ruminacio > summary(Ruminacio)[[3]], "Átlag felett","")))),
          groupN = if_else(NegA <= summary(NegA)[[2]], "Alacsony NegA",
                           if_else(NegA >= summary(NegA)[[5]], "Magas NegA", "Átlagos NegA"))) %>% 
   mutate(group = factor(group, levels = unique(rev(group)))) %>% 
   mutate(group = fct_relevel(group, "Alacsony", "Átlag alatt", "Átlag felett", "Magas"))
 
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
                               "harag" = "angry",
                               "szomorú" = "sad")) %>% 
   arrange(sub, bin, Emotion, group, groupN)
 
 PANAS <- original_dataset %>% select(felvetel.eve, Subject, PosA, NegA) %>% ungroup() %>% 
   group_by(felvetel.eve, Subject) %>% 
   summarise(NP = sum(NegA)/sum(PosA))
 plot_data_PANAS <- plot_data %>% left_join(PANAS, by = c(felvetel.eve = "felvetel.eve", Subject = "Subject")) 
 
 plot_NP_thres_wins <- plot_data_PANAS %>% 
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
     caption = "A grafikon az eredeti adatok felhasználásával készült; a ruminálók csoportjait a 25., 50. és 75. quartilisek értékei alapján képeztük"
   )+
   theme(text = element_text(family = "Garamond", size = 22))
 ggsave("rumi_np_wins.png",  plot_NP_thres_wins , device = "png", dpi = 400, width = 16, height = 12)
 img_path = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/rumi_np.png"
 
 

# Reaction time winsorized trends -----------------------------------------

 x = 6
reaction_time_trend <- original_dataset %>% 
   mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
   mutate(trial = as.integer(trial)) %>%
   arrange(felvetel.eve, Subject, group, groupN, trial) %>%
   group_by(felvetel.eve, Subject) %>%
   mutate(index = row_number()) %>%
   mutate(bin = (index %/% x)+1) %>%
   ungroup() %>% 
   select(-c(12:23))

 winsorized_timely_trend <- 
   reaction_time_trend %>% 
   group_by(bin, Emotion, label) %>% 
   summarise(win_mean_RT = WRS::win(RT, tr = .25)) %>% 
   ungroup()

 winsorized_timely_trend %>% 
   ggplot(aes(bin, win_mean_RT, group = label)) +
   geom_point() +
   geom_path()+
   modelr::geom_ref_line(h = WRS::win(RT, tr = .25), size = 1, colour = "navyblue") + 
   facet_grid(~Emotion~lable) +
   scale_x_continuous(breaks = seq(1,20, by = 1)) +
   viridis::scale_colour_viridis(option = "D", direction = -1, discrete = TRUE) +

   theme_minimal()

 

# Lag ---------------------------------------------------------------------

 lag_data <- original_dataset %>% 
   mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
   mutate(trial = as.integer(trial)) %>%
   arrange(felvetel.eve, Subject, group, groupN, trial) %>%
   group_by(Subject, felvetel.eve, Subject) %>%
   mutate(index = row_number()) %>%
   mutate(bin = (index %/% x)+1) %>%
   ungroup() %>% 
   arrange(Subject, trial) %>% 
   mutate(lag_trial = lag(RT, 1),
          RT_delta = RT- lag_trial) %>% 
   ungroup() %>% 
   group_by(bin, Emotion, group) %>% 
   summarise(RT_diff = WRS::win(RT_delta, tr = .25),
             NP = sum(NegA)/sum(PosA))
 
 lag_plot <- lag_data %>% 
   filter(!is.na(group)) %>% 
   ggplot(aes(bin, RT_diff, group = interaction(Emotion, group))) +
   geom_point(aes(size = NP, color = RT_diff), alpha = 1/1.5) +
   geom_path(colour = "black", alpha = 1/2) +
   facet_grid(~group~Emotion) +
   viridis::scale_colour_viridis(option = "D") + 
   modelr::geom_ref_line(h = 0, colour = "black", size = 0.5) +
   theme_minimal() +
   labs(
     title = "Az érzelmi ingerek tekintetében megfigyelhető mintázatok vannak a reakcióidő *lag* változásában",
     x = "Bin sorszáma (1 bin = 6 egymást követő próba)",
     y = "A reakcióidő *lag* winsorizált átlaga",
     caption = "A grafikon az eredeti adatok felhasználásával készült; a ruminálók csoportjait a winsorizált átlag és szórás értékei alapján képeztük"
   )+
   theme(text = element_text(family = "Garamond", size = 14))
 ggsave("lag.png", lag_plot, device = "png", dpi = 400, width = 10, height = 8)
 
 