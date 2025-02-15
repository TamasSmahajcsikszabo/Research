library(tidyverse)
library(readxl)
library(extrafont)
extrafont::loadfonts()

source('~/Documents/REPO/kitin/functions_dev.R')

data <- read_xlsx("FaceDot2015_16_Tamasnak.xlsx", sheet = 1)
data <- data[, c(1:116)]

### emotions:  
emotions <- read_xlsx("probak_arcos_dotprobe.xlsx", sheet = 1)
emotions_adjusted <- emotions %>% dplyr::select(TRIAL, Emotion) %>% 
  mutate(TRIAL = paste("X", row_number(), sep="")) %>% 
  rename("trial" = "TRIAL") %>% 
  mutate(Emotion = fct_recode(Emotion,
                              "sad" = "Sad"))

### defining to be exlcuded inaccurate datasets:
excluded_records <- c(300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 
                      106, 109, 111, 119, 131, 159, 172, 438, 444, 121, 526, 555, 558, 572, 654, 677, 729, 812, 855, 856)

data_cleaned <- data %>% filter(!Subject %in% excluded_records)

tidied <- data_cleaned %>%
  gather(3:116, key="trial", value="RT") %>% 
  dplyr::select(`felvetel eve`, Subject, trial, RT) 


winsorized <-data.frame(RT_win = winsorize(tidied$RT))


# Win plot ----------------------------------------------------------------

winsorized_data <- tidied %>% 
  mutate(`Winsorizálás` = ifelse(RT >= max(winsorized$RT) | RT <= min(winsorized$RT), "Winsorizált", "Változatlan"))

# base histogram
winsorized_data %>% 
  ggplot(aes(RT))  +
  geom_histogram(aes(fill = `Winsorizálás`), binwidth = 2, alpha = 1/2) + 
  theme_grey() + 
  labs(title = "Érzelmi Arcok Dot-Probe Teszt reakcióidők",
       subtitle = "20% winsorizálás mellett az érintett adat",
       x = "Reakcióidő (ms)",
       y = "Gyakoriság",
       caption = "A reakcióidőket 1500 ms alá szűrtük a láthatóság végett") +
  xlim(0,1500) +
  theme(panel.grid = element_blank(),
        text = element_text(size = 17, family = "Garamond", face = "bold"),
        legend.position = "bottom") + 
  scale_fill_manual(values = c("cornflowerblue", "coral")) + 
  geom_text(aes(x = max(winsorized$RT_win)+50, y= 200, label = paste0(round(max(winsorized$RT_win)), " ms")), family = "Garamond", size = 4) +
  geom_text(aes(x = min(winsorized$RT_win)-80, y= 200, label = paste0(round(min(winsorized$RT_win)), " ms")), family = "Garamond", size = 4)


# SD plot -----------------------------------------------------------------

mean_RT <- mean(tidied$RT, na.rm = TRUE)
SD_RT <- sd(tidied$RT, na.rm = TRUE)

sd_data <- tidied %>% 
  mutate(`Szórással szűrt` = ifelse(RT >= mean_RT + 1 * SD_RT | RT <= mean_RT - 1 * SD_RT, "Szórással szűrt", "Változatlan"))

min_max_sd <- sd_data %>% 
  group_by(`Szórással szűrt`) %>% 
  summarise(min = min(RT),
            max = max(RT))

# base histogram
sd_data %>% 
  ggplot(aes(RT))  +
  geom_histogram(aes(fill = `Szórással szűrt`), binwidth = 2, alpha = 1/2) + 
  theme_grey() + 
  labs(title = "Érzelmi Arcok Dot-Probe Teszt reakcióidők",
       subtitle = "+/- 1 standard szórás mentén szűrt adat",
       x = "Reakcióidő (ms)",
       y = "Gyakoriság",
       caption = "A reakcióidőket 1500 ms alá szűrtük a láthatóság végett") +
  xlim(0,1500) +
  theme(panel.grid = element_blank(),
        text = element_text(size = 17, family = "Garamond", face = "bold"),
        legend.position = "bottom") + 
  scale_fill_manual(values = c("cornflowerblue", "coral")) + 
  geom_text(aes(x = as.numeric(min_max_sd[3,3]), y= 200, label = paste0(round(as.numeric(min_max_sd[3,3])), " ms")), family = "Garamond", size = 4) +
  geom_text(aes(x = as.numeric(min_max_sd[3,2]), y= 200, label = paste0(round(as.numeric(min_max_sd[3,2])), " ms")), family = "Garamond", size = 4)


# Sampling distributions -----
base_distribution <- tidied %>% filter(!is.na(RT))
base_distribution <- as.numeric(base_distribution$RT)
RT_win = winsorize(tidied$RT)
SD_filtered <- tidied %>% 
  mutate(`Szórással szűrt` = ifelse(RT >= mean_RT + 1 * SD_RT | RT <= mean_RT - 1 * SD_RT, "Szórással szűrt", "Változatlan")) %>% 
  filter(`Szórással szűrt` == "Változatlan")
SD_filtered <- as.numeric(SD_filtered$RT)
## base mean bootstrap
boot_base_mean <- bootstrap2(base_distribution, fun = "mean", percentile = FALSE, seed = FALSE, trimming = FALSE)
boot_trimmed_mean <- bootstrap2(base_distribution, fun = "mean", percentile = FALSE, seed = FALSE, trimming = TRUE)
boot_winsorized_mean <- bootstrap2(RT_win, fun = "mean", percentile = FALSE, seed = FALSE, trimming = FALSE)
boot_base_median <- bootstrap2(base_distribution, fun = "median", percentile = FALSE, seed = FALSE, trimming = FALSE)
boot_SD_filtered_mean <- bootstrap2(SD_filtered, fun = "mean", percentile = FALSE, seed = FALSE, trimming = FALSE)

bootstrap_data <- data.frame(
  "alapeloszlás" = boot_base_mean,
  "trimmelt" = boot_trimmed_mean,
  "winsorizált" = boot_winsorized_mean,
  "szórással szűrt" = boot_SD_filtered_mean
) %>% 
  gather(1:4, key = "MoL", value = "est")

ggplot(bootstrap_data) +
  geom_density(aes(x = est, group = MoL, color = MoL, linetype = MoL, fill = MoL), alpha = 1/3) +
  theme_grey() + 
  labs(x = "Lokáció (átlag)",
       y = "Denzitás",
       title = "Bootstrap eloszlások átlagra, trimmelt átlagra, winsorizált átlagra") +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, family = "Garamond")) + 
  scale_fill_manual(values = c("coral", "cornflowerblue", "coral4", "lawngreen")) +
  scale_color_manual(values = c("coral", "cornflowerblue", "coral4", "lawngreen"))


# Relationship ------------------------------------------------------------

other <- read_xlsx("Kerdoives_FaceDot2015_16_Tamasnak.xlsx", sheet = 1)
RT <- tidied
trial_types <- read.delim("trial_types.txt")
type_names <- tribble(
  ~type, ~name,
  "RpLe","Incongruent",
  "RpRe","Congruent",
  "LpRe","Incongruent",
  "LpLe","Congruent"
)

#merging trial types with codenames
trial_types <- trial_types %>%
  left_join(type_names, by=c(Type="type"))

#creating the proper trial codenames
trial_types <- trial_types %>%
  mutate(trial = paste(row_number(), sep="")) %>%
  dplyr::select(trial, Type, name) %>%
  rename(type = Type)
RT <- RT %>% left_join(trial_types) %>% 
  rename(year = `felvetel eve`)
  # REAC functions
source('~/Documents/REPO/REAC/R/functions.R')

original_dataset_abv <- ABV(RT,
                              grouping = c("year", "Subject"), 
                              trial = "trial", 
                              bin_width = 29, 
                              value = "RT",
                              type = c("Incongruent", "Congruent"), 
                              ID = "Subject")
