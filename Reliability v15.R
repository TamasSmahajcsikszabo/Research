options(java.parameters = "-xmx6000m")

# Libraries ---------------------------------------------------------------
library(xlsx)
library(tidyverse)
library(ICC)
library(forcats)
library(lazyeval)

# Data Import -------------------------------------------------------------
setwd("C:\\OneDrive\\R Projects\\Kutatás")
### reading data:
data <- read.xlsx("FaceDot2015_16_Tamasnak.xlsx", sheetIndex = 1) 

### emotions:  
emotions <- read.xlsx("probak_arcos_dotprobe.xlsx", sheetIndex = 1)
emotions_adjusted <- emotions %>% select(TRIAL, Emotion) %>% 
  mutate(TRIAL = paste("X", row_number(), sep="")) %>% 
  rename("trial" = "TRIAL") %>% 
  mutate(Emotion = fct_recode(Emotion,
                            "sad" = "Sad"))

### defining to be exlcuded inaccurate datasets:
excluded_records <- c(300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 106, 109, 111, 119, 131, 159, 172, 438, 444, 526, 555, 558, 572, 654, 677, 729, 812, 855, 856)

### trimming off last two columns:
data <- data[,1:116]


# Data Cleansing ----------------------------------------------------------

#1. excluding blue lines
data_cleaned <- data %>% filter(!Subject %in% excluded_records)

#2. removal of first three trials of each block
###optional
#data_cleaned <- data_cleaned[,c(1,2,6:116)]

#3. tidying:
tidied <- data_cleaned %>%
  gather(3:116, key="trial", value="RT") %>% 
  select(felvetel.eve, Subject, trial, RT)
write.csv(tidied, "tidied.csv", row.names = FALSE)

# 1. RT threshold -----------------------------------------------
#declaring:
lower_threshold <- c(200:300)
upper_threshold <- c(2500:9000)

#filtering:
thresholded <- tidied %>%
  filter(!RT %in% lower_threshold & !RT %in% upper_threshold) %>% unique()

#reshaping the dataset into original form
thresholded <- thresholded %>%
  spread(key=trial, value=RT )

write.csv(thresholded, "RT_thresholded.csv", na="", row.names = FALSE)

# 2. SD ranges ---------------------------------------------------------------
#SD ranges
#calculating SD
SD <- tidied %>%
  group_by(felvetel.eve, Subject) %>%
  mutate(SD = sd(RT, na.rm=TRUE),
         mean = mean(RT, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key=trial, value=RT ) %>% unique()

#matching original dataset with SDs
data_for_analysis <- data_cleaned %>%
 left_join(SD) %>% as_tibble() %>% unique()

#definining SD ranges
SD_filtered <- data_for_analysis %>%
  gather(3:116, key="trial", value="RT") %>%
  filter(!RT > 3*SD + mean  & !RT < mean-3*SD) %>%
  spread(key=trial, value=RT ) %>%
  as_tibble() %>%
  select(-SD, -mean)

SD_filtered %>% count(trial, Subject, felvetel.eve, RT) %>% arrange(n)
write.csv(SD_filtered, "SD_filtered.csv", na="", row.names = FALSE) 

# 3. Winsorizing ------------------------------------------------------------
winsorized <- tidied %>%
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
  select(felvetel.eve, Subject, trial, RT) %>%
  spread(key=trial, value=RT) 

write.csv(winsorized_for_analysis, "winsorized.csv", na="", row.names = FALSE) 


# Index calculations ------------------------------------------------------

#trial name tidying
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
  left_join(type_names, by=c(Type="type")) %>% 
  rename(trial = Trial)

#creating the proper trial codenames
trial_types <- trial_types %>%
  mutate(trial = paste("X", row_number(), sep="")) %>%
  select(trial, Type, name) %>%
  rename(type = Type) %>%
  rename(label = name)

#merging trial names and emotions with analytic datasets
thresholded <- thresholded %>%
  gather(3:116, key=trial, value=RT) %>%
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted)

SD_filtered <- SD_filtered %>%
  gather(3:116, key=trial, value=RT) %>%
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted)

winsorized_for_analysis <- winsorized_for_analysis %>%
  gather(3:116, key=trial, value=RT) %>%
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted)

original_dataset <- tidied %>% 
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted)

write.csv(thresholded, "thresholded.csv", row.names = FALSE)
write.csv(SD_filtered, "SD_filtered.csv", row.names = FALSE)
write.csv(winsorized_for_analysis, "winsorized_for_analysis.csv", row.names = FALSE)
write.csv(original_dataset, "original_dataset.csv", row.names = FALSE)

#using the custom `indices formula`
#thresfhold <- ICVC(thresholded)
#SD_filtering <- ICVC(SD_filtered)
#winsorizing <- ICVC(winsorized_for_analysis)

#using the custom ABV formula
threshold_abv <- ABV(thresholded,29)
SD_filtering_abv <- ABV(SD_filtered,29)
winsorizing_abv <- ABV(winsorized_for_analysis,29)
original_dataset_abv <- ABV(original_dataset,29)

all_abv <- threshold_abv %>% 
  bind_rows(SD_filtering_abv) %>% 
  bind_rows(winsorizing_abv) %>% 
  bind_rows(original_dataset_abv)

# Export ------------------------------------------------------------------

#merging methods and RTs together
thresholded <- thresholded %>% mutate(method = "threshold")
SD_filtered <- SD_filtered %>% mutate(method = "SD filtering")
winsorized_for_analysis <- winsorized_for_analysis %>% mutate(method = "winsorizing")

merge <- thresholded %>% rbind(SD_filtered) %>% rbind(winsorized_for_analysis) %>% 
  mutate(flag = paste(method,Emotion, label, sep="_")) %>% group_by(felvetel.eve,Subject, flag) %>% 
  summarise(RT_mean = mean(RT, na.rm = TRUE)) %>% 
  spread(3:4, key=flag, value=RT_mean)

merge <- thresholded %>% rbind(SD_filtered) %>% rbind(winsorized_for_analysis) %>% 
  mutate(method = paste(method),
        Emotion = paste(Emotion),
        label = paste(label)) %>% group_by(felvetel.eve,Subject, method, Emotion, label) %>%
  summarise(RT_mean = mean(RT, na.rm = TRUE))
data <- as.data.frame(merge)
write.xlsx(data, "result.xlsx", col.names =TRUE, row.names=FALSE)
write.csv(data, "result.csv")
write.csv(tidied, "tidied.csv")

test <- winsorize(tidied, 0.25, 0.75, 1.5, grouping = c("Subject", "felvetel.eve"))
#testing <- data.frame(test == winsorized_for_analysis) %>% 
#  filter(FALSE)


# Summaries of original datasets ------------------------------------------
datasets <- c("original", "winsorized_data", "SD_filtered_data", "thresholded_data") %>% as_tibble() %>% 
  rename(names = value)

original_sum <- summary(original_dataset$RT)
wins_sum <- summary(winsorized_for_analysis$RT)
SD_sum <- summary(SD_filtered$RT)
thres_sum <- summary(thresholded$RT)

original_n <- original_dataset %>% filter(!is.na(RT)) %>% summarise(n = n()) %>% mutate(names = "original")
winsorized_data_n <- winsorized_for_analysis %>% filter(!is.na(RT)) %>% summarise(n = n(),
                                                                                  delta = original_n$n - n,
                                                                                  data_loss_ratio = paste0(round((delta/n)*100, digits=2),"%")) %>% mutate(names = "winsorized_data")
SD_filtered_data_n <- SD_filtered %>% filter(!is.na(RT)) %>% summarise(n = n(), 
                                                                       delta = original_n$n - n,
                                                                       data_loss_ratio = paste0(round((delta/n)*100, digits=2),"%")) %>% mutate(names = "SD_filtered_data")
thresholded_data_n <- thresholded %>% filter(!is.na(RT)) %>% summarise(n = n(),
                                                                       delta = original_n$n - n,
                                                                       data_loss_ratio = paste0(round((delta/n)*100, digits=2),"%")) %>% mutate(names = "thresholded_data")

n_summary <- bind_rows(original_n, winsorized_data_n, SD_filtered_data_n, thresholded_data_n)

summary <- bind_rows(original_sum, wins_sum, SD_sum, thres_sum)
summary <- bind_cols(summary,datasets) %>% 
  select(names, names(summary))

summary_detail <- summary %>% left_join(n_summary)

# Adding PANAS and rumination scores
other_metrics_data <- read.xlsx("Kerdoives_FaceDot2015_16_Tamasnak.xlsx", sheetIndex = 1)

winsorizing_abv_final <- winsorizing_abv %>% ungroup() %>% 
  select(Subject, Emotion, ABV) %>% 
  unique() %>% 
  spread(Emotion, ABV) %>% 
  rename(
    winsorized_angry = angry,
    winsorized_happy = happy,
    winsorized_sad = sad
  )

SD_filtering_abv_final <- SD_filtering_abv %>% ungroup() %>% 
  select(Subject, Emotion, ABV) %>% 
  unique() %>% 
  spread(Emotion, ABV) %>% 
  select(-5)%>% 
  rename(
    SD_filtered_angry = angry,
    SD_filtered_happy = happy,
    SD_filtered_sad = sad
  )

threshold_abv_final <- threshold_abv %>% ungroup() %>% 
  select(Subject, Emotion, ABV) %>% 
  unique() %>% 
  spread(Emotion, ABV)%>% 
  rename(
    thresholded_angry = angry,
    thresholded_happy = happy,
    thresholded_sad = sad
  )

final_result <- other_metrics_data %>% 
  left_join(threshold_abv_final, by = c(Subject = "Subject")) %>% 
  left_join(SD_filtering_abv_final, by = c(Subject = "Subject")) %>% 
  left_join(winsorizing_abv_final, by = c(Subject = "Subject"))
write.xlsx(final_result, "results.xlsx")
