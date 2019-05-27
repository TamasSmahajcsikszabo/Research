data = data_cleaned 

### shaker() function - rotates and reshapes database
tidied_data <- shaker(data, grouping = c("felvetel.eve", "Subject"), gather = "trial", value = "RT") %>% 
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted)

### abvyr() calculates ABV-index

ABV <- abvyr(tidied_data,
  grouping = c("felvetel.eve", "Subject"),
  trial = "trial",
  bin_width = 29,
  value = "RT",
  label = "label",
  type = c("Incongruent", "Congruent"),
  ID = "Subject")


### winzer() function
winsorized_data <- winzer(tidied_data,
  grouping = c("felvetel.eve", "Subject"),
  x = 0.25,
  y = 0.75,
  z = 1.5,
  value = "RT")


### winsorize() function - winsorizes variance
winsorized_data <- winsorize(tidied_data,0.10, 0.90, 1, grouping = c("Subject", "felvetel.eve"))

### using alternative functions
SD_filtered_data <- SD_filter(tidied_data, 2, grouping = c("felvetel.eve", "Subject"))
thresholded_data <- threshold(tidied_data, grouping = c("felvetel.eve", "Subject"), lower_threshold = c(200:400), upper_threshold = c(2400:3000))

# looking up and connecting trial type and emotion flags into winsorized dataset
winsorized_data <- winsorized_data  %>%
  gather(3:116, key=trial, value=RT) %>%
  left_join(trial_types, by=c(trial = "trial")) %>% 
  left_join(emotions_adjusted)

### ABV() function --  calculates ABV and ICVC
ABV_data <- ABV(winsorized_data, 20)


### mixing outlier handling techniques

grouping = c("felvetel.eve", "Subject")
lower_threshold = c(200:400)
upper_threshold = c(2400:3000)

thresholded_winsorized <- tidied_data %>% 
  threshold(grouping= c("felvetel.eve", "Subject")) %>% 
  tidy_data(grouping= c("felvetel.eve", "Subject")) %>% 
  winsorize(0.25, 0.75, 1.5, grouping= c("felvetel.eve", "Subject"))

winsorized_data_SDfiltered <- tidied_data %>% 
  winsorize(0.25, 0.75, 1.5, grouping= c("felvetel.eve", "Subject")) %>% 
  tidy_data(grouping= c("felvetel.eve", "Subject")) %>% 
  SD_filter(2, grouping= c("felvetel.eve", "Subject"))


### export function

export(winsorized_data, "winsorized", "C:\\Users\\tamas\\Desktop", format = "excel")
