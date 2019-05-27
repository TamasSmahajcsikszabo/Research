# Functions ---------------------------------------------------------------
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
ICVC <- function(data, ...) {
  grouping = c(...)
  data %>% 
    group_by_(grouping) %>%
    summarise(mean_RT = mean(RT, na.rm=TRUE)) %>%
    spread(2:3, key=label, value=mean_RT) %>%
    mutate(ICVC = Incongruent - Congruent)}

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
    group_by(Subject) %>%
    summarise(mean_RT = mean(RT, na.rm=TRUE))
  
  mean_rt <- mean_rt %>% ungroup() %>% 
    mutate(Subject = as.integer(Subject))
  
  ICVC <- ICVC %>%
    left_join(mean_rt, by=c(Subject = "Subject")) %>%
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
