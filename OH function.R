outlier_handling <- function(data, ...){
  grouping = c(...)
  method = c(...)
  winsorize <- function(data, ...) {
    gc()
    grouping = c(...)
    winsorized <- data %>%
      group_by_(grouping) %>%
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
    winsorized_for_analysis <- data
    
  }
  threshold <- function(data, ...) {
    lower_threshold <- c(...)
    upper_threshold <- c(...)
    
    thresholded <- data %>%
      filter(!RT %in% lower_threshold) %>%
      filter(!RT %in% upper_threshold)
    
    thresholded <- thresholded %>%
      spread(key=trial, value=RT )
    thresholded <- data
  }
  SD_filter <- function(data, x, ...) {
    grouping = c(...)
    
    SD <- data %>%
      group_by_(grouping) %>%
      mutate(SD = sd(RT, na.rm=TRUE)) %>%
      ungroup() %>%
      spread(key=trial, value=RT )
    
    data_for_analysis <- data_cleaned %>%
      left_join(SD) %>% as_tibble()
    
    SD_filtered <- data_for_analysis %>%
      gather(3:116, key="trial", value="RT") %>%
      filter(!RT > x*SD | !RT < -x*SD)
      spread(key=trial, value=RT ) %>%
      as_tibble() %>% 
        select(-SD)

      SD_filtered <- data
  }
  
  
}
