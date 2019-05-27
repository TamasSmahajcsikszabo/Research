data <- winsorized_for_analysis
x <- 20

ICVC <- data %>%
    mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
    mutate(trial = as.integer(trial)) %>%
    arrange(felvetel.eve, Subject, trial) %>%
    group_by(felvetel.eve, Subject) %>%
    mutate(index = row_number()) %>%
    mutate(bin = (index %/% x)+1) %>%
    ungroup() %>%
    group_by(Subject, bin, label) %>%
    summarise(mean_RT = mean(RT, na.rm=TRUE)) %>%
    spread(4:5, key=label, value=mean_RT) %>%
    mutate(ICVC = Incongruent - Congruent) %>% 
  ungroup()
  
  SD <- ICVC %>%
    group_by(Subject) %>%   
    summarise(SD = sd(ICVC, na.rm = TRUE))
  
  ICVC <- 
    ICVC %>% left_join(SD, by=c(Subject = "Subject"))  
  
  mean_rt <- data %>%
    mutate(Subject = as.character(Subject)) %>%
    mutate(trial = str_sub(trial, start=2, end=length(trial))) %>%
    mutate(trial = as.integer(trial)) %>%
    arrange(felvetel.eve, Subject, trial) %>%
    group_by(felvetel.eve, Subject) %>%
    mutate(index = row_number()) %>%
    mutate(bin = (index %/% x)+1) %>%
    ungroup() %>% 
    group_by(Subject, bin) %>%
    summarise(mean_RT = mean(RT, na.rm=TRUE))
  
  mean_rt <- mean_rt %>% ungroup() %>% 
    mutate(Subject = as.integer(Subject))
  
  ICVC <- ICVC %>%
    left_join(mean_rt, by=c(Subject = "Subject", bin = "bin")) %>%
    mutate(SD = as.double(SD)) %>%
    mutate(ABV = SD / mean_RT)
