data = winsorized_for_analysis
x = 29

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