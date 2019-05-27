p <- ggplot(original, aes(RT)) +
  geom_histogram(colour = "black", fill = "lightgrey", binwidth = 10) + 
  theme_minimal() +
  labs(
    title = paste("A reakcióidő a mintán tipikus ex-Gaussian eloszlást mutat"),
    x = "Reakcióidő (ms)",
    y = "Elemszám (n)"
  ) + 
  geom_segment(x = 200, y = 2200, xend = 1000, yend=2200, linetype = 3, size = 1, colour = "lightgrey") + 
  geom_segment(x = mean(tidied$RT, na.rm = TRUE), y = 2150, yend = 2250, xend = mean(tidied$RT, na.rm = TRUE)) +
  geom_label(aes(x = meanRT, y = 2160, label = paste0("µ: ", round(meanRT, digits = 2), " ms")), data = meanRT, nudge_y = 160) +
  geom_segment(x =200, y = 2150, yend = 2250, xend = 200) +
  geom_segment(x =1000, y = 2150, yend = 2250, xend = 1000) +
  geom_segment(x = mean(tidied$RT, na.rm = TRUE) -  sd(tidied$RT, na.rm = TRUE), y = 2150, yend = 2250, xend = mean(tidied$RT, na.rm = TRUE) -  sd(tidied$RT, na.rm = TRUE)) +
  geom_label(aes(x = (mean(tidied$RT, na.rm = TRUE) -  sd(tidied$RT, na.rm = TRUE)), y = 2160, label = paste0("σ: ", round(sd(tidied$RT, na.rm = TRUE), digits = 2), " ms")), nudge_y = 160)
plotly::ggplotly(p)