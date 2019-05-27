Row
------
  
  ### Az ABV-index értékek megoszlása {data-height=300}
  ```{r echo=FALSE, message=FALSE, warning=FALSE, dependson="setup"}

library(plotly)
library(broom)
m <- loess(ABV ~ Ruminacio, data = ruminacio_data)
plot_data <- ruminacio_data %>% 
  filter(!is.na(Ruminacio),
         !is.na(ABV))

plot_ly(data = plot_data,
        x = ~Ruminacio, 
        color = ~erzelem,
        size = ~NegA/PosA,
        frame = ~erzelem,
        colors = viridis_pal(option = "D")((3))) %>%
  add_trace(y = ~ ABV, showlegend = FALSE)

```