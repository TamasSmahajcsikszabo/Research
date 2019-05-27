
library(officer)
library(magrittr)
library(zip)

my_pres <- read_pptx()
my_pres <- my_pres %>% 
  add_slide(layout = "Title and Content", master = "Office Theme")
layout_summary(my_pres)

my_pres <- my_pres %>% 
  ph_with_text(type = "title", str = "A title") %>%
  ph_with_text(type = "ftr", str = "A footer") %>%
  ph_with_text(type = "dt", str = format(Sys.Date())) %>%
  ph_with_text(type = "sldNum", str = "slide 1") %>%
  ph_with_text(str = "Hello world", type = "body")
annotate_base(output_file ="C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/first_example.pptx")
print(my_pres, target = "C:/Users/tamas/OneDrive/Dokumentumok/Kutatás/first_example.pptx") 

install.packages('tinytex')
tinytex::install_tinytex()