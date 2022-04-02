library(tidyverse)

mpg %>% 
  filter(displ < 2) %>% 
  write_csv("./Code_Examples/test.csv")
