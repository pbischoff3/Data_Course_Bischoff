library(tidyverse)
library(easystats)
library(janitor)

install.packages("readxl")
library(readxl)

df <- read_excel("../../../Documents/Medical Conditions and Science Motivation Data 3.4.22.xlsx")

df <- df %>% 
  janitor::clean_names()

glimpse(df)

df <- df