# test 1 review 
# gganimate is how to animate 


library(tidyverse)
library(palmerpenguins)
library(dplyr)

penguins %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_jitter()

penguins %>% 
  mutate(opinion = case_when(body_mass_g > 5000 ~ "chonky",
                           body_mass_g <= 5000 ~ "not chonky")) %>% 
  ggplot(aes(x = species, y = body_mass_g,color = opinion)) +
  geom_jitter()


df <- read_csv("./Data/BioLog_Plate_Data.csv")
df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance",
               names_prefix = "Hr_") %>% # removes prefix from all values
  mutate( Time = as.numeric(Time)) %>% 
  mutate(SampleType= case_when(`Sample ID` == "Clear_Creek"~ "Water",
                               `Sample ID` == "Waste_Water" ~ "Water",
                               TRUE ~ "Soil")) %>% #everything else that doesn't meet these conditions will be known as soil
  ggplot()




# pivot_longer()
# pivot_wider()
# mutate()
# arrange()
# filter() - pick rows 
# select() - -picking columns

table1 %>% 
  filter(year == 2000 & cases > 3000) %>% 
  select(-year) %>% 
  mutate(rate = cases/population) %>% 
  arrange(rate,desc(population)) #have to rely on fact that ascending is default.

table2 %>% 
  pivot_wider(id_cols = c(country,year),
              names_from = type,
              values_from = count)
table3 %>% 
  separate(rate, into = c("Cases","Population"))

full_join(
  table4a %>% 
    pivot_longer(-country, names_to = "year", values_to = "cases"),
  table4b %>% 
    pivot_longer(-country, names_to = "year", values_to = "population"))

library(janitor)

iris <- clean_names(iris)

#gganimate- give it type of transition 
  #transition state categorical
  # transition_reveal(time - what variable to base animation on)