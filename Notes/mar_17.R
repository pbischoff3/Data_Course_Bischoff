# End with four hashtags == sets "chapter" names 


# Best practices for making a "clean" R script

# Load packages ####
library(tidyverse)
library(janitor)
library(modelr)
library(easystats)


# Set themes, etc. ####
theme_set(theme_minimal())
pal <- c("#1c4587","#4a0f4a","#3cb0c9")




# Load functions ####
# type the same hting more than twice? Needs to be a function


# Import data ####
# raw bird measurements
df <- read_csv("https://raw.githubusercontent.com/gzahn/Data_Course/master/Data/Bird_Measurements.csv")



# Clean data ####
glimpse(df)
df <- janitor::clean_names(df)

#split df into 3 data frames (one for each sex value)
male <- df %>% 
  select(-ends_with("_n")) %>% 
  select(family,species_number,species_name,english_name,clutch_size,egg_mass,mating_system,starts_with("m_"))
female <- df %>% 
  select(-ends_with("_n")) %>% 
  select(family,species_number,species_name,english_name,clutch_size,egg_mass,mating_system,starts_with("f_"))
unsexed <- df %>% 
  select(-ends_with("_n")) %>% 
  select(family,species_number,species_name,english_name,clutch_size,egg_mass,mating_system,starts_with("unsexed_"))         

# pivot_longer on each new df
male <- male %>% 
  pivot_longer(starts_with("m_"),
               names_to = "measurement",
               values_to = "value",
               names_prefix = "m_") %>% 
  mutate(sex="male")

female <- female %>% 
  pivot_longer(starts_with("f_"),
               names_to = "measurement",
               values_to = "value",
               names_prefix = "f_") %>% 
  mutate(sex="female")

unsexed <- unsexed %>% 
  pivot_longer(starts_with("unsexed"),
               names_to = "measurement",
               values_to = "value",
               names_prefix = "unsexed_") %>% 
  mutate(sex="unsexed")

#stick them back together 
full <- full_join(male,female) %>% 
  full_join(unsexed)

#clean up environment 
rm(male)
rm(female)
rm(unsexed)
rm(df)

# pivot wider
full <- full %>% 
  pivot_wider(id_cols = -measurement,
              names_from = measurement,
              values_from = value)

#take a look at 'full'
full %>% glimpse


# Visualize clean data ####
full %>% names

#quick visualizations
full %>% 
  ggplot(aes(x=egg_mass,y=mass,color=sex))+
  geom_point(alpha=.5,size=3)+
  facet_wrap(~sex)+
  scale_color_manual(values=pal)
ggsave("./Notes/myplot.png",height=4,width = 4,dpi = 300)

#wait, is mass normally distributed?


# Model and test hypotheses ####
mod <- glm(data = full,
           formula = log10(mass) ~ egg_mass + sex)
summary(mod)

sink("./Notes/mar_17_stat_table.txt")
summary(mod)
sink(NULL)

report(mod)
