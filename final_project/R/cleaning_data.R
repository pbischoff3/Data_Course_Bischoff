# Load Packages ####
library(tidyverse)
library(readxl)
library(easystats)
library(janitor)

# Read Excel and Clean Names ####
df <- read_xlsx("../../../Documents/Medical Conditions and Science Motivation Data 3.4.22.xlsx",range = "A1:BR501")

df <- df %>% 
  janitor::clean_names()

glimpse(df)

# Recode Data####
df <- df %>% 
  mutate(major=case_when(major == "1" ~ "Biology",
                         major == "4" ~ "Chemistry",
                         major == "5" ~ "Earth_sci",
                         major == "6" ~ "Mathematics",
                         major == "7" ~ "Physics",
                         major == "8" ~ "Engineering",
                         major == "9" ~ "Computer_sci",
                         major == "10" ~ "Health_sci",
                         major == "3" ~ "Non_STEM",
                         major == "11" ~ "Other"))
df <- df %>% 
  mutate(career=case_when(career == "1" ~ "medical_doctor",
                          career == "4" ~ "dentist",
                          career == "5" ~ "health_care_pro",
                          career == "6" ~ "scientist",
                          career == "7" ~ "engineer",
                          career == "8" ~ "science_communicator",
                          career == "9" ~ "educator",
                          career == "10" ~ "technician",
                          career == "11" ~ "researcher",
                          career == "3" ~ "non_stem"))

df <- df %>% 
  mutate(gender=case_when(gender == "1" ~ "male",
                          gender == "2" ~ "trans_m",
                          gender == "5" ~ "trans_w",
                          gender == "6" ~ "female",
                          gender == "7" ~ "non_conforming",
                          gender == "8" ~ "intersex",
                          gender == "9" ~ "two_spirited",
                          gender == "10" ~ "prefer_not_answer",
                          gender == "11" ~ "other"))

df <- df %>% 
  mutate(ethnicity=case_when(ethnicity == "1" ~ "african_american",
                             ethnicity == "4" ~ "american_indian",
                             ethnicity == "5" ~ "arab",
                             ethnicity == "6" ~ "asian",
                             ethnicity == "7" ~ "latinx",
                             ethnicity == "8" ~ "biracial",
                             ethnicity == "9" ~ "pacific_islander",
                             ethnicity == "10" ~ "white",
                             ethnicity == "11" ~ "prefer_not_answer",
                             ethnicity == "12" ~ "other"))

# Save clean data####
write.csv(df,"./\\clean_data.csv", row.names = TRUE)

