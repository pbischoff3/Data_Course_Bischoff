#####################################
######## SCIENCE IDENTITY ###########
#####################################

# Load Packages####
library(tidyverse)
library(readxl)
library(easystats)
library(janitor)
library(modelr)
library(GGally)
library(vegan)

# Read in and clean Data ####
df <- readxl::read_excel("./Data/Raw/Medical Conditions and Science Motivation Data 3.4.22.xlsx")
df <- df %>% 
  janitor::clean_names()
df <- df %>% 
  select(-index,-semester,-type,-course,-pre_post) %>% 
  select(-starts_with("resilience"))

# Clean 
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

df <- df[complete.cases(df),]

si <- df %>% 
  select(starts_with("si"))

demo <- df %>% 
  select(major,career,gender,ethnicity,parents_grad_college,dependents,med_condition,med_condition_dependent)

# SI ~ Major ####
meta_si <- metaMDS(si)

data.frame(MDS1=meta_si$points[,1],
           MDS2=meta_si$points[,2]) %>% 
  cbind(demo) %>% 
  ggplot(aes(x=MDS1,y=MDS2,color=factor(major)))+
  geom_point()+
  stat_ellipse()

# Is this relevant?
adonis(si ~ demo$major)
# THERE IS A SIGNIFICANCE

# SI ~ Career ####
meta_si <- metaMDS(si)

data.frame(MDS1=meta_si$points[,1],
           MDS2=meta_si$points[,2]) %>% 
  cbind(demo) %>% 
  ggplot(aes(x=MDS1,y=MDS2,color=factor(career)))+
  geom_point()+
  stat_ellipse()

# Is this relevant?
adonis(si ~ demo$career)
# THERE IS A SIGNIFICANCE

# SI ~ Gender ####
meta_si <- metaMDS(si)

data.frame(MDS1=meta_si$points[,1],
           MDS2=meta_si$points[,2]) %>% 
  cbind(demo) %>% 
  ggplot(aes(x=MDS1,y=MDS2,color=factor(gender)))+
  geom_point()+
  stat_ellipse()

# Is this relevant?
adonis(si ~ demo$gender)
# No significance

# SI ~ Ethnicity ####
meta_si <- metaMDS(si)

data.frame(MDS1=meta_si$points[,1],
           MDS2=meta_si$points[,2]) %>% 
  cbind(demo) %>% 
  ggplot(aes(x=MDS1,y=MDS2,color=factor(ethnicity)))+
  geom_point()+
  stat_ellipse()

# Is this relevant?
adonis(si ~ demo$ethnicity)
# No significance

# SI ~ Parents Graduated College ####
meta_si <- metaMDS(si)

data.frame(MDS1=meta_si$points[,1],
           MDS2=meta_si$points[,2]) %>% 
  cbind(demo) %>% 
  ggplot(aes(x=MDS1,y=MDS2,color=factor(parents_grad_college)))+
  geom_point()+
  stat_ellipse()

# Is this relevant?
adonis(si ~ demo$parents_grad_college)
# No significance

# SI ~ Dependents ####
meta_si <- metaMDS(si)

data.frame(MDS1=meta_si$points[,1],
           MDS2=meta_si$points[,2]) %>% 
  cbind(demo) %>% 
  ggplot(aes(x=MDS1,y=MDS2,color=factor(dependents)))+
  geom_point()+
  stat_ellipse()

# Is this relevant?
adonis(si ~ demo$dependents)
# No significance

# SI ~ Medical Condition ####
meta_si <- metaMDS(si)

data.frame(MDS1=meta_si$points[,1],
           MDS2=meta_si$points[,2]) %>% 
  cbind(demo) %>% 
  ggplot(aes(x=MDS1,y=MDS2,color=factor(med_condition)))+
  geom_point()+
  stat_ellipse()

# Is this relevant?
adonis(si ~ demo$med_condition)
# No significance