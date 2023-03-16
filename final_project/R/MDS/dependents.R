#####################################
########### dependents ##############
#####################################

# Load Packages####
library(tidyverse)
library(readxl)
library(easystats)
library(janitor)
library(modelr)
library(GGally)
library(vegan)
library(patchwork)

# Read in data ####
df <- readxl::read_excel("./Data/Raw/Medical Conditions and Science Motivation Data 3.4.22.xlsx")
df <- df %>% 
  janitor::clean_names()
df <- df %>% 
  select(-index,-semester,-type,-course,-pre_post) %>% 
  select(-starts_with("resilience"))

# Clean data ####
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

# Subplots for Adonis ####
si <- df %>% 
  select(starts_with("si"))

cm <- df %>% 
  select(starts_with("cm"))

im <- df %>% 
  select(starts_with("im"))

sd <- df %>% 
  select(starts_with("sd"))

se <- df %>% 
  select(starts_with("se"))

gm <- df %>% 
  select(starts_with("gm"))

sci_comp <- df %>% 
  select(starts_with("sci_comp"))

per_comm_orient <- df %>% 
  select(starts_with("per_comm_orient"))

sci_comm_orient <- df %>% 
  select(starts_with("sci_comm_orient"))

demo <- df %>% 
  select(major,gender,career,ethnicity,parents_grad_college,dependents,med_condition,med_condition_dependent)

questions <- df %>% 
  select(-major,-gender,-career,-ethnicity,-parents_grad_college,-dependents,-med_condition,-med_condition_dependent)

# Create Function ####
mysubsetMDS <- function(x){
  mysubset <- df %>% 
    select(starts_with(x))
  
  meta <- metaMDS(mysubset)
  MDS_df <- data.frame(MDS1=meta$points[,1],MDS2=meta$points[,2]) %>%
    cbind(demo)
  return(MDS_df)
}

# Create For Loop for all variables for career  ####
x=1
dependents_plots <- list()
for(i in c("si","cm","im","sd","se","gm","sci_comp","per_comm_orient","sci_comm_orient")){
  dependents_plots[[x]] <- mysubsetMDS(i) %>% 
    ggplot(aes(x=MDS1,y=MDS2,color=factor(dependents)))+
    geom_point()+
    labs(title = i)+
    stat_ellipse()
  x <- x+1
}
# Combine all factor plots together ####
dependents_plots[[1]]
adonis(si ~ demo$dependents)
# not significant

dependents_plots[[2]]
adonis(cm ~ demo$dependents)
# not significant

dependents_plots[[3]]
adonis(im ~ demo$dependents)
# not significant

dependents_plots[[4]]
adonis(sd ~ demo$dependents)
# not significant

dependents_plots[[5]]
adonis(se ~ demo$dependents)
# not significant

dependents_plots[[6]]
adonis(gm ~ demo$dependents)
# not significant

dependents_plots[[7]]
adonis(sci_comp ~ demo$dependents)
# not significant

dependents_plots[[8]]
adonis(per_comm_orient ~ demo$dependents)
# not significant

dependents_plots[[9]]
adonis(sci_comm_orient ~ demo$dependents)
# not significant
