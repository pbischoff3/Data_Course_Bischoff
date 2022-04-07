# Load packages ####
library(tidyverse)
library(readxl)
library(easystats)
library(janitor)
library(modelr)

# Read in Data####
source("")

# Model 1 ####
# Does gender have an effect on science motivation?
mod1 <- glm(data=df,
            formula = overall_sci_mot ~ gender)
summary(mod1)

# Model 2 ####
# Does gender and medical experiences have an effect on science motivation?
mod2 <- glm(data = df,
            formula = overall_sci_mot ~ gender + med_condition)
summary(mod2)


# All possible models #####
library(caret) #premier machine learning 
library(MASS)

filtered <- df %>% 
  dplyr::select(-index) %>% 
  dplyr::select(-pre_post)
  
  
mod_all <- glm(data=filtered,
      formula = overall_sci_mot ~ .^2)

library(skimr)

filtered %>% skim() %>% view()

#because of the error, need to kick out what might be useless

#library(MASS)
#stepAIC

#mod_all <- df %>% 
#dplyr::select(-hwy) %>% 
#glm(data=.,
#formual = city~.^2) # every variable plus all interactions