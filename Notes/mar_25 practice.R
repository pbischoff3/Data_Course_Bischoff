# Load Packages ####

library(tidyverse)
library(janitor)
library(modelr)
library(easystats)


# Load .csv ####
df <- read_csv("./Data/GradSchool_Admissions.csv")

df %>% glimpse()


# Make T/F ####
df <- df %>% 
  mutate(admit=case_when(admit==1 ~ TRUE,
                         admit== 0 ~ FALSE),
         rank=factor(rank))

df %>% glimpse()


# What should our model be? ####
library(GGally)
ggpairs(df)


# Model 1####
# Admittance according to gre, gpa, and rank
mod1 <- glm(data=df,
            formula = admit ~ gre + gpa + rank,
            family = "binomial")
# Adding Predictions
add_predictions(df, mod1,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank))+
  geom_smooth()



# Model 2####
# Admittance according to GRE and GPA
mod2 <- glm(data=df,
            formula = admit ~ gre + gpa,
            family = "binomial")
# Adding Predictions
add_predictions(df, mod2,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank))+
  geom_smooth()



# Model 3 #### 
# Admittance according to gre and rank

mod3 <- glm(data=df,
            formula = admit ~ gre + rank,
            family = "binomial")
# Adding Predictions
add_predictions(df, mod3,type = "response") %>% 
  ggplot(aes(x=gre,y=pred,color=rank))+
  geom_smooth()






mod1 <- glm(data=df,
            formula=admit ~ gre + gpa + rank,
            family = "binomial") #changing from normal distribution to binomial distribution 

add_predictions(df, mod1,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank))+
  geom_smooth()

# Model 4 ####
# Admittance according to gre, gpa, and rank, where y int can change
mod4 <- glm(data=df,
            formula = admit ~ gre * gpa + rank,
            family = "binomial")
# Adding Predictions
add_predictions(df, mod4,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank))+
  geom_smooth()


#Model 5 ####
mod5 <- glm(data=df,
            formula = admit ~ gre + gpa * rank,
            family = "binomial")
# Adding Predictions
add_predictions(df, mod5,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank))+
  geom_smooth()

# Model Analysis ####
compare_models(mod1,mod2,mod3,mod4,mod5, style = "se_p")
compare_performance(mod1,mod2,mod3,mod4,mod5) %>% plot()

# Mod1 and Mod4 look interesting, lets look deeper there
compare_performance(mod1,mod4) %>% plot()

# For us, Mod4 looks like the best 


