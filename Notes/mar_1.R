library(tidyverse)
library(modelr)
library(easystats)

df <- read.csv("./Data/GradSchool_Admissions.csv")

df %>% glimpse()

#df$admit %>% as.logical() easier way to make it t/f

#change to true/false
df <- df %>% 
  mutate(admit=case_when(admit==1 ~ TRUE,
                          admit== 0 ~ FALSE),
         rank=factor(rank))

df %>% glimpse()

library(GGally)
ggpairs(df)

#logistic regression - when response variable is true/false
mod1 <- glm(data=df,
            formula=admit ~ gre + gpa + rank,
            family = "binomial") #changing from normal distribution to binomial distribution 

add_predictions(df, mod1,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank))+
  geom_smooth()



library(palmerpenguins)

penguins <- penguins %>% 
  mutate(Fatty = case_when(body_mass_g >5000 ~ TRUE,
                           body_mass_g<= 5000 ~ FALSE))
mod2 <- glm(data = penguins,
            formula = Fatty ~ bill_length_mm * bill_depth_mm,
            family="binomial")        

add_predictions(penguins,mod2,type = "response") %>% 
  ggplot(aes(x=bill_length_mm, y = pred))+
  geom_smooth()


#cross-validation

training_set <- penguins %>% 
  slice_sample(prop = .75) #took a random 75% of full data set 

testing_set <- anti_join(penguins,training_set)

testing_set
nrow(training_set)+nrow(testing_set) == nrow(penguins)

mod3 <- glm(data = training_set,
            formula = bill_length_mm ~ sex * species * body_mass_g)
add_predictions(testing_set,mod3) %>% 
  ggplot(aes(x=body_mass_g)) +
  geom_point(aes(y=bill_length_mm,color=sex))+
  geom_point(aes(y=pred))
