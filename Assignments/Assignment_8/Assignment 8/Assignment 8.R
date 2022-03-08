library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)


mushroom <- read.csv("../../../Data/mushroom_growth.csv")

########################################################

mod1 <- glm(data = mushroom,
            formula = GrowthRate ~ Temperature + Species)
summary(mod1)
mod1_plot <- mushroom %>% 
  ggplot(aes(x=Temperature,y=GrowthRate))+
  geom_point()+
  geom_smooth(method= "lm")+
  theme_minimal()
mod1_plot

predictions1 <- add_predictions(mushroom,mod1) %>% 
  ggplot(aes(x=Temperature)) +
  geom_point(aes(y=GrowthRate, color = Species))+
  geom_point(aes(y=pred, color = Species, size=3))
predictions1

########################################################

mod2 <- glm(data = mushroom,
            formula = GrowthRate ~ mushroom$Light + mushroom$Species)
summary(mod2)
mod2_plot <- mushroom %>% 
  ggplot(aes(x=Light,y=GrowthRate))+
  geom_point()+
  geom_smooth(method= "lm")+
  theme_minimal()
mod2_plot

predictions2 <- add_predictions(mushroom,mod2) %>% 
  ggplot(aes(x=Light)) +
  geom_point(aes(y=GrowthRate, color = Species))+
  geom_point(aes(y=pred,color= Species,size=3))
predictions2

#######################################################

mod3 <- glm(data = mushroom,
            formula = GrowthRate ~ mushroom$Nitrogen + mushroom$Species)
summary(mod3)
mod3_plot <- mushroom %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate))+
  geom_point()+
  geom_smooth(method= "lm")+
  theme_minimal()
mod3_plot

predictions3 <- add_predictions(mushroom,mod3) %>% 
  ggplot(aes(x=Nitrogen)) +
  geom_point(aes(y=GrowthRate, color= Species))+
  geom_point(aes(y=pred,color= Species,size=3))
predictions3

######################################################

mod4 <- glm(data = mushroom,
            formula = GrowthRate ~ mushroom$Species + mushroom$Species)
summary(mod4)
mod4_plot <- mushroom %>% 
  ggplot(aes(x=Species,y=GrowthRate))+
  geom_point()+
  geom_smooth(method= "lm")+
  theme_minimal()
mod4_plot

predictions4 <- add_predictions(mushroom,mod4) %>% 
  ggplot(aes(x=Species)) +
  geom_point(aes(y=GrowthRate, color = Species))+
  geom_smooth(aes(y=GrowthRate),method="lm")+
  geom_point(aes(y=pred,color=Species,size=3))+
  geom_smooth(aes(y=pred),method="lm")
predictions4

######################################################

## MEAN SQUARED ##
compare_performance(mod1,mod2,mod3,mod4)
# The RMSE is the square root of the mean squared data. The smaller the better, so mod2 looks the best right now. 
# This being said, plotting mod2 with the predictors being the bigger dot would yield:
predictions_real <- predictions4
predictions_real
