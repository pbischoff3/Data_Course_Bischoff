library(tidyverse)

# bigger sample size = closer to a bell curve / normal distribution
# glm = general linear model (data = , formula = "x" ~ "y")
# residuals = distance from mean

# add easystats and modelr 
#add_predictions 
library(easystats)
library(modelr)

#performance 
  # r^2 = percent improvement in comparison to total mean
  # RMSE (root mean squared error) = squared distance + combine + take square root. On average we are "..." off
  # AIC = smaller is better
  # BIC = smaller is better 

mpg
mod4 <- glm(data = mpg,
            formula = cty ~ cyl + trans + drv)
mpg %>% 
  add_predictions(mod4) %>% 
  ggplot(aes(x=cyl,y=cty,color=trans))+
  geom_point()+
  geom_point(aes(y=pred),color="Black",alpha=.25)

data.frame(cyl=4,trans="manual(m5)",drv="f") %>% 
  add_predictions(mod4)

#building basic models 
#hyp_testing_intro
