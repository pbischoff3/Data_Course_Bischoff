library(tidyverse)
library(easystats)
library(modelr)
library(GGally)

df <- mpg

df %>% names()
ggpairs(df,columns=c("cty","displ","drv"))

df %>% ggplot(aes(x=displ,y=cty,color=drv))+
  geom_point()

mod1 <- glm(data = df,
            formula = cty ~ displ)  
mod2 <- glm(data = df,
            formula = cty ~ displ + drv) #only intercept can change
mod3 <- glm(data = df,
            formula = cty ~ displ * drv) #effect of displacement on city depends on types of drv, slope and intercept can vary 

summary(mod1)
summary(mod2)
summary(mod3)

compare_models(mod1,mod2,mod3, style = "se_p")
compare_performance(mod1,mod2,mod3) %>%  #AIC and BIC = lower is better. # rmse = room mean squared error, how far from predicted line 
  plot()

#hypothetical car
hyp_preds <- data.frame(displ=6,drv="r") %>%
  gather_predictions(mod1,mod2,mod3)
hyp_preds




sim3
sim_mod_1 <- glm(data=sim3,
                 formula = y ~ x1 + x2)
sim_mod_2 <- glm(data=sim3,
                 formula = y ~ x1 * x2)
compare_models(sim_mod_1,sim_mod_2)
compare_performance(sim_mod_1,sim_mod_2) %>% plot()


sim3 %>% gather_predictions(sim_mod_1,sim_mod_2) %>% 
  ggplot(aes(x=x1,y=pred,color=x2))+
  geom_smooth(method="lm")+
  facet_wrap(~model)


library(MASS)

mod3 <- glm(data=mpg,
            formula = cty ~ displ * year * cyl * drv * model)
step <- stepAIC(mod3)
step$formula

mod4 <- glm(data=mpg,
            formula = step$formula)
add_predictions(mpg,mod4) %>% 
  ggplot(aes(x=displ,y=pred,color=factor(cyl)))+
  geom_smooth(method="lm")


mpg %>% 
  ggplot(x=displ,y=cty))+
  geom_point()

mod5 <- glm(data=mpg,
            formula = cty ~ poly(displ,12))
            
add_predictions(mpg,mod5) %>% 
  ggplot(aes(x=displ))+
  geom_point(aes(y=cty))+
  geom_smooth(aes(y=pred))
