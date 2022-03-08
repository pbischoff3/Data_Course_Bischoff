library(tidyverse)
library(modelr)
library(palmerpenguins)

# import <- tiday <- visualize <- model <- communicate 

# Does flipper_length_mm differ significantly between male and female penguins?

aov_mod <- penguins %>% 
  aov(data=.,
      formula = flipper_length_mm ~ sex * species)
summary(aov_mod)

penguins %>% 
  ggplot(aes(x=species,y=flipper_length_mm,fill=sex))+
  geom_boxplot()
    #low p value = good chance that it passed 

TukeyHSD(aov_mod) #run after anova 

# Does flipper_length_mm change from year to year?
penguins %>% 
  ggplot(aes(x=as.factor(year),y=flipper_length_mm))+
  geom_point()
penguins

best_penguins <- penguins %>% 
  mutate(year = as.factor(year))

aov_mod2 <-  best_penguins %>% 
  aov(data= .,formula = flipper_length_mm ~ year)
summary(aov_mod2)

TukeyHSD(aov_mod2) %>% plot()

#your tools:
filter()
aov()
glm()
ggplot()
summary()

penguins

penguins %>% glimpse()

mod1 <- glm(data = penguins,
            formula = bill_length_mm ~ sex)
summary(mod1)
add_predictions(data = penguins,
                mod1, type = "response")
penguins %>% ggplot(aes(x=sex,y=bill_length_mm))+
    geom_boxplot()


data("us_rent_income")

tidy_rent <- us_rent_income %>% 
  select(-moe) %>% 
pivot_wider(id_cols = NAME,
  names_from = variable,
            values_from = estimate)

tidy_rent %>% 
  ggplot(aes(x=income,y=rent))+
  geom_point()+
  geom_smooth(method="lm")

mod2 <- tidy_rent %>% 
  glm(data=.,
      formula = rent ~ income)
summary(mod2)

