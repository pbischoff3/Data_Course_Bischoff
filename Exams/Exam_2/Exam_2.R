library(tidyverse)
library(modelr)
library(easystats)
library(dplyr)

### Read Data In ###
unicef <- read.csv("../unicef-u5mr.csv")
glimpse((unicef))
################################################################


### Cleaning data ###
clean_data <- unicef %>% 
  pivot_longer(starts_with("U5MR"),
               names_to = "Year" , 
               values_to = "U5MR",
               names_prefix = "U5MR.") %>% 
  mutate(Year = as.integer(Year)) %>% 
  mutate(U5MR = as.numeric(U5MR))
################################################################


### Plot each country's U5MR over time ###
continent_data <- clean_data %>% 
  ggplot(aes(x=Year, y=U5MR, group=CountryName)) +
  geom_line(size=2)+
  facet_wrap(~Continent)+
  scale_x_continuous(breaks = seq(1950,2015,20),
                     limits = c(1950,2015))
plot(continent_data)
###############################################################


### Save Plot 1 ###
ggsave(continent_data,filename = "BISCHOFF_Plot_1.png")
##############################################################


### Mean U5MR for all countries in a given continent ### 
asia <- clean_data %>% 
  filter(Continent == "Asia")
asia_real <- asia %>% 
  select(Year,U5MR) %>% 
  na.omit(U5MR) %>% 
  group_by(Year) %>% 
  mutate(Mean_U5MR = mean(U5MR)) %>% 
  add_column(Continent = "Asia")


africa <- clean_data %>% 
  filter(Continent == "Africa")
africa_real <- africa %>% 
  select(Year,U5MR) %>% 
  na.omit(U5MR) %>% 
  group_by(Year) %>% 
  mutate(Mean_U5MR = mean(U5MR))%>% 
  add_column(Continent = "Africa")


americas <- clean_data %>% 
  filter(Continent == "Americas")
americas_real <- americas %>% 
  select(Year,U5MR) %>% 
  na.omit(U5MR) %>% 
  group_by(Year) %>% 
  mutate(Mean_U5MR = mean(U5MR))%>% 
  add_column(Continent = "Americas")


europe <- clean_data %>% 
  filter(Continent == "Europe")
europe_real <- europe %>% 
  select(Year,U5MR) %>% 
  na.omit(U5MR) %>% 
  group_by(Year) %>% 
  mutate(Mean_U5MR = mean(U5MR))%>% 
  add_column(Continent = "Europe")


oceania <- clean_data %>% 
  filter(Continent == "Oceania")
oceania_real <- oceania %>% 
  select(Year,U5MR) %>% 
  na.omit(U5MR) %>% 
  group_by(Year) %>% 
  mutate(Mean_U5MR = mean(U5MR))%>% 
  add_column(Continent = "Oceania")


real_data <- rbind(africa_real,americas_real,asia_real,europe_real,oceania_real)


## actual plot
mean_data <- real_data %>% 
  ggplot(aes(x=Year,y=Mean_U5MR,color=Continent))+
  geom_line(size=2)+
  scale_x_continuous(breaks = seq(1950,2015,20),
                     limits = c(1950,2015))
plot(mean_data)
###############################################################


### Save Plot 2 ###
ggsave(mean_data,filename = "BISCHOFF_Plot_2.png")
##############################################################


### Create 3 Models ###
mod1 <- glm(data=clean_data,
            formula = U5MR ~ Year)
mod2 <- glm(data=clean_data,
            formula = U5MR ~ Year + Continent)
mod3 <- glm(data=clean_data,
            formula = U5MR ~ Year * Continent)

## COMPARE 3 MODELS ##
compare_models(mod1,mod2,mod3, style = "se_p")
compare_performance(mod1,mod2,mod3) %>%  # ACCORDING TO THIS, MOD3 WOULD BE BEST WITH THE HIGHEST R2 AND SMALLEST RMSE 
  plot()


pred1 <- clean_data %>% 
  add_column(Model = "Mod1") %>% 
  add_predictions(mod1)

pred2 <- clean_data %>% 
  add_column(Model = "Mod2") %>% 
  add_predictions(mod2)

pred3 <- clean_data %>% 
  add_column(Model = "Mod3") %>% 
  add_predictions(mod3)

## Plot 3 Models ##
pred_total <- rbind(pred1,pred2,pred3)
pred_total %>% 
  ggplot(aes(x=Year,color=Continent))+
  facet_grid(~Model)+
  geom_smooth(aes(y=pred),method="lm")+
  ylab("Predicted U5MR")
###################################################################


### BONUS ###
data.frame(CountryName= "Ecuador", Year = 2020, Continent= "Americas") %>% 
  add_predictions(mod3)
# our model was not entirely accurate, as the prediction is roughly 23 points off, showing a negative value 

mod4 <- glm(data=clean_data,
            formula = U5MR ~ Region + Continent * Year)

data.frame(CountryName= "Ecuador", Year = 2020, Continent= "Americas", Region = "South America") %>% 
  add_predictions(mod4)
# though it is as not as close as yours, the predicted value is -4.17, considerably closer than -10 from "mod3".