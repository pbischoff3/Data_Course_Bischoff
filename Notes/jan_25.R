iris #automatically added in R so we can practice
# what kind of project is Iris?
class(iris)
#data frame = group of vectors that have inherent order. each column/vector is a variable

#what kinds of species did they find in this data set 
iris$Species

class(iris$Species)
  # factors have levels, list of acceptable values. I can't just throw in another species 

unique(iris$Species)
table(iris$Species)


#packages, functions written by someone else that you can download to analyze data
  # click on passages tab, then click install 
  # CRAN r, task views, peer reviewed packages 
  
install.packages("easystats", repos = "https://easystats.r-universe.dev")


# R doesn't know about them, they are hiding somewhere on my computer
library(tidyverse)

# "::" - see all functions under package title

# %>%  #control+shift+m takes output from left as first argument of next line. Pipe

iris$Species %>% table()

#ggplot2 - best way to make graphs right now 

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,color=Species)) + 
  geom_point() + 
  geom_smooth()

iris$Sepal.Length %>% mean()
iris$Sepal.Length>6
iris$Species == "setosa" # puts out 150 true/false list 
setosa <- iris[iris$Species == "setosa",] #shoots out all labelled setosa
versicolor <- iris[iris$Species == "versicolor",]
virginica <- iris[iris$Species == "virginica",]

setosa$Sepal.Length %>% mean()
versicolor$Sepal.Length %>% mean()
virginica$Sepal.Length %>% mean()

# highlight and do control+F to find and replace

virginica$Sepal.Length %>% sd()

# take data frame, get only Species, then get summary
iris %>% 
  group_by(Species) %>% 
  summarize(MeanSepLength = mean(Sepal.Length),      # pay attention to SUMMARIZE
            SDSepLength = sd(Sepal.Length) ,
            MinSepLength = min(Sepal.Length),
            MaxSepLength = max(Sepal.Length))

iris$Sepal.Length %>% summary()

iris %>% 
  ggplot(aes(x=Sepal.Length,color=Species))+
  geom_density()



library(palmerpenguins)
names(penguins)        
summary(penguins)

table(penguins$species,penguins$island)

library(GGally)
ggpairs(penguins) #gives all possible graphs


ggplot(penguins,aes(x=body_mass_g,y=flipper_length_mm))+
  geom_point()+
  geom_smooth()
