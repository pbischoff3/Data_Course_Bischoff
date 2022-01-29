p <- penguins %>% 
  ggplot(aes(x=body_mass_g,y=sex,color=species))
  geom_point()
p
p +
  geom_point()+
  labs(x="",
       y="Sex",
       title="This is the ugliest plot alive")+
  theme(axis.title.x = element_blank())+
  facet_wrap(~island)
