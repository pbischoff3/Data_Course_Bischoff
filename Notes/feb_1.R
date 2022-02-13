library(tidyverse)
library(palmerpenguins)


theme_set(theme(axis.text.x = element_text(angle=60,hjust=1)))

#assigment 4 - what I want to do with my final project 

penguins %>% glimpse

ggplot(penguins,aes(bill_length_mm,fill=species))+ #aesthetics is where you map columns to different aspects of plot
  geom_histogram(alpha=.5) #alpha needs to be outside of aes
      #geom_density and geom_histogram are counterparts

# not all chart types need or can even accepts x & y aesthetics

#make custom color pallette
pal <- c("#3d528a","#115c3f","#853e96")

ggplot(penguins,aes(x=body_mass_g,y=bill_length_mm))+
  geom_point(aes(color=species),alpha=.5,size=4) + # this gives a scatter plot 
  geom_smooth(method="lm",aes(group=species),color="black",se=FALSE) + #se= standard error bar
  labs(x="Body Mass (g)",
       y="Bill Length (mm)",
       color="Species",
       title="Body Mass v. Bill Length\nIn 3 Penguin Species")+ #\n means new line 
  #theme_minimal() +
  theme(plot.title = element_text(hjust=.5)) + # center title
  #scale_color_viridis_d()
  scale_color_manual(values=pal)
ggsave("myplot.pdf",width=6,height=5,dpi=300)


data() #built in data sets