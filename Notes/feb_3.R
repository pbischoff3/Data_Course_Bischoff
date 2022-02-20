library(tidyverse)

mpg %>% glimpse()

mpg %>% 
  ggplot(aes(x=displ,y=cty,color=factor(year))) + #x and y don't need to be in x / y order
  geom_point() +
  geom_smooth(method = "lm",formula = y ~ poly(x,2)) + # ~ = as a function of 
  theme_bw() +
  labs(x="Engine Displacement (L)",
       y="City MPG",
       color="Year") +
  # scale_color_viridis_d()
  scale_color_manual(values = c("Orange","Purple")) +
  facet_wrap(~drv) +
  theme(strip.text = (face="bold",color="Red"),
        strip.background = element_rect(fill="White",color="Blue",
                                        linetype=2),
        axis.title.x = element_text(angle=180),
        plot.background = element_rect(fill="Yellow"))
mpg %>% 
  ggplot(aes(x=hwy,y=model,color=factor(year))) +
  geom_boxplot()

ordered_by_med <- mpg %>% 
  group_by(class) %>% 
  summarize(MedHwy = median(hwy)) %>%  #SUMMARIZE ALWAYS COMES AFTER GROUP BY
  arrange(desc(MedHwy))

mpg %>% 
  mutate(ordered_class = factor(class,levels = ordered_by_med$class)) %>%  # create new columns based on existing ones 
  ggplot(aes(x=ordered_class,y=hwy)) +
  geom_violin(fill="DarkGreen") +
  geom_jitter(size=.5) +
  geom_boxplot(alpha=.5) +
  coord_flip()


#install.packages("ggimage")
