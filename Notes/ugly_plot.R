library(ggplot2)
# sample data for plot ----




points <- 
  data.frame(
    x = rep(1:10,3), 
    y = rep(1:10,3), 
    z = sort(rep(letters[1:2], 15)),
    w = rep(letters[3:4], 15)
  )
# ggplot using many theme options ----
iris %>% ggplot(data = points, 
       mapping = aes(x = "Species", y = "Petal.Width", col = factor("Sepal.Length"))) + 
  geom_jitter(size = 8) + 
  facet_grid(w ~ z, switch = "both") +
  theme(
    
    plot.background = element_rect(fill = "brown"),
    plot.title = element_text(size = 25, hjust = 0.25, color = "aquamarine"),
    plot.subtitle = element_text(size = 15, hjust = 0.833, color = "mediumvioletred", family = "serif"),
    plot.caption = element_text(size = 16, face = "bold.italic", angle = 25),
    
    panel.background = element_rect(fill = 'lightblue', colour = 'deeppink', size = 4),
    panel.border = element_rect(fill = FALSE, color = "gold", size = 3),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 5),
    panel.grid.minor.y = element_blank(),
    
    axis.title.x = element_text(face = "bold.italic", color = "darkturquoise"),
    axis.title.y = element_text(family = "mono", face = "bold", size = 16, hjust = 0.25, color = "mediumseagreen"),
    axis.text = element_text(face = "italic", size = 17, angle = 90, color ="goldenrod4"),
    axis.text.x.bottom = element_text(angle = 36, color = "darkmagenta"), # note that axis.text options from above are inherited
    
    strip.background = element_rect(fill = "chocolate4"),
    strip.text.y = element_text(color = "olivedrab4"),
    strip.placement = "inside",
    
    legend.background = element_rect(fill = "lawngreen"), # generally will want to match w plot background
    legend.key = element_rect(fill = "lightpink1"),
    legend.direction = "horizontal",
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_text(family = "serif", color = "darkslategrey"),
    legend.text = element_text(family = "mono", face = "italic", color = "limegreen")
    
  ) +
  labs(title = "hi Dr Zahn",
       subtitle = "this plot is awesomely horrible",
       x = "This class is interesting",
       y = "and I am learning a lot",
       caption = "is this presentation quality?",
       col = "my name is Porter, here is my ugly plot") 





library(tidyverse)
data_frame(t=seq(-pi, 0, .001),
           x1=16*(sin(t))^2,
           x2=-x1,
           y=13*cos(t) -5 * cos(2*t) -2*cos(3*t) - cos(4*t)) %>%
  gather(side, x, x1, x2) %>%
  ggplot(aes(x,y)) +
  geom_polygon(fill="red") +
  coord_fixed() +
  theme_minimal()
