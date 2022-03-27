library(tidyverse)


# function practice ####

# write a function that returns min, mean, median, max from a numberic vector 

fn1 <- function(x){
  if(
    !is.numeric(x)){
    stop("numeric only moron")
    }
        min <- min(x)
        max <- max(x)
        mean <- mean(x)
        median <- median(x) 
  return(data.frame(min,mean,median,max))
}
fn1(c("a","b","c"))

# write function that returns vals
view_pal <- function(pal){
  p <- ggplot()+
    geom_col(aes(x=pal,fill=pal,y=1))+
    scale_fill_manual(values=pal)+
    theme(axis.text = element_blank(),
          legend.position = 'none')
  print(p)
}

view_pal(c("Light Blue", "Red"))


# creating reports ####
