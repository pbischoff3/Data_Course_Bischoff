# Write your own functions

fn1 <- function(x){
  z <- x^2
  return(z)
}
fn1(3)

fn_pythag <- function(A,B){
  C <- sqrt(A^2 + B^2)
  return(C)
}
fn_pythag(3,3)

fn2 <- function(x,y,z){
  if(!is.numeric(x)| !is.numeric(y) | !is.numeric(z)){
    stop("Hey, x y and z needs to be numeric")
  }
  q <- sum(x,y,z)
  return(q)
}
fn2(5,9,10)


# source() - way to get another R function 



library(modelr)
library(tidyverse)

#set.seed(12345) #will let us all have the same proportions 

mpg

x_val <- function(data=dat,n=100,form){

x <- list()
for (i in 1:n) {
train <- data %>% 
  slice_sample(prop = .25)

test <- anti_join(data,train)

mod <- train %>% 
  glm(data = .,
      formula = form)
x[[i]] <- test %>% 
  add_residuals(model = mod) %>% 
  pluck("resid") %>% 
  abs() %>% 
  mean()
}

return(x %>% unlist())

}


x_val(data = mpg,n = 100,form = formula(cty ~ displ))

data.frame(x=x %>% unlist()) %>% 
             ggplot(aes(x=x))+
             geom_density()
