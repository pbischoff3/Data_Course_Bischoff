library(tidyverse)

table1
table2
table3
table4a
table4b
table5

#pivot_longer- makes the amount of rows longer. variable as columns
#pivot_wider- increase number of columns, decrease number of rows.variable is built into rows
#separate- 1 column into multiple columns
#unite- opposite of separate, paste strings together
#full_join- bring 2 dfs together looking for column names in common
#mutate
#case_when


table2 %>% pivot_wider(id_cols = c(country,year), #gives the unique values
                       names_from = type, #which columns to split?
                       values_from = count) #where to find values to fill

table3 %>% 
  separate(rate, #which column to separate
           into = c("cases","population"), #new names for columns
           convert = TRUE) #turned into integers 

x <- table4a %>% 
  pivot_longer(-country,#what columns the current data is in 
               names_to = "year",
               values_to = "cases")
y <- table4b %>% 
  pivot_longer(-country,#what columns the current data is in 
               names_to = "year",
               values_to = "population")

full_join(x,y)

table5 %>% 
  separate(rate,into = c("cases","population")) %>% 
  unite(col="year", #new name for column
        sep = "", #what to separate values by, ""= nothing
        century,year) %>%  #names of columns to paste together
  mutate(year=as.numeric(year),cases=as.numeric(cases),population=as.numeric(population))

