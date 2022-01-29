# for loop to add 5 to c(1,2,3)
for (i in 1:3){
  print(i + 5)
}

for(VARIABLE NAME in LIST OF VALUES)
  # do some stuff to VARIABLE NAME
  # each time through, VARIABLENAME will take on the next value 
  
  
# load wingspan_vs_mass.csv as "df"
  df <- read.csv("./Data/wingspan_vs_mass.csv")

# "../ +TAB" gives you the list of all the files 

#"as." change into different type of info
as.numeric(c(1,2,3,"Bob"))

# press "df"(words), pull up excel type expression

#locations are always rows then columns 
#find row 5 column 3
df[5,3]
df[5,] #info from 5th row
df[,1] #info from 1st column
df[,"variety"] #get everything from one column w/ words
df[,c("variety","mass")] # taking from two columns  

df$variety #gives vector of variety, $ gives options for names 


#momentum = mass * velocity 
df$velocity*df$mass
df$momentum <- df$mass * df$velocity

df$test <- c("oops",rep(NA,999))
df$test <- NULL # get rid of a column
# NA means missing 
# NULL = does not exist 

# expressions (logical)
1>3
3>=3 # greater than or equal to 
"bob"=="bob" # "==" is equal to
"billy"!="bob" # ! is "is not" equal to
3<5

# TRUE is 1 , FALSE is 0
NULL + 1 # numeric vector who's length is zero

df$momentum > 2000
momentous <- df$momentum > 2000
which(momentous == TRUE)

df[momentous,] # returns full list of data from "momentous"
df2 <- df[momentous,] 


1 %in% 2:3 # is the thing on the left found anywhere in the list on the right
1:20 %in% 1:10

3>2 & 3<10

3>2 | 3<0 # | means "or"

#bonus (base plotting)
plot(x=df$mass,y=df$momentum)
plot(x=df$variety,y=df$momentum) #this is an error due to df$variety being characters
plot(x=as.factor(df$variety),y=df$momentum)
