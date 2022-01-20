# find all .txt files in ./Data/ (With full file paths)
# save that list as an object named txt_files
txt_files  <- list.files(path = "Data",
                     pattern = ".txt",
                     recursive = TRUE,
                     full.names = TRUE)
txt_files
# get first 3 of those, titled first3
first3 <- txt_files[1:3]
first3


# c - concatenate (to stick together)
c(1,3,5) # c() makes vectors

txt_files[c(1,3,5)]

# first ten and 15 element of txt_file
txt_files[c(1:10,15)]

thestuffiwant <- c(1:10,15)          
txt_files[thestuffiwant] #take the first 10 and the 15th element


#read in the first 3 files
readLines(first3[1])
#to get just third element
readLines(first3[1])[3]
readLines(first3[2])[3]
readLines(first3[3])[3]


# for-loop , runs conditions to a whole set of numbers, helps with huge data set 
for(i in 1:3){
  print(readLines(first3[i])[3])
}

x <- c("Billy","Bob","Jane")
paste0("My name is ","Bob",".") #stick together character vector type items, "0" is for no space 


for(i in x){
  print(paste0("My name is ",i,".")) # do not put it in quotes
}
}


