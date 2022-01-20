# "../" go up one directory, go backwards
  # this pulls all of the .csv files from our original file 
csv_files <- list.files(path = "../../Data",
           pattern = ".csv",
           full.names = TRUE) 

# "length" when we want to use the data
  #finding how many files are present
length(csv_files)

#open the wingspan_vs_mass.csv file, store as "df" using read.csv() 
wvm <- list.files(path = "../../Data",
           pattern ="wingspan_vs_mass.csv",
           full.names = TRUE)
df <- read.csv(wvm)


#Inspect the first 5 lines of this data set using the head() function

first5 <- head(df,n = 5) #give me the first "n" rows of a data frame 

# Find any files in the data/directory that begin with the letter "b" (lowercase)
letter_b  <- list.files(path = "../../Data",
                         pattern = "^b",
                         recursive = TRUE,
                         full.names = TRUE)

# Write a command that displays the first line of each of those "b" files 
for(i in 1:3){
  print(readLines(letter_b[i])[1])
}

#Do same thing for all files ending in ".csv"
letter_csv  <- list.files(path = "../../Data",
                        pattern = ".csv",
                        recursive = TRUE,
                        full.names = TRUE)
#find number of files 
length(letter_csv)

# Write a command that displays the first line of each of those ".csv" files 
for(i in letter_csv){
  print(readLines(i)[1])
}