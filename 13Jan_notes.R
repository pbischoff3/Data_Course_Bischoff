# "alt [minus]" is how the sweet arrow thing pops up
# control+enter goes from playground to environment
# readLines shows contents of file, include file path 
csvfiles <- list.files(path = "Data",pattern = ".csv",full.names = TRUE)
readLines("Data/Fake_grade_data.csv")
csvfiles
# if we want a specific item from list, include []
csvfiles[1]
readLines(csvfiles[1])
# vector= list of things that are all the same type
nums <- 1:10
nums
# nums + 1 yields all of the numbers inside "nums" plus 1
nums +1
nums*nums
# find all files in data directory beginning with letter "M"
#       ^ means "starts with"
mfiles <- list.files(path = "Data",
           pattern = "^m",
           recursive = TRUE,
           full.names = TRUE)
mush <- readLines(mfiles[11])
mush



# HOMEWORK: push commits to github, find different patterns, find different files on CPU