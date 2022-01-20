# "alt [minus]" is how the sweet arrow thing pops up
# control+enter goes from playground to environment
# readLines shows contents of file, include file path 
csvfiles <- list.files(path = "Data",pattern = ".csv",full.names = TRUE)
readLines("Data/Fake_grade_data.csv")
csvfiles
# if we want a specific item from list, include []
csvfiles[1]
readLines(csvfiles[1])
