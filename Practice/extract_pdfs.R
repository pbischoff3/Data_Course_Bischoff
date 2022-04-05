# Load Packages####
library(pdftools)
library(tidyverse)

# Read in files ####
files <- list.files("../Holland/")
files

# Extract Text ####
opinions <- lapply(X = files,
                  FUN = pdf_text)
length(opinions)
lapply(opinions,length)

# Begin text mining ####
#install.packages("tm")
library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))
opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf)))) 
inspect(opinions.tdm[1:10,]) 
corp <- tm_map(corp, removePunctuation, ucp = TRUE)
opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf)))) 
inspect(opinions.tdm[1:10,]) 
findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)

ft <- findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)
as.matrix(opinions.tdm[ft,]) 

ft.tdm <- as.matrix(opinions.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)
# https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

# Create a word cloud ####
# Load Packages ####
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)


set.seed(1234)

wordcloud2(data = holland,
           size = 1.6,
           color = 'random-dark')

colnames(holland) <- c("words")

holland <- unlist(ft.tdm)
as.numeric(holland)

# Create a corpus  
docs <- Corpus(VectorSource(corp))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 1,           
          max.words=60, 
          random.order=FALSE, 
          rot.per=0.35,
          scale=c(2.5,.5),
          colors = pb_colors)

pb_colors <- c("cadetblue","aquamarine3","darkgoldenrod","darkolivegreen3")
