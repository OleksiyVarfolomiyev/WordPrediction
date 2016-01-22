# Text data tokenization, ngram model
#Author: Alex (Oleksiy) Varfolomiyev

# load("./data/en_US/tokens.Rda")
# bigram <- dfm(tokens, ngrams = 2, concatenator = " ")
# bigramFreq <- topfeatures(bigram, nfeature(bigram))
# tops<-topfeatures(bigram, n = bigram@Dim[2], decreasing=T)
## Reading Data

require(tm)
require(stringi)
require(SnowballC)
require(RWeka)
require(ggplot2)
require(wordcloud)
require(parallel)
require(stylo)

# Download text files
if(!file.exists("final")) {
  fileURL <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  fileName <- "Coursera-SwiftKey.zip"
  download.file(fileURL, fileName)
  dateDownloaded <- date()
  dateDownloaded
  unzip(zipfile = dataDestination)
}



if(!exists("blg")) {
  fileName <- file("./final/en_US/en_US.blogs.txt", "rt")
  blg <- readLines(fileName, encoding="UTF-8")
  close(fileName)
}

if(!exists("nws")) {
  fileName <- file("./final/en_US/en_US.news.txt", "rt")
  nws <- readLines(fileName, encoding="UTF-8")
  close(fileName)
}

if(!exists("twit")) {
  fileName <- file("./final/en_US/en_US.twitter.txt", "rt")
  twit <- readLines(fileName, encoding="UTF-8", skipNul = T)
  close(fileName)
}

#  if(!exists("smplBlg")) {
#    smplBlg <- readRDS("./data/en_US/blogSample_p1.Rds")
#    smplNws <- readRDS("./data/en_US/newsSample_p1.Rds")
#    smplTwit <- readRDS("./data/en_US/twitterSample_p1.Rds")
#  }

## Data Processing
if(!exists("dat")) {
  #Sample data
  
  set.seed(111)
  smplFraction <- 0.05
  
  smplBlg <- sample(blg, smplFraction*length(blg))
  saveRDS(smplBlg, file = "./data/en_US/blogSample_p1.Rds")
  rm(blg)
  
  smplNws <- sample(nws, smplFraction*length(nws))
  saveRDS(smplNws, file = "./data/en_US/newsSample_p1.Rds")
  rm(nws)
  
  smplTwit <- sample(twit, smplFraction*length(twit))
  saveRDS(smplTwit, file = "./data/en_US/twitterSample_p1.Rds")
  rm(twit)
  
  # Combine all texts to Corpus
  dat <- list(smplBlg, smplNws, smplTwit)
  dat <- VCorpus(VectorSource(dat))
  
  # Data Cleaning
  tStart <- proc.time()
  
  myCluster <- makeCluster(detectCores(), type = "FORK") # FORK will not work on Windows
  
  rmSpecialChars <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  dat <- tm_map(dat, removePunctuation)
  dat <- tm_map(dat, removeNumbers)
  #dat <- tm_map(dat, removeWords, stopwords("english"))
  profanities <- readLines("./final/en_US/profanities.txt")
  dat <- tm_map(dat, removeWords, profanities)
  dat <- tm_map(dat, rmSpecialChars, "/|@|\\|#")
  dat <- tm_map(dat, stripWhitespace)
  dat <- tm_map(dat, content_transformer(tolower))
  dat <- tm_map(dat, rmSpecialChars,"\U2019")
  #dot <- tm_map(dat, stemDocument)
  saveRDS(dat, file = "./data/en_US/sample_p1.Rds")
  
  stopCluster(myCluster)
  
  tEndClean <- proc.time() - tStart
  tEndClean
}

#if(!exists("dat")) dat <- readRDS("./data/en_US/sample_p1.Rds")

# build and save N-grams
if(!exists("tdm")) {
  options(mc.cores=1)
  tStart <- proc.time()
  # Term Document Matrices init
  myCluster <- makeCluster(detectCores(), type = "FORK") # FORK will not work on Windows
  
  tdm <- TermDocumentMatrix(dat)
  pSparsity <- .999
  tdm <- removeSparseTerms(tdm, pSparsity)
   unigrams <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  rm(tdm)
  unigrams <- data.frame(word = names(unigrams), frequency = unigrams, stringsAsFactors = F)
  saveRDS(unigrams, file = "./data/en_US/unigramsSample_p05.Rds")

  bigramTokenizer <- function(x) NGramTokenizer(x, control = Weka_control(min = 2, max = 2))
  tdm2 <- TermDocumentMatrix(dat, control = list(tokenize = bigramTokenizer))
  tdm2 <- removeSparseTerms(tdm2, 0.999)
  bigrams <- slam::row_sums(tdm2, na.rm = T)
  rm(tdm2)

  bigrams <- data.frame(bigram = names(bigrams), frequency = bigrams, stringsAsFactors = F)
  # add colums to bigram with 1st and 2nd word
  saveRDS(bigrams, file = "./data/en_US/bigramsSample_p05.Rds")
  
  trigramTokenizer <- function(x) NGramTokenizer(x, control = Weka_control(min = 3, max = 3))
  tdm3 <- TermDocumentMatrix(dat, control = list(tokenize = trigramTokenizer))

  tdm3 <- removeSparseTerms(tdm3, 0.999)
  trigrams <- slam::row_sums(tdm3, na.rm = T)
  rm(tdm3)
  #trigrams <- sort(trigrams, decreasing = TRUE)
  trigrams <- data.frame(trigram = names(trigrams), frequency = trigrams, stringsAsFactors = F)
  saveRDS(trigrams, file = "./data/en_US/trigramsSample_p05.Rds")

  
    fourgramTokenizer <- function(x) NGramTokenizer(x, control = Weka_control(min = 4, max = 4))
     tdm4 <- TermDocumentMatrix(dat, control = list(tokenize = fourgramTokenizer))
     tdm4 <- removeSparseTerms(tdm4, 0.999)
     fourgrams <- slam::row_sums(tdm4, na.rm = T)
     rm(tdm4)
     fourgrams <- data.frame(fourgram = names(fourgrams), frequency = fourgrams, stringsAsFactors = F)
     
     saveRDS(fourgrams, file = "./data/en_US/fourgramsSample_p05.Rds")
     
     stopCluster(myCluster)
     tEndNgramsInit <- proc.time() - tStart
     tEndNgramsInit
}





