
rm(list=ls())
require(parallel)
myCluster <- makeCluster(detectCores(), type = "FORK") # FORK will not work on Windows


threshold <- 0.95
 unigrams <- readRDS("./data/unigramsSample_p05.Rds")

  n = sum(unigrams$frequency)
  p = 0
  i = 1
  
  while(p < threshold) {
    p = p + unigrams[i,"frequency"]/n
    i = i+1
  }
  uniP <- i/nrow(unigrams)*100
  #########
  
  bigrams <- readRDS("./data/bigramsSample_p05.Rds")
  
  n = sum(bigrams$frequency)
  p = 0
  j = 1
  
  library(dplyr)
  #bigrams <- bigrams[with(bigrams, order(-frequency)), ]
  
  while(p < threshold) {
    p = p + bigrams[j,"frequency"]/n
    #p = p + bigrams[j]/n
    j = j+1
  }
  biP <- j/nrow(bigrams)*100
  #biP <- j/length(bigrams)*100
  #########
  
  trigrams <- readRDS("./data/trigramsSample_p05.Rds")
  
  n = sum(trigrams$frequency)
  p = 0
  k = 1
  
  #trigrams <- trigrams[with(trigrams, order(-frequency)), ]
  while(p < threshold) {
    p = p + trigrams[k, "frequency"]/n
    #p = p + trigrams[k]/n
    k = k+1
  }
  triP <- k/nrow(trigrams)*100
  #triP <- k/length(trigrams)*100
  
  fourgrams <- readRDS("./data/fourgramsSample_p05.Rds")
  
  n = sum(fourgrams$frequency)
  p = 0
  l = 1
  
  #fourgrams <- fourgrams[with(fourgrams, order(-frequency)), ]
  while(p < threshold) {
    p = p + fourgrams[l, "frequency"]/n
    #p = p + fourgrams[l]/n
    l = l+1
  }
  #  triP <- k/nrow(trigrams)*100
  fourP <- l/nrow(fourgrams)*100
  
  bigrams <- readRDS("./data/bigramsSample_p05.Rds")
  trigrams <- readRDS("./data/trigramsSample_p05.Rds")
  fourgrams <- readRDS("./data/fourgramsSample_p05.Rds")
  
  unigrams <- head(unigrams, i)
  bigrams <- head(bigrams, j)
  trigrams <- head(trigrams,k)
  fourgrams <- head(fourgrams,l)

  saveRDS(unigrams, file = "./data/unigramsSample_p05_p95.Rds")
  saveRDS(bigrams, file = "./data/bigramsSample_p05_p95.Rds")
  saveRDS(trigrams, file = "./data/trigramsSample_p05_p95.Rds")
  saveRDS(fourgrams, file = "./data/fourgramsSample_p05_p95.Rds")
  stopCluster(myCluster)
  