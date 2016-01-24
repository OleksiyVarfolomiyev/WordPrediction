# Word Prediction App
# Author: Alex (Oleksiy) Varfolomiyev

library(shiny)
library(shinyBS)
library(stylo)
library(gdata)

# source("fourgramModel.R")
# source("trigramModel.R")
# source("bigramModel.R")
# source("predNextWord.R")

if(!exists("unigrams")) unigrams <- readRDS("./data/unigramsSample_p05_p95.Rds")

if(!exists("bigrams")) bigrams <- readRDS("./data/bigramsSample_p05_p95.Rds")
secondWord <- function(word) {txt.to.words(word)[2]}

if(!exists("trigrams")) trigrams <- readRDS("./data/trigramsSample_p05_p95.Rds")
thirdWord <- function(word) {txt.to.words(word)[3]}

if(!exists("fourgrams")) fourgrams <- readRDS("./data/fourgramsSample_p05_p95.Rds")
fourthWord <- function(word) {txt.to.words(word)[4]}

##################################################################################
bigramModel <- function(word, nPredictions) {
  
  # find bigrams starting from the word 'word'
  #predBi  <- subset(bigrams, startsWith(rownames(bigrams), paste0(word, " ") ))
  predBi  <- rownames(bigrams)[ startsWith(rownames(bigrams), paste0(word, " ") )]
  #predBi <- rownames(bigrams[substring(rownames(bigrams), 1, nchar(word)) == word, ])
  
  # compute scores for the found bigrams
  if(length(predBi) > 0) {
    #predBi$ngram <- rownames(predBi)
    #predictions <- sapply( head( predBi[with(predBi, order(-frequency)),"ngram" ], nPredictions), secondWord)
    predictions <- sapply(head(predBi, nPredictions), secondWord)
  }
  else 
      predictions <-  head(rownames(unigrams), nPredictions)
  
  predictions    
  
} # function bigramModel

##################################################################################
 # function predicting next word using trigrams
 trigramModel <- function(iStr, nPredictions) {
   
   # break text into words
   iWrds <- txt.to.words(iStr)
   # number of words
   nWrds <- length(iWrds)
   
   #predTri  <- subset(trigrams, startsWith(rownames(trigrams),  paste0(iWrds[nWrds-1], iWrds[nWrds]) ))
   predTri  <- rownames(trigrams)[ startsWith(rownames(trigrams),  paste0(iWrds[nWrds-1], iWrds[nWrds]) )]
   
   # if trigram found
   if(length(predTri) > 0) {  
     #predTri$ngram <- rownames(predTri)
     #predictions <- sapply( head(predTri[with(predTri, order(-frequency)),"ngram" ], nPredictions), thirdWord)
     predictions <- sapply(head(predTri, nPredictions), thirdWord)
   }
   else predictions <-  bigramModel(iWrds[nWrds], nPredictions)
   
   predictions
 } # function trigramModel
 
##################################################################################
 # function predicting next word using fourgrams
 fourgramModel <- function(iStr, nPredictions) {
   
   # break text into words
   iWrds <- txt.to.words(iStr)
   # number of words
   nWrds <- length(iWrds)
   
   #predFour  <- subset(fourgrams, startsWith(rownames(fourgrams),  paste0(iWrds[nWrds-2], iWrds[nWrds-1], iWrds[nWrds]) ))
   predFour  <- rownames(fourgrams)[ startsWith(rownames(fourgrams),  paste0(iWrds[nWrds-2], iWrds[nWrds-1], iWrds[nWrds]) )]
   
   # if fourgram found
   if(length(predFour) > 0) {  
     # take 3 highest score bigrams
     #predFour$ngram <- rownames(predFour)
     #predictions <- sapply(head( predFour[with(predFour, order(-frequency)), "ngram" ], nPredictions), fourthWord)
     predictions <- sapply(head(predFour, nPredictions), fourthWord)
     
   }
   else predictions <-  trigramModel(iStr, nPredictions)
   
   predictions
 } # function fourgramModel
 
################################################################################## 
 # function predicting next word
 predNextWord <- function(iStr, nPredictions, algDepth) {
   #  if(nPredictions == 0) {
   # break text into words
   iWrds <- txt.to.words(iStr)
   # number of words
   nWrds <- length(iWrds)
   
   if(nWrds > 1) {
     if(algDepth == 2) 
       return (trigramModel(iStr, nPredictions))
      else 
        if(algDepth == 1) 
          return (bigramModel(iWrds[nWrds], nPredictions))
     else 
       if(algDepth == 3 && nWrds > 2) 
         return (fourgramModel(iWrds[nWrds], nPredictions))
       else 
         return (trigramModel(iStr, nPredictions))
   }  
   
   if(nWrds == 1) return (bigramModel(iWrds[nWrds], nPredictions))
   if(nWrds == 0) return(head(rownames(unigrams), nPredictions))   
   #  }  
 } # function predNextWord

################################################################################## 
 
predictions <- list()

shinyServer(
  function(input, output, session){
    # count clicks on the buttons with the predictions
    if(!exists("nClicks")) {
      nClicks <- reactiveValues(clicks = 0)
    }
    # call function for the next word prediction
      prediction <- reactive({
        predNextWord(tolower(input$inputTxt), {input$sliderPredictionsN}, {input$radio})
      })
    
    # output buttons with next word predictions
    output$words <- renderUI({
      
      predictWords <- prediction()
      assign('savedWords', predictWords, envir = .GlobalEnv)

        predictWords <- predictWords[!is.na(predictWords)]
       # predictWords[predictWords== "i"] <- "I"
       # output$text <- renderPrint(predictWords)
       # output$value <- renderPrint(min({input$sliderPredictionsN}, length(predictWords)) )
        output$nWords <- renderPrint(length(txt.to.words(input$inputTxt)))
        output$clicks <- renderPrint(nClicks$clicks)

        #output$predTime <- renderPrint(tPred$tPrediction)
        
        for(i in 1:min({input$sliderPredictionsN}, length(predictWords))) 
            predictions <- list(predictions, list(bsButton(inputId = paste("word", i, sep = ""), 
                                                           label = predictWords[i], style = "primary")))
          
          tagList(predictions)
    }) # output$Words

    
    # add chosen predicted word to the text
    observeEvent(input$word1, {
      updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[1]))
      nClicks$clicks <- nClicks$clicks+1
    })

    observeEvent(input$word2, {
      updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[2]))
      nClicks$clicks <- nClicks$clicks+1
    })

    observeEvent(input$word3, {
      updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[3]))
      nClicks$clicks <- nClicks$clicks+1
    })
    
   observeEvent(input$word4, {
     updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[4]))
     nClicks$clicks <- nClicks$clicks+1
   })
   
     observeEvent(input$word5, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[5]))
       nClicks$clicks <- nClicks$clicks+1
     })
     
     observeEvent(input$word6, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[6]))
       nClicks$clicks <- nClicks$clicks+1
       nKeystrokesSaved$keystrokes <- nKeystrokesSaved$keystrokes + nchar(get('savedWords', envir=.GlobalEnv)[6])
     })
     
     observeEvent(input$word7, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[7]))
       nClicks$clicks <- nClicks$clicks+1
     })
     
     observeEvent(input$word8, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[8]))
       nClicks$clicks <- nClicks$clicks+1
     })
     
     observeEvent(input$word9, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[9]))
       nClicks$clicks <- nClicks$clicks+1
     })
     
     observeEvent(input$word10, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[10]))
       nClicks$clicks <- nClicks$clicks+1
     })
     
  } #function (input, output, session)
) # shinyServer
