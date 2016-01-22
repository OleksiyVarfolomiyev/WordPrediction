# Word Prediction App
# Author: Alex (Oleksiy) Varfolomiyev

library(shiny)
library(shinyBS)
library(stylo)

unigrams <- readRDS("./data/unigramsSample_p05.Rds")

bigrams <- readRDS("./data/bigramsSample_p05.Rds")
secondWord <- function(word) {txt.to.words(word)[2]}

trigrams <- readRDS("./data/trigramsSample_p05.Rds")
thirdWord <- function(word) {txt.to.words(word)[3]}

fourgrams <- readRDS("./data/fourgramsSample_p05.Rds")
fourthWord <- function(word) {txt.to.words(word)[4]}

##################################################################################
 # function predicting next word via bigram model
 bigramModel <- function(word, nPredictions) {
   
   # find bigrams starting from the word
   predBi <- bigrams[substring(bigrams$bigram, 1, nchar(word)) == word, ]
   # compute scores for the found bigrams
   if(nrow(predBi) > 0) {
     # take 3 highest score bigrams
     predictions <- head(predBi[with(predBi, order(-frequency)), "bigram"], nPredictions)
     # target predictions are the second words in the found bigrams
     predictions <- sapply(predictions, secondWord) 
   }
   else predictions <-  head(unigrams$word, nPredictions)
   
   predictions    
   
 } # function bigramModel
 
##################################################################################
 # function predicting next word using trigrams
 trigramModel <- function(iStr, nPredictions) {
   
   # break text into words
   iWrds <- txt.to.words(iStr)
   # number of words
   nWrds <- length(iWrds)
   
   # trigram predictions
   predTri <- trigrams[substring(trigrams$trigram, 1, nchar(paste(iWrds[nWrds-1], iWrds[nWrds]))) == 
                         paste(iWrds[nWrds-1],iWrds[nWrds]), ]
   
   # if trigram found
   if(nrow(predTri) > 0) {  
     predictions <- head(predTri[with(predTri, order(-frequency)), "trigram"], nPredictions)
     predictions <- sapply(predictions, thirdWord) 
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
   
   # trigram predictions
   predFour <- fourgrams[substring(fourgrams$fourgram, 1, nchar(paste(iWrds[nWrds-2], iWrds[nWrds-1], iWrds[nWrds]))) == 
                           paste(iWrds[nWrds-2], iWrds[nWrds-1],iWrds[nWrds]), ]
   
   # if trigram found
   if(nrow(predFour) > 0) {  
     predictions <- head(predFour[with(predFour, order(-frequency)), "fourgram"], nPredictions)
     predictions <- sapply(predictions, fourthWord) 
   }
   else predictions <-  trigramModel(iStr, nPredictions)
   
   predictions
 } # function trigramModel
 
################################################################################## 
 # function predicting next word with give n-gram model
 predNextWord <- function(iStr, nPredictions, algDepth) {
   #  if(nPredictions == 0) {
   # break text into words
   iWrds <- txt.to.words(iStr)
   # number of words
   nWrds <- length(iWrds)
   
   if(nWrds > 1) 
     if(algDepth == 2) 
       return (trigramModel(iStr, nPredictions))
   else 
     if(algDepth == 1) 
       return (bigramModel(iWrds[nWrds], nPredictions))
   else
     if(algDepth == 3)
       return (fourgramModel(iStr, nPredictions))
   
   if(nWrds == 1) return (bigramModel(iWrds[nWrds], nPredictions))
   if(nWrds == 0) return(head(unigrams$word, nPredictions))   
   #  }  
 } # function predNextWord

################################################################################## 
 
predictions <- list()

shinyServer(
  function(input, output, session){
    # count clicks on the buttons with the predictions
    if(!exists("nClicks"))
      nClicks <- reactiveValues(clicks = 0)
  
    # call function for the next word prediction
    prediction <- reactive({
        predNextWord(tolower(input$inputTxt), {input$sliderPredictionsN}, {input$radio})
    })
    
    # output buttons with next word predictions
    output$words <- renderUI({
      
        predictWords <- prediction()
        assign('savedWords', predictWords, envir = .GlobalEnv)

        predictWords <- predictWords[!is.na(predictWords)]
        predictWords[predictWords== "i"] <- "I"
        output$nWords <- renderPrint(length(txt.to.words(input$inputTxt)))
        output$clicks <- renderPrint(nClicks$clicks)

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
    })

        observeEvent(input$word2, {
      nClicks$clicks <- nClicks$clicks+1
    })
    
    observeEvent(input$word3, {
      updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[3]))
      nClicks$clicks <- nClicks$clicks+1
    })
    
   observeEvent(input$word4, {
     updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[4]))
   })
   
     observeEvent(input$word5, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[5]))
       nClicks$clicks <- nClicks$clicks+1
     })
     
     observeEvent(input$word6, {
       updateTextInput(session, "inputTxt", value = paste(input$inputTxt, get('savedWords', envir=.GlobalEnv)[6]))
       nClicks$clicks <- nClicks$clicks+1
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