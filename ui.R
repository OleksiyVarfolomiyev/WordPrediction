# Word Prediction App
# Author: Alex (Oleksiy) Varfolomiyev

library(shiny)

shinyUI(   
 # fluidPage(
  
  navbarPage("Word Prediction App",               
    
    tabPanel("Home",
             
        #pageWithSidebar(  
        #titlePanel("Word Prediction App"),
        
        sidebarPanel(
            sliderInput("sliderPredictionsN", label = h3("Predictions number"), min = 3, max = 10, value = 4, step = 1),
              radioButtons("radio", label = h3("Predictions depth"),
                      choices = list("Bigrams" = 1, "Trigrams" = 2, "Fourgrams" = 3), 
                      selected = 2)
        ), #sidebarPanel
      
        mainPanel(
          wellPanel(
            uiOutput("words"),
            textInput("inputTxt","")
          ),
          wellPanel(
            h4('Predictions Clicked'),
            textOutput('clicks'),
             
             h4('Words Count'),
             textOutput('nWords')
          ) # wellPanel
        ) # mainPanel
        
    #  ) # pageWithSidebar
    ), # tabPanel
    
    tabPanel("Exploratory Data Analysis",
          mainPanel(
              includeMarkdown("./ExploratoryDataAnalysis/ExploratoryDataAnalysis.html")
          )
    ), # tabPanel
    
    tabPanel("About",
                h6("John Hopkins School of Public Health on Coursera", align = 'center'),
                h5("Data Science Specialization Capstone Project", align = 'center'), 
                h4("sponsored by SwiftKey", align = 'center'), 
                br(),
                h3("Web application simpifies typing on the mobile devices", align = 'center'), 
                h3("Predictive algorithm gives the next word suggestions for the typed text", align = 'center'),
                br(),
                h2("Using the developed prediction algorithm necessary typing reduces by 70%!", align = 'center'),
                h2("Type only one-in-three words, use predictions instead!", align = 'center'),
                br(),
                h1("Author: Alex (Oleksiy) Varfolomiyev", align = 'center'),
                br(),
                img(src='IMG_2126.PNG', align = "center"),
                h6("word prediction image above: WhatsApp with SwiftKey Keyboard") 
    ) # tabPanel
#  ) # fluidPage
  ) # navbarPage
) # shinyUI
