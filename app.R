#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
library(shinythemes)
library(readr)
library(tm)
library(colourpicker)
library(rsconnect)


#Source functions

source("inputcleaner.R")  
source("StupidBackOff.R")



# Define UI for application 
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Next Word Prediction Application"),
  
  
  sidebarLayout(
    sidebarPanel(
      textInput('text', 'Type your words'),
      submitButton('Submit'),
      colourInput("col", "Select a colour for plot", value = "#41ADEB")
      
    ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Next Word Prediction",
                           h4('Word Prediction Results'),
                           p(strong('Cleaned Input')),
                           verbatimTextOutput("cleanedwords"),
                           p(strong('Top 3 predicted next words')),
                           verbatimTextOutput("predicted"),
                           p(strong('Type of prediction')),
                           verbatimTextOutput("pred_type"),
                           h4('Barplot of top 3 predicted next words'),
                           plotOutput("plot", width = "50%")),
                  
                  
                  tabPanel("About the App",
                           p(strong("This is a prototype next word prediction app developed as part of
                             the JHU Coursera Data Science Specialization Capstone Project.")),
                           p("Next word prediction is the task of suggesting the most probable word a user will
                             type next. Current approaches are based on the empirical analysis of corpora (large
                             text files) resulting in probability distributions over the different sequences that occur
                             in the corpus. The resulting language models are then used for predicting the most
                            likely next word."),
                           p("The English language corpus used in this case to build the 'n-gram' model was 
                            collected from publicly available sources - blogs, twitter and news by a web crawler.
                             In order to reduce the size of the data and allow the prototype to run
                             smoothly, about 10 % of the data was sampled, cleaned and tokenized into 6,5,4,3,2, and 1 word n-grams."),
                           p("A 'Stupid Back-off' model was used to determine the three most probable next words 
                             based on the probabilities of occurrences of the relevant n-grams."),
                           p(strong("Once you input your text in the app, please allow the app a few seconds to load its predictions. A bar graph 
                             of the top three predicted words for the model along with their probabilties is also displayed")),
                           p("For more details please see the accompanying pitch presentation."))
                           
                          
                        
      )
    )
  )
)


# Define server logic 
server <- function(input, output) {
  
  cleaned <- reactive({
    textInput <- input$text
    clean_input(textInput)
  })
  
  
  SBdataset <- reactive({
    predictSimple(cleaned(), unigramdata, bigramdata, 
                  trigramdata, quadgramdata, pentagramdata, hexagramdata)
  })
  
  
  
  predictword <- reactive({print(SBdataset()$nextword2)})
  
  output$predicted <- renderPrint({
    if(!input$text == ""){
      predictword()
    }
  })
  
  output$cleanedwords <- renderText({cleaned()})
  
  output$pred_type <- renderPrint({
    if(!input$text == ""){
      if(ncol(SBdataset()) == 9){
        print('Hexagram prediction')
      }
        if(ncol(SBdataset()) == 8){
          print('Pentagram prediction')
        }
        if(ncol(SBdataset()) == 7){
          print('Quadgram prediction')
        }
      
        if (ncol(SBdataset()) == 6){
          print('Trigram prediction')
        }
        if(ncol(SBdataset()) == 5){
          print('Bigram prediction')
        }
        if(ncol(SBdataset()) == 4){
          print('Unigram prediction')
        }
    }
    
  })
  
  output$plot <- renderPlot({
    if(input$text == ""){
      return(NULL)
    }
    else{
      p <- ggplot(SBdataset(), aes(x = reorder(nextword2, prob), y = prob))+
          geom_bar(width = 0.5, stat = "identity",fill = input$col)+ xlab("nextword") + ylab("Probability")+
          geom_text(aes(label = round(prob,2)), hjust = 0.5)+coord_flip()+theme(panel.grid = element_blank())
       
      print(p)
  }
    
})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



