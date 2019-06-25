#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library("rio")
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(tm)
library(ggplot2)
library(caret)
library(tokenizers)
library(ngram)
library(NLP)
data1ngram <- import("https://github.com/ravinderpratap/Capstone_coursera/raw/master/Ngram1.rds")
data2ngram <- import("https://github.com/ravinderpratap/Capstone_coursera/raw/master/Ngram2.rds")
data3ngram <- import("https://github.com/ravinderpratap/Capstone_coursera/raw/master/Ngram3.rds")
data4ngram <- import("https://github.com/ravinderpratap/Capstone_coursera/raw/master/Ngram4.rds")

source(url("https://raw.githubusercontent.com/ravinderpratap/Capstone_coursera/master/Next_Word_Predictor.R"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  word_expect <- reactive({
    sentence <- input$text_input
    result <- word_predict_model(sentence, data1ngram, data2ngram, data3ngram, data4ngram)
    if (nrow(result) == 0){
      final <- "This combination of words rarely used to predict next word"
    } else {
      final <- result[1:20,]
    }
    return(final)
  })
  
  output$outputText <- renderPrint({
    if (word_expect()[1] == "This combination of words rarely used to predict next word" ) {
      word1 <- word_expect()[1]
    } else {
      word1 <- word_expect()[1]
    }
    if (!is.na(word_expect()[2])) {
      word1 <- paste(word1, word_expect()[2])
    }
    if (!is.na(word_expect()[3])) {
      word1 <- paste(word1, word_expect()[3])
    }
    word1
    #    paste(result[1,], result[2,], result[3,])
  })
  
  output$Wordcloud <- renderPlot({
    dist <- c(length(word_expect()):1)
    wordcloud(words = word_expect(), freq = dist, min.freq = 1, colors = brewer.pal(8,"Paired"))
  })
  
  output$documentation <- renderPrint({
    "End Of Documentation"
  })
  
})
