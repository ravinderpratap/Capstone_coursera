#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(windowTitle = "Next Word Prediction Model", title = "Next Word Prediction (English)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( position = "left",
                 sidebarPanel("",
                              helpText("Type your Text Below in the Text Box"),
                              textInput("text_input", "Keyboard Typed Text",
                                        placeholder = "Start typing..."),
                              br()
                              
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel( 
                   tabsetPanel(type = "tabs",
                               tabPanel("Word Predictions", 
                                        br(), 
                                        mainPanel(
                                          h3('Results of the Prediction : Top 3'),
                                          verbatimTextOutput("outputText"),
                                          h3('wordcloud of Top 20 words'),
                                          plotOutput("Wordcloud")
                                        )),
                               tabPanel("Documentation", br(), 
                                        p("Next Word Prediction application has been built for the final Capstone project of the Data Science Specialization 
                                          by the Johns Hopkins University through Coursera piloting for SwiftKey"),
                                        br(),
                                        h3("Approach Followed for Capstone Project:"),
                                        h4("1. Exploration and Cleaning of Data"),
                                        p("     - Cleaning of Data done through Lowering the case of Text."),
                                        p("     - Removing of Punctuation and numbers."),
                                        p("     - Removing of Special Characters."),
                                        p("     - Removing of Profanities or Bad Words from Text."),
                                        p(" "),
                                        h4("2. N-Gram Modelling"),
                                        p("     - Preparing 1-GRam, 2-Gram, 3-Gram and 4-Gram Model Dataset."),
                                        p("     - Process the N-grams by breaking them in several different indexes."),
                                        p("     - Predicting the Next Word using these Index Dataset prepared from N-Gram Models."),
                                        p("     - Plotting the Word cloud of top 20 Predicted words."),
                                        br(),
                                        h4("Runtime: The amount of time the algorithm takes to make a prediction given the acceptable input with Backoff Model. Total 0.10 seconds elapsed time with back off and 68MB data object size."),
                                        h4("Size: the amount of memory (physical RAM) required to run the model in R in MB : 68 MB size in total for 1-Gram, 2-Gram , 3-Gram and 4-Gram Model Dataset for Prediction"),
                                        verbatimTextOutput("documentation"))
                               
                               )
                 )
  )
))
