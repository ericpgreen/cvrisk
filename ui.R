# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AMA CV Risk Calculator
# http://my.americanheart.org/professional/StatementsGuidelines/
# PreventionGuidelines/Prevention-Guidelines_UCM_457698_SubHomePage.jsp
# downloaded 2013-11-18 @ 21:45
# @ericpgreen
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file is not intended to replace the AMA CV Risk Calculator or be used in 
# a clincal context. The only purpose is to explore alternate forms of data
# visualization.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny)

# Define UI
  shinyUI(pageWithSidebar(
  
  # Application title
    headerPanel("Statin"),

    sidebarPanel(
      # gender
        selectInput("gender", "Gender:", 
                    choices = c("Male", "Female")),
      
      # age
        sliderInput("age", "Age:", 
                    min=20, max=79, value=50),
        
      # race
        selectInput("race", "Race:", 
                    choices = c("African American", "White")),
      
      # total cholesterol
        sliderInput("cholesterol", "Total Cholesterol:", 
                    min=130, max=320, value=170),
      
      # hdl cholesterol
        sliderInput("hdl", "HDL-Cholesterol:", 
                    min=20, max=100, value=50),
      
      # systolic blood pressure
        sliderInput("sbp", "Systolic Blood Pressure:", 
                    min=90, max=200, value=110), 
        
      # treatment for high blood pressure
        selectInput("treatment", "Treatment for High Blood Pressure:", 
                    choices = c("Yes", "No")),
        
      # diabetes
        selectInput("diabetes", "Diabetes:", 
                    choices = c("Yes", "No")),
        
      # smoker
        selectInput("smoker", "Smoker:", 
                    choices = c("Yes", "No")),
    ),
    
    # plot
      mainPanel(
        plotOutput("riskPlot")
      )
  ))
