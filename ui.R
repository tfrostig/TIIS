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
  titlePanel("TIIS"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("alpha",
                   "Alpha (Probability for Type I Error)",
                   min = 0,
                   max = 0.5,
                   value = 0.01),
       numericInput("mean",
                    "Average value", 
                    value = NA),
       numericInput("Sigma",
                   "Known S.D", 
                   value = 1),
       numericInput("n",
                    "Number of observations",
                    value = 5),
       numericInput("H0", 
                    "Value of mean under H0",
                    value = 0),
       radioButtons("radio",
                    "Bigger / Smaller / Different than",
                    choices = list(">" = 1, 
                                   "<" = 2,
                                   "!=" = 3),
                    selected = 1),
       numericInput("H1", 
                    "Value of mean under H1",
                    value = NA)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
