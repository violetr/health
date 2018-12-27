#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
  
  # Application title
  titlePanel("AIDS incidence in the world over the years"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("year",
                   "Year:",
                   min = 1995,
                   max = 2015,
                   value = 2000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("coloredmap")
    )
  )
))
