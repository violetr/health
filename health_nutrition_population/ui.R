#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(semantic.dashboard)

# icon:  money bill alternate

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "HNP Data", text = "HNP Data"),
    dashboardSidebar(size = "thin", sidebarMenu(
      menuItem(tabName = "home", text = "Home", icon = icon("home")),
      menuItem(tabName = "map", text = "Map", icon = icon("map")),
      menuItem(tabName = "umap", text = "Countries", icon = icon("spinner")),
      menuItem(tabName = "other", text = "Statistics", icon = icon("chart line")),
      menuItem(tabName = "yother", text = "Gender", icon = icon("venus mars"))
    )),
    dashboardBody(
      tabItems(
        selected = 1,
        tabItem(
          tabName = "home",
          headerPanel("HNP Data Analysis for the UseR! Datathon"),
          mainPanel(
            h2("You can check some interesting visualizations that came out from the analysis of the Health, Nutrition and Population dataset 
                holded by the World Bank Group blablablablablabla"),
            h4("The code to generate this App is available here ")
          )
        ),
        tabItem(
          tabName = "map",

          fluidRow(
            # Sidebar with a slider input for number of bins


            # Show a plot of the generated distribution
            box(
              sliderInput("year1",
                "Year:",
                min = 1995,
                max = 2015,
                value = 2000
              ),
              plotOutput("coloredmap", width = "100%")
            )
          )
        ),
        tabItem(
          tabName = "umap",
          fluidRow(
            # Sidebar with a slider input for number of bins


            # Show a plot of the generated distribution
            box(
              sliderInput("year2",
                "Year:",
                min = 1995,
                max = 2015,
                value = 2000
              ),
              plotOutput("pumap", width = "100%")
            )
          )
        )
      ),
      theme = "paper"
    ) # or paper, lumen
  )
)
