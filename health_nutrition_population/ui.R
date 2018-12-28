# mejorar mapa ggplotly o leafeat
# agregar imagen fondo

library(shiny)
library(semantic.dashboard)
library(shinyWidgets)

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
            box(
            h1("HNP Data Analysis for the UseR!2019 Datathon", align = "center"),
            img(src = "user2019.jpg", height = 40),
            h2("You can check some interesting visualizations that came out from the analysis of the Health, Nutrition and Population dataset 
                holded by the World Bank Group blablablablablabla"),
            p("The code to generate this App is available", 
            a("here.", href = "http://shiny.rstudio.com"))
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
