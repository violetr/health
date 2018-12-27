#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(here)
library(ggmap)
library(maps)
library(ggthemes)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  datasetInput <- read_csv(here::here("data", "dataset_hnp_health_exp.csv"))
  
  map_world <- map_data("world") %>%
    filter(region!="Antarctica", 
           region!="French Southern and Antarctic Lands")
  
  output$coloredmap <- renderPlot({
     
    anio <- input$year
    
    aids_countries <- datasetInput %>% 
       select(country_code, year, SH.DYN.AIDS, SP.POP.TOTL) %>%
       filter(year == anio) %>%
       mutate(proportion_aids = SH.DYN.AIDS / SP.POP.TOTL) %>%
       filter(!is.na(proportion_aids)) 
     
    # fix scale 
    # 
     aids_countries %>%
       inner_join(maps::iso3166, by = c("country_code" = "a3")) %>%
       right_join(world, by = c(mapname = "region")) %>%
       ggplot(aes(long, lat, group=group, fill= proportion_aids)) +
       scale_fill_gradient2(low="blue", high="red", 
                            midpoint = 0.1, , limits=c(0, 0.18),
                            labels= scales::percent_format()) +
       geom_polygon() +
       theme_map() +
       coord_cartesian(ylim = c(-50, 90)) 
  })
})
