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
  
  dataseth_input <- read_csv(here::here("data", "dataset_hnp_health_exp.csv"))
  
  indicators_input <- read_csv(here::here("data", "indicators_hnp_exp.csv"))
  
  comparable_health_indicators <- indicators_input %>%
    filter(str_detect(indicator_name, "%"),
           gen_topic == "Health",
           ! part_topic %in% " Population") %>%
    pull(series_code)
  
  map_world <- map_data("world") %>%
    filter(region!="Antarctica", 
           region!="French Southern and Antarctic Lands")
  
  output$coloredmap <- renderPlot({
     
    an_year_1 <- input$year1

    aids_countries <- dataseth_input %>% 
       select(country_code, year, SH.DYN.AIDS, SP.POP.TOTL) %>%
       filter(year == an_year_1) %>%
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
  
  output$pumap <- renderPlot({
    
    an_year_2 <- input$year2
    
    dataset_comparable_health <- dataseth_input %>%
      filter(year == an_year_2) %>%
      select(country_name, country_code, one_of(comparable_health_indicators))  %>%
      select_if(function(x) (sum(is.na(x)) <= (0.2 * length(x))) || is.character(x)) %>%
      drop_na()
    
    data_to_umap <- as.matrix(dataset_comparable_health[, 3:ncol(dataset_comparable_health)])
    
    data_health_umap <- umap(data_to_umap)
    
    data_umap_health <- tibble(x = data_health_umap$layout[, 1], 
                               y = data_health_umap$layout[, 2],
                               country = dataset_comparable_health$country_code)
    
    # esto se puede optimizar poniendolo desde el principio en dataset
    data_umap_health <- data_umap_health %>%
      left_join(countries_hnp_keep, by = c("country"= "country_code")) %>%
      select(x, y, country, region)
    
    #pdf(here::here("figures/umap_1995.pdf"), height = 8, width = 18)
    data_umap_health %>% 
      ggplot(aes(x, y, color= region, label = country)) +
      geom_point() +
      geom_text(aes(label=country),hjust=0, vjust=0) +
      theme_void()
  })
  
})
