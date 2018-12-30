library(shiny)
library(semantic.dashboard)
library(tidyverse)
library(crosstalk)
library(here)
library(ggmap)
library(maps)
library(ggthemes)
library(umap)
library(leaflet)
library(geojsonio)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  dataseth_input <- read_csv(here::here("data", "dataset_hnp_health_exp.csv"))
  
  indicators_input <- read_csv(here::here("data", "indicators_hnp_exp.csv"))
  
  comparable_health_indicators <- indicators_input %>%
    filter(str_detect(indicator_name, "%"),
           gen_topic == "Health",
           ! part_topic %in% " Population") %>%
    pull(series_code)
  
  countries_geo <- readRDS(here::here("app_hnp","data","countries_simple.rds"))
  
  output$coloredmap <- renderLeaflet({
     
    an_year_1 <- input$year1

    aids_countries <- dataseth_input %>% 
       select(country_code, year, SH.DYN.AIDS, SP.POP.TOTL) %>%
       filter(year == an_year_1) %>%
       mutate(proportion_aids = SH.DYN.AIDS / SP.POP.TOTL) %>%
       filter(!is.na(proportion_aids)) %>%
      select(-year)
     
    countries_geo@data <- countries_geo@data %>%
      left_join(aids_countries, by= c("ISO_A3"="country_code")) %>%
      select(ADMIN, ISO_A3, proportion_aids)
    
    leaflet(data = countries_geo) %>%
      setView(0, 37.8, 1.3) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(fillColor = ~binpal(proportion_aids),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "2",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#999",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = countries_geo$ADMIN,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
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
