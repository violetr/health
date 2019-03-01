library(shiny)
library(semantic.dashboard)
library(tidyverse)
library(feather)
library(here)
library(maps)
library(umap)
library(leaflet)
library(geojsonio)
library(htmltools)
library(missMDA)


# Define server logic
shinyServer(function(input, output) {

  ### read data ####

  # despues filtrar solo variables utilizadas
  dataset_hnp_health <- read_feather("dataset_hnp_health_exp")
  countries_hnp_keep <- read_feather("countries_hnp_exp")
  indicators_hnp_keep <- read_feather("indicators_hnp_exp")
  dataset_hnp <- read_feather("dataset_hnp_exp")

  #owundata_input <- readRDS(here::here("data", "overweigh_undernourish.rds"))
  dataset_hnp_education <- read_feather("dataset_hnp_education_exp")
  countries_geo <- readRDS("countries_simple.rds")
  dataset_hnp_others <- read_feather("dataset_hnp_others_exp")
  
  real_countries <- countries_hnp_keep %>% # real countries are not aggregations
    filter(!is.na(currency_unit)) %>% # I identify them because they don't
    pull(table_name) # have currency unit
  ### common variables ####

  binpal <- colorBin("Reds", domain = c(0, 18), 9, pretty = FALSE)
  
 # observeEvent(input, {cat("You have chosen:", input[[ns(map_bounds)]])})
  
  
  ### choropleth ####

  output$coloredmap <- renderLeaflet({
    
    an_year_1 <- input$year1

    aids_countries <- dataset_hnp_health %>%
      select(country_code, year, SH.HIV.TOTL, SP.POP.TOTL) %>%
      filter(year == an_year_1) %>%
      mutate(proportion_aids = SH.HIV.TOTL / SP.POP.TOTL * 100) %>%
      filter(!is.na(proportion_aids)) %>%
      select(-year)

    countries_geo_year <- countries_geo

    countries_geo_year@data <- countries_geo_year@data %>%
      left_join(aids_countries, by = c("ISO_A3" = "country_code")) %>%
      select(ADMIN, ISO_A3, proportion_aids)

    labels_geo_year <-
      paste0(
        "<strong>", countries_geo_year$ADMIN, "</strong>",
        ifelse(is.na(countries_geo_year$proportion_aids),
          "",
          paste0(
            "<br/>",
            round(countries_geo_year$proportion_aids, 2),
            "%"
          )
        )
      ) %>%
      lapply(htmltools::HTML)

    leaflet(data = countries_geo_year) %>%
      setView(35, 37.8, 1.4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv("MAPBOX_ACCESS_TOKEN")
      )) %>%
      addPolygons(
        fillColor = ~ binpal(proportion_aids),
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
          bringToFront = TRUE
        ),
        label = labels_geo_year,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = binpal,
        values = ~ (countries_geo_year$proportion_aids),
        opacity = 0.7,
        title = "incidence of HIV (%)",
        position = "bottomright",
        na.label = "No data"
      )
  })

  # output$time <- renderPlot({
  #   
  #   if (selected_countries() == "all") {
  #     ggplot() +
  #       geom_line(data = owundata_input, aes(
  #         x = year,
  #         y = value,
  #         group = country_code,
  #         color = region
  #       )) +
  #       facet_wrap(~measure) +
  #       scale_y_continuous(limits = c(0, 0.8)) +
  #       scale_x_discrete(breaks = c(1975, 1990, 2005, 2016)) +
  #       ylab("Percent of adults (%)")
  #     
  #   } else {
  #     out_region <- owundata_input %>%
  #       filter(!country_code %in% selected_countries())
  #     in_region <- owundata_input %>%
  #       filter(country_code %in% selected_countries())  
  #     
  #     ggplot() +
  #       geom_line(
  #         data = out_region, aes(year, value, group = country_code),
  #         colour = alpha("grey", 0.65)
  #       ) +
  #       geom_line(data = in_region, aes(
  #         x = year,
  #         y = value,
  #         group = country_code,
  #         color = region
  #       )) +
  #       facet_wrap(~measure) +
  #       scale_y_continuous(limits = c(0, 0.8)) +
  #       scale_x_discrete(breaks = c(1975, 1990, 2005, 2016)) +
  #       ylab("Percent of adults (%)")
  #     
  #   }
  # })

  ### UMAP embedding ####

  output$pumap <- renderPlot({
    an_year_2 <- input$year2
    custom.config <- umap.defaults
    custom.config$random_state <- 44
    custom.config$min_dist <- 0.1
    if(input$topics == "HIV") {
      # setting umap options for reproducibility
      # I keep comparable indicators (%) HIV related
      comparable_health_indicators <- indicators_hnp_keep %>%
        filter(
          (str_detect(indicator_name, "%") | str_detect(indicator_name, "rate")) & (str_detect(indicator_name, "AIDS") | str_detect(indicator_name, "HIV") |
                                                                                      str_detect(indicator_name, "Condom")),
          gen_topic == "Health",
          !part_topic %in% " Population"
        ) %>%
        pull(series_code)
      
      dataset_comparable <- dataset_hnp_health %>%
        filter(year == an_year_2) %>%
        select(country_name, country_code, region, one_of(comparable_health_indicators)) %>%
        select_if(function(x) (sum(is.na(x)) <= (0.4 * length(x))) || is.character(x))
      
      # I impute missing data
      res.comp <- imputePCA(as.data.frame(dataset_comparable[, 4:ncol(dataset_comparable)]))
      dataset_comparable_health_com <- res.comp$comp
      
      data_to_umap <- as.matrix(dataset_comparable_health_com)
    }
    if (input$topics == "Gender") {
      comparable_gender_indicators <- indicators_hnp_keep %>%
        filter(((str_detect(indicator_name, "%") | str_detect(indicator_name, "rate"))  & (str_detect(indicator_name, "female") | str_detect(indicator_name, "women")))) %>%
        pull(series_code)
      
      dataset_hnp_gender = dataset_hnp %>% 
        left_join(indicators_hnp_keep, by= c("indicator_code"="series_code")) %>%
        select(-topic, -indicator_name.x, -indicator_name.y, -long_definition, -part_topic, -gen_topic) %>%
        filter(country_name %in% real_countries) %>%
        filter(indicator_code %in% comparable_gender_indicators) %>%
        gather(key = year, value = indicator_value, `1960`:`2017`) %>%
        spread(key = indicator_code, value = indicator_value) %>%
        left_join(countries_hnp_keep, by = c("country_code"= "country_code")) 
      
      dataset_comparable <- dataset_hnp_gender %>%
        filter(year == an_year_2) %>%
        select(country_name, country_code, region, one_of(comparable_gender_indicators))  %>%
        select_if(function(x) (sum(is.na(x)) <= (0.3 * length(x))) || is.character(x)) %>%
        drop_na()
      
      data_to_umap <- as.matrix(dataset_comparable[, 4:ncol(dataset_comparable)])  
    }
    if (input$topics == "Economy") {
      comparable_oth_indicators <- indicators_hnp_keep %>%
        filter((str_detect(indicator_name, "%") & !str_detect(indicator_name, "female") & !str_detect(indicator_name, "male") & !str_detect(indicator_name, "women")) |
                 (str_detect(indicator_name, "HCI") & !str_detect(indicator_name, "female") & !str_detect(indicator_name, "male")) |
                 str_detect(indicator_name, "GNI"), 
               gen_topic != "Education",
               gen_topic != "Health") %>%
        pull(series_code)
      
      dataset_comparable <- dataset_hnp_others %>%
        filter(year == an_year_2) %>%
        select(country_name, country_code, region, one_of(comparable_oth_indicators))  %>%
        select_if(function(x) (sum(is.na(x)) <= (0.3 * length(x))) || is.character(x)) %>%
        drop_na()
      
      data_to_umap <- as.matrix(dataset_comparable[, 4:ncol(dataset_comparable)])
      
    }
    if (input$topics == "Education") {
      comparable_ed_indicators <- indicators_hnp_keep %>%
        filter((str_detect(indicator_name, "%") & !str_detect(indicator_name, "female") & !str_detect(indicator_name, "male") & !str_detect(indicator_name, "women")),
               gen_topic == "Education") %>%
        pull(series_code)
      
      dataset_comparable <- dataset_hnp_education %>%
        filter(year == an_year_2) %>%
        select(country_name, country_code, region, one_of(comparable_ed_indicators))  %>%
        select_if(function(x) (sum(is.na(x)) <= (0.3 * length(x))) || is.character(x)) %>%
        drop_na()
      
      data_to_umap <- as.matrix(dataset_comparable[, 4:ncol(dataset_comparable)])
    }
    
    data_health_umap <- umap(data_to_umap, config = custom.config)
    
    data_umap_health <- tibble(
      x = data_health_umap$layout[, 1],
      y = data_health_umap$layout[, 2],
      country = dataset_comparable$country_code,
      region = dataset_comparable$region
    )
    
    # pdf(here::here("figures/umap_1995.pdf"), height = 8, width = 18)
    data_umap_health %>%
      ggplot(aes(x, y, color = region, label = country)) +
      geom_point() +
      geom_text(aes(label = country), hjust = 0, vjust = 0, show.legend = FALSE) +
      theme_void()
  })
})
