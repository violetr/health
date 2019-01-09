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
library(htmltools)
library(gghighlight)

is_in_bounds <- function(contry_coord, bounds) {
  country_lat <- contry_coord[[1]]
  country_long <- contry_coord[[2]]  
  return(country_lat > bounds$south & 
           country_lat < bounds$north & 
           country_long < bounds$east & 
           country_long > bounds$west)
}

countries_in_bounding_box <- function(geo_data, sel_bounds) {
  
  filter_in_bounds <- geo_data@polygons %>%
    purrr::map( ~ .x@labpt) %>%
    purrr::map_lgl(is_in_bounds, bounds = sel_bounds)
  
  geo_data@data %>%
    filter(filter_in_bounds) %>%
    pull(ISO_A3)
}

# Define server logic
shinyServer(function(input, output) {

  ### read data ####

  # despues filtrar solo variables utilizadas
  dataseth_input <- read_csv(here::here("data", "dataset_hnp_health_exp.csv"))

  indicators_input <- read_csv(here::here("data", "indicators_hnp_exp.csv"))

  # countries_input <- read_csv(here::here("data", "countries_hnp_exp.csv"))

  owundata_input <- readRDS(here::here("data", "overweigh_undernourish.rds"))

  countries_geo <- readRDS(here::here("app_hnp", "data", "countries_simple.rds"))

  ### common variables ####

  # esto tambien puede hacerse antes y cargar
  comparable_health_indicators <- indicators_input %>%
    filter(
      str_detect(indicator_name, "%"),
      gen_topic == "Health",
      !part_topic %in% " Population"
    ) %>%
    pull(series_code)

  binpal <- colorBin("Reds", domain = c(0, 18), 9, pretty = FALSE)
  
 # observeEvent(input, {cat("You have chosen:", input[[ns(map_bounds)]])})
  
  selected_countries <- reactive({
    cat(file=stderr(), "drawing histogram with", input$map_bounds, "bins", "\n")
    if (is.null(input$coloredmap_bounds) || is.na(input$coloredmap_bounds)){
      "all"
    } else {
      sel_bounds <- input$coloredmap_bounds
      print(sel_bounds)
      countries_in_bounding_box(countries_geo, sel_bounds)
    }
  })
  
  
  
  ### choropleth ####

  output$coloredmap <- renderLeaflet({
    an_year_1 <- input$year1

    aids_countries <- dataseth_input %>%
      select(country_code, year, SH.DYN.AIDS, SP.POP.TOTL) %>%
      filter(year == an_year_1) %>%
      mutate(proportion_aids = SH.DYN.AIDS / SP.POP.TOTL * 100) %>%
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

  output$time <- renderPlot({
    
    if (selected_countries() == "all") {
      ggplot() +
        geom_line(data = owundata_input, aes(
          x = year,
          y = value,
          group = country_code,
          color = region
        )) +
        facet_wrap(~measure) +
        scale_y_continuous(limits = c(0, 0.8)) +
        scale_x_discrete(breaks = c(1975, 1990, 2005, 2016)) +
        ylab("Percent of adults (%)")
      
    } else {
      out_region <- owundata_input %>%
        filter(!country_code %in% selected_countries())
      in_region <- owundata_input %>%
        filter(country_code %in% selected_countries())  
      
      ggplot() +
        geom_line(
          data = out_region, aes(year, value, group = country_code),
          colour = alpha("grey", 0.65)
        ) +
        geom_line(data = in_region, aes(
          x = year,
          y = value,
          group = country_code,
          color = region
        )) +
        facet_wrap(~measure) +
        scale_y_continuous(limits = c(0, 0.8)) +
        scale_x_discrete(breaks = c(1975, 1990, 2005, 2016)) +
        ylab("Percent of adults (%)")
      
    }
  })

  ### UMAP embedding ####

  output$pumap <- renderPlot({
    an_year_2 <- input$year2

    dataset_comparable_health <- dataseth_input %>%
      filter(year == an_year_2) %>%
      select(country_name, country_code, region, one_of(comparable_health_indicators)) %>%
      select_if(function(x) (sum(is.na(x)) <= (0.2 * length(x))) || is.character(x)) %>%
      drop_na()

    print(dataset_comparable_health)

    data_to_umap <- as.matrix(dataset_comparable_health[, 4:ncol(dataset_comparable_health)])

    data_health_umap <- umap(data_to_umap)

    data_umap_health <- tibble(
      x = data_health_umap$layout[, 1],
      y = data_health_umap$layout[, 2],
      country = dataset_comparable_health$country_code,
      region = dataset_comparable_health$region
    )

    data_umap_health %>%
      ggplot(aes(x, y, color = region, label = country)) +
      geom_point() +
      geom_text(aes(label = country), hjust = 0, vjust = 0) +
      theme_void()
  })
})
