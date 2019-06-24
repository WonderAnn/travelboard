# Module UI
  
#' @title   mod_airbnb_ui and mod_airbnb_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_airbnb
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import tidyverse
#' @import leaflet

mod_airbnb_ui <- function(id, dest){
  ns <- NS(id)

  tagList(
    tags$h1(paste(dest, "Airbnb", sep = "-")),
    fluidRow(
      leafletOutput(ns("map1"))
    )
  )  
}
    
# Module Server
    
#' @rdname mod_airbnb
#' @export
#' @keywords internal
mod_airbnb_server <- function(input, output, session, dest){
  ns <- session$ns
  
  mydata <- readRDS("~/workshop/data/airbnb/crete.rds")
  
  #reads the input data only if the dest() variable changes:
  # mydata <- reactive({
  #   readRDS(file = sprintf("%s.rds", dest()))
  # })
  
  output$map1 <- renderLeaflet({  
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng=mydata$longitude, 
                 lat=mydata$latitude, popup=mydata$name, 
                 clusterOptions = markerClusterOptions())
  })
  
}

 
