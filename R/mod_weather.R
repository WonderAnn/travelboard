
# Module UI

#' @title   mod_weather_ui and mod_weather_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_weather
#'
#' @keywords internal
#' @import leaflet
#' @export 
#' @importFrom shiny NS tagList 
mod_weather_ui <- function(id, dest){
  ns <- NS(id)
  tagList(
    tags$h1(paste(dest, "Weather", sep = "-")),
    fluidPage(
      tabsetPanel(id = "tabs", 
                  tabPanel("Map", box(title = dest, leafletOutput(ns("map"), width = 800 ))),
                  tabPanel("forecast", box(title = dest, plotOutput(ns("plot1"), height = 250)))
             #     tabPanel("Historical", plotOutput("Plot2"))
      )))
     
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  
}

# Module Server

#' @rdname mod_weather
#' @export
#' @keywords internal

mod_weather_server <- function(input, output, session, dest){
  ns <- session$ns
  
  
  # This is just an example Server to be modified
  # Please change for your purpose
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data, main = dest())
  })
  
  
  apikey <- "d7eae13fe954ea0e04b0c40a172c4a10"
  owmr_settings(apikey)
  mymap = leaflet() %>% 
    addTiles() %>% 
    addProviderTiles(providers$OpenWeatherMap.Temperature, options = providerTileOptions(apiKey=apikey)) %>% 
    setView(24.80927, 35.24012, zoom = 9)
  
  output$map <- renderLeaflet(mymap)
  
#  output$plot2 <- renderPlot({
#    fname <- sprintf("~/workshop/data/weather/%s.rds", tolower(dest()))
#    dat <- readRDS(fname)
#    dat %>% 
#      mutate(date = as.Date(date)) %>% 
#      ggplot() + 
#      geom_line(aes(date, value, color = datatype, group = datatype)) +  
#      facet_wrap(~datatype, scales = "free_y", ncol = 1)
#  })
}

## To be copied in the UI
# mod_weather_ui("weather_ui_1")

## To be copied in the server
# callModule(mod_weather_server, "weather_ui_1")