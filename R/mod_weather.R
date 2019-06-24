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
#' @export 
#' @importFrom shiny NS tagList r
mod_weather_ui <- function(id, dest){
  ns <- NS(id)
  
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tagList(
    
    tags$h1(paste(dest, "Weather", sep = "-")),
    
    fluidRow(
      tabBox(
        title = "First tabBox",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Tab1", "First tab content"),
        tabPanel("Tab2", "Tab content 2"),
        tabPanel("Tab3", "Tab content 3")
      ),
      tabBox(
        side = "left", height = "250px",
        selected = "Tab2",
        tabPanel("Tab1", "Tab content 1"),
        tabPanel("Tab2", "Tab content 2"), 
        tabPanel("Tab3", "Tab content 3")
      ),
      tabBox(
        side = "left", height = "250px",
        selected = "Tab3",
        tabPanel("Tab1", "Tab content 1"),
        tabPanel("Tab2", "Tab content 2"), 
        tabPanel("Tab3", "Tab content 3")
      )
    ),
    
    fluidBox(
      box(title = dest, plotOutput(ns("Map"), height = 800)),
      box(title = dest, plotOutput(ns("Historical_info"), height = 800)),
      box(title = dest, plotOutput(ns("Forecast_info"), height = 800))
      ) 
  )
}
    
# Module Server
    
#' @rdname mod_weather
#' @export
#' @keywords internal
    
mod_weather_server <- function(input, output, session, dest){
  ns <- session$ns
  
  # This is just an example Server to be modified
  # Please change for your purpose
#  histdata <- rnorm(500)
#  output$plot1 <- renderPlot({
#    data <- histdata[seq_len(input$slider)]
#    hist(data, main = dest())
#  })

map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenWeatherMap.Rain,
                   options = providerTileOptions(apiKey=apikey)) %>%
  setView(24.80927, 35.24012, zoom = 9)
}    
## To be copied in the UI
# mod_weather_ui("weather_ui_1")

    
## To be copied in the server
# callModule(mod_weather_server, "weather_ui_1")
 
