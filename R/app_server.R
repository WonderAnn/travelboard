#' @import shiny
app_server <- function(input, output, session) {
  output$ui <- renderUI({
    item <- input$menu
    switch(item, 
           airbnb = mod_airbnb_ui("airbnb_ui_1", input$dest),
           news = mod_news_ui("news_ui_1", input$dest),
           places = mod_places_ui("places_ui_1", input$dest),
           statistics = mod_statistics_ui("statistics_ui_1", input$dest),
           weather = mod_weather_ui("weather_ui_1", input$dest)
           )
  })

  # List the first level callModules here
  callModule(mod_airbnb_server, "airbnb_ui_1", reactive(input$dest))
  callModule(mod_news_server, "news_ui_1", reactive(input$dest))
  callModule(mod_places_server, "places_ui_1", reactive(input$dest))
  callModule(mod_statistics_server, "statistics_ui_1", reactive(input$dest))
  callModule(mod_weather_server, "weather_ui_1", reactive(input$dest))
}
