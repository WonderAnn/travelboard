
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
#' @importFrom shiny NS tagList 
mod_weather_ui <- function(id, dest){
  ns <- NS(id)
  
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tagList(
    tags$h1(paste(dest, "Weather", sep = "-")),
    fluidRow(
      column(4, selectInput("visual", "Choose visual", choices = c("Map", "Plot historical info"), selected = "Map")),
      column(4,dateInput("datefrom", label = "Date from", value = as.Date("2000-01-01"))),
      column(4,dateInput("dateto", label = "Date to", value = as.Date("2000-01-01")))
    ),
    fluidRow(
      box(title = dest, plotOutput(ns("plot1"), height = 250)),
      box(title = dest, plotOutput(ns("plot2"), height = 250)),
      uiOutput(ns("infobox")),
      
      box(
        title = "Controls",
        sliderInput(ns("slider"), "Number of observations:", 1, 100, 50)
      )
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
  
   histdata <- rnorm(500)
    output$plot1 <- renderPlot({
     data <- histdata[seq_len(input$slider)]
     hist(data, main = dest())
    })
  
    output$plot2 <- renderPlot({
      fname <- sprintf("~/workshop/data/weather/%s.rds", tolower(dest()))
      dat <- readRDS(fname)
      dat %>% 
        mutate(date = as.Date(date)) %>% 
        ggplot() + 
        geom_line(aes(date, value, color = datatype, group = datatype)) +  
        facet_wrap(~datatype, scales = "free_y", ncol = 1)
    })
  

  output$plot2 <- renderPlot({
    fname <- sprintf("~/workshop/data/weather/%s.rds", tolower(dest()))
    dat <- readRDS(fname)
    dat %>% 
      mutate(date = as.Date(date)) %>% 
      ggplot() + 
      geom_line(aes(date, value, color = datatype, group = datatype)) +  
      facet_wrap(~datatype, scales = "free_y", ncol = 1)
  })
  
  output$infobox <- renderUI({
    currtemp <- readRDS("~/workshop/data/weather/currtemp.rds")
    infoBox(title="Current Temp", value=currtemp[[dest()]]$temp)
  })
}

## To be copied in the UI
# mod_weather_ui("weather_ui_1")

## To be copied in the server
# callModule(mod_weather_server, "weather_ui_1")