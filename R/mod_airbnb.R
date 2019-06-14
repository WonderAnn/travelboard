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
mod_airbnb_ui <- function(id, dest){
  ns <- NS(id)
  
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tagList(
    tags$h1(paste(dest, "Airbnb", sep = "-")),
    fluidRow(
      box(title = dest, plotOutput(ns("plot1"), height = 250)),
      
      box(
        title = "Controls",
        sliderInput(ns("slider"), "Number of observations:", 1, 100, 50)
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_airbnb
#' @export
#' @keywords internal
mod_airbnb_server <- function(input, output, session, dest){
  ns <- session$ns
  
  # This is just an example Server to be modified
  # Please change for your purpose
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data, main = dest())
  })
}
    
## To be copied in the UI
# mod_airbnb_ui("airbnb_ui_1")
    
## To be copied in the server
# callModule(mod_airbnb_server, "airbnb_ui_1")
 
