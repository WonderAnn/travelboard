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
#' 
#' @import tidyverse
#' @import leaflet
#' @import leaflet.extras
#' @import googleway
#' @import rgdal
#' 
mod_airbnb_ui <- function(id, dest){
  ns <- NS(id)
  
  # This is just an example UI to be modified
  # Please change for your purpose
  # Do not forget to put ns() around all input ids!!!
  tagList(
    
    tags$h1(paste(dest, "Airbnb", sep = "-")),
    
    fluidRow(
      box(title = "FILTERS", width = 10,
          div(style="display: inline-block;vertical-align:top; width: 200px;",sliderInput(ns("input_price"), "Price", 0, 500, value = c(0,500), dragRange = TRUE)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 100px;",sliderInput(ns("input_beds"), "No. of beds", 0, 50, value = c(0,50), dragRange = TRUE)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 80px;",numericInput(ns("input_rating"), "Min. rating", value = 0, min = 0, max = 100))
          )
    ),
    
    
    fluidRow(
      DT::dataTableOutput(ns("airbnb_table"))
    )
    
  )
}
    
# Module Server
    
#' @rdname mod_airbnb
#' @export
#' @keywords internal
mod_airbnb_server <- function(input, output, session, dest){
  ns <- session$ns
  
  # Loads the selected data
  dt_country <- reactive({
    dt_imported <- read_rds(paste0("data/airbnb/", tolower(dest()), ".rds"))
    dt_imported <- dt_imported %>% mutate(price = as.numeric(sub("$", "", price, fixed = TRUE)))
  })
  
  dt_filtered <- reactive({
    dt_filtered <- filter(dt_country(), 
                          price >= input$input_price[1] & price <= input$input_price[2],
                          beds >= input$input_beds[1] & beds <= input$input_beds[2],
                          review_scores_rating >= input$input_rating
                          )
  })
  
  observe({
    updateSliderInput(session, "input_price", value = c(0,max(dt_country()$price, na.rm = TRUE)), 
                      max = max(dt_country()$price, na.rm = TRUE))
    updateSliderInput(session, "input_beds", value = c(0,max(dt_country()$beds, na.rm = TRUE)), 
                      max = max(dt_country()$beds, na.rm = TRUE))
  })
  


  output$airbnb_table <- DT::renderDataTable(
      DT::datatable(data = dt_filtered()[, c("name","city","price", "beds", "review_scores_rating", "room_type")], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
  )
  
  
}
    
## To be copied in the UI
# mod_airbnb_ui("airbnb_ui_1")
    
## To be copied in the server
# callModule(mod_airbnb_server, "airbnb_ui_1")
 
