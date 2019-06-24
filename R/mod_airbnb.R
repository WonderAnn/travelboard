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
      box(title = "FILTERS", width = '100%',
          div(style="display: inline-block;vertical-align:top; width: 200px;",sliderInput(ns("input_price"), "Price", 0, 500, value = c(0,500), dragRange = TRUE)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 100px;",sliderInput(ns("input_beds"), "No. of beds", 0, 50, value = c(0,50), dragRange = TRUE)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 80px;",numericInput(ns("input_rating"), "Min. rating", value = 0, min = 0, max = 100))
          )
    ),
    
    
    fluidRow(
      column(6, DT::dataTableOutput(ns("airbnb_table")) ),
      column(6, plotOutput(ns("price_distr"))) 
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
  
  # Country level price data 
  total_price  <- reactive({
    data.frame(sample = "Total", price = as.matrix(dt_country()$price))
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
  
  output$price_distr <- renderPlot({
    
    dt_price <- rbind(total_price(), data.frame(sample = "Selected", price = dt_filtered()$price))
    means    <- dt_price %>% group_by(sample) %>% summarize(mean_price = mean(price, na.rm = TRUE))
    
    ggplot(dt_price, aes(x = price, group = sample, color = sample, fill = sample)) + 
      geom_density(alpha = 0.4) + 
      labs(title = "Price distribution",
           x     = "Price / Night", 
           y     = "Relative frequency") +
      geom_vline(data = means, 
                 aes(xintercept = means$mean_price, 
                     color      = sample),
                 linetype   = "dashed")
    
    
  })
  
}
    
## To be copied in the UI
# mod_airbnb_ui("airbnb_ui_1")
    
## To be copied in the server
# callModule(mod_airbnb_server, "airbnb_ui_1")
 
