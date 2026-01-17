# modules/organizations.R - Organizations tab module

# UI function for the Organizations module
organizationsUI <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    shinyjs::useShinyjs(),
    
    # Page heading
    h2("Plan Review Tracking"),
    
    # Output text that will appear when button is clicked
    h3(textOutput(ns("jurisdiction_txt"))),
    br(),
    
    # Buttons to trigger the database queries
    div(
      style = "display: inline-block;",
      actionButton(ns("jurisdiction_btn"), "Jurisdictions", class = "btn-primary"),
      actionButton(ns("centers_btn"), "Centers and CPPs", class = "btn-primary")
    ),
    br(), br(),
    
    # Output area for database results in table format with scrollable container
    # Displays approximately 12 rows with vertical scrollbar
    div(
      style = "height: 500px; width: 250px; overflow-y: auto; border: 1px solid #ddd;",
      tableOutput(ns("db_results"))
    )
  )
}

# Server function for the Organizations module
organizationsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to track which button was last clicked
    active_source <- reactiveVal(NULL)
    
    # Store the current data reactively
    current_data <- reactiveVal(data.frame())
    
    # Observe Jurisdictions button click
    observeEvent(input$jurisdiction_btn, {
      active_source("jurisdictions")
      current_data(get_cities_counties())
      
      # Update button styles - Jurisdictions is primary, Centers is default
      # Use shinyjs to update button classes
      shinyjs::removeClass(ns("jurisdiction_btn"), "btn-default")
      shinyjs::addClass(ns("jurisdiction_btn"), "btn-primary")
      shinyjs::removeClass(ns("centers_btn"), "btn-primary")
      shinyjs::addClass(ns("centers_btn"), "btn-default")
    })
    
    # Observe Centers and CPPs button click
    observeEvent(input$centers_btn, {
      active_source("centers")
      current_data(get_centers())
      
      # Update button styles - Centers is primary, Jurisdictions is default
      # Use shinyjs to update button classes
      shinyjs::removeClass(ns("centers_btn"), "btn-default")
      shinyjs::addClass(ns("centers_btn"), "btn-primary")
      shinyjs::removeClass(ns("jurisdiction_btn"), "btn-primary")
      shinyjs::addClass(ns("jurisdiction_btn"), "btn-default")
    })
    
    # Create reactive text that appears when button is clicked
    output$jurisdiction_txt <- renderText({
      source <- active_source()
      if (is.null(source)) {
        ""
      } else if (source == "jurisdictions") {
        "Jurisdictions"
      } else {
        "Centers and CPPs"
      }
    })
    
    # Create reactive output for database results as table
    # Display only DisplayName column
    output$db_results <- renderTable({
      data <- current_data()
      if (nrow(data) > 0 && "DisplayName" %in% names(data)) {
        data.frame(DisplayName = data$DisplayName)
      } else {
        data.frame()
      }
    })
  })
}
