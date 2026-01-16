# server.R - Server logic

function(input, output, session) {
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
    shinyjs::removeClass("jurisdiction_btn", "btn-default")
    shinyjs::addClass("jurisdiction_btn", "btn-primary")
    shinyjs::removeClass("centers_btn", "btn-primary")
    shinyjs::addClass("centers_btn", "btn-default")
  })
  
  # Observe Centers and CPPs button click
  observeEvent(input$centers_btn, {
    active_source("centers")
    current_data(get_centers())
    
    # Update button styles - Centers is primary, Jurisdictions is default
    # Use shinyjs to update button classes
    shinyjs::removeClass("centers_btn", "btn-default")
    shinyjs::addClass("centers_btn", "btn-primary")
    shinyjs::removeClass("jurisdiction_btn", "btn-primary")
    shinyjs::addClass("jurisdiction_btn", "btn-default")
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
}
