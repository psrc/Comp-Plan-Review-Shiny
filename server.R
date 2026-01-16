# server.R - Server logic

function(input, output) {
  # Store the full data reactively for future filter use
  # Contains: ID, DisplayName, JurisdictionType
  cities_counties_data <- eventReactive(input$hello_button, {
    get_cities_counties()
  })
  
  # Create reactive text that appears when button is clicked
  output$hello_text <- renderText({
    if (input$hello_button > 0) {
      "Jurisdictions"
    } else {
      ""
    }
  })
  
  # Create reactive output for database results as table
  # Display only DisplayName column, but full data is available in cities_counties_data()
  output$db_results <- renderTable({
    if (input$hello_button > 0) {
      data <- cities_counties_data()
      # Display only DisplayName column
      data.frame(DisplayName = data$DisplayName)
    } else {
      # Return empty data frame when button hasn't been clicked
      data.frame()
    }
  })
}
