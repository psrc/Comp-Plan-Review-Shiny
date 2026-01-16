# server.R - Server logic

function(input, output) {
  # Create reactive text that appears when button is clicked
  output$hello_text <- renderText({
    if (input$hello_button > 0) {
      "Hello, World!"
    } else {
      ""
    }
  })
  
  # Create reactive output for database results as table
  output$db_results <- renderTable({
    if (input$hello_button > 0) {
      # Get data from the database when button is clicked
      #get_tenure_data()
      get_cities_counties()
    } else {
      # Return empty data frame when button hasn't been clicked
      data.frame()
    }
  })
}
