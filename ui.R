# ui.R - User interface definition

fluidPage(
  titlePanel("Plan Review Tracking"),
  
  mainPanel(
    # Output text that will appear when button is clicked
    h3(textOutput("hello_text")),
    br(),
    
    # Button to trigger the hello world message and database query
    actionButton("jurisdiction_button", "Jurisdictions", class = "btn-primary"),
    br(), br(),
    
    # Output area for database results in table format with scrollable container
    # Displays approximately 12 rows with vertical scrollbar
    div(
      style = "height: 500px; width: 250px; overflow-y: auto; border: 1px solid #ddd;",
      tableOutput("db_results")
    )
  )
)
