# ui.R - User interface definition

fluidPage(
  titlePanel("Hello World Shiny App"),
  
  mainPanel(
    # Output text that will appear when button is clicked
    h3(textOutput("hello_text")),
    br(),
    
    # Button to trigger the hello world message and database query
    actionButton("hello_button", "Click me!", class = "btn-primary"),
    br(), br(),
    
    # Output area for database results in table format
    tableOutput("db_results")
  )
)
