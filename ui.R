# ui.R - User interface definition

fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Plan Review Tracking"),
  mainPanel(
    # Output text that will appear when button is clicked
    h3(textOutput("jurisdiction_txt")),
    br(),
    
    # Buttons to trigger the database queries
    div(
      style = "display: inline-block;",
      actionButton("jurisdiction_btn", "Jurisdictions", class = "btn-primary"),
      actionButton("centers_btn", "Centers and CPPs", class = "btn-primary")
    ),
    br(), br(),
    
    # Output area for database results in table format with scrollable container
    # Displays approximately 12 rows with vertical scrollbar
    div(
      style = "height: 500px; width: 250px; overflow-y: auto; border: 1px solid #ddd;",
      tableOutput("db_results")
    )
  )

)
