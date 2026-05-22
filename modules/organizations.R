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
    
    fluidRow(
      column(6,
        div(
          style = "height: 500px; overflow-y: auto;",
          DT::DTOutput(ns("db_results"))
        )
      ),
      column(6,
        uiOutput(ns("selected_detail"))
      )
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
    
    table_proxy <- DT::dataTableProxy("db_results", session = session)

    # Observe Jurisdictions button click
    observeEvent(input$jurisdiction_btn, {
      active_source("jurisdictions")
      current_data(get_cities_counties())
      DT::selectRows(table_proxy, NULL)

      shinyjs::removeClass(ns("jurisdiction_btn"), "btn-default")
      shinyjs::addClass(ns("jurisdiction_btn"), "btn-primary")
      shinyjs::removeClass(ns("centers_btn"), "btn-primary")
      shinyjs::addClass(ns("centers_btn"), "btn-default")
    })

    # Observe Centers and CPPs button click
    observeEvent(input$centers_btn, {
      active_source("centers")
      current_data(get_centers())
      DT::selectRows(table_proxy, NULL)

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
    
    output$db_results <- DT::renderDT({
      data <- current_data()
      if (nrow(data) > 0 && "DisplayName" %in% names(data)) {
        DT::datatable(
          data.frame(DisplayName = data$DisplayName),
          selection = "single",
          rownames = FALSE,
          options = list(
            dom = "t",
            paging = FALSE,
            autoWidth = TRUE,
            #columnDefs = list(list(width = "400px", targets = 0))
          )
        )
      } else {
        DT::datatable(data.frame(DisplayName = character(0)),
          rownames = FALSE, options = list(dom = "t"))
      }
    })

    output$selected_detail <- renderUI({
      idx <- input$db_results_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      row <- current_data()[idx, ]
      tagList(
        h4(row$DisplayName),
        p(strong("ID: "), row$ID),
        p(strong("Type: "), row$JurisdictionType)
      )
    })
  })
}
