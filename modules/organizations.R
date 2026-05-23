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
      column(3,
        selectInput(ns("org_select"), label = NULL, choices = character(0),
                    selectize = FALSE, width = "100%")
      ),
      column(9,
        div(
          style = "border: 1px solid #ddd;",
          uiOutput(ns("selected_detail"))
        )
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

    # Observe Jurisdictions button click
    observeEvent(input$jurisdiction_btn, {
      active_source("jurisdictions")
      data <- get_cities_counties()
      current_data(data)
      updateSelectInput(session, "org_select",
                        choices = setNames(data$ID, data$DisplayName))

      shinyjs::removeClass(ns("jurisdiction_btn"), "btn-default")
      shinyjs::addClass(ns("jurisdiction_btn"), "btn-primary")
      shinyjs::removeClass(ns("centers_btn"), "btn-primary")
      shinyjs::addClass(ns("centers_btn"), "btn-default")
    })

    # Observe Centers and CPPs button click
    observeEvent(input$centers_btn, {
      active_source("centers")
      data <- get_centers()
      current_data(data)
      updateSelectInput(session, "org_select",
                        choices = setNames(data$ID, data$DisplayName))

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

    output$materials_table <- DT::renderDT({
      id_val <- input$org_select
      req(id_val, id_val != "")
      data <- get_materials(as.integer(id_val))
      cols <- c("MaterialDateReceived", "MaterialTitle", "Status", "Staff_Reviewer", "ID")
      data <- data[, intersect(cols, names(data)), drop = FALSE]
      if ("MaterialDateReceived" %in% names(data)) {
        data$MaterialDateReceived <- format(as.Date(data$MaterialDateReceived), "%Y-%m-%d")
      }
      names(data)[names(data) == "MaterialDateReceived"] <- "Received"
      names(data)[names(data) == "MaterialTitle"]        <- "Title"
      names(data)[names(data) == "Staff_Reviewer"]       <- "Staff Reviewer"
      data
    }, rownames = FALSE)

    output$selected_detail <- renderUI({
      id_val <- input$org_select
      if (is.null(id_val) || id_val == "") return(NULL)
      data <- current_data()
      row <- data[data$ID == as.integer(id_val), ]
      if (nrow(row) == 0) return(NULL)
      tagList(
        h4(row$DisplayName),
        p(strong("ID: "), row$ID),
        p(strong("Type: "), row$JurisdictionType),
        tabsetPanel(
          tabPanel("Materials", DT::DTOutput(ns("materials_table"))),
          tabPanel("Contacts"),
          tabPanel("Actions"),
          tabPanel("Correspondence"),
          tabPanel("Notes")
        )
      )
    })
  })
}
