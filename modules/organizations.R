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

    materials_refresh <- reactiveVal(0)
    materials_proxy   <- DT::dataTableProxy("materials_table", session = session)

    materials_data <- reactive({
      materials_refresh()
      id_val <- input$org_select
      req(id_val, id_val != "")
      get_materials(as.integer(id_val))
    })

    output$materials_table <- DT::renderDT({
      data <- materials_data()
      cols <- c("MaterialDateReceived", "MaterialTitle", "Status", "Staff_Reviewer", "ID")
      data <- data[, intersect(cols, names(data)), drop = FALSE]
      if ("MaterialDateReceived" %in% names(data)) {
        data$MaterialDateReceived <- format(as.Date(data$MaterialDateReceived), "%Y-%m-%d")
      }
      names(data)[names(data) == "MaterialDateReceived"] <- "Received"
      names(data)[names(data) == "MaterialTitle"]        <- "Title"
      names(data)[names(data) == "Staff_Reviewer"]       <- "Staff Reviewer"
      DT::datatable(data, selection = "single", rownames = FALSE,
                    options = list(dom = "t", paging = FALSE))
    })

    output$material_edit_form <- renderUI({
      idx <- input$materials_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)

      data        <- materials_data()
      material_id <- data$ID[idx]
      title       <- data$MaterialTitle[idx]
      fk_ids      <- get_material_fk_ids(material_id)
      status_opts <- get_status_lookup()
      staff_opts  <- get_staff_lookup()

      if (is.null(fk_ids) || nrow(fk_ids) == 0) return(NULL)

      wellPanel(
        strong(paste("Edit:", title)),
        br(), br(),
        fluidRow(
          column(4,
            selectInput(ns("edit_status"), "Status",
                        choices  = setNames(status_opts$ID, status_opts$Status),
                        selected = fk_ids$MaterialStatus)
          ),
          column(4,
            selectInput(ns("edit_staff"), "Staff Reviewer",
                        choices  = setNames(staff_opts$ID, staff_opts$Staff),
                        selected = fk_ids$MaterialStaffReviewer)
          ),
          column(4,
            br(),
            actionButton(ns("save_material_btn"),   "Save",   class = "btn-success"),
            actionButton(ns("cancel_material_btn"), "Cancel")
          )
        )
      )
    })

    observeEvent(input$save_material_btn, {
      idx <- input$materials_table_rows_selected
      req(idx)
      material_id <- materials_data()$ID[idx]
      update_material(material_id, input$edit_status, input$edit_staff)
      materials_refresh(materials_refresh() + 1)
      DT::selectRows(materials_proxy, NULL)
    })

    observeEvent(input$cancel_material_btn, {
      DT::selectRows(materials_proxy, NULL)
    })

    contacts_trigger <- reactiveVal(0)

    contacts_data <- reactive({
      contacts_trigger()
      id_val <- input$org_select
      req(id_val, id_val != "")
      get_jurisdiction_contacts(as.integer(id_val))
    })

    output$contacts_panel <- renderUI({
      c <- contacts_data()
      if (is.null(c) || nrow(c) == 0) return(NULL)
      c[is.na(c)] <- ""
      staff_opts    <- get_staff_lookup()
      commerce_opts <- get_commerce_lookup()

      wellPanel(
        textInput(ns("edit_address"), "Address", value = c$Address),
        fluidRow(
          column(6,
            textInput(ns("edit_cname1"),  "Contact 1 Name", value = c$ContactName1),
            textInput(ns("edit_ctitle1"), "Title",          value = c$ContactTitle1),
            textInput(ns("edit_cphone1"), "Phone",          value = c$ContactPhone1),
            textInput(ns("edit_cemail1"), "Email",          value = c$ContactEmail1),
            selectInput(ns("edit_staff_contact"), "Staff Assignment",
                        choices  = setNames(staff_opts$ID, staff_opts$Staff),
                        selected = c$StaffContact)
          ),
          column(6,
            textInput(ns("edit_cname2"),  "Contact 2 Name", value = c$ContactName2),
            textInput(ns("edit_ctitle2"), "Title",          value = c$ContactTitle2),
            textInput(ns("edit_cphone2"), "Phone",          value = c$ContactPhone2),
            textInput(ns("edit_cemail2"), "Email",          value = c$ContactEmail2),
            selectInput(ns("edit_commerce_contact"), "Commerce Assignment",
                        choices  = setNames(commerce_opts$ID, commerce_opts$CommerceContact),
                        selected = c$CommerceContact)
          )
        ),
        actionButton(ns("contacts_save_btn"),   "Save",   class = "btn-success"),
        actionButton(ns("contacts_cancel_btn"), "Cancel")
      )
    })

    observeEvent(input$contacts_save_btn, {
      id_val <- input$org_select
      req(id_val, id_val != "")
      update_jurisdiction_contacts(
        as.integer(id_val),
        input$edit_address,
        input$edit_cname1, input$edit_ctitle1, input$edit_cphone1, input$edit_cemail1,
        input$edit_staff_contact,
        input$edit_cname2, input$edit_ctitle2, input$edit_cphone2, input$edit_cemail2,
        input$edit_commerce_contact
      )
      contacts_trigger(contacts_trigger() + 1)
    })

    observeEvent(input$contacts_cancel_btn, {
      contacts_trigger(contacts_trigger() + 1)
    })

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
          tabPanel("Materials",
            DT::DTOutput(ns("materials_table")),
            uiOutput(ns("material_edit_form"))
          ),
          tabPanel("Contacts", uiOutput(ns("contacts_panel"))),
          tabPanel("Actions"),
          tabPanel("Correspondence"),
          tabPanel("Notes")
        )
      )
    })
  })
}
