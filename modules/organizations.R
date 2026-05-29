# modules/organizations.R - Organizations tab module

# UI function for the Organizations module
organizationsUI <- function(id) {
  ns <- NS(id)

  page_fluid(
    shinyjs::useShinyjs(),

    # Page heading
    h2("Plan Review Tracking"),

    h3(textOutput(ns("jurisdiction_txt"))),
    br(),
    actionButton(ns("jurisdiction_btn"), "Jurisdictions", class = "btn-primary"),
    actionButton(ns("centers_btn"), "Centers and CPPs", class = "btn-primary"),
    br(), br(),
    selectInput(ns("org_select"), label = NULL, choices = character(0),
                selectize = FALSE, width = "200px"),
    br(),
    div(
      style = "border: 1px solid #ddd;",
      uiOutput(ns("selected_detail"))
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

    actions_trigger <- reactiveVal(0)
    actions_proxy   <- DT::dataTableProxy("actions_table", session = session)

    actions_data <- reactive({
      actions_trigger()
      id_val <- input$org_select
      req(id_val, id_val != "")
      get_actions(as.integer(id_val))
    })

    output$actions_table <- DT::renderDT({
      data <- actions_data()
      if (nrow(data) > 0 && "ActionsDate" %in% names(data)) {
        data$ActionsDate <- format(as.Date(data$ActionsDate), "%m/%d/%Y")
      }
      cols <- c("ActionsDate", "Actions", "ActionsFile")
      data <- data[, intersect(cols, names(data)), drop = FALSE]
      names(data)[names(data) == "ActionsDate"]  <- "Date"
      names(data)[names(data) == "Actions"]      <- "Action"
      names(data)[names(data) == "ActionsFile"]  <- "File Location"
      DT::datatable(data, selection = "single", rownames = FALSE,
                    options = list(dom = "t", paging = FALSE,
                                   scrollY = "300px", scrollCollapse = TRUE,
                                   autoWidth = TRUE,
                                   columnDefs = list(list(width = "110px", targets = 0))))
    })

    output$actions_form <- renderUI({
      idx  <- input$actions_table_rows_selected
      data <- actions_data()
      has_selection <- !is.null(idx) && length(idx) > 0 && nrow(data) >= idx

      if (has_selection) {
        row      <- data[idx, ]
        date_val <- if (!is.na(row$ActionsDate)) as.Date(row$ActionsDate) else Sys.Date()
        text_val <- if (!is.na(row$Actions))     row$Actions     else ""
        file_val <- if (!is.na(row$ActionsFile)) row$ActionsFile else ""
        header   <- "Edit Selected Action"
      } else {
        date_val <- Sys.Date()
        text_val <- ""
        file_val <- ""
        header   <- "New Action"
      }

      wellPanel(
        strong(header),
        br(), br(),
        dateInput(ns("action_date"),  "Date",     value = date_val),
        textAreaInput(ns("action_text"),  "Action",   value = text_val, rows = 3),
        textInput(ns("action_file"),  "File/URL", value = file_val),
        actionButton(ns("actions_save_btn"), "Save", class = "btn-success"),
        if (has_selection) actionButton(ns("actions_delete_btn"), "Delete", class = "btn-danger"),
        if (has_selection) actionButton(ns("actions_clear_btn"),  "Clear")
      )
    })

    observeEvent(input$actions_save_btn, {
      id_val <- input$org_select
      req(id_val, id_val != "")
      idx <- input$actions_table_rows_selected

      if (!is.null(idx) && length(idx) > 0) {
        action_id <- actions_data()$ID[idx]
        update_action(action_id, input$action_date, input$action_text, input$action_file)
      } else {
        insert_action(as.integer(id_val), input$action_date, input$action_text, input$action_file)
      }
      actions_trigger(actions_trigger() + 1)
      DT::selectRows(actions_proxy, NULL)
    })

    observeEvent(input$actions_delete_btn, {
      idx <- input$actions_table_rows_selected
      req(idx)
      action_id <- actions_data()$ID[idx]
      delete_action(action_id)
      actions_trigger(actions_trigger() + 1)
      DT::selectRows(actions_proxy, NULL)
    })

    observeEvent(input$actions_clear_btn, {
      DT::selectRows(actions_proxy, NULL)
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
          tabPanel("Actions",
            DT::DTOutput(ns("actions_table")),
            uiOutput(ns("actions_form"))
          ),
          tabPanel("Correspondence"),
          tabPanel("Notes")
        )
      )
    })
  })
}
