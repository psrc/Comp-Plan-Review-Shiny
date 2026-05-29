# modules/organizations.R - Organizations tab module

hfield <- function(label_text, input_el) {
  div(style = "display: flex; align-items: flex-start; margin-bottom: 10px;",
    tags$label(label_text,
               style = "min-width: 130px; text-align: right; padding-right: 10px; padding-top: 7px; margin-bottom: 0; font-weight: normal; white-space: nowrap;"),
    div(style = "flex: 1;", input_el)
  )
}

# UI function for the Organizations module
organizationsUI <- function(id) {
  ns <- NS(id)

  page_fluid(
    shinyjs::useShinyjs(),

    # Page heading
    #h2("Plan Review Tracking"),

    actionButton(ns("jurisdiction_btn"), "Jurisdictions",
                 style = "background-color:#E2F1CF;border-color:#c8ddb0;color:#333333;"),
    actionButton(ns("centers_btn"), "Centers and CPPs",
                 style = "background-color:#E2F1CF;border-color:#c8ddb0;color:#333333;"),
    br(), br(),
    selectInput(ns("org_select"), label = NULL, choices = character(0),
                selectize = FALSE, width = "200px"),
    br(),
    uiOutput(ns("selected_detail"))
  )
}

# Server function for the Organizations module
organizationsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store the current data reactively
    current_data <- reactiveVal(data.frame())

    # Observe Jurisdictions button click
    observeEvent(input$jurisdiction_btn, {
      data <- get_cities_counties()
      current_data(data)
      updateSelectInput(session, "org_select",
                        choices = setNames(data$ID, data$DisplayName))

      shinyjs::runjs(paste0(
        "document.getElementById('", ns("jurisdiction_btn"), "').style.cssText='background-color:#C0E095;border-color:#a8cc7a;color:#333333;';",
        "document.getElementById('", ns("centers_btn"), "').style.cssText='background-color:#E2F1CF;border-color:#c8ddb0;color:#333333;';"
      ))
    })

    # Observe Centers and CPPs button click
    observeEvent(input$centers_btn, {
      data <- get_centers()
      current_data(data)
      updateSelectInput(session, "org_select",
                        choices = setNames(data$ID, data$DisplayName))

      shinyjs::runjs(paste0(
        "document.getElementById('", ns("centers_btn"), "').style.cssText='background-color:#C0E095;border-color:#a8cc7a;color:#333333;';",
        "document.getElementById('", ns("jurisdiction_btn"), "').style.cssText='background-color:#E2F1CF;border-color:#c8ddb0;color:#333333;';"
      ))
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
        data$MaterialDateReceived <- format(as.Date(data$MaterialDateReceived), "%m/%d/%Y")
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
      if (nrow(data) > 0) {
        data$Actions <- paste0(
          ifelse(is.na(data$Actions), "", data$Actions),
          ifelse(is.na(data$ActionsFile) | data$ActionsFile == "", "",
                 paste0("<br><span class='file-path'>", data$ActionsFile, "</span>"))
        )
      }
      cols <- c("ActionsDate", "Actions")
      data <- data[, intersect(cols, names(data)), drop = FALSE]
      names(data)[names(data) == "ActionsDate"] <- "Date"
      names(data)[names(data) == "Actions"]     <- "Action/File Path"
      DT::datatable(data, selection = "single", rownames = FALSE, escape = FALSE,
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

    correspondence_trigger <- reactiveVal(0)
    correspondence_proxy   <- DT::dataTableProxy("correspondence_table", session = session)

    correspondence_data <- reactive({
      correspondence_trigger()
      id_val <- input$org_select
      req(id_val, id_val != "")
      get_correspondence(as.integer(id_val))
    })

    output$correspondence_table <- DT::renderDT({
      data <- correspondence_data()
      if (nrow(data) > 0 && "CorrespondenceDate" %in% names(data)) {
        data$CorrespondenceDate <- format(as.Date(data$CorrespondenceDate), "%m/%d/%Y")
      }
      if (nrow(data) > 0) {
        data$CorrespondenceDescription <- paste0(
          ifelse(is.na(data$CorrespondenceDescription), "", data$CorrespondenceDescription),
          ifelse(is.na(data$CorrespondenceFile) | data$CorrespondenceFile == "", "",
                 paste0("<br><span class='file-path'>", data$CorrespondenceFile, "</span>"))
        )
      }
      cols <- c("CorrespondenceDate", "CorrespondenceDescription")
      data <- data[, intersect(cols, names(data)), drop = FALSE]
      names(data)[names(data) == "CorrespondenceDate"]        <- "Date"
      names(data)[names(data) == "CorrespondenceDescription"] <- "Correspondence/File Path"
      DT::datatable(data, selection = "single", rownames = FALSE, escape = FALSE,
                    options = list(dom = "t", paging = FALSE,
                                   scrollY = "300px", scrollCollapse = TRUE,
                                   autoWidth = TRUE,
                                   columnDefs = list(list(width = "110px", targets = 0))))
    })

    output$correspondence_form <- renderUI({
      idx  <- input$correspondence_table_rows_selected
      data <- correspondence_data()
      has_selection <- !is.null(idx) && length(idx) > 0 && nrow(data) >= idx

      if (has_selection) {
        row      <- data[idx, ]
        date_val <- if (!is.na(row$CorrespondenceDate)) as.Date(row$CorrespondenceDate) else Sys.Date()
        desc_val <- if (!is.na(row$CorrespondenceDescription)) row$CorrespondenceDescription else ""
        file_val <- if (!is.na(row$CorrespondenceFile)) row$CorrespondenceFile else ""
        header   <- "Edit Selected Correspondence"
      } else {
        date_val <- Sys.Date()
        desc_val <- ""
        file_val <- ""
        header   <- "New Correspondence"
      }

      wellPanel(
        strong(header),
        br(), br(),
        dateInput(ns("correspondence_date"), "Date",          value = date_val),
        textAreaInput(ns("correspondence_desc"), "Description", value = desc_val, rows = 3),
        textInput(ns("correspondence_file"),  "File/URL",    value = file_val),
        actionButton(ns("correspondence_save_btn"), "Save", class = "btn-success"),
        if (has_selection) actionButton(ns("correspondence_delete_btn"), "Delete", class = "btn-danger"),
        if (has_selection) actionButton(ns("correspondence_clear_btn"),  "Clear")
      )
    })

    observeEvent(input$correspondence_save_btn, {
      id_val <- input$org_select
      req(id_val, id_val != "")
      idx <- input$correspondence_table_rows_selected

      if (!is.null(idx) && length(idx) > 0) {
        corr_id <- correspondence_data()$ID[idx]
        update_correspondence(corr_id, input$correspondence_date,
                              input$correspondence_desc, input$correspondence_file)
      } else {
        insert_correspondence(as.integer(id_val), input$correspondence_date,
                              input$correspondence_desc, input$correspondence_file)
      }
      correspondence_trigger(correspondence_trigger() + 1)
      DT::selectRows(correspondence_proxy, NULL)
    })

    observeEvent(input$correspondence_delete_btn, {
      idx <- input$correspondence_table_rows_selected
      req(idx)
      corr_id <- correspondence_data()$ID[idx]
      delete_correspondence(corr_id)
      correspondence_trigger(correspondence_trigger() + 1)
      DT::selectRows(correspondence_proxy, NULL)
    })

    observeEvent(input$correspondence_clear_btn, {
      DT::selectRows(correspondence_proxy, NULL)
    })

    notes_trigger <- reactiveVal(0)
    notes_proxy   <- DT::dataTableProxy("notes_table", session = session)

    notes_data <- reactive({
      notes_trigger()
      id_val <- input$org_select
      req(id_val, id_val != "")
      get_notes(as.integer(id_val))
    })

    output$notes_table <- DT::renderDT({
      data <- notes_data()
      if (nrow(data) > 0 && "NotesDate" %in% names(data)) {
        data$NotesDate <- format(as.Date(data$NotesDate), "%m/%d/%Y")
      }
      data <- data[, !names(data) %in% c("NotesID", "NotesStaff"), drop = FALSE]
      names(data)[names(data) == "NotesDate"]  <- "Date"
      names(data)[names(data) == "StaffName"]  <- "Staff"
      DT::datatable(data, selection = "single", rownames = FALSE,
                    options = list(dom = "t", paging = FALSE,
                                   scrollY = "300px", scrollCollapse = TRUE,
                                   autoWidth = TRUE,
                                   columnDefs = list(
                                     list(width = "110px", targets = 0),
                                     list(width = "75%",   targets = 1),
                                     list(width = "15%",   targets = 2)
                                   )))
    })

    output$notes_form <- renderUI({
      idx  <- input$notes_table_rows_selected
      data <- notes_data()
      has_selection <- !is.null(idx) && length(idx) > 0 && nrow(data) >= idx
      staff_opts <- get_staff_lookup()

      if (has_selection) {
        row      <- data[idx, ]
        date_val <- if (!is.na(row$NotesDate)) as.Date(row$NotesDate) else Sys.Date()
        note_val <- if (!is.na(row$Notes))     row$Notes     else ""
        staff_sel <- row$NotesStaff
        header   <- "Edit Selected Note"
      } else {
        date_val  <- Sys.Date()
        note_val  <- ""
        staff_sel <- NULL
        header    <- "New Note"
      }

      wellPanel(
        strong(header),
        br(), br(),
        dateInput(ns("note_date"),  "Date",  value = date_val),
        textAreaInput(ns("note_text"),  "Note",  value = note_val, rows = 5),
        selectInput(ns("note_staff"), "Staff",
                    choices  = setNames(staff_opts$ID, staff_opts$Staff),
                    selected = staff_sel),
        actionButton(ns("notes_save_btn"), "Save", class = "btn-success"),
        if (has_selection) actionButton(ns("notes_delete_btn"), "Delete", class = "btn-danger"),
        if (has_selection) actionButton(ns("notes_clear_btn"),  "Clear")
      )
    })

    observeEvent(input$notes_save_btn, {
      id_val <- input$org_select
      req(id_val, id_val != "")
      idx <- input$notes_table_rows_selected

      if (!is.null(idx) && length(idx) > 0) {
        note_id <- notes_data()$NotesID[idx]
        update_note(note_id, input$note_date, input$note_text, input$note_staff)
      } else {
        insert_note(as.integer(id_val), input$note_date, input$note_text, input$note_staff)
      }
      notes_trigger(notes_trigger() + 1)
      DT::selectRows(notes_proxy, NULL)
    })

    observeEvent(input$notes_delete_btn, {
      idx <- input$notes_table_rows_selected
      req(idx)
      note_id <- notes_data()$NotesID[idx]
      delete_note(note_id)
      notes_trigger(notes_trigger() + 1)
      DT::selectRows(notes_proxy, NULL)
    })

    observeEvent(input$notes_clear_btn, {
      DT::selectRows(notes_proxy, NULL)
    })

    details_trigger      <- reactiveVal(0)
    jurisdictions_lookup <- reactive({ get_all_jurisdictions_lookup() })
    geography_lookup     <- reactive({ get_geography_lookup() })

    details_data <- reactive({
      details_trigger()
      id_val <- input$org_select
      req(id_val, id_val != "")
      get_jurisdiction_details(as.integer(id_val))
    })

    observeEvent(input$details_save_btn, {
      id_val <- input$org_select
      req(id_val, id_val != "")
      update_jurisdiction_details(
        as.integer(id_val),
        input$details_certified,
        input$details_cert_date,
        input$details_parent1,
        input$details_parent2,
        input$details_geography,
        input$details_airport
      )
      details_trigger(details_trigger() + 1)
    })

    output$selected_detail <- renderUI({
      id_val <- input$org_select
      if (is.null(id_val) || id_val == "") return(NULL)
      data <- current_data()
      row <- data[data$ID == as.integer(id_val), ]
      if (nrow(row) == 0) return(NULL)
      d       <- details_data()
      jur_opts <- jurisdictions_lookup()
      geo_opts <- geography_lookup()

      tagList(
        if (!is.null(d) && nrow(d) > 0) tagList(
          fluidRow(
            tags$div(class = "col-md-4",
              hfield("Certified",
                selectInput(ns("details_certified"), label = NULL,
                            choices  = c("", "Yes", "No", "Conditionally"),
                            selected = if (!is.na(d$Certified)) d$Certified else ""))
            ),
            tags$div(class = "col-md-4",
              hfield("Certification Date",
                dateInput(ns("details_cert_date"), label = NULL,
                          value  = if (!is.na(d$CertificationDate)) as.Date(d$CertificationDate) else NA,
                          format = "mm/dd/yyyy"))
            ),
            tags$div(class = "col-md-4",
              hfield("Adjacent to Airport",
                checkboxInput(ns("details_airport"), label = NULL,
                              value = isTRUE(as.logical(d$AirportAdjacent))))
            )
          ),
          fluidRow(
            tags$div(class = "col-md-4",
              hfield("Located In",
                selectInput(ns("details_parent1"), label = NULL,
                            choices  = c("(none)" = "", setNames(as.character(jur_opts$ID), jur_opts$DisplayName)),
                            selected = if (!is.na(d$JurisdictionParent1)) d$JurisdictionParent1 else ""))
            ),
            tags$div(class = "col-md-4",
              hfield("Also In",
                selectInput(ns("details_parent2"), label = NULL,
                            choices  = c("(none)" = "", setNames(as.character(jur_opts$ID), jur_opts$DisplayName)),
                            selected = if (!is.na(d$JurisdictionParent2)) d$JurisdictionParent2 else ""))
            ),
            tags$div(class = "col-md-4",
              hfield("Regional Geography",
                selectInput(ns("details_geography"), label = NULL,
                            choices  = c("(none)" = "", setNames(as.character(geo_opts$ID), geo_opts$RegionalGeography)),
                            selected = if (!is.na(d$RegionalGeography)) d$RegionalGeography else ""))
            )
          ),
          fluidRow(
            column(12,
              actionButton(ns("details_save_btn"), "Save", class = "btn-success")
            )
          ),
          hr()
        ),
        tabsetPanel(
          tabPanel("Materials",
            fluidRow(
              column(7, DT::DTOutput(ns("materials_table"))),
              column(5, uiOutput(ns("material_edit_form")))
            )
          ),
          tabPanel("Contacts", uiOutput(ns("contacts_panel"))),
          tabPanel("Actions",
            fluidRow(
              column(7, DT::DTOutput(ns("actions_table"))),
              column(5, uiOutput(ns("actions_form")))
            )
          ),
          tabPanel("Correspondence",
            fluidRow(
              column(7, DT::DTOutput(ns("correspondence_table"))),
              column(5, uiOutput(ns("correspondence_form")))
            )
          ),
          tabPanel("Notes",
            fluidRow(
              column(7, DT::DTOutput(ns("notes_table"))),
              column(5, uiOutput(ns("notes_form")))
            )
          )
        )
      )
    })
  })
}
