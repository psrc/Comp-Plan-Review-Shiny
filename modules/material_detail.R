# modules/material_detail.R - standalone, editable Material Detail page
# Opened in its own browser tab via a link from the Materials table
# (see ?view=material&material_id=...&jurisdiction=... routing in ui.R)
#
# The material_id/jurisdiction query-string values are only reliably readable
# on the initial HTTP request (in ui.R), not on the follow-up websocket
# handshake that runs server code. So ui.R parses them once and pushes them
# into the session as plain Shiny inputs via a small init script; the server
# below reads those inputs to drive the editable form.

# Escape forward slashes so a value containing "</script>" can't break out of
# the inline <script> block it's embedded in.
material_detail_json <- function(x) {
  gsub("/", "\\/", jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), fixed = TRUE)
}

# Materials.* bit columns shown as checkboxes, mapped to their display labels.
MATERIAL_TYPE_FIELDS <- c(
  MaterialTypeCFP                 = "CFP",
  MaterialTypeCPP                 = "CPP",
  MaterialTypeEconomicDevelopment = "Economic",
  MaterialTypeFLUM                = "FLUM",
  MaterialTypeFunctionalPlan      = "FunctionalPlan",
  MaterialTypeHousing             = "Housing",
  MaterialTypeLandUse             = "Land Use",
  MaterialTypeMinorAmendments     = "Minor Amendment",
  MaterialTypeOther               = "Other",
  MaterialTypeParks               = "Parks",
  MaterialTypePeriodicUpdate      = "Periodic Update",
  MaterialTypeRegionalCenter      = "Regional Center",
  MaterialTypeRural               = "Rural",
  MaterialTypeScopingNotice       = "Scoping Notice",
  MaterialTypeSMP                 = "SMP",
  MaterialTypeSubareaPlan         = "Subarea Plan",
  MaterialTypeTransportation      = "Transportation",
  MaterialTypeUGAChange           = "UGA Change",
  MaterialTypeUtilities           = "Utilities"
)

materialDetailUI <- function(material_id, jurisdiction_name) {
  page_fluid(
    tags$head(tags$title("Material Detail")),
    tags$script(HTML(sprintf(
      "$(document).on('shiny:sessioninitialized', function() {
         Shiny.setInputValue('material_id', %s);
         Shiny.setInputValue('jurisdiction', %s);
       });",
      material_detail_json(material_id),
      material_detail_json(jurisdiction_name)
    ))),
    uiOutput("material_detail_content")
  )
}

materialDetailServer <- function(input, output, session) {
  refresh <- reactiveVal(0)

  material_id <- reactive({
    req(input$material_id)
    input$material_id
  })

  edit_data <- reactive({
    refresh()
    get_material_edit_data(material_id())
  })

  status_opts <- reactive({ get_status_lookup() })
  phase_opts  <- reactive({ get_material_phase_lookup() })
  source_opts <- reactive({ get_material_source_lookup() })
  staff_opts  <- reactive({ get_staff_lookup() })

  safe_val <- function(x) if (is.null(x) || is.na(x)) "" else x

  output$material_detail_content <- renderUI({
    edit <- edit_data()
    if (is.null(edit) || nrow(edit) == 0) {
      return(tags$p("Material not found."))
    }

    tagList(
      h4(safe_val(input$jurisdiction)),
      wellPanel(
        fluidRow(
          column(6, textInput("edit_title", "Title", value = safe_val(edit$MaterialTitle))),
          column(6, selectInput("edit_status", "Status",
                                 choices  = setNames(status_opts()$ID, status_opts()$Status),
                                 selected = edit$MaterialStatus))
        ),
        fluidRow(
          column(6, dateInput("edit_received", "Material Received",
                               value  = if (!is.na(edit$MaterialDateReceived)) as.Date(edit$MaterialDateReceived) else NA,
                               format = "mm/dd/yyyy")),
          column(6, selectInput("edit_phase", "Phase",
                                 choices  = setNames(phase_opts()$ID, phase_opts()$Phase),
                                 selected = edit$MaterialPhase))
        ),
        fluidRow(
          column(6, selectInput("edit_source", "Source",
                                 choices  = setNames(source_opts()$ID, source_opts()$Source),
                                 selected = edit$MaterialSource)),
          column(6, selectInput("edit_staff", "Staff Reviewer",
                                 choices  = setNames(staff_opts()$ID, staff_opts()$Staff),
                                 selected = edit$MaterialStaffReviewer))
        ),
        fluidRow(
          column(12, textAreaInput("edit_description", "Description", value = safe_val(edit$MaterialDescription), rows = 4))
        ),
        fluidRow(
          column(12, textInput("edit_file", "File Location", value = safe_val(edit$MaterialFile)))
        ),
        fluidRow(
          lapply(names(MATERIAL_TYPE_FIELDS), function(col) {
            column(3,
              checkboxInput(col, MATERIAL_TYPE_FIELDS[[col]],
                            value = isTRUE(as.logical(edit[[col]])))
            )
          })
        ),
        actionButton("material_save_btn", "Save", class = "btn-success")
      )
    )
  })

  observeEvent(input$material_save_btn, {
    type_values <- setNames(
      sapply(names(MATERIAL_TYPE_FIELDS), function(col) isTRUE(input[[col]])),
      names(MATERIAL_TYPE_FIELDS)
    )
    ok <- update_material_detail(
      material_id(),
      input$edit_title,
      input$edit_description,
      input$edit_file,
      input$edit_received,
      input$edit_status,
      input$edit_phase,
      input$edit_source,
      input$edit_staff,
      type_values
    )
    showNotification(if (ok) "Saved." else "Save failed.",
                      type = if (ok) "message" else "error")
    if (ok) refresh(refresh() + 1)
  })
}
