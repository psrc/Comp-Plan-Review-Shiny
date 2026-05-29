# ui.R - User interface definition

navbarPage(
  title = tags$span(
    tags$img(
      src = "RegionalGem2016.png",
      height = "30px",
      style = "margin-right: 8px; vertical-align: middle;"
    ),
    "Plan Review Tracking"
  ),
  
  # Custom CSS for active tab styling
  header = tags$head(
    tags$style(HTML("
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: #BCBEC0 !important;
        color: #333333 !important;
      }
      .btn.btn-active {
        background-color: #C0E095 !important;
        border-color: #74a833 !important;
        color: #ffffff !important;
      }
      .btn.btn-active:hover, .btn.btn-active:focus {
        background-color: #74a833 !important;
        border-color: #5e8a29 !important;
        color: #ffffff !important;
      }
      table.dataTable tbody tr.selected td,
      table.dataTable tbody tr.selected {
        background-color: #C0E095 !important;
        color: #333333 !important;
        box-shadow: inset 0 0 0 9999px #C0E095 !important;
      }
      .nav-tabs > li > a {
        color: #91268F !important;
      }
      .btn-danger, .btn-danger:hover, .btn-danger:focus, .btn-danger:active {
        background-color: #EBA9BE !important;
        border-color: #d98eaa !important;
        color: #333333 !important;
      }
      .btn-success, .btn-success:hover, .btn-success:focus, .btn-success:active {
        background-color: #E2F1CF !important;
        border-color: #c8ddb0 !important;
        color: #333333 !important;
      }
      .file-path {
        font-family: Consolas, 'Courier New', monospace;
        background-color: #f0f4f8 !important;
        padding: 1px 4px;
        border-radius: 2px;
        font-size: 0.85em;
      }
    "))
  ),
  
  # Organizations tab - uses module UI
  tabPanel(
    "Organizations",
    organizationsUI("organizations")
  ),
  
  # Materials tab - uses module UI
  tabPanel(
    "Materials",
    materialsUI("materials")
  )
)
