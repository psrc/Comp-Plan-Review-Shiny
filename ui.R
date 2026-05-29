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
        background-color: #8CC63E !important;
        border-color: #74a833 !important;
        color: #ffffff !important;
      }
      .btn.btn-active:hover, .btn.btn-active:focus {
        background-color: #74a833 !important;
        border-color: #5e8a29 !important;
        color: #ffffff !important;
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
