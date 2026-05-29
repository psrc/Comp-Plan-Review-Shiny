# ui.R - User interface definition

navbarPage(
  title = "Plan Review Tracking",
  
  # Custom CSS for active tab styling
  header = tags$head(
    tags$style(HTML("
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: #D2A679 !important;
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
