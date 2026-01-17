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
