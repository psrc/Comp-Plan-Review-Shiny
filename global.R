# global.R - Runs once when app starts, shared resources

# Load required libraries
library(shiny)
library(shinyjs)
library(bslib)
library(DBI)
library(odbc)

# Auto-source all R files from the modules folder
# This allows code to be compartmentalized and automatically available
# to both ui.R and server.R without manual sourcing
modules_dir <- file.path(getwd(), "modules")

if (dir.exists(modules_dir)) {
  module_files <- list.files(
    path = modules_dir,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = FALSE
  )
  
  # Source each module file
  for (module_file in module_files) {
    source(module_file, local = FALSE)
  }
  
  # Optional: Print loaded modules for debugging (comment out in production)
  if (length(module_files) > 0) {
    message("Loaded modules: ", paste(basename(module_files), collapse = ", "))
  }
}
