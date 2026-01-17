# global.R - Runs once when app starts, shared resources

# Load required libraries
library(shiny)
library(shinyjs)
library(bslib)
library(DBI)
library(odbc)

# Source the database module
source("db_module.R")

