# Comp Plan Review Shiny App

A Shiny app intended to help in the tracking of comp plan review documents and submissions

## Features

- (in development)

## Requirements

- R (version 3.6 or higher recommended)
- Shiny package
- DBI package
- odbc package
- Access to SQL Server named "SQLserver" with "Elmer" database
- Windows Authentication enabled for database connection

## Installation

If you don't have the required packages installed, run this in R:

```r
# Install required packages
install.packages(c("shiny", "DBI", "odbc"))
```

**Database Setup:**
- Ensure you have access to the SQL Server named "SQLserver"
- The "Elmer" database should be accessible
- Windows Authentication must be configured for your account
- The table `[chas].[tenure_dim]` should exist and be accessible

## Running the App

1. Open R or RStudio
2. Set your working directory to the folder containing this app:
   ```r
   setwd("path/to/Comp-Plan-Review-Shiny")
   ```
3. Run the app:
   ```r
   shiny::runApp()
   ```

Alternatively, you can run it directly:
```r
shiny::runApp("app.R")
```

The app will open in your default web browser.

## How it Works

- The UI creates a button using `actionButton()`
- The server logic uses `renderText()` to display data from a db query when the button is clicked
- The text appears above the button as requested
- When clicked, the app also connects to the Elmer database and retrieves a record from `[chas].[tenure_dim]`
- Database results are displayed below the button in a formatted text box
- Uses DBI and odbc packages for SQL Server connectivity with Windows Authentication
- The button counter (`input$jurisdiction_btn`) starts at 0 and increments each time it's clicked

## Database Module

The app includes a separate `db_module.R` file that handles:
- Database connection management
- Error handling for connection failures
- Data retrieval from the tenure dimension table
- Proper connection cleanup
