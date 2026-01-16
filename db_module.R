# Database connection module for Elmer database
library(DBI)
library(odbc)


# Function to create database connection
get_db_connection <- function() {
  tryCatch({
    # Connection string for SQL Server using Windows Authentication
    con <- dbConnect(
      odbc::odbc(),
      Driver = "SQL Server",
      Server = "SQLserver",
      Database = "CompPlanReview",
      Trusted_Connection = "yes"
    )
    return(con)
  }, error = function(e) {
    warning(paste("Database connection failed:", e$message))
    return(NULL)
  })
}

get_cities_counties <- function() {
  con <- get_db_connection()

  if (is.null(con)) {
    return(data.frame(Error = "Database connection failed. Please check your connection settings."))
  }

  tryCatch({
    query <- "SELECT [ID], [DisplayName], [JurisdictionType] FROM dbo.vwCityCounty"
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    if (nrow(result) > 0) {
      return(result)
    } else {
      return(data.frame(Message = "No records found in [tenure_dim]"))
    }

  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame(Error = paste("Query failed in get_cities_counties():", e$message)))
  })

}

# Function to get data from tenure_dim table

# Function to test database connection
test_db_connection <- function() {
  con <- get_db_connection()
  if (is.null(con)) {
    return(FALSE)
  }
  dbDisconnect(con)
  return(TRUE)
}
