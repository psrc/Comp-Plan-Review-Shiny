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

get_centers <- function() {
  con <- get_db_connection()

  if (is.null(con)) {
    return(data.frame(Error = "Database connection failed. Please check your connection settings."))
  }

  tryCatch({
    query <- "SELECT [ID], [DisplayName], [JurisdictionType] FROM dbo.vwCenterCPP"
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    if (nrow(result) > 0) {
      return(result)
    } else {
      return(data.frame(Message = "No records found in [vwCenterCPP]"))
    }

  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame(Error = paste("Query failed in get_centers():", e$message)))
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
      return(data.frame(Message = "No records found in [vwCenterCPP]"))
    }

  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame(Error = paste("Query failed in get_cities_counties():", e$message)))
  })
}

get_materials <- function(org_id) {
  con <- get_db_connection()
  if (is.null(con)) {
    return(data.frame(Error = "Database connection failed. Please check your connection settings."))
  }
  tryCatch({
    result <- dbGetQuery(con,
      "SELECT * FROM dbo.vwMaterials WHERE JurisdictionID = ?",
      params = list(as.integer(org_id)))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame(Error = paste("Query failed in get_materials():", e$message)))
  })
}

get_status_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame(ID = integer(), Status = character()))
  tryCatch({
    result <- dbGetQuery(con, "SELECT ID, [Status] FROM dbo.MaterialsStatus")
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame(ID = integer(), Status = character()))
  })
}

get_commerce_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame(ID = integer(), CommerceContact = character()))
  tryCatch({
    result <- dbGetQuery(con, "SELECT ID, CommerceContact FROM dbo.CommerceContact")
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame(ID = integer(), CommerceContact = character()))
  })
}

get_staff_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame(ID = integer(), Staff = character()))
  tryCatch({
    result <- dbGetQuery(con, "SELECT ID, Staff FROM dbo.Staff")
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame(ID = integer(), Staff = character()))
  })
}

get_material_fk_ids <- function(material_id) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  tryCatch({
    result <- dbGetQuery(con,
      "SELECT MaterialStatus, MaterialStaffReviewer FROM dbo.Materials WHERE ID = ?",
      params = list(as.integer(material_id)))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    return(NULL)
  })
}

update_material <- function(material_id, status_id, staff_id) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "UPDATE dbo.Materials SET MaterialStatus = ?, MaterialStaffReviewer = ? WHERE ID = ?",
      params = list(as.integer(status_id), as.integer(staff_id), as.integer(material_id)))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Update failed in update_material():", e$message))
    return(FALSE)
  })
}

get_jurisdiction_contacts <- function(jurisdiction_id) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  tryCatch({
    result <- dbGetQuery(con,
      "SELECT j.Address, j.ContactName1, j.ContactTitle1, j.ContactPhone1, j.ContactEmail1,
              j.ContactName2, j.ContactTitle2, j.ContactPhone2, j.ContactEmail2,
              j.StaffContact, s.Staff AS StaffAssignment,
              j.CommerceContact, cc.CommerceContact AS CommerceAssignment
       FROM dbo.Jurisdiction j
       LEFT JOIN dbo.Staff s ON j.StaffContact = s.ID
       LEFT JOIN dbo.CommerceContact cc ON j.CommerceContact = cc.ID
       WHERE j.ID = ?",
      params = list(as.integer(jurisdiction_id)))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Query failed in get_jurisdiction_contacts():", e$message))
    return(NULL)
  })
}

update_jurisdiction_contacts <- function(jurisdiction_id, address,
                                          contact_name1, contact_title1,
                                          contact_phone1, contact_email1,
                                          staff_contact,
                                          contact_name2, contact_title2,
                                          contact_phone2, contact_email2,
                                          commerce_contact) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "UPDATE dbo.Jurisdiction
       SET Address = ?, ContactName1 = ?, ContactTitle1 = ?,
           ContactPhone1 = ?, ContactEmail1 = ?,
           StaffContact = ?,
           ContactName2 = ?, ContactTitle2 = ?,
           ContactPhone2 = ?, ContactEmail2 = ?,
           CommerceContact = ?
       WHERE ID = ?",
      params = list(address,
                    contact_name1, contact_title1, contact_phone1, contact_email1,
                    as.integer(staff_contact),
                    contact_name2, contact_title2, contact_phone2, contact_email2,
                    as.integer(commerce_contact),
                    as.integer(jurisdiction_id)))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Update failed in update_jurisdiction_contacts():", e$message))
    return(FALSE)
  })
}

get_actions <- function(jurisdiction_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  tryCatch({
    result <- dbGetQuery(con,
      "SELECT ID, ActionsDate, [Actions], ActionsFile
       FROM dbo.Actions WHERE Jurisdiction = ?
       ORDER BY ActionsDate DESC",
      params = list(as.integer(jurisdiction_id)))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Query failed in get_actions():", e$message))
    return(data.frame())
  })
}

insert_action <- function(jurisdiction_id, actions_date, actions, actions_file) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "INSERT INTO dbo.Actions (Jurisdiction, ActionsDate, [Actions], ActionsFile)
       VALUES (?, ?, ?, ?)",
      params = list(as.integer(jurisdiction_id),
                    if (is.na(actions_date)) NA else as.character(actions_date),
                    if (actions == "") NA else actions,
                    if (actions_file == "") NA else actions_file))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Insert failed in insert_action():", e$message))
    return(FALSE)
  })
}

update_action <- function(action_id, actions_date, actions, actions_file) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "UPDATE dbo.Actions SET ActionsDate = ?, [Actions] = ?, ActionsFile = ?
       WHERE ID = ?",
      params = list(if (is.na(actions_date)) NA else as.character(actions_date),
                    if (actions == "") NA else actions,
                    if (actions_file == "") NA else actions_file,
                    as.integer(action_id)))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Update failed in update_action():", e$message))
    return(FALSE)
  })
}

delete_action <- function(action_id) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "DELETE FROM dbo.Actions WHERE ID = ?",
      params = list(as.integer(action_id)))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Delete failed in delete_action():", e$message))
    return(FALSE)
  })
}

get_correspondence <- function(jurisdiction_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  tryCatch({
    result <- dbGetQuery(con,
      "SELECT ID, CorrespondenceDate, CorrespondenceDescription, CorrespondenceFile
       FROM dbo.Correspondence WHERE Jurisdiction = ?
       ORDER BY CorrespondenceDate DESC",
      params = list(as.integer(jurisdiction_id)))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Query failed in get_correspondence():", e$message))
    return(data.frame())
  })
}

insert_correspondence <- function(jurisdiction_id, correspondence_date,
                                   correspondence_desc, correspondence_file) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "INSERT INTO dbo.Correspondence
         (Jurisdiction, CorrespondenceDate, CorrespondenceDescription, CorrespondenceFile)
       VALUES (?, ?, ?, ?)",
      params = list(as.integer(jurisdiction_id),
                    if (is.na(correspondence_date)) NA else as.character(correspondence_date),
                    if (correspondence_desc == "") NA else correspondence_desc,
                    if (correspondence_file == "") NA else correspondence_file))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Insert failed in insert_correspondence():", e$message))
    return(FALSE)
  })
}

update_correspondence <- function(correspondence_id, correspondence_date,
                                   correspondence_desc, correspondence_file) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "UPDATE dbo.Correspondence
       SET CorrespondenceDate = ?, CorrespondenceDescription = ?, CorrespondenceFile = ?
       WHERE ID = ?",
      params = list(if (is.na(correspondence_date)) NA else as.character(correspondence_date),
                    if (correspondence_desc == "") NA else correspondence_desc,
                    if (correspondence_file == "") NA else correspondence_file,
                    as.integer(correspondence_id)))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Update failed in update_correspondence():", e$message))
    return(FALSE)
  })
}

delete_correspondence <- function(correspondence_id) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  tryCatch({
    dbExecute(con,
      "DELETE FROM dbo.Correspondence WHERE ID = ?",
      params = list(as.integer(correspondence_id)))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    warning(paste("Delete failed in delete_correspondence():", e$message))
    return(FALSE)
  })
}

# Function to test database connection
test_db_connection <- function() {
  con <- get_db_connection()
  if (is.null(con)) {
    return(FALSE)
  }
  dbDisconnect(con)
  return(TRUE)
}
