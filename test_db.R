# Test script for database connectivity
cat("Testing database setup...\n\n")

# Test package loading
packages <- c("shiny", "DBI", "odbc")
missing_packages <- c()

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat("✗ Package", pkg, "is not installed\n")
  } else {
    cat("✓ Package", pkg, "is available\n")
  }
}

if (length(missing_packages) > 0) {
  cat("\nTo install missing packages, run:\n")
  cat("install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n\n")
} else {
  cat("\n✓ All required packages are installed\n\n")
}

# Test database module loading
tryCatch({
  source("db_module.R")
  cat("✓ Database module loaded successfully\n")
  
  # Test connection function existence
  if (exists("get_elmer_connection")) {
    cat("✓ get_elmer_connection() function is defined\n")
  } else {
    cat("✗ get_elmer_connection() function is missing\n")
  }
  
  if (exists("get_tenure_data")) {
    cat("✓ get_tenure_data() function is defined\n")
  } else {
    cat("✗ get_tenure_data() function is missing\n")
  }
  
  if (exists("test_db_connection")) {
    cat("✓ test_db_connection() function is defined\n")
  } else {
    cat("✗ test_db_connection() function is missing\n")
  }
  
}, error = function(e) {
  cat("✗ Error loading database module:", e$message, "\n")
})

cat("\nDatabase setup test completed!\n")
cat("Note: Actual database connectivity depends on network access to 'SQLserver'\n")
cat("and proper Windows Authentication configuration.\n")
