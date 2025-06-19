# Enhanced Data Merger with Excel Support
# Merging multiple data sources into the primary sentiment dataset on date column

# Load required libraries
library(tidyverse)
library(lubridate)
library(readxl)  # For reading Excel files

# File paths 
primary_file <- "03_ml_prediction/ready_sentiment_data_with_features.csv"  # Updated to use merged file
news_sentiment_file <- "market_macro_data/news_sentiment_data (1).xlsx"  # Excel file
output_file <- "03_ml_prediction/ready_sentiment_data_complete.csv"
log_file <- "enhanced_merge_validation_report.txt"

# Excel sheet configuration
excel_sheet <- "Data"  # Sheet name containing the data

cat("=== ENHANCED DATA MERGER WITH EXCEL SUPPORT ===\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Start logging
sink(log_file)
cat("=== ENHANCED MERGE VALIDATION REPORT ===\n")
cat("Generated at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("STEP 1: READING INPUT FILES\n")
cat("================================\n")

# Read the primary file (already merged sentiment data with features)
tryCatch({
  primary_data <- read_csv(primary_file, show_col_types = FALSE)
  cat("✓ Successfully read primary file:", primary_file, "\n")
  cat("  - Rows:", nrow(primary_data), "\n")
  cat("  - Columns:", ncol(primary_data), "\n")
  cat("  - Date range:", min(primary_data$date, na.rm = TRUE), "to", max(primary_data$date, na.rm = TRUE), "\n")
}, error = function(e) {
  cat("✗ ERROR reading primary file:", e$message, "\n")
  sink()
  stop("Failed to read primary file")
})

# Read the Excel news sentiment file
tryCatch({
  # First, check if the Excel file exists
  if (!file.exists(news_sentiment_file)) {
    stop(paste("Excel file not found:", news_sentiment_file))
  }
  
  # Get sheet names to verify our target sheet exists
  sheet_names <- excel_sheets(news_sentiment_file)
  cat("Available sheets in Excel file:", paste(sheet_names, collapse = ", "), "\n")
  
  if (!excel_sheet %in% sheet_names) {
    cat("⚠ WARNING: Sheet '", excel_sheet, "' not found. Using first sheet: ", sheet_names[1], "\n")
    excel_sheet <- sheet_names[1]
  }
  
  # Read the Excel file
  news_data <- read_excel(news_sentiment_file, sheet = excel_sheet)
  cat("✓ Successfully read Excel file:", news_sentiment_file, "\n")
  cat("  - Sheet:", excel_sheet, "\n")
  cat("  - Rows:", nrow(news_data), "\n")
  cat("  - Columns:", ncol(news_data), "\n")
  cat("  - Column names:", paste(names(news_data), collapse = ", "), "\n")
  
  # Check date range if date column exists
  if ("date" %in% names(news_data)) {
    cat("  - Date range:", min(news_data$date, na.rm = TRUE), "to", max(news_data$date, na.rm = TRUE), "\n")
  } else {
    cat("  - Available columns:", paste(names(news_data), collapse = ", "), "\n")
    cat("⚠ WARNING: 'date' column not immediately found. Will check for date-like columns.\n")
  }
}, error = function(e) {
  cat("✗ ERROR reading Excel file:", e$message, "\n")
  sink()
  stop("Failed to read Excel file")
})

cat("\nSTEP 2: VALIDATING AND CLEANING NEWS SENTIMENT DATA\n")
cat("===================================================\n")

# Check for date column in news data - try different possible names
date_column_candidates <- c("date", "Date", "DATE", "dates", "Dates")
date_col_found <- NULL

for (col_name in date_column_candidates) {
  if (col_name %in% names(news_data)) {
    date_col_found <- col_name
    break
  }
}

if (is.null(date_col_found)) {
  cat("✗ ERROR: No date column found in news sentiment data\n")
  cat("Available columns:", paste(names(news_data), collapse = ", "), "\n")
  sink()
  stop("Date column missing in news sentiment data")
} else {
  cat("✓ Found date column:", date_col_found, "\n")
  
  # Rename to standard 'date' if needed
  if (date_col_found != "date") {
    names(news_data)[names(news_data) == date_col_found] <- "date"
    cat("✓ Renamed", date_col_found, "to 'date'\n")
  }
}

# Ensure primary file has date column
if (!"date" %in% names(primary_data)) {
  cat("✗ ERROR: 'date' column not found in primary file\n")
  sink()
  stop("Date column missing in primary file")
}

# Convert date columns to Date type
primary_data <- primary_data %>%
  mutate(date = as.Date(date))

# Handle potential date format issues in Excel data
news_data <- news_data %>%
  mutate(date = as.Date(date))

cat("✓ Date columns validated and converted to Date type\n")

# Clean column names in news data to avoid conflicts
# Rename non-date columns to have 'news_' prefix if they might conflict
existing_cols <- names(primary_data)
news_cols <- names(news_data)
news_non_date_cols <- setdiff(news_cols, "date")

# Check for potential conflicts and rename if necessary
renamed_cols <- character(0)
for (col in news_non_date_cols) {
  if (col %in% existing_cols) {
    new_name <- paste0("news_", col)
    names(news_data)[names(news_data) == col] <- new_name
    renamed_cols <- c(renamed_cols, paste(col, "->", new_name))
    cat("✓ Renamed conflicting column:", col, "to", new_name, "\n")
  }
}

if (length(renamed_cols) == 0) {
  cat("✓ No column name conflicts found\n")
}

# Check for duplicate dates in each file
primary_dupes <- primary_data %>% 
  count(date) %>% 
  filter(n > 1) %>% 
  nrow()

news_dupes <- news_data %>% 
  count(date) %>% 
  filter(n > 1) %>% 
  nrow()

cat("Primary file duplicate dates:", primary_dupes, "\n")
cat("News data duplicate dates:", news_dupes, "\n")

if (primary_dupes > 0 || news_dupes > 0) {
  cat("⚠ WARNING: Duplicate dates found. This may cause issues with the merge.\n")
  
  # If news data has duplicates, aggregate them
  if (news_dupes > 0) {
    cat("Aggregating duplicate dates in news data (taking mean of numeric columns)...\n")
    numeric_cols <- news_data %>% 
      select(-date) %>% 
      select_if(is.numeric) %>% 
      names()
    
    if (length(numeric_cols) > 0) {
      news_data <- news_data %>%
        group_by(date) %>%
        summarise(across(all_of(numeric_cols), ~mean(., na.rm = TRUE)),
                  across(!all_of(numeric_cols) & !date, ~first(.)),
                  .groups = "drop")
      cat("✓ Aggregated", news_dupes, "duplicate dates in news data\n")
    }
  }
}

cat("\nSTEP 3: ANALYZING DATE OVERLAP\n")
cat("==============================\n")

# Analyze date overlap
primary_dates <- unique(primary_data$date)
news_dates <- unique(news_data$date)

common_dates <- intersect(primary_dates, news_dates)
only_in_primary <- setdiff(primary_dates, news_dates)
only_in_news <- setdiff(news_dates, primary_dates)

cat("Primary file unique dates:", length(primary_dates), "\n")
cat("News sentiment unique dates:", length(news_dates), "\n")
cat("Common dates (will have news sentiment):", length(common_dates), "\n")
cat("Dates only in primary (will have NA news sentiment):", length(only_in_primary), "\n")
cat("Dates only in news sentiment (will be ignored):", length(only_in_news), "\n")

expected_match_rate <- length(common_dates) / length(primary_dates) * 100
cat("Expected news sentiment coverage:", sprintf("%.1f%%", expected_match_rate), "\n")

# Show date range overlap
if (length(common_dates) > 0) {
  cat("Date overlap range:", min(common_dates), "to", max(common_dates), "\n")
}

cat("\nSTEP 4: CHECKING COLUMN INFORMATION\n")
cat("===================================\n")

# Check for column name conflicts (excluding date)
primary_cols <- setdiff(names(primary_data), "date")
news_cols <- setdiff(names(news_data), "date")

conflicting_cols <- intersect(primary_cols, news_cols)
new_cols <- setdiff(news_cols, primary_cols)

cat("Columns in primary file (excluding date):", length(primary_cols), "\n")
cat("Columns in news sentiment (excluding date):", length(news_cols), "\n")
cat("Conflicting column names:", length(conflicting_cols), "\n")
cat("New columns to be added:", length(new_cols), "\n")

if (length(conflicting_cols) > 0) {
  cat("⚠ WARNING: Conflicting columns found:", paste(conflicting_cols, collapse = ", "), "\n")
  cat("These will be overwritten with values from news sentiment data.\n")
}

if (length(new_cols) > 0) {
  cat("New columns to be added from news sentiment:\n")
  for (col in new_cols) {
    cat("  -", col, "\n")
  }
}

# Show sample of news data
cat("\nSample of news sentiment data:\n")
sample_news <- head(news_data, 3)
print(sample_news)

sink()  # Stop logging to file, continue to console

cat("\nSTEP 5: PERFORMING LEFT JOIN WITH NEWS SENTIMENT\n")
cat("================================================\n")

# Perform the left join
merged_data <- primary_data %>%
  left_join(news_data, by = "date", suffix = c("", "_news"))

cat("✓ Left join with news sentiment completed successfully\n")
cat("Merged data dimensions:", nrow(merged_data), "rows ×", ncol(merged_data), "columns\n")

# Verify the merge
cat("\nSTEP 6: VALIDATION CHECKS\n")
cat("=========================\n")

# Check that we didn't lose any rows from primary data
if (nrow(merged_data) != nrow(primary_data)) {
  cat("✗ ERROR: Row count mismatch after merge!\n")
  cat("Original primary rows:", nrow(primary_data), "\n")
  cat("Merged rows:", nrow(merged_data), "\n")
  stop("Merge validation failed - row count changed")
} else {
  cat("✓ Row count preserved:", nrow(merged_data), "rows\n")
}

# Check how many rows got new news sentiment data
if (length(new_cols) > 0) {
  rows_with_news <- sum(!is.na(merged_data[[new_cols[1]]]))
  news_coverage <- rows_with_news / nrow(merged_data) * 100
  
  cat("✓ Rows with news sentiment data:", rows_with_news, "of", nrow(merged_data), 
      sprintf("(%.1f%%)", news_coverage), "\n")
} else {
  cat("⚠ No new columns were added from news sentiment data\n")
  news_coverage <- 0
}

# Check for any unexpected changes in existing columns
original_cols_check <- all.equal(
  primary_data[order(primary_data$date), primary_cols],
  merged_data[order(merged_data$date), primary_cols],
  check.attributes = FALSE
)

if (isTRUE(original_cols_check)) {
  cat("✓ Original columns preserved without changes\n")
} else {
  cat("⚠ WARNING: Changes detected in original columns\n")
}

cat("\nSTEP 7: SAVING MERGED DATA\n")
cat("==========================\n")

# Save the merged data
write_csv(merged_data, output_file)
cat("✓ Merged data saved to:", output_file, "\n")

# Create a summary of what was added
summary_stats <- data.frame(
  metric = c(
    "Original rows",
    "Original columns", 
    "New news sentiment columns added",
    "Rows with news sentiment data",
    "News sentiment coverage %",
    "Total columns after merge"
  ),
  value = c(
    nrow(primary_data),
    ncol(primary_data),
    length(new_cols),
    if(length(new_cols) > 0) rows_with_news else 0,
    if(length(new_cols) > 0) round(news_coverage, 1) else 0,
    ncol(merged_data)
  )
)

cat("\nSUMMARY STATISTICS:\n")
print(summary_stats)

# Save summary to log file
cat("\n", file = log_file, append = TRUE)
cat("FINAL SUMMARY:\n", file = log_file, append = TRUE)
cat("==============\n", file = log_file, append = TRUE)
capture.output(print(summary_stats), file = log_file, append = TRUE)

cat("\nSTEP 8: SAMPLE VERIFICATION\n")
cat("===========================\n")

# Show a sample of the merged data to verify
cat("Sample of merged data (first 5 rows, key columns only):\n")
sample_cols <- c("date", head(primary_cols, 3), head(new_cols, 3))
sample_cols <- sample_cols[sample_cols %in% names(merged_data)]

print(head(merged_data[, sample_cols], 5))

cat("\n=== ENHANCED MERGE COMPLETED SUCCESSFULLY ===\n")
cat("Output file:", output_file, "\n")
cat("Validation log:", log_file, "\n")
cat("New news sentiment columns added:", length(new_cols), "\n")
if (length(new_cols) > 0) {
  cat("News sentiment coverage:", sprintf("%.1f%%", news_coverage), "\n")
}

# Return summary information
list(
  success = TRUE,
  output_file = output_file,
  original_rows = nrow(primary_data),
  original_cols = ncol(primary_data),
  final_rows = nrow(merged_data),
  final_cols = ncol(merged_data),
  new_news_columns = length(new_cols),
  news_coverage = if(length(new_cols) > 0) news_coverage else 0
)

