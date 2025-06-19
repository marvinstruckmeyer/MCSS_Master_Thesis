# Combined ML-Ready Sentiment Data Preparation (Streamlined)
# This script prepares sentiment indices for ML modeling with essential features:
# 1. Filters to trading days and handles missing values
# 2. Adds basic momentum and volatility features
# 3. Calculates cross-method variability

# Load required libraries
library(tidyverse)
library(lubridate)
library(imputeTS)  # For imputation methods
library(zoo)       # For rolling window calculations

# Set file paths
input_file <- "02_sentiment_analysis/daily_sentiment_indices_enhanced.csv"
output_file <- "02_sentiment_analysis/ml_ready_sentiment_data.csv"

# Read the data
cat("Reading enhanced sentiment indices data...\n")
sentiment_data <- read_csv(input_file, show_col_types = FALSE)

# Ensure date column is in Date format
sentiment_data <- sentiment_data %>%
  mutate(date = as.Date(date))

# Display basic information
cat("Original dataset dimensions:", dim(sentiment_data), "\n")
cat("Date range:", min(sentiment_data$date, na.rm = TRUE), "to", 
    max(sentiment_data$date, na.rm = TRUE), "\n")

# ===== STEP 1: FILTER BY DATE RANGE =====
# Select the balanced approach - starting from May 2023
start_date <- as.Date("2023-05-01")
cat("Using balanced approach with start date:", start_date, "\n")

filtered_data <- sentiment_data %>%
  filter(date >= start_date)

cat("After date filtering:", nrow(filtered_data), "rows\n")

# ===== STEP 2: IDENTIFY WEEKENDS AND TRADING DAYS =====
filtered_data <- filtered_data %>%
  mutate(
    weekday = weekdays(date),
    is_weekend = weekday %in% c("Saturday", "Sunday"),
    is_trading_day = !is_weekend
  )

cat("Weekend days in filtered data:", sum(filtered_data$is_weekend), "\n")
cat("Trading days in filtered data:", sum(filtered_data$is_trading_day), "\n")

# ===== STEP 3: FILTER TO TRADING DAYS ONLY =====
trading_data <- filtered_data %>%
  filter(is_trading_day)

cat("Data filtered to trading days only:", nrow(trading_data), "rows\n")

# ===== STEP 4: DEFINE KEY SENTIMENT COLUMNS =====
# Define the four main sentiment methods based on your data
sentiment_methods <- c("dict_sentiment_avg", "sentimentr_score_avg", 
                       "finbert_score_avg", "ollama_score_avg")

# Also include the combined indices
key_columns <- c("simple_sentiment_index", "weighted_sentiment_index", sentiment_methods)

# Check which columns actually exist in the data
existing_columns <- key_columns[key_columns %in% names(trading_data)]
cat("\nFound sentiment columns:\n")
print(existing_columns)

# ===== STEP 5: CALCULATE CROSS-METHOD VARIABILITY =====
cat("\nCalculating cross-method variability...\n")

# Calculate standard deviation across the four sentiment methods for each day
trading_data <- trading_data %>%
  mutate(
    sentiment_method_std = pmap_dbl(
      select(., all_of(sentiment_methods)), 
      function(...) {
        values <- c(...)
        if (sum(!is.na(values)) < 2) return(NA)  # Need at least 2 values
        return(sd(values, na.rm = TRUE))
      }
    ),
    sentiment_method_range = pmap_dbl(
      select(., all_of(sentiment_methods)), 
      function(...) {
        values <- c(...)
        if (sum(!is.na(values)) < 2) return(NA)
        return(max(values, na.rm = TRUE) - min(values, na.rm = TRUE))
      }
    ),
    sentiment_method_count = pmap_int(
      select(., all_of(sentiment_methods)), 
      function(...) sum(!is.na(c(...)))
    )
  )

cat("Added cross-method variability features\n")

# ===== STEP 6: IMPUTATION OF MISSING VALUES =====
cat("\nApplying imputation...\n")

# Function to impute and track imputed values
impute_and_track <- function(data, column_name) {
  # Create imputation flag
  flag_column <- paste0(column_name, "_imputed")
  data[[flag_column]] <- is.na(data[[column_name]])
  
  # Check if column has any non-NA values
  if (all(is.na(data[[column_name]]))) {
    cat("WARNING: Column", column_name, "contains only NA values. Skipping imputation.\n")
    confidence_column <- paste0(column_name, "_confidence")
    data[[confidence_column]] <- 0.0
    return(data)
  }
  
  # Apply LOCF imputation
  original_values <- data[[column_name]]
  imputed_values <- na_locf(original_values, option = "locf")
  
  # For any leading NAs, use next observation carried backward
  if (any(is.na(imputed_values))) {
    imputed_values <- na_locf(imputed_values, option = "nocb")
  }
  
  # Add confidence column
  confidence_column <- paste0(column_name, "_confidence")
  data[[confidence_column]] <- 1.0
  
  # Assign lower confidence to imputed values
  if (any(data[[flag_column]])) {
    runs <- rle(data[[flag_column]])
    run_positions <- c(1, cumsum(runs$lengths)[-length(runs$lengths)] + 1)
    
    for (i in seq_along(runs$lengths)) {
      if (runs$values[i]) {  # If this is a run of NAs
        start_pos <- run_positions[i]
        end_pos <- start_pos + runs$lengths[i] - 1
        run_length <- runs$lengths[i]
        
        # Decreasing confidence for longer runs
        confidence_values <- 0.9 / (1:run_length)^0.5
        confidence_values <- pmax(confidence_values, 0.3)  # Minimum confidence
        
        data[[confidence_column]][start_pos:end_pos] <- confidence_values
      }
    }
  }
  
  # Update the column with imputed values
  data[[column_name]] <- imputed_values
  
  return(data)
}

# Apply imputation to existing key columns
imputed_data <- trading_data
for (col in existing_columns) {
  imputed_data <- impute_and_track(imputed_data, col)
  cat("Processed column:", col, "\n")
}

# ===== STEP 7: CREATE ESSENTIAL FEATURES =====
cat("\nCreating essential features...\n")

# Ensure data is sorted by date
imputed_data <- imputed_data %>% arrange(date)

ml_ready_data <- imputed_data %>%
  # Add basic date components
  mutate(
    year = year(date),
    month = month(date),
    day_of_week = wday(date),
    quarter = quarter(date),
    is_monday = weekday == "Monday",
    is_month_end = {
      month_current <- floor_date(date, "month")
      next_date <- lead(date, default = date[n()] + days(1))
      month_next <- floor_date(next_date, "month")
      month_current != month_next
    }
  )

# Add momentum features (1-day lag only)
cat("Adding momentum features...\n")
for (col in existing_columns) {
  momentum_col <- paste0(col, "_momentum_1d")
  
  ml_ready_data[[momentum_col]] <- (ml_ready_data[[col]] - lag(ml_ready_data[[col]], 1)) / 
    abs(lag(ml_ready_data[[col]], 1))
  
  # Handle division issues
  ml_ready_data[[momentum_col]] <- ifelse(
    is.infinite(ml_ready_data[[momentum_col]]) | is.nan(ml_ready_data[[momentum_col]]), 
    NA, ml_ready_data[[momentum_col]]
  )
}

# Add volatility features (10-day window only)
cat("Adding volatility features...\n")
for (col in existing_columns) {
  volatility_col <- paste0(col, "_volatility_10d")
  
  ml_ready_data[[volatility_col]] <- rollapply(
    ml_ready_data[[col]], 
    width = 10, 
    FUN = sd, 
    align = "right", 
    fill = NA, 
    na.rm = TRUE
  )
}

# Calculate overall confidence
confidence_columns <- grep("_confidence$", names(ml_ready_data), value = TRUE)
if (length(confidence_columns) > 0) {
  ml_ready_data <- ml_ready_data %>%
    mutate(overall_confidence = rowMeans(select(., all_of(confidence_columns)), na.rm = TRUE))
} else {
  ml_ready_data$overall_confidence <- 1.0
}

# ===== STEP 8: CLEAN UP AND IMPUTE NEW FEATURES =====
cat("\nCleaning up new features...\n")

# Get list of newly created features
original_columns <- names(imputed_data)
new_features <- setdiff(names(ml_ready_data), original_columns)

# Handle NAs in new features using forward fill
for (col in new_features) {
  if (is.numeric(ml_ready_data[[col]]) && sum(is.na(ml_ready_data[[col]])) > 0) {
    # Forward fill
    ml_ready_data[[col]] <- na_locf(ml_ready_data[[col]], option = "locf")
    
    # Handle any remaining NAs with zeros
    ml_ready_data[[col]] <- replace_na(ml_ready_data[[col]], 0)
  }
}

# ===== STEP 9: SAVE THE PREPARED DATASET =====
cat("\nFinal ML-ready dataset dimensions:", dim(ml_ready_data), "\n")
cat("Final date range:", min(ml_ready_data$date), "to", max(ml_ready_data$date), "\n")
cat("Number of trading days with data:", nrow(ml_ready_data), "\n")

# Save to CSV
write_csv(ml_ready_data, output_file)
cat("ML-ready dataset saved to:", output_file, "\n")

# ===== STEP 10: GENERATE SUMMARY STATISTICS =====
cat("\n===== SUMMARY STATISTICS =====\n")

# Sentiment indices summary
if (length(existing_columns) > 0) {
  cat("\nSentiment indices summary:\n")
  summary_stats <- ml_ready_data %>%
    select(all_of(existing_columns)) %>%
    summary()
  print(summary_stats)
}

# Cross-method variability summary
cat("\nCross-method variability summary:\n")
variability_stats <- ml_ready_data %>%
  select(sentiment_method_std, sentiment_method_range, sentiment_method_count) %>%
  summary()
print(variability_stats)

# Imputation statistics
imputation_columns <- grep("_imputed$", names(ml_ready_data), value = TRUE)
if (length(imputation_columns) > 0) {
  imputation_stats <- ml_ready_data %>%
    summarise(across(all_of(imputation_columns), ~sum(.))) %>%
    pivot_longer(everything(), 
                 names_to = "column", 
                 values_to = "imputed_count") %>%
    mutate(percentage = imputed_count / nrow(ml_ready_data) * 100)
  
  cat("\nImputation statistics:\n")
  print(imputation_stats)
}

# New features summary
cat("\nNew features created:\n")
cat("- Cross-method variability: sentiment_method_std, sentiment_method_range, sentiment_method_count\n")
cat("- Momentum features (1-day): ", sum(grepl("_momentum_1d$", names(ml_ready_data))), " columns\n")
cat("- Volatility features (10-day): ", sum(grepl("_volatility_10d$", names(ml_ready_data))), " columns\n")
cat("- Date/time features: year, month, day_of_week, quarter, is_monday, is_month_end\n")
cat("- Confidence tracking: overall_confidence and individual confidence scores\n")

cat("\nTotal columns in final dataset:", ncol(ml_ready_data), "\n")
cat("\nData preparation complete!\n")

