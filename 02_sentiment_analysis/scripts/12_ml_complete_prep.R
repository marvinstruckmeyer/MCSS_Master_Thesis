# Complete ML Data Pipeline - Enhanced Version
# Step 1: Merge ml_ready_sentiment_data.csv + features_engineered.csv
# Step 2: Merge with news_sentiment_data.xlsx
# Step 3: Remove market closure days (GSPC_ret = NA)
# Step 4: Add lagged variables and enhanced sentiment features
# Step 5: Apply smart normalization for large-scale variables
# Result: One final ML-ready dataset with enhanced features

# Load required libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(zoo)  # For rolling functions

# File paths - adjust these to your actual file locations
sentiment_file <- "02_sentiment_analysis/ml_ready_sentiment_data.csv"
features_file <- "market_macro_data/features_engineered_fixed.csv" 
news_file <- "market_macro_data/news_sentiment_data (1).xlsx"
news_sheet <- "Data"

final_output <- "03_ml_prediction/ml_data_final_complete_enhanced.csv"
log_file <- "03_ml_prediction/complete_pipeline_enhanced_log.txt"

# Create output directory
dir.create("03_ml_prediction", recursive = TRUE, showWarnings = FALSE)

cat("=== ENHANCED ML DATA PIPELINE ===\n")
cat("🔄 Step 1: Merge sentiment data + features\n")
cat("🔄 Step 2: Merge with news sentiment\n") 
cat("🔄 Step 3: Remove market closure days\n")
cat("🔄 Step 4: Add lagged variables & enhanced sentiment features\n")
cat("🔄 Step 5: Normalize large-scale variables\n")
cat("🎯 Target: GSPC_ret (S&P 500 returns)\n")
cat("📅 Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Start comprehensive logging
sink(log_file)
cat("=== ENHANCED ML PIPELINE LOG ===\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ===== STEP 1: MERGE SENTIMENT DATA + FEATURES =====
cat("STEP 1: MERGING SENTIMENT DATA + FEATURES\n")
cat("==========================================\n")

# Read sentiment data
tryCatch({
  sentiment_data <- read_csv(sentiment_file, show_col_types = FALSE)
  cat("✓ Read sentiment data:", sentiment_file, "\n")
  cat("  - Rows:", nrow(sentiment_data), "| Columns:", ncol(sentiment_data), "\n")
  cat("  - Date range:", min(sentiment_data$date, na.rm = TRUE), "to", max(sentiment_data$date, na.rm = TRUE), "\n")
}, error = function(e) {
  cat("✗ ERROR reading sentiment data:", e$message, "\n")
  sink()
  stop("Failed to read sentiment data")
})

# Read features data
tryCatch({
  features_data <- read_csv(features_file, show_col_types = FALSE)
  cat("✓ Read features data:", features_file, "\n")
  cat("  - Rows:", nrow(features_data), "| Columns:", ncol(features_data), "\n")
  cat("  - Date range:", min(features_data$date, na.rm = TRUE), "to", max(features_data$date, na.rm = TRUE), "\n")
}, error = function(e) {
  cat("✗ ERROR reading features data:", e$message, "\n")
  sink()
  stop("Failed to read features data")
})

# Ensure date columns are properly formatted
sentiment_data <- sentiment_data %>% mutate(date = as.Date(date))
features_data <- features_data %>% mutate(date = as.Date(date))

# Check overlap
sentiment_dates <- unique(sentiment_data$date)
features_dates <- unique(features_data$date)
common_dates_1 <- intersect(sentiment_dates, features_dates)

cat("Date overlap analysis (sentiment + features):\n")
cat("  - Sentiment dates:", length(sentiment_dates), "\n")
cat("  - Features dates:", length(features_dates), "\n")
cat("  - Common dates:", length(common_dates_1), "\n")
cat("  - Expected coverage:", round(length(common_dates_1)/length(sentiment_dates)*100, 1), "%\n")

# Perform first merge
step1_data <- sentiment_data %>%
  left_join(features_data, by = "date", suffix = c("", "_features"))

cat("✓ Step 1 merge completed:\n")
cat("  - Final rows:", nrow(step1_data), "\n")
cat("  - Final columns:", ncol(step1_data), "\n")

# ===== STEP 2: MERGE WITH NEWS SENTIMENT =====
cat("\nSTEP 2: MERGING WITH NEWS SENTIMENT\n")
cat("===================================\n")

# Read news sentiment data from Excel
tryCatch({
  if (!file.exists(news_file)) {
    cat("⚠ WARNING: News file not found:", news_file, "- skipping news sentiment\n")
    step2_data <- step1_data
    news_added <- FALSE
  } else {
    # Check available sheets
    sheet_names <- excel_sheets(news_file)
    cat("Available Excel sheets:", paste(sheet_names, collapse = ", "), "\n")
    
    # Use specified sheet or fall back to first sheet
    if (!news_sheet %in% sheet_names) {
      cat("⚠ WARNING: Sheet '", news_sheet, "' not found. Using first sheet:", sheet_names[1], "\n")
      news_sheet <- sheet_names[1]
    }
    
    # Read the Excel file
    news_data <- read_excel(news_file, sheet = news_sheet)
    cat("✓ Read news sentiment data:", news_file, "(", news_sheet, ")\n")
    cat("  - Rows:", nrow(news_data), "| Columns:", ncol(news_data), "\n")
    cat("  - Column names:", paste(names(news_data), collapse = ", "), "\n")
    
    # Find date column (try different names)
    date_candidates <- c("date", "Date", "DATE", "dates", "Dates")
    date_col <- NULL
    for (candidate in date_candidates) {
      if (candidate %in% names(news_data)) {
        date_col <- candidate
        break
      }
    }
    
    if (is.null(date_col)) {
      cat("⚠ WARNING: No date column found in news data - skipping news sentiment\n")
      step2_data <- step1_data
      news_added <- FALSE
    } else {
      # Standardize date column name
      if (date_col != "date") {
        names(news_data)[names(news_data) == date_col] <- "date"
        cat("✓ Renamed date column from '", date_col, "' to 'date'\n")
      }
      
      # Convert to Date type
      news_data <- news_data %>% mutate(date = as.Date(date))
      
      # Handle column conflicts by prefixing with 'news_'
      existing_cols <- names(step1_data)
      news_cols <- names(news_data)
      conflicting_cols <- intersect(setdiff(news_cols, "date"), setdiff(existing_cols, "date"))
      
      if (length(conflicting_cols) > 0) {
        cat("✓ Resolving", length(conflicting_cols), "column conflicts with 'news_' prefix\n")
        for (col in conflicting_cols) {
          names(news_data)[names(news_data) == col] <- paste0("news_", col)
        }
      }
      
      # Check date overlap
      step1_dates <- unique(step1_data$date)
      news_dates <- unique(news_data$date)
      common_dates_2 <- intersect(step1_dates, news_dates)
      
      cat("Date overlap analysis (step1 + news):\n")
      cat("  - Step1 dates:", length(step1_dates), "\n")
      cat("  - News dates:", length(news_dates), "\n")
      cat("  - Common dates:", length(common_dates_2), "\n")
      cat("  - Expected coverage:", round(length(common_dates_2)/length(step1_dates)*100, 1), "%\n")
      
      # Perform second merge
      step2_data <- step1_data %>%
        left_join(news_data, by = "date", suffix = c("", "_news"))
      
      cat("✓ Step 2 merge completed:\n")
      cat("  - Final rows:", nrow(step2_data), "\n")
      cat("  - Final columns:", ncol(step2_data), "\n")
      news_added <- TRUE
    }
  }
}, error = function(e) {
  cat("⚠ WARNING: Error reading news data:", e$message, "- skipping news sentiment\n")
  step2_data <- step1_data
  news_added <- FALSE
})

# ===== STEP 3: REMOVE MARKET CLOSURE DAYS =====
cat("\nSTEP 3: REMOVING MARKET CLOSURE DAYS\n")
cat("====================================\n")

# Check target variable
target_var <- "GSPC_ret"
if (!target_var %in% names(step2_data)) {
  cat("✗ ERROR: Target variable '", target_var, "' not found!\n")
  cat("Available columns:", paste(names(step2_data), collapse = ", "), "\n")
  sink()
  stop("Target variable missing")
}

# Analyze missing target values
target_missing <- sum(is.na(step2_data[[target_var]]))
cat("Missing values in", target_var, ":", target_missing, "out of", nrow(step2_data), "\n")

if (target_missing > 0) {
  # Show market closure days
  closure_days <- step2_data %>% 
    filter(is.na(!!sym(target_var))) %>%
    select(date) %>%
    mutate(weekday = weekdays(date))
  
  cat("Market closure days to be removed:\n")
  for (i in 1:min(nrow(closure_days), 25)) {  # Show up to 25 days
    cat("  -", as.character(closure_days$date[i]), "(", closure_days$weekday[i], ")\n")
  }
  if (nrow(closure_days) > 25) {
    cat("  ... and", nrow(closure_days) - 25, "more\n")
  }
  
  # Remove market closure days
  step3_data <- step2_data %>% filter(!is.na(!!sym(target_var)))
  
  cat("✓ Removed", target_missing, "market closure days\n")
  cat("  - Remaining observations:", nrow(step3_data), "\n")
} else {
  cat("✓ No market closure days found - target variable complete\n")
  step3_data <- step2_data
}

# ===== STEP 4: ADD LAGGED VARIABLES & ENHANCED SENTIMENT FEATURES =====
cat("\nSTEP 4: ADDING LAGGED VARIABLES & ENHANCED SENTIMENT FEATURES\n")
cat("=============================================================\n")

# Sort by date to ensure proper lagging
step3_data <- step3_data %>% arrange(date)

# 4.1: Add 1-day lagged variables
cat("4.1: Adding 1-day lagged variables\n")
lagged_vars <- c("GSPC_ret", "DGS10_bps", "VIX_chg")
lagged_vars_present <- intersect(lagged_vars, names(step3_data))

if (length(lagged_vars_present) < length(lagged_vars)) {
  missing_vars <- setdiff(lagged_vars, lagged_vars_present)
  cat("⚠ WARNING: Missing variables for lagging:", paste(missing_vars, collapse = ", "), "\n")
}

step4_data <- step3_data

for (var in lagged_vars_present) {
  new_var_name <- paste0(var, "_1D_lagged")
  step4_data[[new_var_name]] <- c(NA, step4_data[[var]][-nrow(step4_data)])
  cat("  ✓ Created", new_var_name, "\n")
}

cat("✓ Added", length(lagged_vars_present), "lagged variables\n")

# 4.2: Enhanced sentiment feature engineering
cat("4.2: Adding enhanced sentiment features\n")

# Define sentiment variables
sentiment_vars <- c("dict_sentiment_avg", "sentimentr_score_avg", "finbert_score_avg", "ollama_score_avg", "aggregate_sentiment_avg")
sentiment_vars_present <- intersect(sentiment_vars, names(step4_data))

if (length(sentiment_vars_present) < length(sentiment_vars)) {
  missing_sentiment <- setdiff(sentiment_vars, sentiment_vars_present)
  cat("⚠ WARNING: Missing sentiment variables:", paste(missing_sentiment, collapse = ", "), "\n")
}

cat("Working with", length(sentiment_vars_present), "sentiment variables:", paste(sentiment_vars_present, collapse = ", "), "\n")

# Function to calculate rolling momentum (avoiding zoo dependency issues)
calculate_momentum <- function(x, n) {
  result <- rep(NA, length(x))
  for (i in (n+1):length(x)) {
    if (!is.na(x[i]) && !is.na(x[i-n])) {  # Only requires 2 specific values
      result[i] <- x[i] - x[i-n]
    }
  }
  return(result)
}

# Function to calculate rolling standard deviation
calculate_rolling_sd <- function(x, n) {
  result <- rep(NA, length(x))
  for (i in n:length(x)) {
    if (sum(!is.na(x[(i-n+1):i])) >= n) {  # Ensure we have enough non-NA values
      result[i] <- sd(x[(i-n+1):i], na.rm = TRUE)
    }
  }
  return(result)
}

# Add momentum features (3-day and 5-day)
for (var in sentiment_vars_present) {
  # 3-day momentum
  momentum_3d_name <- paste0(var, "_momentum_3d")
  step4_data[[momentum_3d_name]] <- calculate_momentum(step4_data[[var]], 3)
  cat("  ✓ Created", momentum_3d_name, "\n")
  
  # 5-day momentum
  momentum_5d_name <- paste0(var, "_momentum_5d")
  step4_data[[momentum_5d_name]] <- calculate_momentum(step4_data[[var]], 5)
  cat("  ✓ Created", momentum_5d_name, "\n")
}

# Add cross-sectional standard deviation across the four main methods
# (excluding aggregate_sentiment_avg to avoid double-counting)
core_sentiment_vars <- c("dict_sentiment_avg", "sentimentr_score_avg", "finbert_score_avg", "ollama_score_avg")
core_sentiment_present <- intersect(core_sentiment_vars, names(step4_data))

if (length(core_sentiment_present) >= 2) {
  cat("Calculating cross-sectional standard deviation across", length(core_sentiment_present), "methods\n")
  
  # Create a matrix of the sentiment scores
  sentiment_matrix <- step4_data[, core_sentiment_present, drop = FALSE]
  
  # Calculate row-wise standard deviation (cross-sectional)
  step4_data$sentiment_cross_sectional_std <- apply(sentiment_matrix, 1, function(row) {
    valid_values <- row[!is.na(row)]
    if (length(valid_values) >= 2) {
      return(sd(valid_values))
    } else {
      return(NA)
    }
  })
  
  cat("  ✓ Created sentiment_cross_sectional_std\n")
  
  # Additional cross-sectional features
  # Range (max - min)
  step4_data$sentiment_cross_sectional_range <- apply(sentiment_matrix, 1, function(row) {
    valid_values <- row[!is.na(row)]
    if (length(valid_values) >= 2) {
      return(max(valid_values) - min(valid_values))
    } else {
      return(NA)
    }
  })
  
  cat("  ✓ Created sentiment_cross_sectional_range\n")
  
  # Coefficient of variation (std/mean)
  step4_data$sentiment_cross_sectional_cv <- apply(sentiment_matrix, 1, function(row) {
    valid_values <- row[!is.na(row)]
    if (length(valid_values) >= 2 && mean(valid_values) != 0) {
      return(sd(valid_values) / abs(mean(valid_values)))
    } else {
      return(NA)
    }
  })
  
  cat("  ✓ Created sentiment_cross_sectional_cv\n")
} else {
  cat("⚠ WARNING: Need at least 2 sentiment methods for cross-sectional analysis. Found:", length(core_sentiment_present), "\n")
}

# Count new features added in this step
new_features_step4 <- ncol(step4_data) - ncol(step3_data)
cat("✓ Step 4 completed: Added", new_features_step4, "new features\n")
cat("  - Lagged variables:", length(lagged_vars_present), "\n")
cat("  - Momentum features:", length(sentiment_vars_present) * 2, "\n")
cat("  - Cross-sectional features:", if(length(core_sentiment_present) >= 2) 3 else 0, "\n")

# ===== STEP 5: NORMALIZE LARGE-SCALE VARIABLES =====
cat("\nSTEP 5: NORMALIZING LARGE-SCALE VARIABLES\n")
cat("=========================================\n")

# Identify variables to analyze (exclude non-predictors)
exclude_vars <- c("date", target_var, "total_views", "total_likes", "video_count")

# Get all numeric variables first, then safely exclude
all_numeric_vars <- step4_data %>% select_if(is.numeric) %>% names()
exclude_vars_present <- intersect(exclude_vars, all_numeric_vars)
numeric_vars <- setdiff(all_numeric_vars, exclude_vars_present)

cat("Analyzing", length(numeric_vars), "potential predictor variables\n")

# Analyze scales
scale_analysis <- map_dfr(numeric_vars, function(var) {
  values <- step4_data[[var]][!is.na(step4_data[[var]])]
  
  if (length(values) == 0) return(NULL)
  
  tibble(
    variable = var,
    min_val = min(values),
    max_val = max(values),
    range = max_val - min_val,
    mean_abs = mean(abs(values)),
    scale_category = case_when(
      max(abs(values)) > 100 ~ "very_large",
      max(abs(values)) > 10 ~ "large", 
      max(abs(values)) > 1 ~ "medium",
      TRUE ~ "small"
    )
  )
}) %>%
  filter(!is.null(variable)) %>%
  arrange(desc(range))

# Show scale distribution
cat("Scale categories:\n")
scale_summary <- scale_analysis %>% count(scale_category)
print(scale_summary)

# Identify normalization candidates
target_scale <- abs(step4_data[[target_var]]) %>% quantile(0.95, na.rm = TRUE)
cat("95th percentile of absolute", target_var, "values:", round(target_scale, 4), "\n")

normalization_candidates <- scale_analysis %>%
  filter(scale_category %in% c("large", "very_large")) %>%
  mutate(
    scale_ratio = pmax(abs(min_val), abs(max_val)) / target_scale,
    needs_normalization = scale_ratio > 10
  ) %>%
  arrange(desc(scale_ratio))

vars_to_normalize <- normalization_candidates %>% 
  filter(needs_normalization) %>% 
  pull(variable)

cat("Variables to normalize:", length(vars_to_normalize), "\n")
if (length(vars_to_normalize) > 0) {
  for (var in vars_to_normalize) {
    range_info <- scale_analysis %>% filter(variable == var)
    cat("  ✓", var, ": [", round(range_info$min_val, 2), 
        "to", round(range_info$max_val, 2), "]\n")
  }
} else {
  cat("  → No variables need normalization\n")
}

# Apply normalization
final_data <- step4_data

if (length(vars_to_normalize) > 0) {
  cat("Applying 0-1 normalization:\n")
  
  # Min-max normalization function
  min_max_normalize <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  
  normalization_log <- tibble(
    variable = character(0),
    original_min = numeric(0),
    original_max = numeric(0),
    scale_ratio = numeric(0)
  )
  
  for (var in vars_to_normalize) {
    # Store original stats
    original_min <- min(final_data[[var]], na.rm = TRUE)
    original_max <- max(final_data[[var]], na.rm = TRUE)
    scale_ratio <- max(abs(original_min), abs(original_max)) / target_scale
    
    # Apply normalization
    final_data[[var]] <- min_max_normalize(final_data[[var]])
    
    # Verify
    new_min <- min(final_data[[var]], na.rm = TRUE)
    new_max <- max(final_data[[var]], na.rm = TRUE)
    
    cat("  ✅", var, ":", 
        "[", round(original_min, 1), ",", round(original_max, 1), "] → ",
        "[", round(new_min, 3), ",", round(new_max, 3), "]\n")
    
    # Log transformation
    normalization_log <- bind_rows(normalization_log, tibble(
      variable = var,
      original_min = original_min,
      original_max = original_max,
      scale_ratio = scale_ratio
    ))
  }
  
  # Save normalization log
  norm_log_file <- str_replace(final_output, "\\.csv$", "_normalization_log.csv")
  write_csv(normalization_log, norm_log_file)
  cat("✓ Normalization log saved:", norm_log_file, "\n")
}

# ===== FINAL VALIDATION =====
cat("\nFINAL VALIDATION\n")
cat("================\n")

cat("Data integrity checks:\n")
cat("  ✓ Final rows:", nrow(final_data), "\n")
cat("  ✓ Final columns:", ncol(final_data), "\n")
cat("  ✓ Missing target values:", sum(is.na(final_data[[target_var]])), "\n")
cat("  ✓ Date range:", min(final_data$date), "to", max(final_data$date), "\n")

# Check total missing values
total_missing <- sum(is.na(final_data))
cat("  ✓ Total missing values:", total_missing, "\n")

# Scale harmony check
target_range <- range(final_data[[target_var]], na.rm = TRUE)
cat("Scale harmony:\n")
cat("  ✓ Target range: [", round(target_range[1], 4), ",", round(target_range[2], 4), "]\n")

if (length(vars_to_normalize) > 0) {
  cat("  ✓ Normalized variables: [0, 1] range\n")
}

# Check new features
cat("New features validation:\n")
for (var in lagged_vars_present) {
  lagged_var <- paste0(var, "_1D_lagged")
  na_count <- sum(is.na(final_data[[lagged_var]]))
  cat("  ✓", lagged_var, ": ", na_count, "NAs (expected: 1 for first row)\n")
}

# ===== SAVE FINAL DATASET =====
cat("\nSAVING FINAL DATASET\n")
cat("====================\n")

write_csv(final_data, final_output)
cat("✓ Final ML dataset saved:", final_output, "\n")

# Create comprehensive summary
summary_data <- tibble(
  step = c("Original sentiment data", "After adding features", "After adding news", "After removing market closures", "After enhanced features", "Final normalized dataset"),
  rows = c(nrow(sentiment_data), nrow(step1_data), nrow(step2_data), nrow(step3_data), nrow(step4_data), nrow(final_data)),
  columns = c(ncol(sentiment_data), ncol(step1_data), ncol(step2_data), ncol(step3_data), ncol(step4_data), ncol(final_data)),
  description = c(
    "Base sentiment data",
    paste("Added", ncol(step1_data) - ncol(sentiment_data), "feature columns"),
    if(news_added) paste("Added", ncol(step2_data) - ncol(step1_data), "news columns") else "News data skipped",
    paste("Removed", nrow(step2_data) - nrow(step3_data), "market closure days"),
    paste("Added", ncol(step4_data) - ncol(step3_data), "enhanced features (lagged + sentiment)"),
    paste("Normalized", length(vars_to_normalize), "large-scale variables")
  )
)

summary_file <- str_replace(final_output, "\\.csv$", "_pipeline_summary.csv")
write_csv(summary_data, summary_file)

# Complete logging
cat("\nPIPELINE SUMMARY:\n")
cat("=================\n")
print(summary_data)

cat("\nENHANCED FEATURES ADDED:\n")
features_added <- ncol(final_data) - ncol(sentiment_data)
cat("Total new features:", features_added, "\n")
cat("Lagged variables:", length(lagged_vars_present), "\n")
cat("Sentiment momentum features:", length(sentiment_vars_present) * 2, "\n")
cat("Cross-sectional sentiment features:", if(length(core_sentiment_present) >= 2) 3 else 0, "\n")
cat("Variables normalized:", length(vars_to_normalize), "\n")
cat("News sentiment added:", if(news_added) "Yes" else "No", "\n")

sink()

# ===== FINAL CONSOLE SUMMARY =====
cat("\n" %>% str_repeat(60), "\n")
cat("🎉 ENHANCED ML PIPELINE FINISHED! 🎉\n")
cat("====================================\n\n")

cat("📊 PIPELINE SUMMARY:\n")
print(summary_data)

cat("\n🔧 TRANSFORMATIONS APPLIED:\n")
cat("  • Features merged: ✅ (", ncol(step1_data) - ncol(sentiment_data), "columns)\n")
cat("  • News sentiment:", if(news_added) "✅ Added" else "⚠️ Skipped", "\n")
cat("  • Market closures removed: ✅ (", nrow(step2_data) - nrow(step3_data), "days)\n")
cat("  • Lagged variables: ✅ (", length(lagged_vars_present), "variables)\n")
cat("  • Sentiment momentum: ✅ (", length(sentiment_vars_present) * 2, "features)\n")
cat("  • Cross-sectional sentiment: ✅ (", if(length(core_sentiment_present) >= 2) 3 else 0, "features)\n")
cat("  • Variables normalized: ✅ (", length(vars_to_normalize), "variables)\n")

cat("\n📈 DATA QUALITY:\n")
cat("  • Final observations:", nrow(final_data), "\n")
cat("  • Final features:", ncol(final_data) - 2, "(excluding date & target)\n")  # -2 for date and target
cat("  • Target completeness: 100% ✅\n")
cat("  • Scale harmony: ✅ Achieved\n")

cat("\n📁 OUTPUT FILES:\n")
cat("  • Main dataset:", basename(final_output), "\n")
cat("  • Pipeline log:", basename(log_file), "\n") 
cat("  • Summary:", basename(summary_file), "\n")
if (length(vars_to_normalize) > 0) {
  cat("  • Normalization log:", basename(str_replace(final_output, "\\.csv$", "_normalization_log.csv")), "\n")
}

cat("\n🚀 READY FOR MACHINE LEARNING!\n")
cat("Your enhanced dataset includes:\n")
cat("  ✓ Original sentiment features\n")
cat("  ✓ Updated engineered features\n")
if(news_added) cat("  ✓ Historical news sentiment\n")
cat("  ✓ 1-day lagged variables\n")
cat("  ✓ 3-day & 5-day sentiment momentum\n")
cat("  ✓ Cross-sectional sentiment analysis\n")
cat("  ✓ Properly scaled variables\n")
cat("  ✓ Clean trading days only\n")

cat("\n🎯 Next steps:\n")
cat("  1. Load", basename(final_output), "\n")
cat("  2. Split into train/test sets\n")
cat("  3. Run Random Forest, XGBoost, Linear Regression\n")
cat("  4. Compare model performance\n")

# Return comprehensive results
list(
  success = TRUE,
  original_rows = nrow(sentiment_data),
  final_rows = nrow(final_data),
  features_added = ncol(final_data) - ncol(sentiment_data),
  lagged_features_added = length(lagged_vars_present),
  sentiment_momentum_features = length(sentiment_vars_present) * 2,
  cross_sectional_features = if(length(core_sentiment_present) >= 2) 3 else 0,
  news_sentiment_added = news_added,
  market_closures_removed = nrow(step2_data) - nrow(step3_data),
  variables_normalized = length(vars_to_normalize),
  data_retention_pct = round(nrow(final_data)/nrow(sentiment_data)*100, 1),
  output_file = final_output,
  ready_for_ml = TRUE)


## fix
# =============================================================================
# SENTIMENT AGGREGATION DIAGNOSTIC & FIX
# =============================================================================

library(tidyverse)

cat("Sentiment Aggregation Diagnostic & Fix\n")
cat("=====================================\n\n")

# Load your enhanced dataset
data <- read_csv("03_ml_prediction/ml_data_final_complete_enhanced.csv", show_col_types = FALSE)
cat("Loaded dataset with", nrow(data), "rows\n")

# =============================================================================
# IDENTIFY THE PROBLEM
# =============================================================================

# Focus on the sentiment columns
sentiment_cols <- c("dict_sentiment_avg", "sentimentr_score_avg", "finbert_score_avg", 
                    "ollama_score_avg", "aggregate_sentiment_avg")

# Check which columns exist
available_sentiment <- intersect(sentiment_cols, names(data))
cat("Available sentiment columns:", paste(available_sentiment, collapse = ", "), "\n")

# Extract sentiment data
sentiment_data <- data %>%
  select(date, all_of(available_sentiment)) %>%
  mutate(row_id = row_number())

# Find rows where aggregate is NA but individuals are not
individual_cols <- c("dict_sentiment_avg", "sentimentr_score_avg", "finbert_score_avg", "ollama_score_avg")
available_individual <- intersect(individual_cols, names(sentiment_data))

cat("\nAnalyzing aggregate_sentiment_avg issues...\n")

if ("aggregate_sentiment_avg" %in% names(sentiment_data)) {
  # Find problematic rows
  problematic_rows <- sentiment_data %>%
    filter(is.na(aggregate_sentiment_avg)) %>%
    select(row_id, date, all_of(available_individual), aggregate_sentiment_avg)
  
  cat("Rows where aggregate_sentiment_avg is NA:", nrow(problematic_rows), "\n")
  
  if (nrow(problematic_rows) > 0) {
    cat("\nFirst 10 problematic rows:\n")
    print(head(problematic_rows, 10))
    
    # Check if individual sentiment scores are available for these rows
    for (i in 1:min(10, nrow(problematic_rows))) {
      row_data <- problematic_rows[i, ]
      individual_values <- as.numeric(row_data[available_individual])
      non_na_count <- sum(!is.na(individual_values))
      
      cat(sprintf("Row %d: %d/%d individual scores available", 
                  row_data$row_id, non_na_count, length(available_individual)))
      
      if (non_na_count > 0) {
        manual_avg <- mean(individual_values, na.rm = TRUE)
        cat(sprintf(" → Manual average: %.6f", manual_avg))
      }
      cat("\n")
    }
  }
} else {
  cat("aggregate_sentiment_avg column not found!\n")
}

# =============================================================================
# CREATE CORRECTED AGGREGATE SENTIMENT
# =============================================================================

cat("\n=== CREATING CORRECTED AGGREGATE SENTIMENT ===\n")

# Create a corrected version of aggregate sentiment
if (length(available_individual) >= 2) {
  cat("Creating corrected aggregate sentiment from", length(available_individual), "individual methods\n")
  
  # Calculate row-wise mean
  sentiment_data <- sentiment_data %>%
    rowwise() %>%
    mutate(
      aggregate_sentiment_corrected = mean(c_across(all_of(available_individual)), na.rm = TRUE),
      available_methods = sum(!is.na(c_across(all_of(available_individual)))),
      # Set to NA if no methods are available
      aggregate_sentiment_corrected = ifelse(available_methods == 0, NA, aggregate_sentiment_corrected)
    ) %>%
    ungroup()
  
  # Compare original vs corrected
  if ("aggregate_sentiment_avg" %in% names(sentiment_data)) {
    comparison <- sentiment_data %>%
      select(row_id, date, aggregate_sentiment_avg, aggregate_sentiment_corrected, available_methods) %>%
      mutate(
        originally_na = is.na(aggregate_sentiment_avg),
        corrected_na = is.na(aggregate_sentiment_corrected),
        difference = abs(aggregate_sentiment_avg - aggregate_sentiment_corrected)
      )
    
    cat("\nComparison of original vs corrected aggregate sentiment:\n")
    cat("- Originally NA:", sum(comparison$originally_na), "rows\n")
    cat("- Corrected NA:", sum(comparison$corrected_na), "rows\n")
    cat("- Rows fixed:", sum(comparison$originally_na & !comparison$corrected_na), "\n")
    
    # Show fixed rows
    fixed_rows <- comparison %>%
      filter(originally_na & !corrected_na) %>%
      head(10)
    
    if (nrow(fixed_rows) > 0) {
      cat("\nFirst 10 rows that were fixed:\n")
      print(fixed_rows %>% select(row_id, date, aggregate_sentiment_corrected, available_methods))
    }
    
    # Check for differences in non-NA cases
    non_na_both <- comparison %>%
      filter(!originally_na & !corrected_na)
    
    if (nrow(non_na_both) > 0) {
      max_diff <- max(non_na_both$difference, na.rm = TRUE)
      cat("Maximum difference in non-NA cases:", round(max_diff, 8), "\n")
      
      if (max_diff > 0.0001) {
        cat("⚠️ WARNING: Significant differences found in existing calculations!\n")
        large_diff <- non_na_both %>% filter(difference > 0.0001) %>% head(5)
        print(large_diff)
      } else {
        cat("✓ Existing calculations match (within rounding error)\n")
      }
    }
  }
  
} else {
  cat("⚠️ Insufficient individual sentiment methods available\n")
}

# =============================================================================
# APPLY FIX TO FULL DATASET
# =============================================================================

cat("\n=== APPLYING FIX TO FULL DATASET ===\n")

# Apply the correction to the full dataset
data_fixed <- data %>%
  rowwise() %>%
  mutate(
    # Recalculate aggregate sentiment
    aggregate_sentiment_avg_fixed = mean(c_across(all_of(available_individual)), na.rm = TRUE),
    methods_available = sum(!is.na(c_across(all_of(available_individual)))),
    # Set to NA if no methods available
    aggregate_sentiment_avg_fixed = ifelse(methods_available == 0, NA, aggregate_sentiment_avg_fixed)
  ) %>%
  ungroup() %>%
  select(-methods_available)  # Remove helper column

# Replace the original column
data_fixed <- data_fixed %>%
  mutate(aggregate_sentiment_avg = aggregate_sentiment_avg_fixed) %>%
  select(-aggregate_sentiment_avg_fixed)

# Verify the fix
original_na_count <- sum(is.na(data$aggregate_sentiment_avg))
fixed_na_count <- sum(is.na(data_fixed$aggregate_sentiment_avg))

cat("Results:\n")
cat("- Original NA count:", original_na_count, "\n")
cat("- Fixed NA count:", fixed_na_count, "\n")
cat("- Rows recovered:", original_na_count - fixed_na_count, "\n")

# =============================================================================
# RECALCULATE MOMENTUM FEATURES WITH FIXED FUNCTION
# =============================================================================

cat("\n=== RECALCULATING MOMENTUM FEATURES ===\n")

# Fixed momentum calculation function
calculate_momentum_fixed <- function(x, n) {
  result <- rep(NA, length(x))
  for (i in (n+1):length(x)) {
    if (!is.na(x[i]) && !is.na(x[i-n])) {
      result[i] <- x[i] - x[i-n]
    }
  }
  return(result)
}

# Sort by date
data_fixed <- data_fixed %>% arrange(date)

# Recalculate momentum features for aggregate sentiment
if ("aggregate_sentiment_avg" %in% names(data_fixed)) {
  cat("Recalculating momentum features for aggregate_sentiment_avg...\n")
  
  # 3-day momentum
  data_fixed <- data_fixed %>%
    mutate(aggregate_sentiment_avg_momentum_3d = calculate_momentum_fixed(aggregate_sentiment_avg, 3))
  
  # 5-day momentum  
  data_fixed <- data_fixed %>%
    mutate(aggregate_sentiment_avg_momentum_5d = calculate_momentum_fixed(aggregate_sentiment_avg, 5))
  
  # Check improvement
  original_3d_na <- sum(is.na(data$aggregate_sentiment_avg_momentum_3d))
  original_5d_na <- sum(is.na(data$aggregate_sentiment_avg_momentum_5d))
  fixed_3d_na <- sum(is.na(data_fixed$aggregate_sentiment_avg_momentum_3d))
  fixed_5d_na <- sum(is.na(data_fixed$aggregate_sentiment_avg_momentum_5d))
  
  cat("Momentum 3D - Original NAs:", original_3d_na, "→ Fixed NAs:", fixed_3d_na, "\n")
  cat("Momentum 5D - Original NAs:", original_5d_na, "→ Fixed NAs:", fixed_5d_na, "\n")
}

# =============================================================================
# SAVE CORRECTED DATASET
# =============================================================================

cat("\n=== SAVING CORRECTED DATASET ===\n")

output_file <- "03_ml_prediction/ml_data_final_complete_enhanced_FIXED.csv"
write_csv(data_fixed, output_file)

cat("✓ Corrected dataset saved:", output_file, "\n")

# Final summary
cat("\n=== FINAL SUMMARY ===\n")
cat("Original dataset observations:", nrow(data), "\n")
cat("Fixed dataset observations:", nrow(data_fixed), "\n")
cat("Aggregate sentiment NAs: ", original_na_count, "→", fixed_na_count, "\n")
cat("Expected observation recovery: ~", original_na_count - fixed_na_count, "\n")

# Test the impact on complete cases
original_complete <- data %>%
  select(date, GSPC_ret, aggregate_sentiment_avg, aggregate_sentiment_avg_momentum_3d, aggregate_sentiment_avg_momentum_5d) %>%
  filter(complete.cases(.)) %>%
  nrow()

fixed_complete <- data_fixed %>%
  select(date, GSPC_ret, aggregate_sentiment_avg, aggregate_sentiment_avg_momentum_3d, aggregate_sentiment_avg_momentum_5d) %>%
  filter(complete.cases(.)) %>%
  nrow()

cat("\nComplete cases (key sentiment features only):\n")
cat("- Original:", original_complete, "\n")
cat("- Fixed:", fixed_complete, "\n")
cat("- Recovery:", fixed_complete - original_complete, "observations\n")

cat("\n🎯 RECOMMENDATION:\n")
cat("Use the fixed dataset:", basename(output_file), "\n")
cat("This should significantly improve your observation count for ML modeling!\n")

# =============================================================================
# BOND MARKET NA DETECTION AND FIX
# =============================================================================
# This script identifies and fixes the NA values in bond market data
# that are causing ML pipeline failures

library(tidyverse)
library(lubridate)

cat("Bond Market NA Detection and Fix\n")
cat("===============================\n\n")

# =============================================================================
# STEP 1: LOAD AND EXAMINE THE DATA
# =============================================================================

# Load the enhanced dataset
data_file <- "03_ml_prediction/ml_data_final_complete_enhanced.csv"

if (!file.exists(data_file)) {
  # Try alternative file name
  data_file <- "03_ml_prediction/ml_data_final_complete_enhanced_FIXED.csv"
}

if (!file.exists(data_file)) {
  cat("❌ ERROR: Cannot find the ML dataset file!\n")
  cat("Please ensure one of these files exists:\n")
  cat("- 03_ml_prediction/ml_data_final_complete_enhanced.csv\n")
  cat("- 03_ml_prediction/ml_data_final_complete_enhanced_FIXED.csv\n")
  stop("Dataset file not found")
}

cat("📊 Loading dataset:", data_file, "\n")
data <- read_csv(data_file, show_col_types = FALSE)
cat("✓ Loaded", nrow(data), "rows and", ncol(data), "columns\n\n")

# =============================================================================
# STEP 2: IDENTIFY BOND MARKET VARIABLES
# =============================================================================

# Define bond market variables that might have the NA issue
bond_vars <- c("DGS2_bps", "DGS5_bps", "DGS10_bps", "DGS30_bps", 
               "T5YIE_bps", "T10YIE_bps", "T5YIFR_bps")

# Check which variables actually exist in the dataset
available_bond_vars <- intersect(bond_vars, names(data))
missing_bond_vars <- setdiff(bond_vars, names(data))

cat("🔍 BOND MARKET VARIABLES ANALYSIS\n")
cat("Available bond variables:", length(available_bond_vars), "\n")
if (length(available_bond_vars) > 0) {
  cat("- ", paste(available_bond_vars, collapse = "\n- "), "\n")
}

if (length(missing_bond_vars) > 0) {
  cat("\n⚠️ Missing bond variables:", length(missing_bond_vars), "\n")
  cat("- ", paste(missing_bond_vars, collapse = "\n- "), "\n")
}

# =============================================================================
# STEP 3: DETECT NA PATTERNS
# =============================================================================

cat("\n🔎 DETECTING NA PATTERNS\n")
cat("========================\n")

if (length(available_bond_vars) == 0) {
  cat("❌ No bond variables found in dataset!\n")
  stop("Cannot proceed without bond variables")
}

# Check for NAs in each bond variable
na_summary <- map_dfr(available_bond_vars, function(var) {
  na_count <- sum(is.na(data[[var]]))
  na_dates <- data$date[is.na(data[[var]])]
  
  tibble(
    variable = var,
    na_count = na_count,
    na_dates = list(na_dates)
  )
})

cat("NA Summary by Variable:\n")
for (i in 1:nrow(na_summary)) {
  var_name <- na_summary$variable[i]
  na_count <- na_summary$na_count[i]
  cat(sprintf("- %-12s: %d NAs\n", var_name, na_count))
  
  if (na_count > 0 && na_count <= 10) {
    na_dates <- na_summary$na_dates[[i]]
    cat("  Dates: ", paste(as.character(na_dates), collapse = ", "), "\n")
  }
}

# =============================================================================
# STEP 4: FOCUS ON THE SPECIFIC PROBLEM DATE
# =============================================================================

cat("\n🎯 EXAMINING 2023-10-09 SPECIFICALLY\n")
cat("===================================\n")

target_date <- as.Date("2023-10-09")

# Check if this date exists in our dataset
if (target_date %in% data$date) {
  target_row <- data %>% filter(date == target_date)
  
  cat("✓ Found target date:", as.character(target_date), "\n")
  cat("Row index:", which(data$date == target_date), "\n\n")
  
  # Check NA status for each bond variable on this date
  cat("Bond variable status on", as.character(target_date), ":\n")
  for (var in available_bond_vars) {
    value <- target_row[[var]]
    status <- if (is.na(value)) "❌ NA" else paste("✓", round(value, 3))
    cat(sprintf("- %-12s: %s\n", var, status))
  }
  
  # Check surrounding dates
  cat("\n📅 SURROUNDING DATES ANALYSIS\n")
  date_index <- which(data$date == target_date)
  
  # Get 3 days before and after
  surrounding_indices <- (date_index - 3):(date_index + 3)
  surrounding_indices <- surrounding_indices[surrounding_indices > 0 & surrounding_indices <= nrow(data)]
  
  surrounding_data <- data[surrounding_indices, c("date", available_bond_vars)]
  
  cat("Values around", as.character(target_date), ":\n")
  print(surrounding_data)
  
} else {
  cat("❌ Target date", as.character(target_date), "not found in dataset\n")
  
  # Find closest dates
  closest_before <- max(data$date[data$date < target_date], na.rm = TRUE)
  closest_after <- min(data$date[data$date > target_date], na.rm = TRUE)
  
  cat("Closest dates:\n")
  cat("- Before:", as.character(closest_before), "\n")
  cat("- After:", as.character(closest_after), "\n")
}

# =============================================================================
# STEP 5: COMPREHENSIVE NA DETECTION
# =============================================================================

cat("\n🔍 COMPREHENSIVE NA DETECTION\n")
cat("=============================\n")

# Find all rows with any NA in bond variables
rows_with_bond_nas <- data %>%
  mutate(row_index = row_number()) %>%
  filter(if_any(all_of(available_bond_vars), is.na)) %>%
  select(row_index, date, all_of(available_bond_vars))

cat("Total rows with bond variable NAs:", nrow(rows_with_bond_nas), "\n")

if (nrow(rows_with_bond_nas) > 0) {
  cat("\nAll rows with bond NAs:\n")
  print(rows_with_bond_nas)
  
  # Check if all bond variables are NA on the same dates
  cat("\n🔗 CHECKING FOR SIMULTANEOUS NAs\n")
  
  # For each row with NAs, count how many bond variables are NA
  rows_with_bond_nas <- rows_with_bond_nas %>%
    rowwise() %>%
    mutate(
      na_count = sum(is.na(c_across(all_of(available_bond_vars)))),
      total_bond_vars = length(available_bond_vars)
    ) %>%
    ungroup()
  
  cat("Pattern analysis:\n")
  pattern_summary <- rows_with_bond_nas %>%
    count(na_count, total_bond_vars) %>%
    mutate(pattern = paste(na_count, "out of", total_bond_vars, "variables NA"))
  
  print(pattern_summary)
  
  # Identify dates where ALL bond variables are NA
  all_na_dates <- rows_with_bond_nas %>%
    filter(na_count == total_bond_vars) %>%
    pull(date)
  
  if (length(all_na_dates) > 0) {
    cat("\n🚨 Dates where ALL bond variables are NA:\n")
    for (d in all_na_dates) {
      cat("-", as.character(d), "\n")
    }
  }
}

# =============================================================================
# STEP 6: PREPARE SOLUTIONS
# =============================================================================

cat("\n🔧 PREPARING SOLUTIONS\n")
cat("=====================\n")

if (nrow(rows_with_bond_nas) == 0) {
  cat("✅ No bond variable NAs found! Your dataset appears to be clean.\n")
} else {
  cat("Found", nrow(rows_with_bond_nas), "problematic rows. Preparing solutions...\n\n")
  
  # Sort data by date to ensure proper interpolation
  data_sorted <- data %>% arrange(date)
  
  # =============================================================================
  # SOLUTION 1: FORWARD FILL
  # =============================================================================
  
  cat("📈 SOLUTION 1: FORWARD FILL\n")
  cat("----------------------------\n")
  
  data_forward_fill <- data_sorted
  
  for (var in available_bond_vars) {
    # Forward fill (carry last observation forward)
    data_forward_fill[[var]] <- na.locf(data_forward_fill[[var]], na.rm = FALSE)
    
    # Count remaining NAs
    remaining_nas <- sum(is.na(data_forward_fill[[var]]))
    cat(sprintf("- %-12s: %d NAs remaining after forward fill\n", var, remaining_nas))
  }
  
  # =============================================================================
  # SOLUTION 2: LINEAR INTERPOLATION
  # =============================================================================
  
  cat("\n📊 SOLUTION 2: LINEAR INTERPOLATION\n")
  cat("-----------------------------------\n")
  
  data_interpolated <- data_sorted
  
  for (var in available_bond_vars) {
    # Linear interpolation
    data_interpolated[[var]] <- na.approx(data_interpolated[[var]], na.rm = FALSE)
    
    # Count remaining NAs
    remaining_nas <- sum(is.na(data_interpolated[[var]]))
    cat(sprintf("- %-12s: %d NAs remaining after interpolation\n", var, remaining_nas))
  }
  
  # =============================================================================
  # SOLUTION 3: HYBRID APPROACH
  # =============================================================================
  
  cat("\n🔄 SOLUTION 3: HYBRID APPROACH (Forward Fill + Interpolation)\n")
  cat("-------------------------------------------------------------\n")
  
  data_hybrid <- data_sorted
  
  for (var in available_bond_vars) {
    # First try linear interpolation
    data_hybrid[[var]] <- na.approx(data_hybrid[[var]], na.rm = FALSE)
    
    # Then forward fill any remaining NAs (typically at the beginning)
    data_hybrid[[var]] <- na.locf(data_hybrid[[var]], na.rm = FALSE)
    
    # Finally, backward fill any still remaining NAs (typically at the very beginning)
    data_hybrid[[var]] <- na.locf(data_hybrid[[var]], fromLast = TRUE, na.rm = FALSE)
    
    # Count remaining NAs
    remaining_nas <- sum(is.na(data_hybrid[[var]]))
    cat(sprintf("- %-12s: %d NAs remaining after hybrid approach\n", var, remaining_nas))
  }
  
  # =============================================================================
  # STEP 7: COMPARE SOLUTIONS
  # =============================================================================
  
  cat("\n📋 SOLUTION COMPARISON\n")
  cat("=====================\n")
  
  # Check the specific problem date if it exists
  if (target_date %in% data$date) {
    cat("Values for", as.character(target_date), ":\n")
    
    target_original <- data %>% filter(date == target_date) %>% select(all_of(available_bond_vars))
    target_forward <- data_forward_fill %>% filter(date == target_date) %>% select(all_of(available_bond_vars))
    target_interp <- data_interpolated %>% filter(date == target_date) %>% select(all_of(available_bond_vars))
    target_hybrid <- data_hybrid %>% filter(date == target_date) %>% select(all_of(available_bond_vars))
    
    comparison_table <- tibble(
      Variable = available_bond_vars,
      Original = as.numeric(target_original[1, ]),
      Forward_Fill = as.numeric(target_forward[1, ]),
      Interpolation = as.numeric(target_interp[1, ]),
      Hybrid = as.numeric(target_hybrid[1, ])
    )
    
    print(comparison_table)
  }
  
  # Overall NA counts by solution
  cat("\nOverall NA counts by solution:\n")
  
  for (var in available_bond_vars) {
    original_nas <- sum(is.na(data[[var]]))
    forward_nas <- sum(is.na(data_forward_fill[[var]]))
    interp_nas <- sum(is.na(data_interpolated[[var]]))
    hybrid_nas <- sum(is.na(data_hybrid[[var]]))
    
    cat(sprintf("%-12s: Original=%d, Forward=%d, Interp=%d, Hybrid=%d\n", 
                var, original_nas, forward_nas, interp_nas, hybrid_nas))
  }
  
  # =============================================================================
  # STEP 8: SAVE RECOMMENDED SOLUTION
  # =============================================================================
  
  cat("\n💾 SAVING RECOMMENDED SOLUTION\n")
  cat("=============================\n")
  
  # Use hybrid approach as it's most robust
  recommended_solution <- data_hybrid
  
  # Restore original column order
  recommended_solution <- recommended_solution %>%
    select(all_of(names(data)))
  
  # Save the fixed dataset
  output_file <- str_replace(data_file, "\\.csv$", "_BOND_FIXED.csv")
  write_csv(recommended_solution, output_file)
  
  cat("✅ Recommended solution saved:", basename(output_file), "\n")
  
  # Verify the fix
  cat("\n🔍 VERIFICATION\n")
  cat("===============\n")
  
  # Check if the specific problem is resolved
  total_nas_original <- sum(is.na(data[available_bond_vars]))
  total_nas_fixed <- sum(is.na(recommended_solution[available_bond_vars]))
  
  cat("Total NAs in bond variables:\n")
  cat("- Original:", total_nas_original, "\n")
  cat("- Fixed:", total_nas_fixed, "\n")
  cat("- Reduction:", total_nas_original - total_nas_fixed, "\n")
  
  if (total_nas_fixed == 0) {
    cat("\n🎉 SUCCESS: All bond variable NAs have been eliminated!\n")
  } else {
    cat("\n⚠️ WARNING:", total_nas_fixed, "NAs remain in bond variables\n")
  }
  
  # Test complete cases for ML readiness
  complete_cases_original <- sum(complete.cases(data))
  complete_cases_fixed <- sum(complete.cases(recommended_solution))
  
  cat("\nComplete cases (all variables):\n")
  cat("- Original:", complete_cases_original, "\n")
  cat("- Fixed:", complete_cases_fixed, "\n")
  cat("- Improvement:", complete_cases_fixed - complete_cases_original, "\n")
  
  cat("\n📋 SUMMARY & RECOMMENDATIONS\n")
  cat("============================\n")
  cat("✅ Problem identified and fixed using hybrid approach:\n")
  cat("   1. Linear interpolation for interior NAs\n")
  cat("   2. Forward fill for remaining NAs\n")
  cat("   3. Backward fill for edge cases\n")
  cat("\n🎯 Next steps:\n")
  cat("   1. Use the fixed dataset:", basename(output_file), "\n")
  cat("   2. Re-run your ML pipeline\n")
  cat("   3. Verify no NA-related errors occur\n")
  
  cat("\n💡 The hybrid approach was chosen because:\n")
  cat("   - Linear interpolation preserves realistic values\n")
  cat("   - Forward/backward fill handles edge cases\n")
  cat("   - Most robust for financial time series\n")
}

cat("\n" %>% str_repeat(50), "\n")
cat("🔧 BOND MARKET NA FIX COMPLETED! 🔧\n")
cat("Use the _BOND_FIXED.csv file for your ML pipeline.\n")

