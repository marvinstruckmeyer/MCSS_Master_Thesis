# Enhanced Analysis of Missing Values in Sentiment Indices
# This script identifies patterns in missing data and suggests handling strategies

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(naniar)  # For missing data visualization
library(imputeTS) # For imputation methods

# Set file paths
input_file <- "sentiment_analysis/daily_sentiment_indices_with_rolling.csv"
output_dir <- "sentiment_analysis/missing_data_analysis"
plots_dir <- file.path(output_dir, "plots")

# Create output directories
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# Read the data
cat("Reading sentiment indices...\n")
sentiment_data <- read_csv(input_file, show_col_types = FALSE)

# Convert date column to proper Date type if it's not already
sentiment_data <- sentiment_data %>%
  mutate(date = as.Date(date))

# Display basic information
cat("Dataset dimensions:", dim(sentiment_data), "\n")
cat("Date range:", min(sentiment_data$date, na.rm = TRUE), "to", 
    max(sentiment_data$date, na.rm = TRUE), "\n")
cat("Total days in dataset:", nrow(sentiment_data), "\n")

# ===== BASIC MISSING DATA ANALYSIS =====

# Calculate overall missing rate for each column
missing_summary <- sentiment_data %>%
  summarise(across(everything(), ~sum(is.na(.))/n()*100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "column", 
               values_to = "missing_percentage") %>%
  arrange(desc(missing_percentage))

# Save missing summary to file
write_csv(missing_summary, file.path(output_dir, "missing_percentage_by_column.csv"))

# Display missing summary
cat("\nMissing data percentage by column:\n")
print(missing_summary)

# Add weekday information for temporal analysis
sentiment_data <- sentiment_data %>%
  mutate(
    weekday = weekdays(date),
    is_weekend = weekday %in% c("Saturday", "Sunday"),
    # Create month and year columns for temporal analysis
    month = month(date),
    year = year(date),
    month_year = floor_date(date, "month")
  )

# ===== TEMPORAL PATTERN ANALYSIS =====

# 1. Missing data by weekday
missing_by_weekday <- sentiment_data %>%
  group_by(weekday) %>%
  summarise(
    total_days = n(),
    missing_days = sum(is.na(simple_sentiment_index)),
    missing_percentage = missing_days / total_days * 100
  ) %>%
  # Reorder weekdays
  mutate(weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", 
                                              "Thursday", "Friday", 
                                              "Saturday", "Sunday")))

# Save weekday analysis
write_csv(missing_by_weekday, file.path(output_dir, "missing_by_weekday.csv"))

# Plot missing data by weekday
weekday_plot <- ggplot(missing_by_weekday, 
                       aes(x = weekday, y = missing_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", missing_percentage)), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Missing Sentiment Data by Weekday",
    subtitle = "Percentage of days with missing sentiment values",
    x = "Weekday",
    y = "Missing Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save plot
ggsave(file.path(plots_dir, "missing_by_weekday.png"), 
       weekday_plot, width = 8, height = 6, dpi = 300)

# 2. Missing data by month-year
missing_by_month <- sentiment_data %>%
  group_by(month_year) %>%
  summarise(
    total_days = n(),
    missing_days = sum(is.na(simple_sentiment_index)),
    missing_percentage = missing_days / total_days * 100
  ) %>%
  arrange(month_year)

# Save monthly analysis
write_csv(missing_by_month, file.path(output_dir, "missing_by_month.csv"))

# Plot missing data by month
month_plot <- ggplot(missing_by_month, 
                     aes(x = month_year, y = missing_percentage)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Missing Sentiment Data by Month",
    subtitle = "Percentage of days with missing sentiment values",
    x = "Month",
    y = "Missing Percentage (%)"
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save plot
ggsave(file.path(plots_dir, "missing_by_month.png"), 
       month_plot, width = 10, height = 6, dpi = 300)

# 3. Create a timeline of available vs. missing data
timeline_data <- sentiment_data %>%
  select(date, simple_sentiment_index) %>%
  mutate(has_data = !is.na(simple_sentiment_index))

# Plot the timeline
timeline_plot <- ggplot(timeline_data, aes(x = date, y = 1, fill = has_data)) +
  geom_tile(height = 0.8) +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red"),
                    labels = c("TRUE" = "Data Available", "FALSE" = "Missing Data")) +
  labs(
    title = "Timeline of Available Sentiment Data",
    subtitle = "Green indicates days with data, red indicates missing data",
    x = "Date",
    y = "",
    fill = "Data Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )

# Save plot
ggsave(file.path(plots_dir, "data_availability_timeline.png"), 
       timeline_plot, width = 12, height = 4, dpi = 300)

# ===== MISSING DATA PATTERNS ANALYSIS =====

# 1. Calculate total number of missing days
missing_days <- sum(is.na(sentiment_data$simple_sentiment_index))
total_days <- nrow(sentiment_data)
missing_percentage <- missing_days / total_days * 100

cat("\nOverall missing data statistics:\n")
cat("Total days:", total_days, "\n")
cat("Days with missing sentiment data:", missing_days, "\n")
cat("Missing percentage:", round(missing_percentage, 2), "%\n")

# 2. Analyze missing data by weekend vs. weekday
weekend_missing <- sentiment_data %>%
  group_by(is_weekend) %>%
  summarize(
    total_days = n(),
    missing_days = sum(is.na(simple_sentiment_index)),
    missing_percentage = missing_days / total_days * 100
  )

cat("\nMissing data by weekend status:\n")
print(weekend_missing)

# 3. Analysis of consecutive runs of missing data
# First, get the run lengths of missing and non-missing data
runs <- rle(is.na(sentiment_data$simple_sentiment_index))

# Extract maximum consecutive missing days
max_consecutive_missing <- max(runs$lengths[runs$values])
cat("\nLongest run of consecutive missing days:", max_consecutive_missing, "\n")

# Get distribution of run lengths for missing data
missing_run_lengths <- runs$lengths[runs$values]
run_length_table <- table(missing_run_lengths)

cat("\nDistribution of consecutive missing days:\n")
print(run_length_table)

# Save run length distribution to file
run_length_df <- data.frame(
  run_length = as.numeric(names(run_length_table)),
  frequency = as.numeric(run_length_table)
)
write_csv(run_length_df, file.path(output_dir, "missing_run_lengths.csv"))

# Plot the distribution of run lengths
run_length_plot <- ggplot(run_length_df, aes(x = run_length, y = frequency)) +
  geom_bar(stat = "identity", fill = "orangered") +
  labs(
    title = "Distribution of Consecutive Missing Days",
    x = "Number of Consecutive Missing Days",
    y = "Frequency"
  ) +
  scale_x_continuous(breaks = sort(unique(run_length_df$run_length))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Save plot
ggsave(file.path(plots_dir, "missing_run_lengths.png"), 
       run_length_plot, width = 8, height = 6, dpi = 300)

# 4. Identify when the missing runs occur (e.g., beginning, middle, end of dataset)
# First, calculate cumulative run lengths to find positions
run_positions <- data.frame(
  run_length = runs$lengths,
  is_missing = runs$values
)

# Add start and end positions
run_positions$start_pos <- c(1, cumsum(runs$lengths)[-length(runs$lengths)] + 1)
run_positions$end_pos <- cumsum(runs$lengths)

# Extract only the missing runs
missing_runs_positions <- run_positions %>%
  filter(is_missing == TRUE) %>%
  mutate(
    start_date = sentiment_data$date[start_pos],
    end_date = sentiment_data$date[end_pos],
    weekday_start = weekdays(start_date),
    weekday_end = weekdays(end_date)
  )

# Save missing runs with their dates
write_csv(missing_runs_positions, file.path(output_dir, "missing_runs_with_dates.csv"))

# Check for long runs (more than 3 consecutive days)
long_runs <- missing_runs_positions %>% 
  filter(run_length > 3) %>%
  arrange(desc(run_length))

cat("\nLong runs of missing data (more than 3 consecutive days):\n")
print(head(long_runs %>% select(run_length, start_date, end_date), 10))

# 5. Check if missing runs primarily start on weekends
weekday_start_counts <- table(missing_runs_positions$weekday_start)
cat("\nWeekday distribution of when missing runs start:\n")
print(weekday_start_counts)

# ===== IDENTIFY PERIODS WITH GOOD DATA COVERAGE =====

# Calculate rolling window completeness (for finding good periods)
window_sizes <- c(30, 60, 90, 180)

for (window_size in window_sizes) {
  cat(sprintf("\nAnalyzing %d-day windows for completeness:\n", window_size))
  
  # Skip if window size is larger than our dataset
  if (window_size > nrow(sentiment_data)) {
    cat("  Window size exceeds dataset length, skipping\n")
    next
  }
  
  # Create a vector to store completeness for each window
  completeness <- numeric(nrow(sentiment_data) - window_size + 1)
  
  # Calculate completeness for each window
  for (i in 1:(nrow(sentiment_data) - window_size + 1)) {
    window_data <- sentiment_data$simple_sentiment_index[i:(i + window_size - 1)]
    completeness[i] <- sum(!is.na(window_data)) / window_size
  }
  
  # Find the most complete window
  best_window_idx <- which.max(completeness)
  best_start_date <- sentiment_data$date[best_window_idx]
  best_end_date <- sentiment_data$date[best_window_idx + window_size - 1]
  best_completeness <- completeness[best_window_idx]
  
  cat(sprintf("  Best %d-day window: %.2f%% complete\n", 
              window_size, best_completeness * 100))
  cat(sprintf("  Start date: %s, End date: %s\n", 
              best_start_date, best_end_date))
  
  # Check recent windows (last 2*window_size days)
  if (nrow(sentiment_data) > 2 * window_size) {
    recent_start_idx <- nrow(sentiment_data) - 2 * window_size
    recent_completeness <- completeness[recent_start_idx:(length(completeness))]
    best_recent_idx <- which.max(recent_completeness) + recent_start_idx - 1
    best_recent_start_date <- sentiment_data$date[best_recent_idx]
    best_recent_end_date <- sentiment_data$date[best_recent_idx + window_size - 1]
    best_recent_completeness <- completeness[best_recent_idx]
    
    cat(sprintf("  Best recent %d-day window: %.2f%% complete\n", 
                window_size, best_recent_completeness * 100))
    cat(sprintf("  Start date: %s, End date: %s\n", 
                best_recent_start_date, best_recent_end_date))
  }
}

# ===== ANALYZE TRADING DAYS ONLY =====

# Create a version of the data with weekends filtered out (for trading days only)
trading_days_data <- sentiment_data %>%
  filter(!is_weekend)

# Calculate statistics for trading days only
trading_days_missing <- trading_days_data %>%
  summarise(
    total_days = n(),
    missing_days = sum(is.na(simple_sentiment_index)),
    missing_percentage = missing_days / total_days * 100
  )

cat("\nTrading days (weekdays) statistics:\n")
cat("Total trading days:", trading_days_missing$total_days, "\n")
cat("Trading days with missing data:", trading_days_missing$missing_days, "\n")
cat("Percentage of trading days with missing data:", 
    round(trading_days_missing$missing_percentage, 2), "%\n")

# Create a heatmap calendar visualization of missing data
# This will help identify patterns like weekends, holidays, etc.
calendar_data <- sentiment_data %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    is_missing = is.na(simple_sentiment_index)
  )

# Create separate dataframes for each year
years <- unique(calendar_data$year)

for (yr in years) {
  year_data <- calendar_data %>% 
    filter(year == yr)
  
  # Create a calendar heatmap for this year
  cal_plot <- ggplot(year_data, aes(x = day, y = reorder(month.name[month], -month), fill = is_missing)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red"),
                      labels = c("FALSE" = "Data Available", "TRUE" = "Missing Data")) +
    labs(
      title = paste("Calendar Heatmap of Missing Data -", yr),
      x = "Day of Month",
      y = "Month",
      fill = "Data Status"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
  
  # Save the plot
  ggsave(file.path(plots_dir, paste0("calendar_heatmap_", yr, ".png")), 
         cal_plot, width = 12, height = 6, dpi = 300)
}

# ===== TEST IMPUTATION METHODS =====

# Function to plot original and imputed data for visual comparison
plot_imputation <- function(original, imputed, method_name, column_name, 
                            start_date = NULL, end_date = NULL) {
  # Combine the data
  plot_data <- data.frame(
    date = original$date,
    original = original[[column_name]],
    imputed = imputed[[column_name]]
  )
  
  # Filter by date range if specified
  if (!is.null(start_date) && !is.null(end_date)) {
    plot_data <- plot_data %>%
      filter(date >= as.Date(start_date) & date <= as.Date(end_date))
  }
  
  # Convert to long format for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c(original, imputed),
                 names_to = "series",
                 values_to = "value")
  
  # Plot
  p <- ggplot(plot_data_long, aes(x = date, y = value, color = series)) +
    geom_line() +
    geom_point(data = plot_data_long %>% filter(series == "imputed" & is.na(plot_data$original)),
               aes(x = date, y = value), color = "blue", size = 2, shape = 4) +
    labs(
      title = paste("Imputation Using", method_name),
      subtitle = paste("Column:", column_name),
      x = "Date",
      y = "Sentiment Value",
      color = "Series"
    ) +
    scale_color_manual(values = c("original" = "black", "imputed" = "red"),
                       labels = c("original" = "Original Data", "imputed" = "Imputed Data")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(p)
}

# Test different imputation methods
cat("\nTesting different imputation methods...\n")

# Column to test imputation on
test_column <- "simple_sentiment_index"

# Method 1: Linear interpolation
data_linear <- sentiment_data
data_linear[[test_column]] <- na_interpolation(sentiment_data[[test_column]], option = "linear")

# Method 2: LOCF (Last Observation Carried Forward)
data_locf <- sentiment_data
data_locf[[test_column]] <- na_locf(sentiment_data[[test_column]])

# Method 3: Kalman smoothing
data_kalman <- sentiment_data
data_kalman[[test_column]] <- na_kalman(sentiment_data[[test_column]])

# Method 4: Moving average
data_ma <- sentiment_data
data_ma[[test_column]] <- na_ma(sentiment_data[[test_column]])

# Generate plots for each method
# Find a period with some missing values to visualize
sample_period_start <- sentiment_data$date[50]
sample_period_end <- sentiment_data$date[150]

linear_plot <- plot_imputation(
  sentiment_data, data_linear, "Linear Interpolation", test_column,
  sample_period_start, sample_period_end
)

locf_plot <- plot_imputation(
  sentiment_data, data_locf, "Last Observation Carried Forward", test_column,
  sample_period_start, sample_period_end
)

kalman_plot <- plot_imputation(
  sentiment_data, data_kalman, "Kalman Smoothing", test_column,
  sample_period_start, sample_period_end
)

ma_plot <- plot_imputation(
  sentiment_data, data_ma, "Moving Average", test_column,
  sample_period_start, sample_period_end
)

# Save plots
ggsave(file.path(plots_dir, "imputation_linear.png"), 
       linear_plot, width = 10, height = 6, dpi = 300)
ggsave(file.path(plots_dir, "imputation_locf.png"), 
       locf_plot, width = 10, height = 6, dpi = 300)
ggsave(file.path(plots_dir, "imputation_kalman.png"), 
       kalman_plot, width = 10, height = 6, dpi = 300)
ggsave(file.path(plots_dir, "imputation_ma.png"), 
       ma_plot, width = 10, height = 6, dpi = 300)

# ===== GENERATE RECOMMENDATIONS =====

# Generate recommendations based on analysis results
cat("\n\n=== RECOMMENDATIONS FOR HANDLING MISSING DATA ===\n")

# Based on weekend analysis
weekend_ratio <- weekend_missing$missing_percentage[weekend_missing$is_weekend] / 
  weekend_missing$missing_percentage[!weekend_missing$is_weekend]

if (weekend_ratio > 1.5) {
  cat("1. Weekend Effect: Missing data is", round(weekend_ratio, 1), 
      "times more common on weekends.\n")
  cat("   Recommendation: Consider filtering out weekends for analysis.\n\n")
} else {
  cat("1. Weekend Effect: There is no strong weekend effect in missing data.\n")
  cat("   Recommendation: Keep all days in the analysis.\n\n")
}

# Based on run length analysis
if (max_consecutive_missing <= 3) {
  cat("2. Missing Runs: The longest run of consecutive missing days is", 
      max_consecutive_missing, "days.\n")
  cat("   Recommendation: Use interpolation methods for filling gaps.\n\n")
} else if (max_consecutive_missing <= 7) {
  cat("2. Missing Runs: There are runs of up to", max_consecutive_missing, 
      "consecutive missing days.\n")
  cat("   Recommendation: Consider a combination of interpolation and forward-filling.\n\n")
} else {
  cat("2. Missing Runs: There are long runs of up to", max_consecutive_missing, 
      "consecutive missing days.\n")
  cat("   Recommendation: Consider using only periods with better data coverage.\n\n")
}

# Based on overall missing percentage
if (missing_percentage < 20) {
  cat("3. Overall Coverage: Missing data is relatively low at", 
      round(missing_percentage, 1), "%.\n")
  cat("   Recommendation: Safe to use imputation methods on the entire dataset.\n\n")
} else if (missing_percentage < 40) {
  cat("3. Overall Coverage: Moderate amount of missing data at", 
      round(missing_percentage, 1), "%.\n")
  cat("   Recommendation: Use imputation but be cautious with analysis.\n\n")
} else {
  cat("3. Overall Coverage: High amount of missing data at", 
      round(missing_percentage, 1), "%.\n")
  cat("   Recommendation: Focus on periods with better coverage.\n\n")
}

# Based on trading days analysis
if (trading_days_missing$missing_percentage < 20) {
  cat("4. Trading Days: Missing data on trading days is", 
      round(trading_days_missing$missing_percentage, 1), "%.\n")
  cat("   Recommendation: Filter to trading days for better coverage.\n\n")
} else {
  cat("4. Trading Days: Missing data is still high on trading days at", 
      round(trading_days_missing$missing_percentage, 1), "%.\n")
  cat("   Recommendation: Further filtering or window selection needed.\n\n")
}

# Final recommendation
cat("FINAL RECOMMENDATIONS:\n\n")

if (trading_days_missing$missing_percentage < 20 && max_consecutive_missing <= 5) {
  cat("Best Approach: Filter to trading days and use Kalman or linear interpolation\n")
  cat("for the remaining missing values. This should provide good quality data\n")
  cat("while maintaining most of the original signal.\n\n")
} else {
  # Find the best window with at least 80% completeness
  best_window_size <- NULL
  best_window_start <- NULL
  
  for (window_size in rev(window_sizes)) {  # Check larger windows first
    if (exists(paste0("best_completeness_", window_size)) && 
        get(paste0("best_completeness_", window_size)) >= 0.8) {
      best_window_size <- window_size
      best_window_start <- get(paste0("best_start_date_", window_size))
      break
    }
  }
  
  if (!is.null(best_window_size)) {
    cat(sprintf("Best Approach: Use a %d-day window starting from %s, which has\n",
                best_window_size, best_window_start))
    cat("at least 80% data completeness. Then apply Kalman imputation for\n")
    cat("the remaining missing values within this window.\n\n")
  } else {
    cat("Best Approach: The dataset has significant missing data challenges.\n")
    cat("Consider using a 7-day rolling average to smooth over gaps, and focus\n")
    cat("on the most recent 60-90 day period which typically has better coverage.\n\n")
  }
}

cat("Alternative Strategies to Consider:\n")
cat("1. Use only periods with consecutive days of data (no gaps)\n")
cat("2. Work with weekly aggregates instead of daily data\n")
cat("3. Include a 'data quality' flag in your models to weight observations\n")
cat("4. For time series analysis, use models that can handle missing values\n")

cat("\nAnalysis complete! Results saved to", output_dir, "\n")