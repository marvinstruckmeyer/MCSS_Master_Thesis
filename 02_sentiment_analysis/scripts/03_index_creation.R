# Enhanced Daily Sentiment Indices Creation
# This script creates simple and weighted daily sentiment indices
# from sentiment analysis data with metadata for ALL four sentiment methods
# and creates an aggregate index combining all methods

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(bizdays)  # For handling trading days (optional, install if needed)

# Set file paths
input_file <- "02_sentiment_analysis/data/sentiment_with_metadata_final.csv"
output_file <- "02_sentiment_analysis/daily_sentiment_indices_enhanced.csv"
plots_dir <- "02_sentiment_analysis/plots"

# Create plots directory if it doesn't exist
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# Read the data
cat("Reading sentiment data with metadata...\n")
sentiment_data <- read_csv(input_file, show_col_types = FALSE)

# Display basic info
cat("Loaded", nrow(sentiment_data), "rows and", ncol(sentiment_data), "columns\n")

# Clean and prepare the data
cat("Cleaning and preparing data...\n")
clean_data <- sentiment_data %>%
  # Convert date columns to Date type
  mutate(
    best_date = as.Date(best_date),
    # Convert string sentiment scores to numeric for all methods
    dict_sentiment = as.numeric(dict_sentiment),
    sentimentr_score = as.numeric(sentimentr_score),
    finbert_score = as.numeric(finbert_score),
    ollama_score = as.numeric(ollama_score)
  ) %>%
  # Remove rows with missing dates
  filter(!is.na(best_date)) %>%
  # Create log transformed columns for weighting
  mutate(
    log_view_count = log1p(view_count),  # log1p to handle zeros
    log_like_count = log1p(like_count),
    
    # Normalize duration to 0-1 scale
    norm_duration = duration / max(duration, na.rm = TRUE),
    
    # Normalize log view counts to 0-1 scale
    norm_log_view_count = log_view_count / max(log_view_count, na.rm = TRUE),
    
    # Normalize log like counts to 0-1 scale
    norm_log_like_count = log_like_count / max(log_like_count, na.rm = TRUE),
    
    # Create combined weight for weighted averages
    combined_weight = (norm_duration + norm_log_view_count + norm_log_like_count) / 3
  )

# Function to create aggregate sentiment score (average of all four methods)
create_aggregate_sentiment <- function(data) {
  data %>%
    mutate(
      # Aggregate sentiment - average of all available sentiment scores
      aggregate_sentiment = rowMeans(
        cbind(
          dict_sentiment, 
          sentimentr_score, 
          finbert_score, 
          ollama_score
        ), 
        na.rm = TRUE
      )
    )
}

# Add aggregate sentiment to data
clean_data <- create_aggregate_sentiment(clean_data)

# Create daily simple average sentiment for ALL methods
cat("Creating daily simple average sentiment indices for all methods...\n")
daily_simple_avg <- clean_data %>%
  group_by(date = best_date) %>%
  summarize(
    video_count = n(),
    
    # Simple averages for each sentiment method
    dict_sentiment_avg = mean(dict_sentiment, na.rm = TRUE),
    sentimentr_score_avg = mean(sentimentr_score, na.rm = TRUE),
    finbert_score_avg = mean(finbert_score, na.rm = TRUE),
    ollama_score_avg = mean(ollama_score, na.rm = TRUE),
    
    # Aggregate simple average sentiment (combining all methods)
    aggregate_sentiment_avg = mean(aggregate_sentiment, na.rm = TRUE),
    
    # Standard deviations as measures of disagreement/volatility for each method
    dict_sentiment_std = sd(dict_sentiment, na.rm = TRUE),
    sentimentr_score_std = sd(sentimentr_score, na.rm = TRUE),
    finbert_score_std = sd(finbert_score, na.rm = TRUE),
    ollama_score_std = sd(ollama_score, na.rm = TRUE),
    aggregate_sentiment_std = sd(aggregate_sentiment, na.rm = TRUE),
    
    # Total views and likes for the day
    total_views = sum(view_count, na.rm = TRUE),
    total_likes = sum(like_count, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(date)

# Create daily weighted average sentiment for ALL methods
cat("Creating daily weighted average sentiment indices for all methods...\n")
cat("Using combined weighting: (duration + log(views) + log(likes)) / 3\n")
daily_weighted_avg <- clean_data %>%
  group_by(date = best_date) %>%
  summarize(
    video_count = n(),
    
    # Combined-weighted sentiment for each method (duration + views + likes)
    dict_weighted = weighted.mean(dict_sentiment, w = combined_weight, na.rm = TRUE),
    sentimentr_weighted = weighted.mean(sentimentr_score, w = combined_weight, na.rm = TRUE),
    finbert_weighted = weighted.mean(finbert_score, w = combined_weight, na.rm = TRUE),
    ollama_weighted = weighted.mean(ollama_score, w = combined_weight, na.rm = TRUE),
    aggregate_weighted = weighted.mean(aggregate_sentiment, w = combined_weight, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(date)

# Join the simple and weighted indices
cat("Combining simple and weighted indices...\n")
daily_sentiment_indices <- daily_simple_avg %>%
  left_join(daily_weighted_avg %>% select(-video_count), by = "date")

# Fill in missing dates (e.g., for a complete time series with no gaps)
date_range <- seq(min(daily_sentiment_indices$date), 
                  max(daily_sentiment_indices$date), 
                  by = "day")

# Create complete time series with all dates
complete_daily_indices <- tibble(date = date_range) %>%
  left_join(daily_sentiment_indices, by = "date") %>%
  arrange(date)

# Save the indices
cat("Saving enhanced daily sentiment indices to", output_file, "...\n")
write_csv(complete_daily_indices, output_file)

# Create enhanced plots
cat("Creating enhanced visualization plots...\n")

# Plot 1: Simple Average Comparison of All Methods
simple_methods_plot <- complete_daily_indices %>%
  select(date, dict_sentiment_avg, sentimentr_score_avg, 
         finbert_score_avg, ollama_score_avg, aggregate_sentiment_avg) %>%
  pivot_longer(
    cols = -date,
    names_to = "method",
    values_to = "sentiment"
  ) %>%
  mutate(
    method = case_when(
      method == "dict_sentiment_avg" ~ "Dictionary",
      method == "sentimentr_score_avg" ~ "SentimentR",
      method == "finbert_score_avg" ~ "FinBERT",
      method == "ollama_score_avg" ~ "Ollama",
      method == "aggregate_sentiment_avg" ~ "Aggregate (All Methods)",
      TRUE ~ method
    )
  ) %>%
  ggplot(aes(x = date, y = sentiment, color = method)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c(
    "Dictionary" = "blue",
    "SentimentR" = "red", 
    "FinBERT" = "green",
    "Ollama" = "purple",
    "Aggregate (All Methods)" = "black"
  )) +
  labs(
    title = "Daily Simple Average Sentiment - All Methods",
    subtitle = "Comparison of all sentiment analysis approaches",
    x = "Date",
    y = "Sentiment Score",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(plots_dir, "simple_all_methods_comparison.png"), 
       simple_methods_plot, width = 12, height = 6, dpi = 300)

# Plot 2: Weighted vs Simple Sentiment Comparison
weighted_vs_simple_plot <- complete_daily_indices %>%
  select(date, 
         dict_sentiment_avg, dict_weighted,
         sentimentr_score_avg, sentimentr_weighted,
         finbert_score_avg, finbert_weighted,
         ollama_score_avg, ollama_weighted,
         aggregate_sentiment_avg, aggregate_weighted) %>%
  pivot_longer(
    cols = -date,
    names_to = "method",
    values_to = "sentiment"
  ) %>%
  mutate(
    method_type = case_when(
      str_detect(method, "_avg$") ~ "Simple Average",
      str_detect(method, "_weighted$") ~ "Weighted Average",
      TRUE ~ "Other"
    ),
    method_name = case_when(
      str_detect(method, "^dict_") ~ "Dictionary",
      str_detect(method, "^sentimentr_") ~ "SentimentR",
      str_detect(method, "^finbert_") ~ "FinBERT",
      str_detect(method, "^ollama_") ~ "Ollama",
      str_detect(method, "^aggregate_") ~ "Aggregate",
      TRUE ~ method
    )
  ) %>%
  ggplot(aes(x = date, y = sentiment, color = method_name, linetype = method_type)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c(
    "Dictionary" = "blue",
    "SentimentR" = "red", 
    "FinBERT" = "green",
    "Ollama" = "purple",
    "Aggregate" = "black"
  )) +
  scale_linetype_manual(values = c("Simple Average" = "solid", "Weighted Average" = "dashed")) +
  labs(
    title = "Simple vs Weighted Sentiment Indices - All Methods",
    subtitle = "Weighted by combined score (duration + log(views) + log(likes))",
    x = "Date",
    y = "Sentiment Score",
    color = "Method",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(plots_dir, "simple_vs_weighted_all_methods.png"), 
       weighted_vs_simple_plot, width = 12, height = 6, dpi = 300)

# Plot 3: Focus on Aggregate Sentiment - Simple vs Weighted
aggregate_comparison_plot <- complete_daily_indices %>%
  select(date, aggregate_sentiment_avg, aggregate_weighted) %>%
  pivot_longer(
    cols = -date,
    names_to = "type",
    values_to = "sentiment"
  ) %>%
  mutate(
    type = case_when(
      type == "aggregate_sentiment_avg" ~ "Simple Average",
      type == "aggregate_weighted" ~ "Weighted Average",
      TRUE ~ type
    )
  ) %>%
  ggplot(aes(x = date, y = sentiment, color = type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(
    "Simple Average" = "blue",
    "Weighted Average" = "red"
  )) +
  labs(
    title = "Aggregate Sentiment Index - Simple vs Weighted",
    subtitle = "Combined index from all four sentiment methods",
    x = "Date",
    y = "Sentiment Score",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(plots_dir, "aggregate_simple_vs_weighted.png"), 
       aggregate_comparison_plot, width = 12, height = 6, dpi = 300)

# Plot 4: Video Count and Aggregate Sentiment
video_count_aggregate_plot <- ggplot(complete_daily_indices, aes(x = date)) +
  geom_bar(aes(y = video_count), stat = "identity", fill = "lightblue", alpha = 0.7) +
  geom_line(aes(y = aggregate_sentiment_avg * max(complete_daily_indices$video_count, na.rm = TRUE) / 2, 
                color = "Aggregate Sentiment"), size = 1) +
  scale_y_continuous(
    name = "Number of Videos",
    sec.axis = sec_axis(~. * 2 / max(complete_daily_indices$video_count, na.rm = TRUE), 
                        name = "Aggregate Sentiment Score")
  ) +
  scale_color_manual(values = c("Aggregate Sentiment" = "darkred")) +
  labs(
    title = "Daily Video Count and Aggregate Sentiment Index",
    subtitle = "Combined view of data availability and sentiment",
    x = "Date",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(plots_dir, "video_count_and_aggregate_sentiment.png"), 
       video_count_aggregate_plot, width = 12, height = 6, dpi = 300)

# Plot 5: Sentiment Volatility (Standard Deviations)
volatility_plot <- complete_daily_indices %>%
  select(date, dict_sentiment_std, sentimentr_score_std, 
         finbert_score_std, ollama_score_std, aggregate_sentiment_std) %>%
  pivot_longer(
    cols = -date,
    names_to = "method",
    values_to = "volatility"
  ) %>%
  mutate(
    method = case_when(
      method == "dict_sentiment_std" ~ "Dictionary",
      method == "sentimentr_score_std" ~ "SentimentR",
      method == "finbert_score_std" ~ "FinBERT",
      method == "ollama_score_std" ~ "Ollama",
      method == "aggregate_sentiment_std" ~ "Aggregate",
      TRUE ~ method
    )
  ) %>%
  ggplot(aes(x = date, y = volatility, color = method)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c(
    "Dictionary" = "blue",
    "SentimentR" = "red", 
    "FinBERT" = "green",
    "Ollama" = "purple",
    "Aggregate" = "black"
  )) +
  labs(
    title = "Daily Sentiment Volatility (Standard Deviation)",
    subtitle = "Measure of sentiment disagreement within each day",
    x = "Date",
    y = "Standard Deviation",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(plots_dir, "sentiment_volatility_comparison.png"), 
       volatility_plot, width = 12, height = 6, dpi = 300)

# Create rolling average version (7-day window) for aggregate sentiment
cat("Creating rolling average sentiment indices...\n")
rolling_indices <- complete_daily_indices %>%
  arrange(date) %>%
  mutate(
    # Rolling averages for all simple methods
    dict_sentiment_7day = zoo::rollmean(dict_sentiment_avg, 7, fill = NA, align = "right"),
    sentimentr_score_7day = zoo::rollmean(sentimentr_score_avg, 7, fill = NA, align = "right"),
    finbert_score_7day = zoo::rollmean(finbert_score_avg, 7, fill = NA, align = "right"),
    ollama_score_7day = zoo::rollmean(ollama_score_avg, 7, fill = NA, align = "right"),
    aggregate_sentiment_7day = zoo::rollmean(aggregate_sentiment_avg, 7, fill = NA, align = "right"),
    
    # Rolling averages for weighted methods
    dict_weighted_7day = zoo::rollmean(dict_weighted, 7, fill = NA, align = "right"),
    sentimentr_weighted_7day = zoo::rollmean(sentimentr_weighted, 7, fill = NA, align = "right"),
    finbert_weighted_7day = zoo::rollmean(finbert_weighted, 7, fill = NA, align = "right"),
    ollama_weighted_7day = zoo::rollmean(ollama_weighted, 7, fill = NA, align = "right"),
    aggregate_weighted_7day = zoo::rollmean(aggregate_weighted, 7, fill = NA, align = "right")
  )

# Plot 6: Rolling Average Aggregate Sentiment
rolling_aggregate_plot <- ggplot(rolling_indices, aes(x = date)) +
  geom_line(aes(y = aggregate_sentiment_avg, color = "Daily Simple"), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = aggregate_weighted, color = "Daily Weighted"), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = aggregate_sentiment_7day, color = "7-day Simple MA"), size = 1) +
  geom_line(aes(y = aggregate_weighted_7day, color = "7-day Weighted MA"), size = 1) +
  scale_color_manual(values = c(
    "Daily Simple" = "skyblue", 
    "Daily Weighted" = "lightcoral",
    "7-day Simple MA" = "blue", 
    "7-day Weighted MA" = "red"
  )) +
  labs(
    title = "Rolling 7-day Average Aggregate Sentiment Index",
    subtitle = "Smoothed aggregate sentiment combining all four methods",
    x = "Date",
    y = "Sentiment Score",
    color = "Index Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(plots_dir, "rolling_aggregate_sentiment.png"), 
       rolling_aggregate_plot, width = 12, height = 6, dpi = 300)

# Save the rolling average indices
write_csv(rolling_indices, 
          "02_sentiment_analysis/daily_sentiment_indices_enhanced_with_rolling.csv")

cat("Done! Enhanced sentiment indices and visualizations created successfully.\n")
cat("\nOutput files:\n")
cat("1. Enhanced daily sentiment indices:", output_file, "\n")
cat("2. Enhanced indices with rolling averages: 02_sentiment_analysis/daily_sentiment_indices_enhanced_with_rolling.csv\n")
cat("3. Enhanced visualization plots saved to:", plots_dir, "\n")

# Print summary statistics for all methods
cat("\n===== SUMMARY STATISTICS =====\n")

# Remove rows where all sentiment methods are NA
valid_data <- daily_sentiment_indices %>%
  filter(!is.na(dict_sentiment_avg) | !is.na(sentimentr_score_avg) | 
           !is.na(finbert_score_avg) | !is.na(ollama_score_avg))

cat("\nNumber of days with sentiment data:", nrow(valid_data), "\n")
cat("Date range:", min(valid_data$date), "to", max(valid_data$date), "\n")

# Summary for each method
methods <- c("dict_sentiment_avg", "sentimentr_score_avg", "finbert_score_avg", 
             "ollama_score_avg", "aggregate_sentiment_avg")
method_names <- c("Dictionary", "SentimentR", "FinBERT", "Ollama", "Aggregate")

for (i in seq_along(methods)) {
  cat("\n", method_names[i], "Sentiment Summary:\n")
  method_summary <- summary(valid_data[[methods[i]]])
  print(method_summary)
  cat("Non-NA observations:", sum(!is.na(valid_data[[methods[i]]])), "\n")
}

# Correlation matrix
cat("\nCorrelation Matrix (Simple Averages):\n")
cor_data <- valid_data %>%
  select(all_of(methods)) %>%
  cor(use = "pairwise.complete.obs")
print(round(cor_data, 3))

cat("\nScript completed successfully!\n")

