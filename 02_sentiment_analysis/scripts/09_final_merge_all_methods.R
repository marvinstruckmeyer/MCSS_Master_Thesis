# Clean Merge Script: Add new sentiment scores to metadata file
# This script performs a clean merge of sentiment scores with metadata

library(tidyverse)

# File paths - update these to match your file locations
older_file_path <- "02_sentiment_analysis/data/processed/sentiment_with_metadata.csv"
newer_file_path <- "02_sentiment_analysis/data/combined_sentiment_results_with_finbert.csv"
output_file_path <- "02_sentiment_analysis/data/sentiment_with_metadata_final.csv"

cat("Merging sentiment scores with metadata...\n")

# Read files
older_df <- read_csv(older_file_path, show_col_types = FALSE)
newer_df <- read_csv(newer_file_path, show_col_types = FALSE)

# Define sentiment columns to add
sentiment_columns <- c("sentimentr_score", "finbert_score", "ollama_score")

# Clean older file: remove duplicates and existing sentiment columns (they're all NA anyway)
older_clean <- older_df %>%
  mutate(video_id = gsub("^=", "", video_id)) %>%
  filter(!is.na(video_id) & video_id != "") %>%
  group_by(video_id) %>%
  slice(1) %>%  # Remove duplicates by taking first occurrence
  ungroup() %>%
  select(-any_of(sentiment_columns))  # Remove existing NA columns

# Clean newer file
newer_clean <- newer_df %>%
  mutate(video_id = gsub("^=", "", video_id)) %>%
  filter(!is.na(video_id) & video_id != "") %>%
  # No duplicates expected, but just in case
  group_by(video_id) %>%
  summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
            across(where(is.character), ~first(.x, na_rm = TRUE)),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x)))

# Select only sentiment columns from newer file
available_cols <- intersect(sentiment_columns, names(newer_clean))
newer_subset <- newer_clean %>%
  select(video_id, all_of(available_cols))

# Perform clean merge (no overlapping columns now)
final_data <- older_clean %>%
  left_join(newer_subset, by = "video_id")

# Summary
cat("Merge complete:\n")
cat("- Final dataset:", nrow(final_data), "rows,", ncol(final_data), "columns\n")

for (col in available_cols) {
  coverage <- sum(!is.na(final_data[[col]]))
  percentage <- round(coverage / nrow(final_data) * 100, 1)
  cat("- ", col, ": ", coverage, "/", nrow(final_data), " videos (", percentage, "%)\n", sep = "")
}

# Save result
write_csv(final_data, output_file_path)
cat("Saved to:", output_file_path, "\n")

