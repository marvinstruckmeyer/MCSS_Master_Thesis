# Merge of Ollama and Dictionary/SentimentR results

library(tidyverse)
library(readr)

# File paths
ollama_file <- "02_sentiment_analysis/ollama_sentiment_results.csv"
dict_sentimentr_file <- "02_sentiment_analysis/sentiment_analysis_results.csv"
output_file <- "02_sentiment_analysis/combined_sentiment_results.csv"

# Read data
ollama_data <- read_csv(ollama_file, show_col_types = FALSE)
dict_data <- read_csv(dict_sentimentr_file, show_col_types = FALSE)

# Clean and prepare data for merge
dict_clean <- dict_data %>%
  select(-ollama_score, -finbert_score) %>%  # Remove empty columns
  distinct(video_id, .keep_all = TRUE)

ollama_clean <- ollama_data %>%
  select(video_id, ollama_score, processing_time) %>%
  distinct(video_id, .keep_all = TRUE)

# Merge datasets
merged_data <- dict_clean %>%
  left_join(ollama_clean, by = "video_id")

# Save merged dataset
write_csv(merged_data, output_file)


## yet another merge
# Merge FinBERT Sentiment Results with Existing Three-Method Dataset
# Handles missing videos and cleans up the "'" prefix issue

# File paths
existing_three_methods_file <- "02_sentiment_analysis/combined_sentiment_results.csv"
finbert_file <- "02_sentiment_analysis/data/finbert_sentiment_results.csv"  # Adjust path if needed
output_file <- "02_sentiment_analysis/combined_sentiment_results_with_finbert.csv"

cat("=== MERGING FINBERT WITH EXISTING SENTIMENT DATA ===\n")

# Read existing three-method data
cat("Reading existing three-method sentiment data...\n")
existing_data <- read_csv(existing_three_methods_file, show_col_types = FALSE)
cat("Existing data dimensions:", dim(existing_data), "\n")
cat("Existing data columns:", paste(names(existing_data), collapse = ", "), "\n")

# Read FinBERT data with careful handling
cat("\nReading FinBERT sentiment data...\n")
finbert_data <- read_csv(finbert_file, show_col_types = FALSE)
cat("FinBERT data dimensions:", dim(finbert_data), "\n")
cat("FinBERT data columns:", paste(names(finbert_data), collapse = ", "), "\n")

# Display first few rows to understand structure
cat("\nFirst few rows of FinBERT data:\n")
print(head(finbert_data, 3))

# Clean FinBERT data
cat("\nCleaning FinBERT data...\n")

finbert_clean <- finbert_data %>%
  # Remove any rows with missing video_id
  filter(!is.na(video_id) & video_id != "") %>%
  
  # Clean the finbert_score column if it exists
  {
    if("finbert_score" %in% names(.)) {
      cat("Found finbert_score column, cleaning...\n")
      mutate(., 
             # Convert to character first to handle the "'" prefix
             finbert_score = as.character(finbert_score),
             # Remove leading single quotes
             finbert_score = str_remove(finbert_score, "^'"),
             # Convert to numeric
             finbert_score = as.numeric(finbert_score)
      )
    } else {
      cat("No finbert_score column found. Available columns:\n")
      print(names(.))
      .
    }
  } %>%
  
  # Remove any duplicate video_ids (keep first occurrence)
  distinct(video_id, .keep_all = TRUE) %>%
  
  # Select only essential columns for merging
  select(video_id, contains("finbert"))

cat("Cleaned FinBERT data dimensions:", dim(finbert_clean), "\n")

# Check for data cleaning results
if("finbert_score" %in% names(finbert_clean)) {
  cat("FinBERT score summary after cleaning:\n")
  print(summary(finbert_clean$finbert_score))
  cat("Non-NA FinBERT scores:", sum(!is.na(finbert_clean$finbert_score)), "\n")
} else {
  cat("Warning: No finbert_score column found after cleaning!\n")
}

# Compare video IDs between datasets
existing_ids <- unique(existing_data$video_id)
finbert_ids <- unique(finbert_clean$video_id)

common_ids <- intersect(existing_ids, finbert_ids)
only_existing <- setdiff(existing_ids, finbert_ids)
only_finbert <- setdiff(finbert_ids, existing_ids)

cat("\nVideo ID comparison:\n")
cat("Videos in existing data:", length(existing_ids), "\n")
cat("Videos in FinBERT data:", length(finbert_ids), "\n")
cat("Common video IDs:", length(common_ids), "\n")
cat("Only in existing data:", length(only_existing), "\n")
cat("Only in FinBERT data:", length(only_finbert), "\n")

if(length(only_existing) > 0) {
  cat("Sample of videos missing FinBERT scores:", head(only_existing, 5), "\n")
}

if(length(only_finbert) > 0) {
  cat("Sample of videos only in FinBERT data:", head(only_finbert, 5), "\n")
}

# Perform the merge
cat("\nPerforming merge...\n")

# Use left_join to keep all videos from existing data
merged_data <- existing_data %>%
  left_join(finbert_clean, by = "video_id")

cat("Merged data dimensions:", dim(merged_data), "\n")
cat("Merged data columns:", paste(names(merged_data), collapse = ", "), "\n")

# Check merge results
if("finbert_score" %in% names(merged_data)) {
  sentiment_coverage <- merged_data %>%
    summarise(
      total_videos = n(),
      has_dict_sentiment = sum(!is.na(dict_sentiment)),
      has_sentimentr_score = sum(!is.na(sentimentr_score)),
      has_ollama_score = sum(!is.na(ollama_score)),
      has_finbert_score = sum(!is.na(finbert_score)),
      has_all_four = sum(!is.na(dict_sentiment) & !is.na(sentimentr_score) & 
                           !is.na(ollama_score) & !is.na(finbert_score))
    )
  
  cat("\nSentiment method coverage after merge:\n")
  print(sentiment_coverage)
  
  # Calculate coverage percentages
  coverage_pct <- sentiment_coverage %>%
    mutate(
      dict_pct = round(has_dict_sentiment / total_videos * 100, 1),
      sentimentr_pct = round(has_sentimentr_score / total_videos * 100, 1),
      ollama_pct = round(has_ollama_score / total_videos * 100, 1),
      finbert_pct = round(has_finbert_score / total_videos * 100, 1),
      all_four_pct = round(has_all_four / total_videos * 100, 1)
    )
  
  cat("\nCoverage percentages:\n")
  cat("Dictionary:", coverage_pct$dict_pct, "%\n")
  cat("SentimentR:", coverage_pct$sentimentr_pct, "%\n") 
  cat("Ollama:", coverage_pct$ollama_pct, "%\n")
  cat("FinBERT:", coverage_pct$finbert_pct, "%\n")
  cat("All four methods:", coverage_pct$all_four_pct, "%\n")
} else {
  cat("Warning: FinBERT score column not found in merged data!\n")
}

# Save the merged dataset
cat("\nSaving merged dataset...\n")
write_csv(merged_data, output_file)
cat("Merged dataset saved to:", output_file, "\n")

# Create a sample comparison for videos with all four methods
if("finbert_score" %in% names(merged_data)) {
  complete_four_methods <- merged_data %>%
    filter(!is.na(dict_sentiment) & !is.na(sentimentr_score) & 
             !is.na(ollama_score) & !is.na(finbert_score)) %>%
    select(video_id, title, dict_sentiment, sentimentr_score, 
           ollama_score, finbert_score) %>%
    slice_head(n = 10)
  
  if(nrow(complete_four_methods) > 0) {
    cat("\nSample of videos with all four sentiment methods:\n")
    print(complete_four_methods)
  } else {
    cat("\nNo videos found with all four sentiment methods.\n")
  }
  
  # Quick correlation check if we have enough complete data
  complete_data_for_cor <- merged_data %>%
    filter(!is.na(dict_sentiment) & !is.na(sentimentr_score) & 
             !is.na(ollama_score) & !is.na(finbert_score))
  
  if(nrow(complete_data_for_cor) >= 10) {
    cat("\nQuick correlation preview (", nrow(complete_data_for_cor), "complete cases):\n")
    cor_data <- complete_data_for_cor %>%
      select(dict_sentiment, sentimentr_score, ollama_score, finbert_score)
    
    cor_matrix <- cor(cor_data, use = "complete.obs")
    print(round(cor_matrix, 3))
  } else {
    cat("\nNot enough complete data for correlation analysis yet.\n")
  }
}

# Check for any data quality issues
cat("\nData quality check:\n")

# Check for missing values by column
missing_summary <- merged_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
  mutate(missing_percentage = round(missing_count / nrow(merged_data) * 100, 1)) %>%
  arrange(desc(missing_count))

cat("Missing values by column:\n")
print(head(missing_summary, 10))

# Display final summary
cat("\n" + "="*50)
cat("\nMERGE SUMMARY")
cat("\n" + "="*50)
cat("\nOriginal three-method dataset:", nrow(existing_data), "videos\n")
cat("FinBERT dataset:", nrow(finbert_data), "videos\n")
cat("Final merged dataset:", nrow(merged_data), "videos\n")

if("finbert_score" %in% names(merged_data)) {
  finbert_available <- sum(!is.na(merged_data$finbert_score))
  cat("Videos with FinBERT scores:", finbert_available, "out of", nrow(merged_data), 
      "(", round(finbert_available/nrow(merged_data)*100, 1), "%)\n")
}


