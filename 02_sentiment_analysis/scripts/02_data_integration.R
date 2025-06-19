# Improved script to join sentiment analysis results with video metadata
# This version more thoroughly handles problematic video IDs (those starting with "=")

# Load required libraries
library(tidyverse)
library(stringr) 

# File paths
sentiment_file_path <- "sentiment_analysis/sentiment_analysis_results.csv"
metadata_file_path <- "data/video_IDs/corrected/all_videos_with_corrected_dates.csv"
output_file_path <- "sentiment_analysis/sentiment_analysis_with_metadata.csv"

cat("=== Starting CSV Join Process ===\n")
cat("Generated at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ======== STEP 1: READ AND CLEAN SENTIMENT DATA ========
cat("STEP 1: Reading and cleaning sentiment data...\n")
tryCatch({
  # Read the sentiment data file
  sentiment_df <- read_csv(sentiment_file_path, show_col_types = FALSE)
  cat("Successfully read sentiment data with", nrow(sentiment_df), "rows and", 
      ncol(sentiment_df), "columns\n")
  
  # Check for problematic video IDs
  problematic_count <- sum(str_detect(sentiment_df$video_id, "^="))
  cat("Found", problematic_count, "video IDs starting with '=' in sentiment data\n")
  
  # Fix problematic video IDs
  if (problematic_count > 0) {
    # Print a few examples before fixing
    cat("Examples of problematic IDs before fixing:\n")
    prob_examples <- sentiment_df %>% 
      filter(str_detect(video_id, "^=")) %>% 
      pull(video_id) %>% 
      head(5)
    print(prob_examples)
    
    # Fix the IDs by removing the "=" prefix
    sentiment_df <- sentiment_df %>%
      mutate(video_id = str_replace(video_id, "^=", ""))
    
    # Verify fix worked
    remaining_problematic <- sum(str_detect(sentiment_df$video_id, "^="))
    cat("After fixing:", remaining_problematic, "problematic IDs remain\n")
    
    # If there are still problematic IDs, try a more aggressive approach
    if (remaining_problematic > 0) {
      cat("Using more aggressive fix for remaining problematic IDs...\n")
      
      # Get examples of still problematic IDs
      still_prob_examples <- sentiment_df %>% 
        filter(str_detect(video_id, "^=")) %>% 
        pull(video_id) %>% 
        head(5)
      cat("Examples of still problematic IDs:\n")
      print(still_prob_examples)
      
      # Try more aggressive fix using gsub instead of str_replace
      sentiment_df <- sentiment_df %>%
        mutate(video_id = gsub("^=", "", video_id))
      
      # Final verification
      final_problematic <- sum(str_detect(sentiment_df$video_id, "^="))
      cat("Final check - problematic IDs remaining:", final_problematic, "\n")
      
      if (final_problematic > 0) {
        cat("WARNING: Unable to automatically fix all problematic IDs.\n")
        cat("Consider manual inspection before proceeding.\n")
      } else {
        cat("All problematic IDs fixed successfully!\n")
      }
    } else {
      cat("All problematic IDs fixed successfully!\n")
    }
  }
}, error = function(e) {
  stop(paste("Error reading or processing sentiment file:", e$message))
})

# ======== STEP 2: READ AND CLEAN METADATA ========
cat("\nSTEP 2: Reading and cleaning metadata...\n")
tryCatch({
  # Read the metadata file
  metadata_df <- read_csv(metadata_file_path, show_col_types = FALSE)
  cat("Successfully read metadata with", nrow(metadata_df), "rows and",
      ncol(metadata_df), "columns\n")
  
  # Check required columns
  required_cols <- c("video_id", "best_date", "view_count", "like_count")
  missing_cols <- setdiff(required_cols, colnames(metadata_df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in metadata:", 
               paste(missing_cols, collapse = ", ")))
  } else {
    cat("All required columns present in metadata\n")
  }
  
  # Check for problematic video IDs
  problematic_count <- sum(str_detect(metadata_df$video_id, "^="))
  cat("Found", problematic_count, "video IDs starting with '=' in metadata\n")
  
  # Fix problematic video IDs
  if (problematic_count > 0) {
    # Print a few examples before fixing
    cat("Examples of problematic IDs before fixing:\n")
    prob_examples <- metadata_df %>% 
      filter(str_detect(video_id, "^=")) %>% 
      pull(video_id) %>% 
      head(5)
    print(prob_examples)
    
    # Fix the IDs by removing the "=" prefix
    metadata_df <- metadata_df %>%
      mutate(video_id = str_replace(video_id, "^=", ""))
    
    # Verify fix worked
    remaining_problematic <- sum(str_detect(metadata_df$video_id, "^="))
    cat("After fixing:", remaining_problematic, "problematic IDs remain\n")
    
    # If there are still problematic IDs, try a more aggressive approach
    if (remaining_problematic > 0) {
      cat("Using more aggressive fix for remaining problematic IDs...\n")
      
      # Try more aggressive fix using gsub
      metadata_df <- metadata_df %>%
        mutate(video_id = gsub("^=", "", video_id))
      
      # Final verification
      final_problematic <- sum(str_detect(metadata_df$video_id, "^="))
      cat("Final check - problematic IDs remaining:", final_problematic, "\n")
      
      if (final_problematic > 0) {
        cat("WARNING: Unable to automatically fix all problematic IDs in metadata.\n")
        cat("Consider manual inspection before proceeding.\n")
      } else {
        cat("All problematic IDs fixed successfully!\n")
      }
    } else {
      cat("All problematic IDs fixed successfully!\n")
    }
  }
}, error = function(e) {
  stop(paste("Error reading or processing metadata file:", e$message))
})

# ======== STEP 3: PREPARE FOR JOIN OPERATION ========
cat("\nSTEP 3: Preparing for join operation...\n")

# Compare video IDs between the files
sentiment_ids <- sentiment_df$video_id
metadata_ids <- metadata_df$video_id

common_ids <- intersect(sentiment_ids, metadata_ids)
only_in_sentiment <- setdiff(sentiment_ids, metadata_ids)
only_in_metadata <- setdiff(metadata_ids, sentiment_ids)

cat("Total unique video IDs in sentiment data:", length(unique(sentiment_ids)), "\n")
cat("Total unique video IDs in metadata:", length(unique(metadata_ids)), "\n")
cat("Common video IDs (exact match):", length(common_ids), "\n")
cat("Video IDs only in sentiment data:", length(only_in_sentiment), "\n")
cat("Video IDs only in metadata:", length(only_in_metadata), "\n")

# Calculate expected match rate
expected_match_rate <- round(length(common_ids) / length(sentiment_ids) * 100, 2)
cat("Expected match rate:", expected_match_rate, "%\n")

if (expected_match_rate < 90) {
  cat("\nWARNING: Low expected match rate. This may indicate:\n")
  cat("1. Different sets of videos in the two files\n")
  cat("2. Remaining issues with video ID formatting\n")
  cat("3. Other data inconsistencies\n")
  
  # Offer to continue or exit
  cat("\nDo you want to continue anyway? (Type 'y' for yes)\n")
  # In non-interactive mode, we'll continue automatically
  cat("Continuing automatically as this is a script...\n")
} else {
  cat("Good expected match rate. Proceeding with join operation...\n")
}

# ======== STEP 4: PERFORM JOIN OPERATION ========
cat("\nSTEP 4: Performing join operation...\n")

# Select only needed columns from metadata to keep the output file clean
metadata_subset <- metadata_df %>%
  select(video_id, best_date, view_count, like_count)

# Perform the join using left_join (keeps all sentiment rows, adds metadata where available)
joined_df <- sentiment_df %>%
  left_join(metadata_subset, by = "video_id")

# Check join results
total_rows <- nrow(joined_df)
matched_rows <- sum(!is.na(joined_df$best_date))
match_rate <- round(matched_rows / total_rows * 100, 2)

cat("Join complete!\n")
cat("Total rows in joined data:", total_rows, "\n")
cat("Rows with matched metadata:", matched_rows, "\n")
cat("Actual match rate:", match_rate, "%\n")

# Compare with expected match rate
match_diff <- match_rate - expected_match_rate
if (abs(match_diff) > 1) {
  cat("NOTE: Actual match rate differs from expected by", 
      round(abs(match_diff), 2), "percentage points\n")
  if (match_diff < 0) {
    cat("This suggests some matching issues occurred during the join.\n")
  } else {
    cat("More matches found than expected. This is good!\n")
  }
}

# ======== STEP 5: FINAL VERIFICATION AND SAVE ========
cat("\nSTEP 5: Final verification and saving results...\n")

# Final check for any remaining problematic IDs
problematic_in_joined <- sum(str_detect(joined_df$video_id, "^="))
if (problematic_in_joined > 0) {
  cat("WARNING:", problematic_in_joined, "problematic IDs still found in the joined data\n")
  cat("Applying final fix to ensure clean data...\n")
  
  # Final fix
  joined_df <- joined_df %>%
    mutate(video_id = gsub("^=", "", video_id))
  
  # Verify once more
  final_check <- sum(str_detect(joined_df$video_id, "^="))
  if (final_check > 0) {
    cat("CRITICAL ERROR: Still found", final_check, 
        "problematic IDs. Manual intervention required.\n")
  } else {
    cat("All problematic IDs have been fixed!\n")
  }
} else {
  cat("No problematic IDs found in the joined data. Good!\n")
}

# Save the joined data
cat("Saving joined data to", output_file_path, "...\n")
write_csv(joined_df, output_file_path)
cat("File saved successfully!\n")

# Fixed summary report code to handle list columns properly
# Add this to the end of your script to replace the erroring section

# ======== STEP 6: GENERATE SUMMARY REPORT ========
cat("\nSTEP 6: Generating summary report...\n")

# Display column names with their types more robustly
col_info <- data.frame(
  column = names(joined_df),
  stringsAsFactors = FALSE
)

# Add type information more carefully
col_info$type <- sapply(joined_df, function(x) {
  if(is.list(x)) {
    return("list")
  } else {
    return(class(x)[1])  # Just take the first class if multiple
  }
})

# Add the rest of the information
col_info$non_missing <- sapply(joined_df, function(x) sum(!is.na(x)))
col_info$missing <- sapply(joined_df, function(x) sum(is.na(x)))
col_info$pct_complete <- sapply(joined_df, function(x) round(sum(!is.na(x))/length(x)*100, 1))

cat("Column summary:\n")
print(col_info)

# Save summary to a report file
report_path <- "sentiment_analysis/join_operation_report.txt"
cat("\nSaving detailed report to", report_path, "...\n")

# Create a sink to write to file
sink(report_path)
cat("=== SENTIMENT ANALYSIS JOIN OPERATION REPORT ===\n")
cat("Generated at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("Input files:\n")
cat("- Sentiment data:", sentiment_file_path, "\n")
cat("- Metadata:", metadata_file_path, "\n")
cat("Output file:", output_file_path, "\n\n")

cat("SUMMARY STATISTICS:\n")
cat("- Total rows in sentiment data:", nrow(sentiment_df), "\n")
cat("- Total rows in metadata:", nrow(metadata_df), "\n")
cat("- Total rows in joined data:", total_rows, "\n")
cat("- Rows with successfully matched metadata:", matched_rows, "\n")
cat("- Match rate:", match_rate, "%\n\n")

cat("COLUMN DETAILS:\n")
print(col_info)

cat("\nJOIN OPERATION COMPLETED SUCCESSFULLY\n")
sink() # Close the file

cat("Process complete!\n")
cat("The joined data is now ready for analysis.\n")

# Display sample of the joined data
cat("\nSample of joined data (first 5 rows):\n")
print(head(joined_df, 5))