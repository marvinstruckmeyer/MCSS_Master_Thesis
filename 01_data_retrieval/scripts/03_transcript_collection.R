# bloomberg_transcript_collector.R
# Script to download high-quality transcripts for Bloomberg videos
# Using the direct method for maximum transcript quality

library(tidyverse)
library(reticulate)
library(glue)
library(lubridate)
library(yaml)
library(future)
library(promises)

# Configuration
CONFIG_FILE <- "config.yaml"

# Load configuration
if (file.exists(CONFIG_FILE)) {
  config <- yaml::read_yaml(CONFIG_FILE)
} else {
  stop("Config file not found. Please run the video collection script first.")
}

# Define constants
API_KEY <- config$api_key
OUTPUT_DIR <- config$output_dir
TRANSCRIPTS_DIR <- file.path(OUTPUT_DIR, "transcripts")
MAX_CONCURRENT <- config$max_concurrent_transcript_downloads

# Create required directories
if (!dir.exists(TRANSCRIPTS_DIR)) {
  dir.create(TRANSCRIPTS_DIR, recursive = TRUE)
}

# Function to load the list of video IDs to process
load_video_ids <- function() {
  # Try to load the comprehensive video ID list
  video_ids_file <- file.path(OUTPUT_DIR, "results", "video_ids_for_transcripts.csv")
  
  if (!file.exists(video_ids_file)) {
    stop("Video IDs file not found. Please run the video collection script first.")
  }
  
  video_data <- read_csv(video_ids_file, show_col_types = FALSE)
  
  # Check if we have the required columns
  required_cols <- c("video_id", "title", "playlist_name")
  if (!all(required_cols %in% colnames(video_data))) {
    warning("Video IDs file is missing some columns. Using minimal format.")
    if (!"title" %in% colnames(video_data)) {
      video_data$title <- NA_character_
    }
    if (!"playlist_name" %in% colnames(video_data)) {
      video_data$playlist_name <- NA_character_
    }
  }
  
  # Make sure we have a transcript_status column
  if (!"transcript_status" %in% colnames(video_data)) {
    video_data$transcript_status <- "pending"
  }
  
  cat("Loaded", nrow(video_data), "video IDs to process\n")
  return(video_data)
}

# Function to check which videos already have transcripts
find_existing_transcripts <- function(video_data) {
  # Get list of existing transcript files
  existing_files <- list.files(TRANSCRIPTS_DIR, pattern = ".*\\.csv$")
  existing_ids <- str_extract(existing_files, "^[\\w-]+")
  
  # Update status in video data
  for (i in 1:nrow(video_data)) {
    if (video_data$video_id[i] %in% existing_ids) {
      video_data$transcript_status[i] <- "complete"
    }
  }
  
  # Count statuses
  status_summary <- video_data %>%
    count(transcript_status) %>%
    arrange(desc(n))
  
  cat("Transcript status summary:\n")
  print(status_summary)
  
  return(video_data)
}

# High-quality transcript downloader using the direct approach
download_transcript <- function(video_id, languages = c('en', 'en-US', 'en-GB')) {
  cat("Downloading transcript for video ID:", video_id, "\n")
  
  # Create languages string for Python
  languages_str <- paste0("['", paste(languages, collapse = "', '"), "']")
  
  # Create Python code to get transcript with error handling and improved processing
  py_code <- glue('
from youtube_transcript_api import YouTubeTranscriptApi
import pandas as pd
import re
import time

try:
    # Try to get transcript with retries
    max_retries = 3
    retry_count = 0
    success = False
    error_message = ""
    
    while not success and retry_count < max_retries:
        try:
            transcript = YouTubeTranscriptApi.get_transcript("{video_id}", languages={languages_str})
            success = True
        except Exception as e:
            error_message = str(e)
            retry_count += 1
            if retry_count < max_retries:
                time.sleep(2)  # Wait before retrying
    
    if success:
        # Process the transcript into a dataframe
        df = pd.DataFrame(transcript)
        
        # Add video_id column
        df["video_id"] = "{video_id}"
        
        # Ensure proper ordering by start time
        df = df.sort_values("start").reset_index(drop=True)
        
        # Now add enhancements for better transcript quality
        
        # 1. Calculate time gaps between segments
        df["next_start"] = df["start"].shift(-1)
        df["time_gap"] = df["next_start"] - (df["start"] + df["duration"])
        df["time_gap"] = df["time_gap"].fillna(0)
        
        # 2. Mark suspected sentence breaks (larger gaps in speech)
        df["sentence_break"] = df["time_gap"] > 0.75
        
        # 3. Identify speaker patterns like ">> NAME:" or "SPEAKER:"
        df["has_speaker"] = df["text"].str.match(r"^(>>|\\[|[A-Z][A-Z]+:)")
        
        # 4. Improve sentence structure by adding punctuation where needed
        for i in range(len(df) - 1):
            # Add period at segment end if there\'s a sentence break and no punctuation
            if df.loc[i, "sentence_break"] and not re.search(r"[.!?]$", df.loc[i, "text"].strip()):
                df.loc[i, "text"] = df.loc[i, "text"].strip() + "."
            
            # Check if next segment starts with capital letter and this one doesn\'t end with punctuation
            elif (i < len(df) - 1 and 
                 re.match(r"^[A-Z]", df.loc[i+1, "text"]) and 
                 not re.search(r"[.!?,:;]$", df.loc[i, "text"].strip()) and
                 not df.loc[i+1, "has_speaker"]):
                df.loc[i, "text"] = df.loc[i, "text"].strip() + "."
        
        # 5. Handle transcript segments that should be merged (very short gap, continuing sentence)
        df["merge_with_next"] = False
        for i in range(len(df) - 1):
            # If gap is very small and the next segment doesn\'t start with capital or speaker tag
            if (df.loc[i, "time_gap"] < 0.3 and 
                not df.loc[i+1, "has_speaker"] and
                not re.match(r"^[A-Z]", df.loc[i+1, "text"]) and
                not re.search(r"[.!?]$", df.loc[i, "text"].strip())):
                df.loc[i, "merge_with_next"] = True
        
        # 6. Clean up unnecessary columns before returning
        df = df.drop(columns=["next_start", "time_gap", "sentence_break", "has_speaker", "merge_with_next"])
        
    else:
        df = pd.DataFrame()
except Exception as e:
    success = False
    error_message = str(e)
    df = pd.DataFrame()
')
  
  # Execute Python code
  result <- py_run_string(py_code, convert = TRUE)
  
  if (!result$success) {
    cat("Failed to download transcript for", video_id, ":", result$error_message, "\n")
    return(NULL)
  }
  
  # Check if we got any data
  transcript_df <- as_tibble(result$df)
  if (nrow(transcript_df) == 0) {
    cat("Transcript contains no data for video ID:", video_id, "\n")
    return(NULL)
  }
  
  cat("Successfully downloaded", nrow(transcript_df), "transcript segments\n")
  return(transcript_df)
}

# Function to enhance a transcript with metadata
enhance_transcript <- function(transcript_df, video_metadata) {
  # If transcript is NULL or empty, return NULL
  if (is.null(transcript_df) || nrow(transcript_df) == 0) {
    return(NULL)
  }
  
  video_id <- transcript_df$video_id[1]
  
  # Find metadata for this video
  video_info <- video_metadata %>% 
    filter(video_id == !!video_id)
  
  # If no metadata found, use placeholder values
  if (nrow(video_info) == 0) {
    enhanced_transcript <- transcript_df %>%
      mutate(
        title = NA_character_,
        playlist_name = NA_character_,
        processed_date = Sys.time()
      )
  } else {
    # Add metadata to transcript
    enhanced_transcript <- transcript_df %>%
      mutate(
        title = video_info$title[1],
        playlist_name = video_info$playlist_name[1],
        processed_date = Sys.time()
      )
  }
  
  return(enhanced_transcript)
}

# Function to save transcript to file
save_transcript <- function(transcript_df, video_id) {
  if (is.null(transcript_df) || nrow(transcript_df) == 0) {
    return(FALSE)
  }
  
  # Create file path
  file_path <- file.path(TRANSCRIPTS_DIR, paste0(video_id, "_transcript.csv"))
  
  # Save to CSV
  write_csv(transcript_df, file_path)
  cat("Saved transcript to", file_path, "\n")
  
  return(TRUE)
}

# Function to process a batch of videos
process_transcript_batch <- function(video_batch, video_metadata) {
  results <- list()
  
  for (i in 1:nrow(video_batch)) {
    video <- video_batch[i, ]
    video_id <- video$video_id
    
    # Skip if already completed
    if (video$transcript_status == "complete") {
      cat("Skipping video ID:", video_id, "- already completed\n")
      results[[video_id]] <- list(
        video_id = video_id,
        status = "skipped",
        message = "Already completed"
      )
      next
    }
    
    # Check if transcript file already exists
    transcript_file <- file.path(TRANSCRIPTS_DIR, paste0(video_id, "_transcript.csv"))
    if (file.exists(transcript_file)) {
      cat("Transcript file already exists for video ID:", video_id, "\n")
      results[[video_id]] <- list(
        video_id = video_id,
        status = "exists",
        message = "File already exists"
      )
      next
    }
    
    # Download transcript
    transcript <- download_transcript(video_id)
    
    if (is.null(transcript)) {
      cat("Failed to download transcript for video ID:", video_id, "\n")
      results[[video_id]] <- list(
        video_id = video_id,
        status = "failed",
        message = "Download failed"
      )
      next
    }
    
    # Enhance transcript with metadata
    enhanced_transcript <- enhance_transcript(transcript, video_metadata)
    
    # Save transcript
    save_result <- save_transcript(enhanced_transcript, video_id)
    
    if (save_result) {
      cat("Successfully processed transcript for video ID:", video_id, "\n")
      results[[video_id]] <- list(
        video_id = video_id,
        status = "complete",
        message = "Success",
        segments = nrow(transcript)
      )
    } else {
      cat("Failed to save transcript for video ID:", video_id, "\n")
      results[[video_id]] <- list(
        video_id = video_id,
        status = "failed",
        message = "Save failed"
      )
    }
    
    # Add a small delay to avoid rate limiting
    Sys.sleep(1)
  }
  
  return(results)
}

# Function to process all transcripts with parallel workers
process_all_transcripts <- function(video_data, batch_size = 50, max_workers = MAX_CONCURRENT) {
  # Filter for pending videos only
  pending_videos <- video_data %>%
    filter(transcript_status == "pending")
  
  if (nrow(pending_videos) == 0) {
    cat("No pending videos to process\n")
    return(video_data)
  }
  
  cat("Processing", nrow(pending_videos), "pending transcripts\n")
  
  # Process in batches to avoid memory issues
  total_batches <- ceiling(nrow(pending_videos) / batch_size)
  cat("Processing in", total_batches, "batches of up to", batch_size, "videos each\n")
  
  all_results <- list()
  
  for (batch_num in 1:total_batches) {
    # Calculate batch range
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, nrow(pending_videos))
    
    cat("\nProcessing batch", batch_num, "of", total_batches, 
        "(videos", start_idx, "to", end_idx, ")\n")
    
    # Get current batch
    current_batch <- pending_videos[start_idx:end_idx, ]
    
    # Process batch
    batch_results <- process_transcript_batch(current_batch, video_data)
    
    # Add to overall results
    all_results <- c(all_results, batch_results)
    
    # Save progress by updating video_data
    for (video_id in names(batch_results)) {
      idx <- which(video_data$video_id == video_id)
      if (length(idx) > 0) {
        video_data$transcript_status[idx] <- batch_results[[video_id]]$status
      }
    }
    
    # Save updated data
    write_csv(video_data, file.path(OUTPUT_DIR, "transcript_status.csv"))
    
    # Pause between batches if there are more to process
    if (batch_num < total_batches) {
      cat("Pausing for 5 seconds before next batch...\n")
      Sys.sleep(5)
    }
  }
  
  # Summarize results
  status_counts <- table(sapply(all_results, function(r) r$status))
  cat("\nTranscript processing complete!\n")
  cat("Results summary:\n")
  print(status_counts)
  
  return(video_data)
}

# Function to combine all transcripts into a single dataset
combine_transcripts <- function() {
  cat("Combining all transcripts into a master dataset...\n")
  
  # List all transcript files
  transcript_files <- list.files(TRANSCRIPTS_DIR, pattern = ".*_transcript\\.csv$", full.names = TRUE)
  
  if (length(transcript_files) == 0) {
    cat("No transcript files found\n")
    return(NULL)
  }
  
  cat("Found", length(transcript_files), "transcript files\n")
  
  # Function to read a single transcript file
  read_transcript <- function(file_path) {
    tryCatch({
      df <- read_csv(file_path, show_col_types = FALSE)
      return(df)
    }, error = function(e) {
      cat("Error reading file", file_path, ":", e$message, "\n")
      return(NULL)
    })
  }
  
  # Read and combine all transcripts
  all_transcripts <- lapply(transcript_files, read_transcript) %>%
    bind_rows()
  
  if (nrow(all_transcripts) == 0) {
    cat("No transcript data found after combining files\n")
    return(NULL)
  }
  
  # Create output directory for combined data
  combined_dir <- file.path(OUTPUT_DIR, "combined")
  dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Handle large datasets by splitting into multiple files if needed
  if (nrow(all_transcripts) > 1000000) {
    cat("Large transcript dataset detected. Splitting into multiple files...\n")
    
    chunk_size <- 1000000  # 1 million rows per file
    chunk_count <- ceiling(nrow(all_transcripts) / chunk_size)
    
    for (i in 1:chunk_count) {
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, nrow(all_transcripts))
      
      chunk <- all_transcripts[start_idx:end_idx, ]
      chunk_file <- file.path(combined_dir, paste0("bloomberg_transcripts_part_", i, "_of_", chunk_count, ".csv"))
      
      write_csv(chunk, chunk_file)
      cat("Saved chunk", i, "with", nrow(chunk), "transcript segments to", chunk_file, "\n")
    }
  } else {
    # Save as a single file if not too large
    combined_file <- file.path(combined_dir, "bloomberg_transcripts_complete.csv")
    write_csv(all_transcripts, combined_file)
    cat("Saved", nrow(all_transcripts), "transcript segments to", combined_file, "\n")
  }
  
  # Create a video-level summary
  video_summary <- all_transcripts %>%
    group_by(video_id, title, playlist_name) %>%
    summarize(
      segments = n(),
      total_duration = sum(duration),
      avg_segment_duration = mean(duration),
      words = sum(str_count(text, "\\S+")),
      avg_words_per_segment = words / segments,
      .groups = "drop"
    ) %>%
    arrange(desc(segments))
  
  # Save video summary
  summary_file <- file.path(combined_dir, "transcript_video_summary.csv")
  write_csv(video_summary, summary_file)
  cat("Saved summary of", nrow(video_summary), "videos to", summary_file, "\n")
  
  # Create playlist-level summary
  playlist_summary <- video_summary %>%
    group_by(playlist_name) %>%
    summarize(
      videos = n(),
      total_segments = sum(segments),
      avg_segments_per_video = mean(segments),
      total_words = sum(words),
      avg_words_per_video = mean(words),
      .groups = "drop"
    ) %>%
    arrange(desc(videos))
  
  # Save playlist summary
  playlist_file <- file.path(combined_dir, "transcript_playlist_summary.csv")
  write_csv(playlist_summary, playlist_file)
  cat("Saved summary of", nrow(playlist_summary), "playlists to", playlist_file, "\n")
  
  return(all_transcripts)
}

# Main function to run the entire transcript collection process
collect_bloomberg_transcripts <- function() {
  cat("Starting Bloomberg Transcript Collection\n")
  
  # 1. Load video IDs
  video_data <- load_video_ids()
  
  # 2. Check for existing transcripts
  video_data <- find_existing_transcripts(video_data)
  
  # 3. Process all transcripts
  video_data <- process_all_transcripts(video_data)
  
  # 4. Combine all transcripts
  combine_transcripts()
  
  cat("\nBloomberg transcript collection complete!\n")
}

# Run the collection
collect_bloomberg_transcripts()