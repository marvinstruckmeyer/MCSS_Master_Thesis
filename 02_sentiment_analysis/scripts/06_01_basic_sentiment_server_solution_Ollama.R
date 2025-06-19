# Direct Ollama Processing of Transcript Files
# Processes the 5 CSV files directly, creates new results file

library(tidyverse)
library(httr)
library(jsonlite)
library(tictoc)

# Configuration
config <- list(
  data_dir = "data/combined",
  transcript_files = c(
    "bloomberg_transcripts_part_1_of_5.csv",
    "bloomberg_transcripts_part_2_of_5.csv", 
    "bloomberg_transcripts_part_3_of_5.csv",
    "bloomberg_transcripts_part_4_of_5.csv",
    "bloomberg_transcripts_part_5_of_5.csv"
  ),
  output_file = "ollama_sentiment_results.csv",
  backup_dir = "ollama_backups",
  
  # Ollama settings
  # Set up local Ollama server at http://localhost:11434 
  # or use your ngrok tunnel URL
  ollama_url = "http://localhost:11434", # Replace with your Ollama server URL
  ollama_model = "llama3:latest",
  
  # Processing settings
  chunk_size = 10,
  save_frequency = 5,
  backup_frequency = 50,
  max_text_length = 3000,
  retry_attempts = 3,
  request_delay = 2,
  
  # Resume capability
  progress_file = "ollama_direct_progress.rds"
)

# Create directories
dir.create(config$backup_dir, showWarnings = FALSE, recursive = TRUE)

# Logging function
log <- function(message, level = "info") {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cat(timestamp, "[", toupper(level), "] ", message, "\n", sep = "")
  flush.console()
}

# Progress functions
save_progress <- function(progress_state) {
  saveRDS(progress_state, config$progress_file)
}

load_progress <- function() {
  if (file.exists(config$progress_file)) {
    return(readRDS(config$progress_file))
  }
  return(NULL)
}

# Ollama sentiment function
calculate_ollama_sentiment <- function(text, video_id = "unknown") {
  if (is.na(text) || text == "" || nchar(text) == 0) {
    return(list(score = 0, processing_time = 0, success = FALSE))
  }
  
  # Truncate if too long
  if (nchar(text) > config$max_text_length) {
    text <- substr(text, 1, config$max_text_length)
    text <- paste0(text, "...")
  }
  
  prompt <- paste0(
    "Analyze the sentiment of this financial news transcript. ",
    "Rate from -1.0 (very negative) to +1.0 (very positive). ",
    "Consider market implications and tone. Respond with only the number:\n\n",
    text
  )
  
  body <- list(
    model = config$ollama_model,
    prompt = prompt,
    stream = FALSE,
    options = list(temperature = 0.1, num_predict = 10)
  )
  
  start_time <- Sys.time()
  
  for (attempt in 1:config$retry_attempts) {
    tryCatch({
      response <- POST(
        url = paste0(config$ollama_url, "/api/generate"),
        body = body,
        encode = "json",
        timeout(90)
      )
      
      processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      if (status_code(response) == 200) {
        result <- content(response, "parsed")
        response_text <- result$response
        
        number_matches <- str_extract_all(response_text, "-?\\d*\\.?\\d+")[[1]]
        
        if (length(number_matches) > 0) {
          sentiment_score <- as.numeric(number_matches[1])
          sentiment_score <- max(-1, min(1, sentiment_score))
          
          Sys.sleep(config$request_delay)
          return(list(score = sentiment_score, processing_time = processing_time, success = TRUE))
        }
      }
      
    }, error = function(e) {
      log(paste("Error for", substr(video_id, 1, 8), "attempt", attempt, ":", substr(as.character(e), 1, 50)), "warning")
    })
    
    if (attempt < config$retry_attempts) {
      Sys.sleep(config$request_delay * 2)
    }
  }
  
  processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  return(list(score = 0, processing_time = processing_time, success = FALSE))
}

# Main processing function
process_transcripts_direct <- function() {
  log("=== STARTING DIRECT TRANSCRIPT PROCESSING ===")
  
  # Load and aggregate all transcript data
  log("Loading all transcript files...")
  all_videos <- data.frame()
  
  for (file_name in config$transcript_files) {
    file_path <- file.path(config$data_dir, file_name)
    
    if (!file.exists(file_path)) {
      log(paste("WARNING: File not found:", file_name), "warning")
      next
    }
    
    log(paste("Reading:", file_name))
    transcript_data <- read_csv(file_path, show_col_types = FALSE)
    
    # Aggregate by video_id
    video_data <- transcript_data %>%
      group_by(video_id) %>%
      summarise(
        full_text = paste(text, collapse = " "),
        title = first(title),
        duration = sum(duration, na.rm = TRUE),
        playlist_name = first(playlist_name),
        word_count = str_count(full_text, "\\S+"),
        .groups = "drop"
      ) %>%
      mutate(source_file = file_name)
    
    all_videos <- bind_rows(all_videos, video_data)
    log(paste("  Found", nrow(video_data), "unique videos"))
  }
  
  # Remove duplicates (in case same video appears in multiple files)
  all_videos <- all_videos %>%
    distinct(video_id, .keep_all = TRUE) %>%
    # Filter for reasonable text length
    filter(word_count >= 100 & word_count <= 50000)
  
  log(paste("Total unique videos to process:", nrow(all_videos)))
  
  # Check for existing results and resume capability
  existing_results <- data.frame()
  if (file.exists(config$output_file)) {
    existing_results <- read_csv(config$output_file, show_col_types = FALSE)
    log(paste("Found existing results for", nrow(existing_results), "videos"))
    
    # Remove already processed videos
    already_processed <- existing_results$video_id[!is.na(existing_results$ollama_score)]
    all_videos <- all_videos %>%
      filter(!(video_id %in% already_processed))
    
    log(paste("Remaining videos to process:", nrow(all_videos)))
  }
  
  if (nrow(all_videos) == 0) {
    log("No videos need processing!")
    return()
  }
  
  # Initialize results structure
  if (nrow(existing_results) == 0) {
    results <- all_videos %>%
      select(video_id, title, duration, playlist_name, word_count, source_file) %>%
      mutate(
        ollama_score = NA_real_,
        processing_time = NA_real_,
        processed_at = NA_character_
      )
  } else {
    # Merge with existing results
    new_videos <- all_videos %>%
      anti_join(existing_results, by = "video_id") %>%
      select(video_id, title, duration, playlist_name, word_count, source_file) %>%
      mutate(
        ollama_score = NA_real_,
        processing_time = NA_real_,
        processed_at = NA_character_
      )
    
    results <- bind_rows(existing_results, new_videos)
  }
  
  # Load progress
  progress_state <- load_progress()
  if (is.null(progress_state)) {
    progress_state <- list(
      total_videos = nrow(all_videos),
      videos_processed = 0,
      start_time = Sys.time(),
      backup_count = 0
    )
  }
  
  # Test connection
  tryCatch({
    response <- GET(paste0(config$ollama_url, "/api/tags"), timeout(10))
    if (status_code(response) != 200) stop("Connection failed")
    log("✅ Ollama connection verified")
  }, error = function(e) {
    stop("❌ Cannot connect to Ollama. Check ngrok tunnel.")
  })
  
  # Process videos
  videos_to_process <- all_videos %>%
    filter(is.na(match(video_id, results$video_id[!is.na(results$ollama_score)])))
  
  for (i in 1:nrow(videos_to_process)) {
    video <- videos_to_process[i, ]
    
    log(paste("Processing video", progress_state$videos_processed + 1, 
              "of", progress_state$total_videos, 
              "- ID:", substr(video$video_id, 1, 10)))
    
    # Calculate sentiment
    result <- calculate_ollama_sentiment(video$full_text, video$video_id)
    
    # Update results
    row_idx <- which(results$video_id == video$video_id)
    if (length(row_idx) > 0) {
      results$ollama_score[row_idx] <- result$score
      results$processing_time[row_idx] <- result$processing_time
      results$processed_at[row_idx] <- as.character(Sys.time())
    }
    
    progress_state$videos_processed <- progress_state$videos_processed + 1
    
    if (result$success) {
      log(paste("✅ Success! Score:", round(result$score, 3), 
                "Time:", round(result$processing_time, 1), "sec"), "debug")
    } else {
      log("❌ Failed!", "warning")
    }
    
    # ADD THIS COOLING BREAK:
    # Every 10 videos, take a 90-second cooling break
    if (progress_state$videos_processed %% 10 == 0) {
      log(paste("🌡️ Cooling break after", progress_state$videos_processed, "videos - pausing 90 seconds"))
      Sys.sleep(90)  # 90 second cooling break
      log("✅ Cooling break complete, resuming processing...")
    }
    
    # Save progress regularly
    if (progress_state$videos_processed %% config$save_frequency == 0) {
      save_progress(progress_state)
      write_csv(results, config$output_file)
      log(paste("Progress saved. Processed:", progress_state$videos_processed, 
                "of", progress_state$total_videos))
    }
    
    # Create backups
    if (progress_state$videos_processed %% config$backup_frequency == 0) {
      progress_state$backup_count <- progress_state$backup_count + 1
      backup_file <- file.path(config$backup_dir, 
                               paste0("backup_", progress_state$backup_count, "_", 
                                      format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
      write_csv(results, backup_file)
      log(paste("Backup created:", basename(backup_file)))
    }
    
    # Time estimates
    if (progress_state$videos_processed >= 10 && progress_state$videos_processed %% 25 == 0) {
      elapsed_hours <- as.numeric(difftime(Sys.time(), progress_state$start_time, units = "hours"))
      rate <- progress_state$videos_processed / elapsed_hours
      remaining <- progress_state$total_videos - progress_state$videos_processed
      est_hours <- remaining / rate
      
      log(paste("Estimated time remaining:", round(est_hours, 1), "hours"))
    }
  }
  
  # Final save
  save_progress(progress_state)
  write_csv(results, config$output_file)
  
  # Clean up
  if (file.exists(config$progress_file)) {
    file.remove(config$progress_file)
  }
  
  log("=== PROCESSING COMPLETE! ===")
  log(paste("Results saved to:", config$output_file))
  log(paste("Total videos processed:", progress_state$videos_processed))
  
  return(results)
}

# Run the processing
log("Starting direct transcript processing...")
log("This will process ALL videos from your 5 transcript files")
log("You can interrupt safely - it will resume from where it left off")

tryCatch({
  final_results <- process_transcripts_direct()
  log("🎉 ALL PROCESSING COMPLETE! 🎉")
}, error = function(e) {
  log(paste("Processing stopped:", e$message), "error")
  log("You can resume by running the script again")
})

