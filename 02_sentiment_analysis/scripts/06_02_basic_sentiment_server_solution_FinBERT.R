# Direct FinBERT Processing of Transcript Files
# Processes the 5 CSV files directly, creates new results file
# Follows the same robust logic as the Ollama processing script

library(tidyverse)
library(reticulate)
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
  output_file = "finbert_sentiment_results.csv",
  backup_dir = "finbert_backups",
  
  # FinBERT settings
  max_text_length = 512,  # FinBERT token limit
  batch_size = 1,        # Process one at a time for stability
  
  # Processing settings
  chunk_size = 10,
  save_frequency = 5,
  backup_frequency = 50,
  retry_attempts = 3,
  request_delay = 0.5,   # Small delay between requests
  
  # Resume capability
  progress_file = "finbert_direct_progress.rds"
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

# Initialize FinBERT model
initialize_finbert <- function() {
  log("Initializing FinBERT model...")
  
  # Ensure we're using the correct virtual environment
  venv_path <- "~/python_venv_finbert"
  python_venv <- file.path(venv_path, "bin", "python")
  
  if (file.exists(python_venv)) {
    Sys.setenv(RETICULATE_PYTHON = python_venv)
    log(paste("Using Python environment:", python_venv))
  } else {
    stop("FinBERT virtual environment not found. Please run the setup script first.")
  }
  
  # Test Python availability
  if (!py_available()) {
    stop("Python is not available. Please check the virtual environment setup.")
  }
  
  tryCatch({
    py_run_string("
import warnings
warnings.filterwarnings('ignore')

import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification
import gc  # For garbage collection

print('Loading FinBERT model...')
try:
    tokenizer = AutoTokenizer.from_pretrained('ProsusAI/finbert')
    model = AutoModelForSequenceClassification.from_pretrained('ProsusAI/finbert')
    
    # Move to CPU to ensure compatibility
    device = 'cpu'
    model = model.to(device)
    model.eval()  # Set to evaluation mode
    
    print(f'✅ FinBERT model loaded successfully on {device}!')
    finbert_available = True
    
except Exception as e:
    print(f'❌ FinBERT model loading failed: {str(e)}')
    finbert_available = False

def analyze_finbert_sentiment(text):
    '''
    Analyze sentiment using FinBERT model
    Returns a score between -1 (negative) and 1 (positive)
    '''
    if not finbert_available:
        print('FinBERT not available, returning neutral score')
        return 0.0
        
    if not text or len(text.strip()) == 0:
        return 0.0
        
    try:
        # Clean and truncate text
        text = text.strip()
        if len(text) > 2000:  # Pre-truncate very long texts
            text = text[:2000] + '...'
        
        # Tokenize with proper truncation and padding
        inputs = tokenizer(
            text, 
            return_tensors='pt', 
            truncation=True, 
            padding=True, 
            max_length=512,
            add_special_tokens=True
        )
        
        # Move inputs to same device as model
        inputs = {k: v.to(device) for k, v in inputs.items()}
        
        # Get prediction with no gradient computation
        with torch.no_grad():
            outputs = model(**inputs)
            predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)
        
        # Extract scores: [negative, neutral, positive]
        scores = predictions.cpu().numpy()[0]
        negative_score = float(scores[0])
        neutral_score = float(scores[1]) 
        positive_score = float(scores[2])
        
        # Calculate composite score: (positive - negative)
        # This gives us a range from -1 to +1
        sentiment_score = positive_score - negative_score
        
        # Clean up tensors to prevent memory leaks
        del inputs, outputs, predictions
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
        gc.collect()
        
        return float(sentiment_score)
        
    except Exception as e:
        print(f'Error in FinBERT analysis: {str(e)}')
        # Clean up on error
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
        gc.collect()
        return 0.0

# Test the function
test_texts = [
    'The market showed strong positive growth today.',
    'There are concerns about declining profits.',
    'Mixed earnings results with both gains and losses.'
]

print('Testing FinBERT with sample texts:')
for i, test_text in enumerate(test_texts):
    score = analyze_finbert_sentiment(test_text)
    print(f'Test {i+1}: {score:.3f} - \"{test_text[:50]}...\"')

print('FinBERT initialization complete!')
")
    
    # Check if initialization was successful
    finbert_available <- py_eval("finbert_available")
    if (finbert_available) {
      log("✅ FinBERT model initialized successfully!")
      return(TRUE)
    } else {
      log("❌ FinBERT model initialization failed", "error")
      return(FALSE)
    }
    
  }, error = function(e) {
    log(paste("FinBERT initialization error:", e$message), "error")
    return(FALSE)
  })
}

# FinBERT sentiment function
calculate_finbert_sentiment <- function(text, video_id = "unknown") {
  if (is.na(text) || text == "" || nchar(text) == 0) {
    return(list(score = 0, processing_time = 0, success = FALSE))
  }
  
  # Truncate if too long (FinBERT has stricter limits than Ollama)
  if (nchar(text) > config$max_text_length * 4) {  # Rough character to token conversion
    text <- substr(text, 1, config$max_text_length * 4)
    text <- paste0(text, "...")
  }
  
  start_time <- Sys.time()
  
  for (attempt in 1:config$retry_attempts) {
    tryCatch({
      # Clean text for Python (escape quotes and newlines)
      clean_text <- gsub("'", "\\\\'", text)
      clean_text <- gsub("\"", "\\\\\"", clean_text)
      clean_text <- gsub("[\r\n]+", " ", clean_text)
      clean_text <- gsub("\\s+", " ", clean_text)  # Normalize whitespace
      
      # Run FinBERT analysis
      python_code <- paste0("analyze_finbert_sentiment('''", clean_text, "''')")
      sentiment_score <- py_eval(python_code)
      
      processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      # Validate score
      if (is.numeric(sentiment_score) && !is.na(sentiment_score)) {
        # Ensure score is within valid range
        sentiment_score <- max(-1, min(1, sentiment_score))
        
        # Small delay to prevent overwhelming the model
        Sys.sleep(config$request_delay)
        return(list(score = sentiment_score, processing_time = processing_time, success = TRUE))
      }
      
    }, error = function(e) {
      log(paste("Error for", substr(video_id, 1, 8), "attempt", attempt, ":", substr(as.character(e), 1, 100)), "warning")
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
  log("=== STARTING DIRECT FINBERT TRANSCRIPT PROCESSING ===")
  
  # Initialize FinBERT
  if (!initialize_finbert()) {
    stop("Failed to initialize FinBERT model. Cannot proceed.")
  }
  
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
  
  # Remove duplicates and filter for reasonable text length
  all_videos <- all_videos %>%
    distinct(video_id, .keep_all = TRUE) %>%
    filter(word_count >= 50 & word_count <= 50000)  # FinBERT can handle smaller texts too
  
  log(paste("Total unique videos to process:", nrow(all_videos)))
  
  # Check for existing results and resume capability
  existing_results <- data.frame()
  if (file.exists(config$output_file)) {
    existing_results <- read_csv(config$output_file, show_col_types = FALSE)
    log(paste("Found existing results for", nrow(existing_results), "videos"))
    
    # Remove already processed videos
    already_processed <- existing_results$video_id[!is.na(existing_results$finbert_score)]
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
        finbert_score = NA_real_,
        processing_time = NA_real_,
        processed_at = NA_character_
      )
  } else {
    # Merge with existing results
    new_videos <- all_videos %>%
      anti_join(existing_results, by = "video_id") %>%
      select(video_id, title, duration, playlist_name, word_count, source_file) %>%
      mutate(
        finbert_score = NA_real_,
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
      backup_count = 0,
      successful_analyses = 0,
      failed_analyses = 0
    )
  }
  
  # Process videos
  videos_to_process <- all_videos %>%
    filter(is.na(match(video_id, results$video_id[!is.na(results$finbert_score)])))
  
  log(paste("Processing", nrow(videos_to_process), "videos with FinBERT..."))
  
  for (i in 1:nrow(videos_to_process)) {
    video <- videos_to_process[i, ]
    
    log(paste("Processing video", progress_state$videos_processed + 1, 
              "of", progress_state$total_videos, 
              "- ID:", substr(video$video_id, 1, 10),
              "- Words:", video$word_count))
    
    # Calculate sentiment
    result <- calculate_finbert_sentiment(video$full_text, video$video_id)
    
    # Update results
    row_idx <- which(results$video_id == video$video_id)
    if (length(row_idx) > 0) {
      results$finbert_score[row_idx] <- result$score
      results$processing_time[row_idx] <- result$processing_time
      results$processed_at[row_idx] <- as.character(Sys.time())
    }
    
    progress_state$videos_processed <- progress_state$videos_processed + 1
    
    if (result$success) {
      progress_state$successful_analyses <- progress_state$successful_analyses + 1
      log(paste("✅ Success! Score:", round(result$score, 3), 
                "Time:", round(result$processing_time, 1), "sec"))
    } else {
      progress_state$failed_analyses <- progress_state$failed_analyses + 1
      log("❌ Failed!", "warning")
    }
    
    # Memory management: Every 20 videos, run garbage collection
    if (progress_state$videos_processed %% 20 == 0) {
      log("🧹 Running memory cleanup...")
      py_run_string("
import gc
import torch
if torch.cuda.is_available():
    torch.cuda.empty_cache()
gc.collect()
")
      gc()  # R garbage collection too
      log("✅ Memory cleanup complete")
    }
    
    # Cooling break every 50 videos (FinBERT is more intensive than Ollama)
    if (progress_state$videos_processed %% 50 == 0) {
      log(paste("🌡️ Cooling break after", progress_state$videos_processed, "videos - pausing 60 seconds"))
      Sys.sleep(60)  # 60 second cooling break for FinBERT
      log("✅ Cooling break complete, resuming processing...")
    }
    
    # Save progress regularly
    if (progress_state$videos_processed %% config$save_frequency == 0) {
      save_progress(progress_state)
      write_csv(results, config$output_file)
      log(paste("Progress saved. Processed:", progress_state$videos_processed, 
                "of", progress_state$total_videos,
                "| Success rate:", round(progress_state$successful_analyses / progress_state$videos_processed * 100, 1), "%"))
    }
    
    # Create backups
    if (progress_state$videos_processed %% config$backup_frequency == 0) {
      progress_state$backup_count <- progress_state$backup_count + 1
      backup_file <- file.path(config$backup_dir, 
                               paste0("finbert_backup_", progress_state$backup_count, "_", 
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
      
      log(paste("Stats: Success rate:", round(progress_state$successful_analyses / progress_state$videos_processed * 100, 1), "%",
                "| Processing rate:", round(rate, 1), "videos/hour",
                "| Est. time remaining:", round(est_hours, 1), "hours"))
    }
  }
  
  # Final save
  save_progress(progress_state)
  write_csv(results, config$output_file)
  
  # Clean up
  if (file.exists(config$progress_file)) {
    file.remove(config$progress_file)
  }
  
  # Final statistics
  log("=== PROCESSING COMPLETE! ===")
  log(paste("Results saved to:", config$output_file))
  log(paste("Total videos processed:", progress_state$videos_processed))
  log(paste("Successful analyses:", progress_state$successful_analyses))
  log(paste("Failed analyses:", progress_state$failed_analyses))
  log(paste("Success rate:", round(progress_state$successful_analyses / progress_state$videos_processed * 100, 1), "%"))
  
  # Generate summary statistics
  final_results <- results %>% filter(!is.na(finbert_score))
  if (nrow(final_results) > 0) {
    log("\n=== SENTIMENT SCORE STATISTICS ===")
    log(paste("Mean sentiment score:", round(mean(final_results$finbert_score, na.rm = TRUE), 3)))
    log(paste("Median sentiment score:", round(median(final_results$finbert_score, na.rm = TRUE), 3)))
    log(paste("Standard deviation:", round(sd(final_results$finbert_score, na.rm = TRUE), 3)))
    log(paste("Min score:", round(min(final_results$finbert_score, na.rm = TRUE), 3)))
    log(paste("Max score:", round(max(final_results$finbert_score, na.rm = TRUE), 3)))
    
    # Score distribution
    positive_scores <- sum(final_results$finbert_score > 0.1, na.rm = TRUE)
    neutral_scores <- sum(abs(final_results$finbert_score) <= 0.1, na.rm = TRUE)
    negative_scores <- sum(final_results$finbert_score < -0.1, na.rm = TRUE)
    
    log(paste("Score distribution:"))
    log(paste("  Positive (>0.1):", positive_scores, "(", round(positive_scores/nrow(final_results)*100, 1), "%)"))
    log(paste("  Neutral (±0.1):", neutral_scores, "(", round(neutral_scores/nrow(final_results)*100, 1), "%)"))
    log(paste("  Negative (<-0.1):", negative_scores, "(", round(negative_scores/nrow(final_results)*100, 1), "%)"))
  }
  
  return(results)
}

# Run the processing
log("Starting direct FinBERT transcript processing...")
log("This will process ALL videos from your 5 transcript files using FinBERT")
log("You can interrupt safely - it will resume from where it left off")
log("FinBERT processing is more intensive than Ollama, so expect slower processing")

tryCatch({
  final_results <- process_transcripts_direct()
  log("🎉 ALL FINBERT PROCESSING COMPLETE! 🎉")
}, error = function(e) {
  log(paste("Processing stopped:", e$message), "error")
  log("You can resume by running the script again")
  log("Check that your virtual environment is still active")
})
