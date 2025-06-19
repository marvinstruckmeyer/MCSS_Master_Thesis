# Bloomberg Transcripts Sentiment Analysis
# This script analyzes sentiment in Bloomberg transcripts using multiple methods
# Currently implements dictionary-based approach, with placeholders for other methods

# Install required packages if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # For data manipulation
  textdata,       # For dictionary-based sentiment
  tidytext,       # For text processing
  sentimentr,     # For method 2 (commented out for now)
  httr,           # For API calls
  jsonlite,       # For API calls
  reticulate,     # For Python integration
  tictoc,         # For timing operations
  furrr,          # For parallel processing
  progressr       # For progress reporting
)

# Configuration options - modify these as needed
config <- list(
  data_dir = "data/combined",                   # Directory containing CSV files
  file_pattern = "bloomberg_transcripts_part_", # Pattern to match CSV files
  output_file = "sentiment_analysis_results.csv", # Output file name
  methods_to_run = c("dictionary"),            # Currently only "dictionary" is active
  # Add "sentimentr", "finbert", "ollama" as needed
  parallel = FALSE,                            # Set to TRUE to enable parallel processing
  max_workers = 4,                             # Number of parallel workers
  save_interim = TRUE,                         # Save interim results after each file
  log_level = "info"                           # "debug", "info", "warning", "error"
)

# Set up logging function
log <- function(message, level = "info") {
  levels <- c("debug", "info", "warning", "error")
  level_num <- match(level, levels)
  config_level_num <- match(config$log_level, levels)
  
  if (level_num >= config_level_num) {
    cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), "[", toupper(level), "] ", message, "\n", sep = "")
  }
}

# Load the Loughran-McDonald sentiment lexicon (financial-specific)
log("Loading Loughran-McDonald sentiment lexicon...")
loughran <- get_sentiments("loughran")

# Function to calculate dictionary-based sentiment
calculate_dict_sentiment <- function(text) {
  # Safety check for empty or NA text
  if (is.na(text) || text == "") {
    return(0)
  }
  
  # Tokenize text
  words <- tibble(text = text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)  # Remove stop words
  
  # Join with sentiment lexicon
  sentiment_words <- words %>%
    inner_join(loughran) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0)
  
  # Calculate score: (positive - negative) / (positive + negative)
  # Default to 0 if no sentiment words found
  if (nrow(sentiment_words) > 0) {
    positive <- if("positive" %in% names(sentiment_words)) sentiment_words$positive else 0
    negative <- if("negative" %in% names(sentiment_words)) sentiment_words$negative else 0
    
    if ((positive + negative) > 0) {
      return((positive - negative) / (positive + negative))
    } else {
      return(0)
    }
  } else {
    return(0)
  }
}

# Function to calculate sentimentr sentiment (for future use)
calculate_sentimentr_sentiment <- function(text) {
  # This function is a placeholder for now
  # When config$methods_to_run includes "sentimentr", implement this
  return(NA)
}

# Function to calculate FinBERT sentiment (for future use)
calculate_finbert_sentiment <- function(text) {
  # This function is a placeholder for now
  # When config$methods_to_run includes "finbert", implement this
  return(NA)
}

# Function to calculate Ollama sentiment (for future use)
calculate_ollama_sentiment <- function(text) {
  # This function is a placeholder for now
  # When config$methods_to_run includes "ollama", implement this
  return(NA)
}

# Get list of files to process
log(paste("Looking for transcript files in", config$data_dir))
transcript_files <- list.files(
  path = config$data_dir, 
  pattern = paste0(config$file_pattern, ".*\\.csv$"), 
  full.names = TRUE
)

log(paste("Found", length(transcript_files), "files to process:"))
for (file in transcript_files) {
  log(paste("  -", basename(file)))
}

# Initialize empty results dataframe
all_results <- data.frame()

# Load existing results if they exist
if (file.exists(config$output_file)) {
  log(paste("Found existing results file:", config$output_file))
  all_results <- read.csv(config$output_file)
  log(paste("Loaded", nrow(all_results), "existing results"))
}

# Function to process a single transcript file
process_file <- function(file_path) {
  log(paste("Processing file:", basename(file_path)), "info")
  
  # Load the data
  transcript_data <- read.csv(file_path, stringsAsFactors = FALSE)
  log(paste("  Loaded", nrow(transcript_data), "rows"), "debug")
  
  # Group by video_id to aggregate transcript segments
  transcript_by_video <- transcript_data %>%
    group_by(video_id) %>%
    summarise(
      full_text = paste(text, collapse = " "),
      title = first(title),
      duration = sum(duration),
      date = first(str_extract(title, "\\| October [0-9]+, 2024")),
      playlist_name = first(playlist_name)
    ) %>%
    ungroup()
  
  log(paste("  Found", nrow(transcript_by_video), "unique videos"), "info")
  
  # Check if any of these videos have already been processed
  if (nrow(all_results) > 0) {
    existing_videos <- intersect(all_results$video_id, transcript_by_video$video_id)
    if (length(existing_videos) > 0) {
      log(paste("  Skipping", length(existing_videos), "already processed videos"), "info")
      transcript_by_video <- transcript_by_video %>% 
        filter(!(video_id %in% existing_videos))
    }
  }
  
  if (nrow(transcript_by_video) == 0) {
    log("  No new videos to process in this file", "info")
    return(data.frame())
  }
  
  # Create a results dataframe for this file
  file_results <- data.frame(
    video_id = transcript_by_video$video_id,
    title = transcript_by_video$title,
    duration = transcript_by_video$duration,
    date = transcript_by_video$date,
    playlist_name = transcript_by_video$playlist_name,
    dict_sentiment = NA,
    sentimentr_score = NA,
    finbert_score = NA,
    ollama_score = NA
  )
  
  # Process with selected methods
  
  # Dictionary-based method
  if ("dictionary" %in% config$methods_to_run) {
    log("  Applying dictionary-based sentiment analysis...", "info")
    tic("Dictionary method")
    
    for (i in 1:nrow(file_results)) {
      if (i %% 10 == 0 || i == nrow(file_results)) {
        log(paste("    Processing video", i, "of", nrow(file_results)), "debug")
      }
      file_results$dict_sentiment[i] <- calculate_dict_sentiment(transcript_by_video$full_text[i])
    }
    
    processing_time <- toc(quiet = TRUE)
    log(paste("  Dictionary method completed in", round(processing_time$toc - processing_time$tic, 2), "seconds"), "info")
  }
  
  # Sentimentr method (placeholder for future)
  if ("sentimentr" %in% config$methods_to_run) {
    log("  Applying sentimentr analysis...", "info")
    tic("Sentimentr method")
    
    for (i in 1:nrow(file_results)) {
      if (i %% 10 == 0 || i == nrow(file_results)) {
        log(paste("    Processing video", i, "of", nrow(file_results)), "debug")
      }
      file_results$sentimentr_score[i] <- calculate_sentimentr_sentiment(transcript_by_video$full_text[i])
    }
    
    processing_time <- toc(quiet = TRUE)
    log(paste("  Sentimentr method completed in", round(processing_time$toc - processing_time$tic, 2), "seconds"), "info")
  }
  
  # FinBERT method (placeholder for future)
  if ("finbert" %in% config$methods_to_run) {
    log("  Applying FinBERT analysis...", "info")
    tic("FinBERT method")
    
    for (i in 1:nrow(file_results)) {
      if (i %% 10 == 0 || i == nrow(file_results)) {
        log(paste("    Processing video", i, "of", nrow(file_results)), "debug")
      }
      file_results$finbert_score[i] <- calculate_finbert_sentiment(transcript_by_video$full_text[i])
    }
    
    processing_time <- toc(quiet = TRUE)
    log(paste("  FinBERT method completed in", round(processing_time$toc - processing_time$tic, 2), "seconds"), "info")
  }
  
  # Ollama method (placeholder for future)
  if ("ollama" %in% config$methods_to_run) {
    log("  Applying Ollama analysis...", "info")
    tic("Ollama method")
    
    for (i in 1:nrow(file_results)) {
      if (i %% 10 == 0 || i == nrow(file_results)) {
        log(paste("    Processing video", i, "of", nrow(file_results)), "debug")
      }
      file_results$ollama_score[i] <- calculate_ollama_sentiment(transcript_by_video$full_text[i])
    }
    
    processing_time <- toc(quiet = TRUE)
    log(paste("  Ollama method completed in", round(processing_time$toc - processing_time$tic, 2), "seconds"), "info")
  }
  
  return(file_results)
}

# Process each file
for (file_path in transcript_files) {
  log(paste("Starting processing for file:", basename(file_path)), "info")
  
  # Process the file
  file_results <- process_file(file_path)
  
  # If results were returned, combine with existing results
  if (nrow(file_results) > 0) {
    all_results <- bind_rows(all_results, file_results)
    log(paste("Added", nrow(file_results), "new results. Total now:", nrow(all_results)), "info")
    
    # Save interim results if configured
    if (config$save_interim) {
      write.csv(all_results, config$output_file, row.names = FALSE)
      log(paste("Saved interim results to", config$output_file), "info")
    }
  }
}

# Save final results
write.csv(all_results, config$output_file, row.names = FALSE)
log(paste("Saved final results to", config$output_file), "info")

# Create summary statistics
log("Generating summary statistics...", "info")

# Calculate overall statistics
dict_stats <- summary(all_results$dict_sentiment)
log("Dictionary sentiment statistics:", "info")
print(dict_stats)

# Create histogram of dictionary sentiment
if (require(ggplot2)) {
  log("Creating visualization...", "info")
  
  # Histogram of dictionary sentiment
  p <- ggplot(all_results, aes(x = dict_sentiment)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    theme_minimal() +
    labs(
      title = "Distribution of Dictionary-Based Sentiment Scores",
      x = "Sentiment Score",
      y = "Count"
    )
  
  # Save the plot
  ggsave("sentiment_distribution.png", p, width = 10, height = 6)
  log("Saved visualization to sentiment_distribution.png", "info")
}

log("Analysis complete!", "info")

