##### continue data collection ########
# bloomberg_complete_collector.R
# Script to collect ALL videos from specified Bloomberg playlists

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(yaml)
library(glue)

# Configuration
CONFIG_FILE <- "config.yaml"

# Load or update config
if (file.exists(CONFIG_FILE)) {
  config <- yaml::read_yaml(CONFIG_FILE)
  
  # Update critical parameters
  config$collection_start_date <- "2000-01-01"  # Way back in time
  config$collection_end_date <- format(Sys.Date() + 1, "%Y-%m-%d")  # Include today
  config$max_pages_per_query <- 100  # Much higher limit
  
  # Write updated config
  write_yaml(config, CONFIG_FILE)
  cat("Updated configuration to collect ALL videos\n")
} else {
  stop("Config file not found. Please run the initial script first.")
}

# Load configuration
API_KEY <- config$api_key
START_DATE <- as.Date(config$collection_start_date)
END_DATE <- as.Date(config$collection_end_date)
OUTPUT_DIR <- config$output_dir
MIN_DURATION_SECONDS <- config$min_duration_seconds
MAX_RESULTS_PER_PAGE <- 50  # Maximum allowed by the API
API_DELAY <- config$api_delay
MAX_PAGES <- config$max_pages_per_query

# Progress tracking file
PROGRESS_FILE <- file.path(OUTPUT_DIR, "collection_progress.rds")

# Function to initialize or load progress tracking
initialize_progress <- function() {
  if (file.exists(PROGRESS_FILE)) {
    cat("Loading existing progress data...\n")
    progress <- readRDS(PROGRESS_FILE)
    
    # Reset playlist status to pending for all non-complete playlists
    for (name in names(progress$playlists)) {
      if (progress$playlists[[name]]$status != "complete") {
        progress$playlists[[name]]$status <- "pending"
        # Reset page token to start over
        progress$playlists[[name]]$last_page_token <- NULL
      }
    }
    
    return(progress)
  } else {
    cat("Initializing new progress tracking...\n")
    
    # Create structure to track progress
    playlists <- list()
    for (playlist in config$playlists) {
      playlists[[playlist$name]] <- list(
        id = playlist$id,  # Use ID from config
        status = "pending",
        videos_collected = 0,
        last_page_token = NULL
      )
    }
    
    progress <- list(
      start_time = Sys.time(),
      api_quota_used = 0,
      playlists = playlists,
      videos = list()
    )
    
    # Save initial progress
    saveRDS(progress, PROGRESS_FILE)
    return(progress)
  }
}

# Function to update and save progress
update_progress <- function(progress) {
  progress$last_updated <- Sys.time()
  saveRDS(progress, PROGRESS_FILE)
  return(progress)
}

# Function to verify a playlist ID is valid
verify_playlist_id <- function(playlist_id, progress) {
  cat("Verifying playlist ID:", playlist_id, "\n")
  
  # API request to get playlist details
  base_url <- "https://www.googleapis.com/youtube/v3/playlists"
  
  params <- list(
    key = API_KEY,
    part = "snippet",
    id = playlist_id
  )
  
  response <- GET(base_url, query = params)
  progress$api_quota_used <- progress$api_quota_used + 1  # List call costs 1 unit
  
  if (status_code(response) != 200) {
    warn_text <- glue("API request failed with status code: {status_code(response)}")
    warning(warn_text)
    cat("Response content:", content(response, "text"), "\n")
    return(FALSE)
  }
  
  data <- content(response, "text") %>% fromJSON(flatten = TRUE)
  
  if (!"items" %in% names(data) || length(data$items) == 0) {
    warning("No playlist found with ID:", playlist_id)
    return(FALSE)
  }
  
  # Show playlist details
  cat("Found playlist:", data$items$snippet.title[1], "\n")
  cat("Channel:", data$items$snippet.channelTitle[1], "\n")
  
  return(TRUE)
}

# Function to get video details (including duration)
get_video_details <- function(video_ids, progress) {
  if (length(video_ids) == 0) return(data.frame())
  
  # YouTube API has a limit of 50 video IDs per request
  batches <- split(video_ids, ceiling(seq_along(video_ids) / 50))
  
  all_details <- data.frame()
  
  for (batch in batches) {
    # API request to get video details
    base_url <- "https://www.googleapis.com/youtube/v3/videos"
    
    params <- list(
      key = API_KEY,
      part = "contentDetails,statistics,snippet",
      id = paste(batch, collapse = ",")
    )
    
    response <- GET(base_url, query = params)
    progress$api_quota_used <- progress$api_quota_used + 1  # Video details costs 1 unit per video
    
    if (status_code(response) != 200) {
      warning("API request failed with status code:", status_code(response))
      cat("Response content:", content(response, "text"), "\n")
      next
    }
    
    data <- content(response, "text") %>% fromJSON(flatten = TRUE)
    
    if (!"items" %in% names(data) || length(data$items) == 0) {
      warning("No video details found for batch")
      next
    }
    
    # Extract video information
    details <- tibble(
      video_id = data$items$id,
      duration = data$items$contentDetails.duration,
      definition = data$items$contentDetails.definition,
      caption = data$items$contentDetails.caption,
      view_count = as.numeric(data$items$statistics.viewCount),
      like_count = as.numeric(data$items$statistics.likeCount),
      title = data$items$snippet.title,
      published_at = data$items$snippet.publishedAt,
      channel_title = data$items$snippet.channelTitle
    )
    
    # Convert duration from ISO 8601 to seconds
    details$duration_seconds <- sapply(details$duration, function(d) {
      # Extract hours, minutes, seconds
      h <- as.numeric(str_extract(d, "(?<=PT)[0-9]+(?=H)") %||% "0")
      m <- as.numeric(str_extract(d, "(?<=H|PT)[0-9]+(?=M)") %||% "0")
      s <- as.numeric(str_extract(d, "(?<=M|PT)[0-9]+(?=S)") %||% "0")
      return(h * 3600 + m * 60 + s)
    })
    
    all_details <- bind_rows(all_details, details)
    
    # Add a small delay to avoid rate limiting
    Sys.sleep(API_DELAY)
  }
  
  return(all_details)
}

# Function to load all existing video IDs from your database
load_existing_video_ids <- function() {
  cat("Loading existing video IDs from database...\n")
  
  # Directory containing playlist CSV files
  playlists_dir <- file.path(OUTPUT_DIR, "playlists")
  
  # Check if directory exists
  if (!dir.exists(playlists_dir)) {
    cat("No existing playlist directory found. Starting fresh.\n")
    return(character(0))
  }
  
  # Get all CSV files
  csv_files <- list.files(playlists_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Empty vector to collect all IDs
  all_ids <- character(0)
  
  # Read each file and extract video IDs
  for (file in csv_files) {
    tryCatch({
      # Only read the video_id column to save memory
      video_data <- read_csv(file, col_select = "video_id", show_col_types = FALSE)
      ids <- video_data$video_id
      
      # Add to our collection
      all_ids <- c(all_ids, ids)
      
      cat("Loaded", length(ids), "video IDs from", basename(file), "\n")
    }, error = function(e) {
      warning("Error reading file ", file, ": ", e$message)
    })
  }
  
  # Remove any duplicates
  unique_ids <- unique(all_ids)
  
  cat("Loaded", length(unique_ids), "unique video IDs from", length(csv_files), "files\n")
  return(unique_ids)
}

# Modified function to get all videos from a playlist, both existing and new
get_playlist_videos_complete <- function(playlist_id, playlist_name, progress, existing_video_ids) {
  cat("\nGetting complete video list from playlist:", playlist_name, "(ID:", playlist_id, ")\n")
  
  # Verify playlist ID
  if (!verify_playlist_id(playlist_id, progress)) {
    warning("Invalid playlist ID:", playlist_id)
    return(list(
      all_videos = data.frame(),
      new_videos = data.frame(),
      stats = list(
        total = 0,
        new = 0,
        existing = 0,
        min_position = NA,
        max_position = NA,
        complete = FALSE
      )
    ))
  }
  
  # API request to get playlist items
  base_url <- "https://www.googleapis.com/youtube/v3/playlistItems"
  
  all_videos <- data.frame()  # All videos (new and existing)
  new_videos_df <- data.frame()  # Just new videos
  
  # Start with no token (fresh request)
  next_page_token <- NULL
  page_count <- 0
  
  # Statistics tracking
  total_videos <- 0
  new_videos_found <- 0
  existing_videos <- 0
  max_position_seen <- -1
  min_position_seen <- Inf
  
  repeat {
    page_count <- page_count + 1
    if (page_count > MAX_PAGES) {
      cat("Reached maximum page limit of", MAX_PAGES, "pages\n")
      break
    }
    
    # Set up parameters
    params <- list(
      key = API_KEY,
      part = "snippet,contentDetails",
      playlistId = playlist_id,
      maxResults = MAX_RESULTS_PER_PAGE  # Maximum allowed by API
    )
    
    # Add page token if we have one for pagination
    if (!is.null(next_page_token)) {
      params$pageToken <- next_page_token
    }
    
    # Make the request with error handling
    response <- tryCatch({
      GET(base_url, query = params)
    }, error = function(e) {
      warning("API request error:", e$message)
      return(NULL)
    })
    
    progress$api_quota_used <- progress$api_quota_used + 1  # List call costs 1 unit
    update_progress(progress)
    
    if (is.null(response) || status_code(response) != 200) {
      if (!is.null(response)) {
        warning("API request failed with status code:", status_code(response))
        cat("Response content:", content(response, "text"), "\n")
      }
      
      cat("Error fetching playlist data. Saving progress and pausing.\n")
      break
    }
    
    data <- content(response, "text") %>% fromJSON(flatten = TRUE)
    
    if (!"items" %in% names(data) || length(data$items) == 0) {
      cat("No more videos found in playlist page\n")
      break
    }
    
    # Extract video information
    videos <- tibble(
      video_id = data$items$contentDetails.videoId,
      title = data$items$snippet.title,
      description = data$items$snippet.description,
      published_at = data$items$snippet.publishedAt,
      channel_title = data$items$snippet.channelTitle,
      playlist_id = playlist_id,
      playlist_name = playlist_name,
      position = as.numeric(data$items$snippet.position),
      source = "playlist",
      page = page_count  # Add page info for debugging
    )
    
    # Convert published_at to datetime
    videos$published_at <- ymd_hms(videos$published_at)
    
    # Track position statistics
    if (nrow(videos) > 0) {
      page_max_position <- max(videos$position, na.rm = TRUE)
      page_min_position <- min(videos$position, na.rm = TRUE)
      max_position_seen <- max(max_position_seen, page_max_position)
      min_position_seen <- min(min_position_seen, page_min_position)
    }
    
    # Split into new and existing videos
    new_videos <- videos %>%
      filter(!(video_id %in% existing_video_ids))
    
    existing_vids <- videos %>%
      filter(video_id %in% existing_video_ids)
    
    # Get video details only for new videos (saves API quota)
    if (nrow(new_videos) > 0) {
      # Get chunks of 50 videos at a time to avoid long API calls
      chunk_size <- 50
      chunked_new_videos <- split(new_videos, ceiling(seq_len(nrow(new_videos)) / chunk_size))
      
      for (chunk in chunked_new_videos) {
        video_details <- get_video_details(chunk$video_id, progress)
        
        if (nrow(video_details) > 0) {
          chunk_with_details <- chunk %>%
            left_join(
              video_details %>% select(video_id, duration_seconds, view_count, like_count),
              by = "video_id"
            )
        } else {
          chunk$duration_seconds <- NA
          chunk$view_count <- NA
          chunk$like_count <- NA
          chunk_with_details <- chunk
        }
        
        # Add to our collection of new videos
        new_videos_df <- bind_rows(new_videos_df, chunk_with_details)
        
        # Save progress periodically
        if (nrow(new_videos_df) %% 100 == 0) {
          update_progress(progress)
        }
      }
    }
    
    # Add all videos to our complete collection
    all_videos <- bind_rows(all_videos, videos)
    
    # Update counters
    page_new_count <- nrow(new_videos)
    page_existing_count <- nrow(existing_vids)
    
    new_videos_found <- new_videos_found + page_new_count
    existing_videos <- existing_videos + page_existing_count
    total_videos <- total_videos + nrow(videos)
    
    # Report progress
    cat(sprintf("Page %d (positions %d-%d): Found %d new videos, %d already in database - Total new: %d\n", 
                page_count, page_min_position, page_max_position, 
                page_new_count, page_existing_count, new_videos_found))
    
    # Get next page token for pagination
    next_page_token <- if ("nextPageToken" %in% names(data)) data$nextPageToken else NULL
    
    # If no more pages, we're done
    if (is.null(next_page_token)) {
      cat("Reached end of playlist\n")
      break
    }
    
    # Add delay between requests
    Sys.sleep(API_DELAY)
    
    # Check API quota
    if (progress$api_quota_used > 8000) {
      warning("Approaching API quota limit. Pausing collection.")
      cat("Will resume later - at page:", page_count, "\n")
      break
    }
  }
  
  # Final statistics
  cat(sprintf("\nCompleted playlist: %s\n", playlist_name))
  cat(sprintf("Total videos in playlist: %d\n", total_videos))
  cat(sprintf("Position range seen: %d to %d\n", min_position_seen, max_position_seen))
  cat(sprintf("Already in database: %d (%.1f%%)\n", existing_videos, 100 * existing_videos / max(total_videos, 1)))
  cat(sprintf("Newly collected: %d (%.1f%%)\n", new_videos_found, 100 * new_videos_found / max(total_videos, 1)))
  
  # Check if we appear to have collected the full playlist
  if (min_position_seen == 0 && is.null(next_page_token)) {
    cat("COMPLETE PLAYLIST COLLECTION: Got all videos from position 0 to", max_position_seen, "\n")
    is_complete <- TRUE
  } else {
    cat("INCOMPLETE COLLECTION: Missing some videos or reached API limits\n")
    is_complete <- FALSE
  }
  
  # Create a list for return values
  result <- list(
    all_videos = all_videos,
    new_videos = new_videos_df,
    stats = list(
      total = total_videos,
      new = new_videos_found,
      existing = existing_videos,
      min_position = min_position_seen,
      max_position = max_position_seen,
      complete = is_complete
    )
  )
  
  return(result)
}

# Function to filter for full episodes based on duration
filter_full_episodes <- function(videos) {
  if (nrow(videos) == 0) {
    return(videos)
  }
  
  if (!"duration_seconds" %in% names(videos) || all(is.na(videos$duration_seconds))) {
    warning("No duration information available for filtering")
    return(videos)
  }
  
  full_episodes <- videos %>%
    filter(!is.na(duration_seconds) & duration_seconds >= MIN_DURATION_SECONDS)
  
  cat("Filtered from", nrow(videos), "to", nrow(full_episodes), 
      "videos with duration ≥", MIN_DURATION_SECONDS, "seconds\n")
  
  return(full_episodes)
}

# Function to save all new results
save_new_results <- function(all_new_videos, existing_video_ids) {
  if (nrow(all_new_videos) == 0) {
    cat("No new videos to save\n")
    return(NULL)
  }
  
  # Remove any duplicates
  all_new_videos <- all_new_videos %>% 
    distinct(video_id, .keep_all = TRUE) %>%
    arrange(desc(published_at))
  
  # Create output directory for results
  results_dir <- file.path(OUTPUT_DIR, "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save all new videos
  new_videos_file <- file.path(results_dir, paste0("new_videos_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write_csv(all_new_videos, new_videos_file)
  cat("Saved", nrow(all_new_videos), "new videos to", new_videos_file, "\n")
  
  # Filter for full episodes
  full_episodes <- filter_full_episodes(all_new_videos)
  
  # Save new full episodes
  full_episodes_file <- file.path(results_dir, paste0("new_full_episodes_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write_csv(full_episodes, full_episodes_file)
  cat("Saved", nrow(full_episodes), "new full episodes to", full_episodes_file, "\n")
  
  # Also update the master files
  update_master_files(all_new_videos, existing_video_ids)
  
  # Create a simple new video IDs file for the transcript processor
  video_ids_only <- full_episodes %>%
    select(video_id, title, published_at, duration_seconds, playlist_name) %>%
    arrange(desc(published_at))
  
  ids_file <- file.path(results_dir, paste0("new_video_ids_for_transcripts_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write_csv(video_ids_only, ids_file)
  cat("Saved", nrow(video_ids_only), "new video IDs for transcript processing to", ids_file, "\n")
  
  # Also save as text file with just IDs (one per line)
  writeLines(video_ids_only$video_id, file.path(results_dir, paste0("new_video_ids_", format(Sys.Date(), "%Y%m%d"), ".txt")))
  
  # Generate summary statistics
  progress <- readRDS(PROGRESS_FILE)
  
  summary_data <- list(
    collection_date = Sys.Date(),
    total_new_videos = nrow(all_new_videos),
    new_full_episodes = nrow(full_episodes),
    api_quota_used = progress$api_quota_used,
    videos_by_playlist = all_new_videos %>% 
      group_by(playlist_name) %>% 
      summarize(count = n(), .groups = "drop"),
    full_episodes_by_playlist = full_episodes %>% 
      group_by(playlist_name) %>% 
      summarize(count = n(), .groups = "drop")
  )
  
  # Create a human-readable summary
  summary_text <- c(
    "=== NEW BLOOMBERG VIDEOS COLLECTION SUMMARY ===",
    paste("Collection Date:", Sys.Date()),
    paste("Total New Videos Collected:", nrow(all_new_videos)),
    paste("New Full Episodes (≥", MIN_DURATION_SECONDS/60, "minutes):", nrow(full_episodes)),
    paste("API Quota Used:", progress$api_quota_used),
    "",
    "New Videos by Playlist:",
    paste("  -", summary_data$videos_by_playlist$playlist_name, ":", summary_data$videos_by_playlist$count),
    "",
    "New Full Episodes by Playlist:",
    paste("  -", summary_data$full_episodes_by_playlist$playlist_name, ":", summary_data$full_episodes_by_playlist$count)
  )
  
  writeLines(summary_text, file.path(results_dir, paste0("new_videos_summary_", format(Sys.Date(), "%Y%m%d"), ".txt")))
  cat("\nCollection summary saved to", file.path(results_dir, paste0("new_videos_summary_", format(Sys.Date(), "%Y%m%d"), ".txt")), "\n")
}

# Function to update master files with new videos
update_master_files <- function(new_videos, existing_video_ids) {
  results_dir <- file.path(OUTPUT_DIR, "results")
  
  # Master all videos file
  master_file <- file.path(results_dir, "all_videos.csv")
  if (file.exists(master_file)) {
    # Load existing master file
    all_videos <- read_csv(master_file, show_col_types = FALSE)
    
    # Add new videos and remove duplicates
    combined_videos <- bind_rows(all_videos, new_videos) %>%
      distinct(video_id, .keep_all = TRUE) %>%
      arrange(desc(published_at))
    
    # Save updated master file
    write_csv(combined_videos, master_file)
    cat("Updated master file with", nrow(new_videos), "new videos. Now has", nrow(combined_videos), "total videos.\n")
  } else {
    # Create new master file
    write_csv(new_videos, master_file)
    cat("Created new master file with", nrow(new_videos), "videos.\n")
  }
  
  # Master full episodes file
  full_episodes_file <- file.path(results_dir, "full_episodes.csv")
  if (file.exists(full_episodes_file)) {
    # Load existing full episodes
    all_full_episodes <- read_csv(full_episodes_file, show_col_types = FALSE)
    
    # Filter new videos for full episodes
    new_full_episodes <- filter_full_episodes(new_videos)
    
    # Add new full episodes and remove duplicates
    combined_full_episodes <- bind_rows(all_full_episodes, new_full_episodes) %>%
      distinct(video_id, .keep_all = TRUE) %>%
      arrange(desc(published_at))
    
    # Save updated full episodes file
    write_csv(combined_full_episodes, full_episodes_file)
    cat("Updated full episodes file with", nrow(new_full_episodes), "new episodes. Now has", 
        nrow(combined_full_episodes), "total full episodes.\n")
  } else {
    # Create new full episodes file
    new_full_episodes <- filter_full_episodes(new_videos)
    write_csv(new_full_episodes, full_episodes_file)
    cat("Created new full episodes file with", nrow(new_full_episodes), "episodes.\n")
  }
  
  # Update video IDs file for transcript processing
  ids_file <- file.path(results_dir, "video_ids_for_transcripts.csv")
  if (file.exists(ids_file)) {
    # Load existing IDs
    all_ids <- read_csv(ids_file, show_col_types = FALSE)
    
    # Filter new videos for full episodes
    new_full_episodes <- filter_full_episodes(new_videos)
    
    # Create ID list for new full episodes
    new_id_list <- new_full_episodes %>%
      select(video_id, title, published_at, duration_seconds, playlist_name)
    
    # Combine and remove duplicates
    combined_ids <- bind_rows(all_ids, new_id_list) %>%
      distinct(video_id, .keep_all = TRUE) %>%
      arrange(desc(published_at))
    
    # Save updated IDs file
    write_csv(combined_ids, ids_file)
    cat("Updated transcript ID list with", nrow(new_id_list), "new videos. Now has", 
        nrow(combined_ids), "total videos for transcript processing.\n")
  } else {
    # Create new IDs file
    new_full_episodes <- filter_full_episodes(new_videos)
    new_id_list <- new_full_episodes %>%
      select(video_id, title, published_at, duration_seconds, playlist_name)
    write_csv(new_id_list, ids_file)
    cat("Created new transcript ID list with", nrow(new_id_list), "videos.\n")
  }
  
  # Also update the text file with just IDs
  txt_ids_file <- file.path(results_dir, "video_ids.txt")
  if (file.exists(ids_file)) {
    video_ids <- read_csv(ids_file, show_col_types = FALSE)$video_id
    writeLines(video_ids, txt_ids_file)
    cat("Updated text ID list with", length(video_ids), "total video IDs.\n")
  }
}

# Main function to collect all Bloomberg videos
collect_all_bloomberg_videos <- function() {
  cat("Starting Complete Bloomberg Playlist Video Collection\n")
  
  # Check API key
  if (is.null(API_KEY) || API_KEY == "" || API_KEY == "YOUR_API_KEY") {
    stop("Please set your YouTube API key in the config file")
  }
  cat("Using API key:", substr(API_KEY, 1, 8), "...\n")
  
  # Initialize or load progress
  progress <- initialize_progress()
  
  # Load existing video IDs
  existing_video_ids <- load_existing_video_ids()
  cat("Found", length(existing_video_ids), "existing video IDs in database\n")
  
  # Get all playlist configs from the configuration
  playlists <- config$playlists
  
  # Convert to a data frame for easier handling
  playlist_df <- tibble(
    name = sapply(playlists, function(p) p$name),
    id = sapply(playlists, function(p) if(!is.null(p$id)) p$id else NA_character_),
    priority = sapply(playlists, function(p) if(!is.null(p$priority)) p$priority else 3)
  )
  
  # Sort by priority
  playlist_df <- playlist_df %>% 
    arrange(priority) %>%
    filter(!is.na(id))  # Skip playlists without IDs
  
  if (nrow(playlist_df) == 0) {
    cat("No playlists with valid IDs found. Please update your config.\n")
    return(NULL)
  }
  
  cat("Processing", nrow(playlist_df), "playlists with IDs\n")
  print(playlist_df)
  
  # Daily quota tracking
  quota_start <- progress$api_quota_used
  collection_start_time <- Sys.time()
  
  all_new_videos <- data.frame()
  collection_stats <- list()
  
  # Process each playlist
  for (i in 1:nrow(playlist_df)) {
    playlist_name <- playlist_df$name[i]
    playlist_id <- playlist_df$id[i]
    
    # Get all videos from playlist (both new and existing)
    result <- get_playlist_videos_complete(
      playlist_id, 
      playlist_name, 
      progress, 
      existing_video_ids
    )
    
    # Save collection stats
    collection_stats[[playlist_name]] <- result$stats
    
    # Save all videos to a complete playlist file
    if (nrow(result$all_videos) > 0) {
      playlists_dir <- file.path(OUTPUT_DIR, "playlists")
      dir.create(playlists_dir, recursive = TRUE, showWarnings = FALSE)
      all_videos_file <- file.path(playlists_dir, paste0(gsub("[^a-zA-Z0-9]", "_", playlist_name), "_complete.csv"))
      write_csv(result$all_videos, all_videos_file)
      cat("Saved complete playlist with", nrow(result$all_videos), "videos to", all_videos_file, "\n")
    }
    
    # Save just the new videos if any
    if (nrow(result$new_videos) > 0) {
      new_videos_dir <- file.path(OUTPUT_DIR, "playlists", "new_videos")
      dir.create(new_videos_dir, recursive = TRUE, showWarnings = FALSE)
      new_videos_file <- file.path(new_videos_dir, paste0(gsub("[^a-zA-Z0-9]", "_", playlist_name), "_new.csv"))
      write_csv(result$new_videos, new_videos_file)
      cat("Saved", nrow(result$new_videos), "NEW videos from playlist to", new_videos_file, "\n")
      
      # Also append to the existing playlist file (if not already saved as complete)
      existing_playlist_file <- file.path(OUTPUT_DIR, "playlists", paste0(gsub("[^a-zA-Z0-9]", "_", playlist_name), ".csv"))
      if (file.exists(existing_playlist_file) && !file.exists(all_videos_file)) {
        existing_videos <- read_csv(existing_playlist_file, show_col_types = FALSE)
        combined_videos <- bind_rows(existing_videos, result$new_videos) %>% 
          distinct(video_id, .keep_all = TRUE)
        
        write_csv(combined_videos, existing_playlist_file)
        cat("Updated existing playlist file with new videos. Now has", nrow(combined_videos), "videos total.\n")
      } else if (!file.exists(existing_playlist_file)) {
        # If no existing file, create one
        write_csv(result$new_videos, existing_playlist_file)
      }
      
      # Add to collection of all new videos
      all_new_videos <- bind_rows(all_new_videos, result$new_videos)
    } else {
      cat("No new videos found for playlist:", playlist_name, "\n")
    }
    
    # Check if we should pause due to API quota limits
    if (progress$api_quota_used - quota_start > 8000) {
      cat("\n===== DAILY QUOTA NEARLY EXHAUSTED =====\n")
      cat("Used", progress$api_quota_used - quota_start, "units today\n")
      cat("Saving progress and stopping until tomorrow\n")
      
      # Save what we have so far
      save_new_results(all_new_videos, existing_video_ids)
      
      # Calculate when we can resume
      elapsed <- difftime(Sys.time(), collection_start_time, units = "hours")
      if (elapsed < 24) {
        wait_hours <- ceiling(24 - as.numeric(elapsed))
        cat("Please wait", wait_hours, "hours before resuming\n")
      }
      
      break
    }
  }
  
  # Save overall collection statistics
  results_dir <- file.path(OUTPUT_DIR, "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(collection_stats, file.path(results_dir, "collection_stats.rds"))
  
  # Create a human-readable summary of the collection
  summary_lines <- c(
    "=== BLOOMBERG VIDEO COLLECTION STATISTICS ===",
    paste("Collection Date:", Sys.Date()),
    ""
  )
  
  for (name in names(collection_stats)) {
    stats <- collection_stats[[name]]
    if (stats$total > 0) {
      pct_collected <- round(stats$existing / stats$total * 100, 1)
      coverage <- if (stats$complete) "COMPLETE" else "PARTIAL"
      
      summary_lines <- c(summary_lines,
                         paste0("Playlist: ", name),
                         paste0("  Position Range: ", stats$min_position, " to ", stats$max_position),
                         paste0("  Total Videos: ", stats$total),
                         paste0("  In Database: ", stats$existing, " (", pct_collected, "%)"),
                         paste0("  Newly Added: ", stats$new),
                         paste0("  Coverage: ", coverage),
                         ""
      )
    }
  }
  
  writeLines(summary_lines, file.path(results_dir, paste0("collection_coverage_", format(Sys.Date(), "%Y%m%d"), ".txt")))
  cat("Saved collection coverage report\n")
  
  # Save all collected new videos
  if (nrow(all_new_videos) > 0) {
    save_new_results(all_new_videos, existing_video_ids)
  } else {
    cat("No new videos found across all playlists\n")
  }
  
  return(list(new_videos = all_new_videos, stats = collection_stats))
}

# Function to incrementally collect only missing videos
collect_missing_bloomberg_videos <- function() {
  cat("Starting Bloomberg Incremental Video Collection\n")
  
  # Check API key
  if (is.null(API_KEY) || API_KEY == "" || API_KEY == "YOUR_API_KEY") {
    stop("Please set your YouTube API key in the config file")
  }
  cat("Using API key:", substr(API_KEY, 1, 8), "...\n")
  
  # Initialize or load progress
  progress <- initialize_progress()
  
  # Load existing video IDs
  existing_video_ids <- load_existing_video_ids()
  cat("Found", length(existing_video_ids), "existing video IDs in database\n")
  
  # Get all playlist configs
  playlists <- config$playlists
  
  # Convert to a data frame for easier handling
  playlist_df <- tibble(
    name = sapply(playlists, function(p) p$name),
    id = sapply(playlists, function(p) if(!is.null(p$id)) p$id else NA_character_),
    priority = sapply(playlists, function(p) if(!is.null(p$priority)) p$priority else 3)
  )
  
  # Sort by priority
  playlist_df <- playlist_df %>% 
    arrange(priority) %>%
    filter(!is.na(id))  # Skip playlists without IDs
  
  if (nrow(playlist_df) == 0) {
    cat("No playlists with valid IDs found. Please update your config.\n")
    return(NULL)
  }
  
  cat("Processing", nrow(playlist_df), "playlists with IDs\n")
  print(playlist_df)
  
  # Daily quota tracking
  quota_start <- progress$api_quota_used
  collection_start_time <- Sys.time()
  
  all_new_videos <- data.frame()
  collection_stats <- list()
  
  # Process each playlist
  for (i in 1:nrow(playlist_df)) {
    playlist_name <- playlist_df$name[i]
    playlist_id <- playlist_df$id[i]
    
    # Get existing videos count for this playlist
    existing_file <- file.path(OUTPUT_DIR, "playlists", paste0(gsub("[^a-zA-Z0-9]", "_", playlist_name), ".csv"))
    existing_count <- 0
    if (file.exists(existing_file)) {
      tryCatch({
        existing_df <- read_csv(existing_file, show_col_types = FALSE)
        existing_count <- nrow(existing_df)
      }, error = function(e) {
        warning("Error reading existing file: ", e$message)
      })
    }
    
    cat("Playlist:", playlist_name, "- Current videos in database:", existing_count, "\n")
    
    # Use the complete function to get all videos, including missing ones
    result <- get_playlist_videos_complete(
      playlist_id, 
      playlist_name, 
      progress, 
      existing_video_ids
    )
    
    # Save collection stats
    collection_stats[[playlist_name]] <- result$stats
    
    # Save just the new videos if any
    if (nrow(result$new_videos) > 0) {
      new_videos_dir <- file.path(OUTPUT_DIR, "playlists", "new_videos")
      dir.create(new_videos_dir, recursive = TRUE, showWarnings = FALSE)
      new_videos_file <- file.path(new_videos_dir, paste0(gsub("[^a-zA-Z0-9]", "_", playlist_name), "_new.csv"))
      write_csv(result$new_videos, new_videos_file)
      cat("Saved", nrow(result$new_videos), "NEW videos from playlist to", new_videos_file, "\n")
      
      # Also update the main playlist file
      updated_file <- file.path(OUTPUT_DIR, "playlists", paste0(gsub("[^a-zA-Z0-9]", "_", playlist_name), ".csv"))
      
      if (file.exists(updated_file)) {
        # Append to existing file
        existing_videos <- read_csv(updated_file, show_col_types = FALSE)
        combined_videos <- bind_rows(existing_videos, result$new_videos) %>% 
          distinct(video_id, .keep_all = TRUE) %>%
          arrange(desc(published_at))
        write_csv(combined_videos, updated_file)
        cat("Updated existing playlist file with new videos. Now has", nrow(combined_videos), "videos total.\n")
      } else {
        # Create new file
        write_csv(result$new_videos, updated_file) 
        cat("Created new playlist file with", nrow(result$new_videos), "videos.\n")
      }
      
      # Add to collection of all new videos
      all_new_videos <- bind_rows(all_new_videos, result$new_videos)
    } else {
      cat("No new videos found for playlist:", playlist_name, "\n")
    }
    
    # Check if we should pause due to API quota limits
    if (progress$api_quota_used - quota_start > 8000) {
      cat("\n===== DAILY QUOTA NEARLY EXHAUSTED =====\n")
      cat("Used", progress$api_quota_used - quota_start, "units today\n")
      cat("Saving progress and stopping until tomorrow\n")
      
      # Save what we have so far
      save_new_results(all_new_videos, existing_video_ids)
      
      # Calculate when we can resume
      elapsed <- difftime(Sys.time(), collection_start_time, units = "hours")
      if (elapsed < 24) {
        wait_hours <- ceiling(24 - as.numeric(elapsed))
        cat("Please wait", wait_hours, "hours before resuming\n")
      }
      
      break
    }
  }
  
  # Save overall collection statistics
  results_dir <- file.path(OUTPUT_DIR, "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(collection_stats, file.path(results_dir, paste0("collection_stats_", format(Sys.Date(), "%Y%m%d"), ".rds")))
  
  # Create a summary of what was collected
  summary_lines <- c(
    "=== BLOOMBERG VIDEO COLLECTION SUMMARY ===",
    paste("Collection Date:", Sys.Date()),
    paste("Total New Videos:", nrow(all_new_videos)),
    "",
    "Playlist Details:"
  )
  
  for (name in names(collection_stats)) {
    stats <- collection_stats[[name]]
    if (stats$total > 0) {
      summary_lines <- c(summary_lines,
                         paste0("  ", name, ":"),
                         paste0("    Total Videos in Playlist: ", stats$total),
                         paste0("    Newly Collected: ", stats$new),
                         paste0("    Position Range: ", stats$min_position, " to ", stats$max_position),
                         ""
      )
    }
  }
  
  writeLines(summary_lines, file.path(results_dir, paste0("collection_summary_", format(Sys.Date(), "%Y%m%d"), ".txt")))
  cat("Saved collection summary\n")
  
  # Save all collected new videos
  if (nrow(all_new_videos) > 0) {
    save_new_results(all_new_videos, existing_video_ids)
  } else {
    cat("No new videos found across all playlists\n")
  }
  
  return(list(new_videos = all_new_videos, stats = collection_stats))
}

# Run the collector
collect_missing_bloomberg_videos()  # For incremental collection of missing videos





