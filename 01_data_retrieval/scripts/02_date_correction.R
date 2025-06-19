# Bloomberg TV Shows Date Correction Script (Simplified Version)
# This script corrects publication dates for Bloomberg TV show videos using:
# 1. The YouTube API to get accurate publication dates (with robust error handling)
# 2. Title extraction as a reliable fallback
# 3. Visual reports and summaries of the corrections

# Set your YouTube API key here
# Get your free API key at: https://console.developers.google.com/apis/credentials
# Enable "YouTube Data API v3" and create an API key
YOUTUBE_API_KEY <- "YOUR_YOUTUBE_API_KEY_HERE"  # Replace with your actual key

INPUT_DIR <- "data/playlists"
OUTPUT_DIR <- "data/playlists/corrected"

# Load required packages
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(ggplot2)

# Create output directory if it doesn't exist
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Read all CSV files ----
read_bloomberg_csvs <- function(directory) {
  # Get all CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if any files were found
  if(length(csv_files) == 0) {
    stop("No CSV files found in directory: ", directory)
  }
  
  # Initialize list to store data
  all_data <- list()
  
  # Process each file
  message(paste("Found", length(csv_files), "CSV files"))
  for(file_path in csv_files) {
    file_name <- basename(file_path)
    message(paste("Reading:", file_name))
    
    tryCatch({
      # Read the CSV file
      data <- read_csv(file_path, show_col_types = FALSE)
      
      # Extract show name from file name
      show_name <- sub("\\.csv$", "", file_name)
      
      # Add source file name and show name
      data$file_source <- file_name
      data$show_name <- show_name
      
      # Add to the list
      all_data[[file_name]] <- data
      
    }, error = function(e) {
      message(paste("Error reading", file_name, ":", e$message))
    })
  }
  
  # Check if any data was read
  if(length(all_data) == 0) {
    stop("No data was successfully read from CSV files")
  }
  
  # Return list of dataframes
  return(all_data)
}

# ---- 2. Extract dates from video titles ----
extract_date_from_title <- function(title, default_year = NULL) {
  # Handle NULL or NA title
  if(is.null(title) || is.na(title)) return(NA)
  
  # Extract date patterns with regex
  # Pattern: MM/DD (e.g., "04/11" in "The Pulse 04/11")
  if(str_detect(title, "\\b\\d{1,2}/\\d{1,2}\\b")) {
    date_str <- str_extract(title, "\\b\\d{1,2}/\\d{1,2}\\b")
    if(!is.null(default_year)) {
      # Add year to MM/DD format
      date_str <- paste0(date_str, "/", default_year)
      return(try(mdy(date_str), silent = TRUE))
    }
  }
  
  # Pattern: MM/DD/YYYY
  if(str_detect(title, "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b")) {
    date_str <- str_extract(title, "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b")
    return(try(mdy(date_str), silent = TRUE))
  }
  
  # Pattern: Month DD (e.g., "April 11")
  month_pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2}"
  if(str_detect(title, month_pattern)) {
    date_str <- str_extract(title, month_pattern)
    if(!is.null(default_year)) {
      # Add year to Month DD format
      date_str <- paste(date_str, default_year)
      return(try(mdy(date_str), silent = TRUE))
    }
  }
  
  # Pattern: Month DD, YYYY
  full_date_pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}"
  if(str_detect(title, full_date_pattern)) {
    date_str <- str_extract(title, full_date_pattern)
    return(try(mdy(date_str), silent = TRUE))
  }
  
  # If no date found
  return(NA)
}

# ---- 3. Get publication dates from YouTube API (ULTRA-SIMPLIFIED) ----
get_youtube_dates <- function(video_ids, api_key, batch_size = 50) {
  # Initialize results dataframe
  results <- data.frame(
    video_id = character(),
    youtube_published_at = character(),
    stringsAsFactors = FALSE
  )
  
  # Total videos and number of batches
  total_videos <- length(video_ids)
  num_batches <- ceiling(total_videos / batch_size)
  
  message(paste("Processing", total_videos, "videos in", num_batches, "batches"))
  
  # Process videos in batches
  for(i in 1:num_batches) {
    # Get current batch of IDs
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, total_videos)
    current_batch <- video_ids[start_idx:end_idx]
    
    message(paste("Processing batch", i, "of", num_batches, 
                  "(videos", start_idx, "to", end_idx, ")"))
    
    # Create comma-separated string of video IDs
    id_string <- paste(current_batch, collapse = ",")
    
    # Construct API URL - ultra simplified
    url <- paste0(
      "https://www.googleapis.com/youtube/v3/videos",
      "?id=", id_string,
      "&part=snippet",
      "&key=", api_key
    )
    
    # Make API request with extreme caution
    tryCatch({
      response <- GET(url)
      
      if(status_code(response) == 200) {
        # VERY careful parsing to avoid nested access errors
        parsed_text <- content(response, "text", encoding = "UTF-8")
        parsed_json <- fromJSON(parsed_text, simplifyDataFrame = FALSE)
        
        # Process each item individually to avoid errors
        if(length(parsed_json$items) > 0) {
          batch_results <- lapply(parsed_json$items, function(item) {
            video_id <- item$id
            # Very defensive access to publishedAt
            published_at <- NA
            if(!is.null(item$snippet) && !is.null(item$snippet$publishedAt)) {
              published_at <- item$snippet$publishedAt
            }
            return(data.frame(
              video_id = video_id,
              youtube_published_at = published_at,
              stringsAsFactors = FALSE
            ))
          })
          
          # Combine batch results
          if(length(batch_results) > 0) {
            batch_df <- do.call(rbind, batch_results)
            results <- rbind(results, batch_df)
            message(paste("  Found dates for", nrow(batch_df), "videos in batch", i))
          }
        } else {
          message(paste("  No video data found in batch", i))
        }
      } else {
        message(paste("API request failed with status:", status_code(response)))
      }
    }, error = function(e) {
      message(paste("Error in batch", i, ":", e$message))
    })
    
    # Pause to avoid rate limiting
    Sys.sleep(0.25)
  }
  
  # Convert published_at to datetime
  if(nrow(results) > 0) {
    results <- results %>%
      mutate(youtube_published_at = ymd_hms(youtube_published_at))
  }
  
  message(paste("Retrieved dates for", nrow(results), "videos from YouTube API"))
  return(results)
}

# ---- 4. Process and correct dates for all files (Simplified approach) ----
process_and_correct_dates <- function(api_key, input_dir, output_dir, use_current_year = FALSE) {
  # Read all CSV files
  message("\n=== STEP 1: Reading Bloomberg TV show CSV files ===")
  all_data <- read_bloomberg_csvs(input_dir)
  
  # Combine all data to get unique video IDs
  combined_data <- bind_rows(all_data)
  unique_ids <- unique(combined_data$video_id)
  message(paste("Total videos found:", nrow(combined_data)))
  message(paste("Unique video IDs:", length(unique_ids)))
  
  # Get dates from YouTube API
  message("\n=== STEP 2: Fetching publication dates from YouTube API ===")
  api_dates <- get_youtube_dates(unique_ids, api_key)
  
  # Process each file
  message("\n=== STEP 3: Processing each file and correcting dates ===")
  corrected_files <- list()
  
  for(file_name in names(all_data)) {
    message(paste("Processing file:", file_name))
    data <- all_data[[file_name]]
    
    # Add YouTube API dates
    data <- data %>%
      left_join(api_dates, by = "video_id")
    
    # Extract dates from titles and create corrected dates
    data <- data %>%
      mutate(
        # Convert original date from CSV to datetime
        original_date = ymd_hms(published_at),
        
        # Determine the year to use for title dates
        year_to_use = ifelse(use_current_year, 
                             year(Sys.Date()), 
                             year(original_date)),
        
        # Extract date from title using determined year
        title_date = mapply(extract_date_from_title, title, year_to_use),
        
        # Use the best available date (priority: YouTube API > Title > Original)
        best_date = case_when(
          !is.na(youtube_published_at) ~ youtube_published_at,
          !is.na(title_date) ~ as_datetime(title_date),
          TRUE ~ original_date
        ),
        
        # Flag to indicate the source of the date
        date_source = case_when(
          !is.na(youtube_published_at) ~ "youtube_api",
          !is.na(title_date) ~ "title_extracted",
          TRUE ~ "original_csv"
        ),
        
        # Calculate the difference in days between original and corrected dates
        days_difference = as.numeric(difftime(original_date, best_date, units = "days"))
      ) %>%
      select(-year_to_use) # Remove temporary column
    
    # Save the corrected file
    output_file <- file.path(output_dir, paste0("corrected_", file_name))
    write_csv(data, output_file)
    message(paste("Saved corrected file to:", output_file))
    
    # Add to the list of corrected files
    corrected_files[[file_name]] <- data
  }
  
  # Generate a summary of date corrections
  all_corrected <- bind_rows(corrected_files)
  
  # Save combined data with corrected dates
  write_csv(all_corrected, file.path(output_dir, "all_videos_with_corrected_dates.csv"))
  
  # Create summary by file and date source
  summary <- all_corrected %>%
    group_by(file_source, date_source) %>%
    summarize(
      count = n(),
      avg_difference_days = mean(abs(days_difference), na.rm = TRUE),
      max_difference_days = max(abs(days_difference), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Save the summary
  write_csv(summary, file.path(output_dir, "date_correction_summary.csv"))
  
  # Print summary statistics
  message("\n=== Date correction summary ===")
  message(paste("Total videos processed:", nrow(all_corrected)))
  message(paste("Videos with YouTube API dates:", sum(all_corrected$date_source == "youtube_api")))
  message(paste("Videos with dates extracted from titles:", sum(all_corrected$date_source == "title_extracted")))
  message(paste("Videos with original CSV dates:", sum(all_corrected$date_source == "original_csv")))
  message(paste("Average difference in days:", round(mean(abs(all_corrected$days_difference), na.rm = TRUE), 2)))
  
  return(list(
    corrected_data = corrected_files,
    summary = summary,
    all_data = all_corrected
  ))
}

# ---- 5. Create visualization report ----
create_date_visualization_report <- function(all_corrected, output_dir) {
  message("\n=== STEP 4: Creating visualization report ===")
  
  # Create directory for plots
  plots_dir <- file.path(output_dir, "date_correction_plots")
  dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Prepare data for visualization
  viz_data <- all_corrected %>%
    mutate(
      original_date_day = as.Date(original_date),
      best_date_day = as.Date(best_date),
      abs_days_diff = abs(days_difference)
    )
  
  # 1. Date source distribution
  date_source_plot <- ggplot(viz_data, aes(x = date_source, fill = date_source)) +
    geom_bar() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Distribution of Date Sources",
      x = "Date Source",
      y = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(plots_dir, "date_source_distribution.png"), date_source_plot, width = 8, height = 6)
  
  # 2. Date differences histogram (with reasonable binwidth)
  max_diff <- max(abs(viz_data$days_difference), na.rm = TRUE)
  
  # If we have very large differences, bin them more coarsely
  binwidth <- if(max_diff > 1000) max(1, round(max_diff / 50)) else 1
  
  diff_hist <- ggplot(viz_data, aes(x = days_difference)) +
    geom_histogram(binwidth = binwidth, fill = "steelblue", color = "white") +
    labs(
      title = "Distribution of Date Differences",
      subtitle = "Negative values mean original date is earlier than corrected date",
      x = "Days Difference (Original - Corrected)",
      y = "Count"
    ) +
    theme_minimal()
  
  ggsave(file.path(plots_dir, "date_differences_histogram.png"), diff_hist, width = 10, height = 6)
  
  # 3. Date differences by show
  show_diff_plot <- viz_data %>%
    group_by(show_name) %>%
    summarize(
      mean_diff = mean(abs_days_diff, na.rm = TRUE),
      median_diff = median(abs_days_diff, na.rm = TRUE),
      max_diff = max(abs_days_diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(mean_diff, median_diff, max_diff),
      names_to = "metric",
      values_to = "value"
    ) %>%
    ggplot(aes(x = reorder(show_name, value), y = value, fill = metric)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "Date Differences by Show",
      x = "Show",
      y = "Days Difference",
      fill = "Metric"
    ) +
    theme_minimal()
  
  ggsave(file.path(plots_dir, "date_differences_by_show.png"), show_diff_plot, width = 12, height = 8)
  
  # 4. Original vs Corrected dates scatter plot
  dates_scatter <- ggplot(viz_data, aes(x = original_date_day, y = best_date_day, color = date_source)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Original vs Corrected Dates",
      subtitle = "Points on the dashed line have matching dates",
      x = "Original Date",
      y = "Corrected Date",
      color = "Date Source"
    ) +
    theme_minimal()
  
  ggsave(file.path(plots_dir, "original_vs_corrected_dates.png"), dates_scatter, width = 10, height = 8)
  
  # 5. Time series of corrections
  time_series_plot <- viz_data %>%
    mutate(month = floor_date(best_date_day, "month")) %>%
    group_by(month, date_source) %>%
    summarize(count = n(), .groups = "drop") %>%
    ggplot(aes(x = month, y = count, color = date_source)) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Corrections Over Time",
      x = "Month",
      y = "Count",
      color = "Date Source"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(plots_dir, "corrections_over_time.png"), time_series_plot, width = 12, height = 6)
  
  # Create an HTML report
  html_report <- file.path(output_dir, "date_correction_report.html")
  
  html_content <- paste0('
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Bloomberg TV Shows Date Correction Report</title>
    <style>
      body { font-family: Arial, sans-serif; margin: 20px; line-height: 1.6; }
      h1 { color: #2c3e50; }
      h2 { color: #3498db; margin-top: 30px; }
      .summary { background-color: #f8f9fa; padding: 15px; border-radius: 5px; }
      .plot { margin: 20px 0; text-align: center; }
      .plot img { max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 5px; }
      table { border-collapse: collapse; width: 100%; margin: 20px 0; }
      th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
      th { background-color: #f2f2f2; }
      tr:nth-child(even) { background-color: #f9f9f9; }
    </style>
  </head>
  <body>
    <h1>Bloomberg TV Shows Date Correction Report</h1>
    
    <div class="summary">
      <h2>Summary</h2>
      <p>Total videos processed: ', nrow(viz_data), '</p>
      <p>Videos with YouTube API dates: ', sum(viz_data$date_source == "youtube_api"), '</p>
      <p>Videos with dates extracted from titles: ', sum(viz_data$date_source == "title_extracted"), '</p>
      <p>Videos with original CSV dates: ', sum(viz_data$date_source == "original_csv"), '</p>
      <p>Average absolute difference in days: ', round(mean(viz_data$abs_days_diff, na.rm = TRUE), 2), '</p>
      <p>Maximum absolute difference in days: ', max(viz_data$abs_days_diff, na.rm = TRUE), '</p>
    </div>
    
    <h2>Visualizations</h2>
    
    <div class="plot">
      <h3>Distribution of Date Sources</h3>
      <img src="date_correction_plots/date_source_distribution.png" alt="Date Source Distribution">
    </div>
    
    <div class="plot">
      <h3>Distribution of Date Differences</h3>
      <img src="date_correction_plots/date_differences_histogram.png" alt="Date Differences Histogram">
    </div>
    
    <div class="plot">
      <h3>Date Differences by Show</h3>
      <img src="date_correction_plots/date_differences_by_show.png" alt="Date Differences by Show">
    </div>
    
    <div class="plot">
      <h3>Original vs Corrected Dates</h3>
      <img src="date_correction_plots/original_vs_corrected_dates.png" alt="Original vs Corrected Dates">
    </div>
    
    <div class="plot">
      <h3>Corrections Over Time</h3>
      <img src="date_correction_plots/corrections_over_time.png" alt="Corrections Over Time">
    </div>
  </body>
  </html>
  ')
  
  # Write the HTML report
  writeLines(html_content, html_report)
  message(paste("HTML report created at:", html_report))
  
  return(plots_dir)
}

# ---- 6. Update original CSV files with corrected dates ----
update_original_files <- function(input_dir, output_dir) {
  message("\n=== STEP 5: Creating updated CSV files with corrected dates ===")
  
  # Get all original CSV files
  original_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  
  for(original_path in original_files) {
    file_name <- basename(original_path)
    corrected_path <- file.path(output_dir, paste0("corrected_", file_name))
    
    # Skip if corrected file doesn't exist
    if(!file.exists(corrected_path)) {
      warning(paste("No corrected file found for:", file_name))
      next
    }
    
    message(paste("Updating file:", file_name))
    
    # Read original and corrected files
    original_data <- read_csv(original_path, show_col_types = FALSE)
    corrected_data <- read_csv(corrected_path, show_col_types = FALSE)
    
    # Update published_at with best_date from corrected data
    updated_data <- original_data %>%
      select(-published_at) %>%
      left_join(
        corrected_data %>% 
          select(video_id, best_date, date_source),
        by = "video_id"
      ) %>%
      mutate(published_at = format(best_date, "%Y-%m-%dT%H:%M:%SZ")) %>%
      select(-best_date, -date_source)
    
    # Save the updated file
    output_path <- file.path(output_dir, paste0("updated_", file_name))
    write_csv(updated_data, output_path)
    message(paste("Saved updated file to:", output_path))
  }
  
  message("All files updated successfully!")
}

# ---- 7. Main function to run the entire process ----
run_date_correction_pipeline <- function(api_key = YOUTUBE_API_KEY, 
                                         input_dir = INPUT_DIR, 
                                         output_dir = OUTPUT_DIR,
                                         use_current_year = FALSE) {
  
  start_time <- Sys.time()
  message(paste("Starting Bloomberg TV dates correction pipeline at", start_time))
  
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Step 1-3: Process and correct dates
  results <- process_and_correct_dates(api_key, input_dir, output_dir, use_current_year)
  
  # Step 4: Create visualization report
  plots_dir <- create_date_visualization_report(results$all_data, output_dir)
  
  # Step 5: Update original files with corrected dates
  update_original_files(input_dir, output_dir)
  
  end_time <- Sys.time()
  elapsed <- end_time - start_time
  message(paste("\nPipeline completed in", round(elapsed, 2), "seconds!"))
  message(paste("Corrected files saved to:", output_dir))
  message(paste("Visualization report saved to:", file.path(output_dir, "date_correction_report.html")))
  
  return(list(
    output_dir = output_dir,
    plots_dir = plots_dir,
    results = results
  ))
}

# ---- 8. Fix for problematic dates (extreme differences) ----
fix_problematic_dates <- function(all_corrected, max_allowed_years = 5) {
  # Find cases where the date difference is extremely large
  max_days <- 365 * max_allowed_years
  problematic <- all_corrected %>%
    filter(abs(days_difference) > max_days) 
  
  if(nrow(problematic) > 0) {
    message(paste("Found", nrow(problematic), "videos with date differences > ", 
                  max_allowed_years, "years"))
    
    # Look for patterns by show
    show_patterns <- problematic %>%
      group_by(show_name) %>%
      summarize(
        count = n(),
        avg_diff_days = mean(days_difference),
        avg_diff_years = mean(days_difference) / 365,
        .groups = "drop"
      )
    
    message("Patterns by show:")
    print(show_patterns)
    
    # Return the problematic videos for manual inspection
    return(problematic)
  } else {
    message("No problematic dates found (all differences < ", max_allowed_years, " years)")
    return(NULL)
  }
}

# Run the pipeline
run_date_correction_pipeline()

# Alternative: Use current year for all videos with partial dates
# run_date_correction_pipeline(use_current_year = TRUE)







###########
# Missing Bloomberg Video Dates - Finder and Fixer
# This script helps you identify and manually fix videos without YouTube API dates


# Missing Bloomberg Video Dates - Finder and Fixer (Fixed Version)
# This script helps you identify and manually fix videos without YouTube API dates

# Load required packages
library(tidyverse)
library(lubridate)

# Configuration
CORRECTED_DATA_PATH <- "data/playlists/corrected/all_videos_with_corrected_dates.csv"
OUTPUT_DIR <- "data/playlists/corrected/manual_fixes"

# Create output directory
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Function to find videos with missing API dates
find_missing_dates <- function(data_path = CORRECTED_DATA_PATH) {
  # Read the corrected data
  message("Reading corrected data...")
  all_data <- read_csv(data_path, show_col_types = FALSE)
  
  # Filter for videos that didn't get API dates
  missing_api_dates <- all_data %>%
    filter(date_source == "original_csv") %>%
    arrange(show_name, title)
  
  # Print summary
  message(paste("Found", nrow(missing_api_dates), "videos without YouTube API dates"))
  message("Distribution by show:")
  
  # Show count by show
  show_counts <- missing_api_dates %>%
    count(show_name) %>%
    arrange(desc(n))
  
  print(show_counts)
  
  # Save the list of missing dates to CSV
  output_path <- file.path(OUTPUT_DIR, "missing_api_dates.csv")
  write_csv(missing_api_dates, output_path)
  message(paste("Saved list of videos with missing dates to:", output_path))
  
  return(missing_api_dates)
}

# Function to create a template for manual date correction (FIXED)
create_manual_template <- function(missing_dates) {
  # Create a simplified template with only the essential fields
  template <- missing_dates %>%
    # Select only columns that definitely exist
    select(video_id, title, show_name, file_source, original_date) %>%
    # Add empty columns for manual entry
    mutate(
      manual_year = NA_integer_,
      manual_month = NA_integer_,
      manual_day = NA_integer_,
      manual_notes = NA_character_,
      # Add helper columns with parsed date parts from original date
      original_year = year(original_date),
      original_month = month(original_date),
      original_day = day(original_date)
    )
  
  # Save the template
  output_path <- file.path(OUTPUT_DIR, "manual_date_template.csv")
  write_csv(template, output_path)
  message(paste("Created manual correction template at:", output_path))
  
  return(template)
}

# Function to attempt to extract more dates from titles with better regex
advanced_title_extraction <- function(missing_dates) {
  message("Attempting advanced title date extraction...")
  
  # Additional regex patterns for date extraction
  extract_with_advanced_regex <- function(title) {
    # Handle NULL or NA title
    if(is.null(title) || is.na(title)) return(list(date = NA, pattern = NA))
    
    # MM/DD pattern
    if(str_detect(title, "\\b\\d{1,2}/\\d{1,2}\\b")) {
      date_str <- str_extract(title, "\\b\\d{1,2}/\\d{1,2}\\b")
      return(list(date = date_str, pattern = "MM/DD"))
    }
    
    # MM.DD pattern (with dot separator)
    if(str_detect(title, "\\b\\d{1,2}\\.\\d{1,2}\\b")) {
      date_str <- str_extract(title, "\\b\\d{1,2}\\.\\d{1,2}\\b")
      return(list(date = date_str, pattern = "MM.DD"))
    }
    
    # MM-DD pattern (with hyphen separator)
    if(str_detect(title, "\\b\\d{1,2}-\\d{1,2}\\b")) {
      date_str <- str_extract(title, "\\b\\d{1,2}-\\d{1,2}\\b")
      return(list(date = date_str, pattern = "MM-DD"))
    }
    
    # Month DD pattern (e.g., "April 11")
    month_pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2}"
    if(str_detect(title, month_pattern)) {
      date_str <- str_extract(title, month_pattern)
      return(list(date = date_str, pattern = "Month DD"))
    }
    
    # Abbreviated month pattern (e.g., "Jan 11")
    abbr_month_pattern <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\\s+\\d{1,2}"
    if(str_detect(title, abbr_month_pattern)) {
      date_str <- str_extract(title, abbr_month_pattern)
      return(list(date = date_str, pattern = "Abbr Month DD"))
    }
    
    # DD Month pattern (e.g., "11 April")
    reverse_pattern <- "\\d{1,2}\\s+(January|February|March|April|May|June|July|August|September|October|November|December)"
    if(str_detect(title, reverse_pattern)) {
      date_str <- str_extract(title, reverse_pattern)
      return(list(date = date_str, pattern = "DD Month"))
    }
    
    # DD Abbreviated month pattern (e.g., "11 Apr")
    reverse_abbr_pattern <- "\\d{1,2}\\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)"
    if(str_detect(title, reverse_abbr_pattern)) {
      date_str <- str_extract(title, reverse_abbr_pattern)
      return(list(date = date_str, pattern = "DD Abbr Month"))
    }
    
    # Full date with year (e.g., "April 11, 2023")
    full_date_pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},?\\s+\\d{4}"
    if(str_detect(title, full_date_pattern)) {
      date_str <- str_extract(title, full_date_pattern)
      return(list(date = date_str, pattern = "Month DD, YYYY"))
    }
    
    # ISO date (YYYY-MM-DD)
    iso_pattern <- "\\b\\d{4}-\\d{2}-\\d{2}\\b"
    if(str_detect(title, iso_pattern)) {
      date_str <- str_extract(title, iso_pattern)
      return(list(date = date_str, pattern = "YYYY-MM-DD"))
    }
    
    # If no date found
    return(list(date = NA, pattern = NA))
  }
  
  # Apply the advanced extraction to each title
  results <- data.frame()
  
  tryCatch({
    results <- missing_dates %>%
      mutate(
        extracted_info = lapply(title, extract_with_advanced_regex),
        date_str = sapply(extracted_info, function(x) x$date),
        pattern_matched = sapply(extracted_info, function(x) x$pattern)
      ) %>%
      select(-extracted_info)
    
    # Count matches by pattern
    pattern_counts <- results %>%
      filter(!is.na(pattern_matched)) %>%
      count(pattern_matched) %>%
      arrange(desc(n))
    
    message("Found date patterns in titles:")
    print(pattern_counts)
  }, error = function(e) {
    message("Error in advanced extraction: ", e$message)
    results <- missing_dates %>%
      mutate(
        date_str = NA_character_,
        pattern_matched = NA_character_
      )
  })
  
  # Save the results
  output_path <- file.path(OUTPUT_DIR, "advanced_title_extraction.csv")
  write_csv(results, output_path)
  message(paste("Saved advanced title extraction results to:", output_path))
  
  return(results)
}

# Function to generate YouTube lookup URLs
generate_youtube_lookup_urls <- function(missing_dates) {
  # Create a dataframe with lookup URLs
  lookup_urls <- missing_dates %>%
    mutate(
      youtube_url = paste0("https://www.youtube.com/watch?v=", video_id)
    ) %>%
    select(video_id, title, show_name, youtube_url)
  
  # Save the lookup URLs
  output_path <- file.path(OUTPUT_DIR, "youtube_lookup_urls.csv")
  write_csv(lookup_urls, output_path)
  message(paste("Saved YouTube lookup URLs to:", output_path))
  
  # Create an HTML file for easier clicking
  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <title>YouTube Video Lookup</title>\n",
    "  <style>\n",
    "    body { font-family: Arial, sans-serif; margin: 20px; }\n",
    "    table { border-collapse: collapse; width: 100%; }\n",
    "    th, td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }\n",
    "    tr:hover { background-color: #f5f5f5; }\n",
    "    th { background-color: #f2f2f2; }\n",
    "  </style>\n",
    "</head>\n",
    "<body>\n",
    "  <h1>YouTube Video Lookup</h1>\n",
    "  <p>Click on the links below to open the videos in YouTube and check their publication dates:</p>\n",
    "  <table>\n",
    "    <tr><th>Video ID</th><th>Title</th><th>Show</th><th>Link</th></tr>\n"
  )
  
  # Add a row for each video
  for(i in 1:nrow(lookup_urls)) {
    html_content <- paste0(
      html_content,
      "    <tr>",
      "<td>", lookup_urls$video_id[i], "</td>",
      "<td>", lookup_urls$title[i], "</td>",
      "<td>", lookup_urls$show_name[i], "</td>",
      "<td><a href=\"", lookup_urls$youtube_url[i], "\" target=\"_blank\">Open in YouTube</a></td>",
      "</tr>\n"
    )
  }
  
  # Close the HTML tags
  html_content <- paste0(
    html_content,
    "  </table>\n",
    "</body>\n",
    "</html>"
  )
  
  html_path <- file.path(OUTPUT_DIR, "youtube_lookup.html")
  writeLines(html_content, html_path)
  message(paste("Created YouTube lookup HTML at:", html_path))
  
  return(lookup_urls)
}

# Function to apply manual corrections
apply_manual_corrections <- function(manual_path, original_data_path = CORRECTED_DATA_PATH) {
  # Check if the manual corrections file exists
  if(!file.exists(manual_path)) {
    stop("Manual corrections file not found: ", manual_path)
  }
  
  # Read the manual corrections
  message("Reading manual corrections...")
  manual_corrections <- read_csv(manual_path, show_col_types = FALSE)
  
  # Read the original data
  message("Reading original data...")
  all_data <- read_csv(original_data_path, show_col_types = FALSE)
  
  # Create corrected dates from manual entries
  corrected <- manual_corrections %>%
    filter(!is.na(manual_year) & !is.na(manual_month) & !is.na(manual_day)) %>%
    mutate(
      manual_date = make_datetime(manual_year, manual_month, manual_day),
      days_difference = as.numeric(difftime(original_date, manual_date, units = "days"))
    )
  
  message(paste("Found", nrow(corrected), "valid manual corrections"))
  
  # Update the original data with manual corrections
  updated_data <- all_data %>%
    left_join(
      corrected %>% select(video_id, manual_date, manual_notes),
      by = "video_id"
    ) %>%
    mutate(
      # Update best_date and date_source if manual_date exists
      best_date = ifelse(!is.na(manual_date), manual_date, best_date),
      date_source = ifelse(!is.na(manual_date), "manual_correction", date_source),
      # Recalculate days_difference
      days_difference = as.numeric(difftime(original_date, best_date, units = "days"))
    ) %>%
    select(-manual_date, -manual_notes)  # Remove temporary columns
  
  # Save the updated data
  output_path <- file.path(OUTPUT_DIR, "all_videos_with_manual_corrections.csv")
  write_csv(updated_data, output_path)
  message(paste("Saved updated data with manual corrections to:", output_path))
  
  # Create summary of manual corrections
  summary <- updated_data %>%
    group_by(date_source) %>%
    summarize(
      count = n(),
      avg_difference_days = mean(abs(days_difference), na.rm = TRUE),
      max_difference_days = max(abs(days_difference), na.rm = TRUE),
      .groups = "drop"
    )
  
  message("Summary of data after manual corrections:")
  print(summary)
  
  return(updated_data)
}

# Main function to run all steps
find_and_fix_missing_dates <- function() {
  # Step 1: Find videos with missing API dates
  missing_dates <- find_missing_dates()
  
  # Step 2: Try advanced title extraction
  advanced_extraction <- advanced_title_extraction(missing_dates)
  
  # Step 3: Create a template for manual entry
  template <- create_manual_template(missing_dates)
  
  # Step 4: Generate lookup URLs for manual checking
  lookup_urls <- generate_youtube_lookup_urls(missing_dates)
  
  message("\n=== Process complete ===")
  message(paste("Found", nrow(missing_dates), "videos without YouTube API dates"))
  message("To fix the remaining dates:")
  message("1. Open the manual template: ", file.path(OUTPUT_DIR, "manual_date_template.csv"))
  message("2. Fill in the manual_year, manual_month, and manual_day columns")
  message("3. Save the file as 'manual_corrections.csv' in the same directory")
  message("4. Run apply_manual_corrections('data/playlists/corrected/manual_fixes/manual_corrections.csv')")
  
  return(list(
    missing_dates = missing_dates,
    advanced_extraction = advanced_extraction,
    template = template,
    lookup_urls = lookup_urls
  ))
}

# Run the main function
find_and_fix_missing_dates()

