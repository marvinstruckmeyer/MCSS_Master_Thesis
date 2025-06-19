# Comprehensive Market and Macro Data Collection Script 

# Load required libraries
cat("=== LOADING REQUIRED PACKAGES ===\n")

# Function to install and load packages safely
load_package <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", package_name, "...\n")
    install.packages(package_name, dependencies = TRUE)
    if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
      stop(paste("Failed to install/load package:", package_name))
    }
  }
  cat("✓", package_name, "loaded successfully\n")
}

# Load packages
required_packages <- c("tidyverse", "quantmod", "lubridate", "zoo")
for (pkg in required_packages) {
  load_package(pkg)
}

# Configuration
config <- list(
  # Get your free FRED API key at: https://fred.stlouisfed.org/docs/api/api_key.html
  fred_api_key = "YOUR_FRED_API_KEY_HERE",  # Your FRED API key here
  start_date = as.Date("2022-06-01"),  # Start from June 1, 2022 as requested
  end_date = Sys.Date(),
  output_dir = "market_macro_data",
  max_retries = 3,
  retry_delay = 2  # seconds
)

# Create output directory
dir.create(config$output_dir, showWarnings = FALSE, recursive = TRUE)

# Set FRED API key
setDefaults(getSymbols.FRED, api.key = config$fred_api_key)

cat("\n=== COMPREHENSIVE MARKET & MACRO DATA COLLECTION V2.0 ===\n")
cat("Start Date:", format(config$start_date, "%Y-%m-%d"), "\n")
cat("End Date:", format(config$end_date, "%Y-%m-%d"), "\n")
cat("New Feature: MOVE Index (^MOVE) added\n")
cat("Fixed: Claims data date adjustment\n\n")

# ===== DEFINE DATA CATEGORIES =====

# Weekly macro indicators (with special handling for claims data)
weekly_macro_indicators <- list(
  # Claims data (requires special date handling - Saturday timestamps -> Thursday releases)
  "ICSA" = list(symbol = "ICSA", name = "Initial Claims", release_day = "Thursday", needs_date_fix = TRUE),
  "CCSA" = list(symbol = "CCSA", name = "Continuing Claims", release_day = "Thursday", needs_date_fix = TRUE),
  
  # Other weekly Thursday releases  
  "WM2NS" = list(symbol = "WM2NS", name = "Money Stock M2", release_day = "Thursday", needs_date_fix = FALSE),
  "WTREGEN" = list(symbol = "WTREGEN", name = "Treasury General Account", release_day = "Thursday", needs_date_fix = FALSE),
  "WRESBAL" = list(symbol = "WRESBAL", name = "Reserve Balances", release_day = "Thursday", needs_date_fix = FALSE),
  
  # Wednesday releases
  "NFCI" = list(symbol = "NFCI", name = "Chicago Fed Financial Conditions", release_day = "Wednesday", needs_date_fix = FALSE),
  "NFCICREDIT" = list(symbol = "NFCICREDIT", name = "Chicago Fed Credit Subindex", release_day = "Wednesday", needs_date_fix = FALSE),
  "NFCILEVERAGE" = list(symbol = "NFCILEVERAGE", name = "Chicago Fed Leverage Subindex", release_day = "Wednesday", needs_date_fix = FALSE),
  "NFCIRISK" = list(symbol = "NFCIRISK", name = "Chicago Fed Risk Subindex", release_day = "Wednesday", needs_date_fix = FALSE)
)

# Daily market-based macro indicators (from FRED)
daily_macro_indicators <- list(
  # Treasury yields
  "DGS2" = list(symbol = "DGS2", name = "2-Year Treasury Constant Maturity Rate"),
  "DGS5" = list(symbol = "DGS5", name = "5-Year Treasury Constant Maturity Rate"),
  "DGS10" = list(symbol = "DGS10", name = "10-Year Treasury Constant Maturity Rate"),
  "DGS30" = list(symbol = "DGS30", name = "30-Year Treasury Constant Maturity Rate"),
  
  # Inflation expectations
  "T5YIE" = list(symbol = "T5YIE", name = "5-Year Breakeven Inflation Rate"),
  "T10YIE" = list(symbol = "T10YIE", name = "10-Year Breakeven Inflation Rate"),
  "T5YIFR" = list(symbol = "T5YIFR", name = "5-Year-5-Year Forward Inflation Expectation Rate"),
  
  # Credit spreads
  "BAMLH0A0HYM2" = list(symbol = "BAMLH0A0HYM2", name = "ICE BofA US High Yield Index OAS"),
  "BAMLC0A0CM" = list(symbol = "BAMLC0A0CM", name = "ICE BofA US Corporate Index OAS"),
  
  # Other daily indicators
  "VIXCLS" = list(symbol = "VIXCLS", name = "CBOE Volatility Index"),
  "DCOILWTICO" = list(symbol = "DCOILWTICO", name = "Crude Oil Prices: West Texas Intermediate")
)

# Market data from Yahoo Finance (UPDATED with MOVE index + Sector ETFs)
yahoo_market_indicators <- list(
  # Main market indices
  "GSPC" = list(symbol = "^GSPC", name = "S&P 500", safe_name = "GSPC"),
  "VIX" = list(symbol = "^VIX", name = "VIX Volatility Index", safe_name = "VIX"),
  "DJI" = list(symbol = "^DJI", name = "Dow Jones Industrial Average", safe_name = "DJI"),
  "IXIC" = list(symbol = "^IXIC", name = "NASDAQ Composite", safe_name = "IXIC"),
  "MOVE" = list(symbol = "^MOVE", name = "MOVE Index (Bond Volatility)", safe_name = "MOVE"),
  
  # Commodities and FX
  "COPPER" = list(symbol = "HG=F", name = "Copper Futures", safe_name = "COPPER"),
  "GOLD" = list(symbol = "GC=F", name = "Gold Futures", safe_name = "GOLD"),
  "USDOLLAR" = list(symbol = "DX=F", name = "US Dollar Index Futures", safe_name = "USDOLLAR"),
  
  # S&P 500 Sector ETFs (SPDR Select Sector ETFs)
  "XLF" = list(symbol = "XLF", name = "Financial Select Sector SPDR Fund", safe_name = "XLF"),
  "XLK" = list(symbol = "XLK", name = "Technology Select Sector SPDR Fund", safe_name = "XLK"),
  "XLV" = list(symbol = "XLV", name = "Health Care Select Sector SPDR Fund", safe_name = "XLV"),
  "XLY" = list(symbol = "XLY", name = "Consumer Discretionary Select Sector SPDR Fund", safe_name = "XLY"),
  "XLP" = list(symbol = "XLP", name = "Consumer Staples Select Sector SPDR Fund", safe_name = "XLP"),
  "XLRE" = list(symbol = "XLRE", name = "Real Estate Select Sector SPDR Fund", safe_name = "XLRE"),
  "XLU" = list(symbol = "XLU", name = "Utilities Select Sector SPDR Fund", safe_name = "XLU"),
  "XLB" = list(symbol = "XLB", name = "Materials Select Sector SPDR Fund", safe_name = "XLB"),
  "XLC" = list(symbol = "XLC", name = "Communication Services Select Sector SPDR Fund", safe_name = "XLC"),
  "XLE" = list(symbol = "XLE", name = "Energy Select Sector SPDR Fund", safe_name = "XLE"),
  "XLI" = list(symbol = "XLI", name = "Industrial Select Sector SPDR Fund", safe_name = "XLI")
)

# ===== HELPER FUNCTIONS =====

# Enhanced FRED data fetching with claims date fix
fetch_fred_data <- function(indicator_info, start_date, end_date, max_retries = 3) {
  symbol <- indicator_info$symbol
  name <- indicator_info$name
  needs_date_fix <- indicator_info$needs_date_fix %||% FALSE
  
  for (attempt in 1:max_retries) {
    tryCatch({
      cat("  Fetching", symbol, "-", name, "(attempt", attempt, ")\n")
      
      # For claims data, fetch with wider date range to ensure we get everything
      fetch_start <- if (needs_date_fix) start_date - days(30) else start_date
      
      # Fetch data from FRED
      getSymbols.FRED(
        symbol,
        env = .GlobalEnv,
        from = format(fetch_start, "%Y-%m-%d"),
        to = format(end_date, "%Y-%m-%d")
      )
      
      # Get the xts object and convert to data frame
      xts_obj <- get(symbol, envir = .GlobalEnv)
      
      # Remove the object from global environment to avoid conflicts
      rm(list = symbol, envir = .GlobalEnv)
      
      # Convert to data frame
      df <- data.frame(
        date = as.Date(index(xts_obj)),
        value = as.numeric(coredata(xts_obj))
      )
      
      # Rename value column to symbol name
      names(df)[2] <- symbol
      
      # Remove rows with NA values
      df <- df[!is.na(df[[symbol]]), ]
      
      # SPECIAL HANDLING FOR CLAIMS DATA
      if (needs_date_fix) {
        cat("    Applying claims data date fix (Saturday -> Thursday)\n")
        df <- df %>%
          mutate(
            # Adjust Saturday dates to Thursday releases
            adjusted_date = case_when(
              weekdays(date) == "Saturday" ~ date - days(2),  # Saturday -> Thursday
              TRUE ~ date  # Keep other dates as is
            )
          ) %>%
          select(date = adjusted_date, !!symbol := !!sym(symbol)) %>%
          # Filter to desired date range after adjustment
          filter(date >= start_date & date <= end_date) %>%
          arrange(date)
      }
      
      cat("    Success! Retrieved", nrow(df), "observations\n")
      return(df)
      
    }, error = function(e) {
      cat("    Attempt", attempt, "failed:", e$message, "\n")
      if (attempt < max_retries) {
        Sys.sleep(config$retry_delay)
      }
    })
  }
  
  cat("    Failed to fetch", symbol, "after", max_retries, "attempts\n")
  return(NULL)
}

# Enhanced Yahoo Finance data fetching
fetch_yahoo_data <- function(indicator_info, start_date, end_date, max_retries = 3) {
  symbol <- indicator_info$symbol
  safe_name <- indicator_info$safe_name
  name <- indicator_info$name
  
  for (attempt in 1:max_retries) {
    tryCatch({
      cat("  Fetching", symbol, "-", name, "(attempt", attempt, ")\n")
      
      # Fetch data from Yahoo Finance
      data <- getSymbols(
        symbol,
        src = "yahoo",
        from = format(start_date, "%Y-%m-%d"),
        to = format(end_date, "%Y-%m-%d"),
        auto.assign = FALSE
      )
      
      # Extract closing prices
      df <- data.frame(
        date = as.Date(index(data)),
        value = as.numeric(Cl(data))
      )
      
      # Rename value column to safe name
      names(df)[2] <- safe_name
      
      # Remove rows with NA values
      df <- df[!is.na(df[[safe_name]]), ]
      
      cat("    Success! Retrieved", nrow(df), "observations\n")
      return(df)
      
    }, error = function(e) {
      cat("    Attempt", attempt, "failed:", e$message, "\n")
      if (attempt < max_retries) {
        Sys.sleep(config$retry_delay)
      }
    })
  }
  
  cat("    Failed to fetch", symbol, "after", max_retries, "attempts\n")
  return(NULL)
}

# Function to create a complete date sequence for business days
create_business_date_sequence <- function(start_date, end_date) {
  # Create complete date sequence
  all_dates <- seq(from = start_date, to = end_date, by = "day")
  
  # Filter to business days (Monday-Friday)
  business_days <- all_dates[weekdays(all_dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
  
  return(data.frame(date = business_days))
}

# Function to forward-fill weekly data to daily frequency
forward_fill_weekly_to_daily <- function(weekly_data, business_dates, release_day = "Thursday") {
  # Merge weekly data with business dates
  daily_data <- business_dates %>%
    left_join(weekly_data, by = "date")
  
  # Get column name (excluding date)
  value_col <- names(weekly_data)[names(weekly_data) != "date"]
  
  # Forward fill the values (carry last observation forward)
  daily_data[[value_col]] <- na.locf(daily_data[[value_col]], na.rm = FALSE)
  
  return(daily_data)
}

# ===== DATA COLLECTION =====

# Create business date sequence
cat("Creating business date sequence...\n")
business_dates <- create_business_date_sequence(config$start_date, config$end_date)
cat("Business days in period:", nrow(business_dates), "\n\n")

# 1. COLLECT WEEKLY MACRO DATA (with claims fix)
cat("=== COLLECTING WEEKLY MACRO INDICATORS ===\n")
weekly_data_list <- list()

for (indicator_name in names(weekly_macro_indicators)) {
  indicator_info <- weekly_macro_indicators[[indicator_name]]
  
  data <- fetch_fred_data(indicator_info, config$start_date, config$end_date)
  
  if (!is.null(data) && nrow(data) > 0) {
    weekly_data_list[[indicator_name]] <- data
    Sys.sleep(0.5)  # Be nice to FRED API
  }
}

# 2. COLLECT DAILY MACRO DATA
cat("\n=== COLLECTING DAILY MACRO INDICATORS ===\n")
daily_macro_list <- list()

for (indicator_name in names(daily_macro_indicators)) {
  indicator_info <- daily_macro_indicators[[indicator_name]]
  
  data <- fetch_fred_data(indicator_info, config$start_date, config$end_date)
  
  if (!is.null(data) && nrow(data) > 0) {
    daily_macro_list[[indicator_name]] <- data
    Sys.sleep(0.5)  # Be nice to FRED API
  }
}

# 3. COLLECT MARKET DATA FROM YAHOO FINANCE (including MOVE index)
cat("\n=== COLLECTING MARKET DATA FROM YAHOO FINANCE ===\n")
market_data_list <- list()

for (indicator_name in names(yahoo_market_indicators)) {
  indicator_info <- yahoo_market_indicators[[indicator_name]]
  
  data <- fetch_yahoo_data(indicator_info, config$start_date, config$end_date)
  
  if (!is.null(data) && nrow(data) > 0) {
    market_data_list[[indicator_name]] <- data
    Sys.sleep(0.5)  # Be nice to Yahoo Finance
  }
}

# ===== DATA PROCESSING AND MERGING =====

cat("\n=== PROCESSING AND MERGING DATA ===\n")

# Create Dataset 1: Daily variables only (market + daily macro)
cat("Creating Dataset 1: Daily variables only...\n")
daily_only_data <- business_dates

# Merge daily macro data
for (name in names(daily_macro_list)) {
  if (!is.null(daily_macro_list[[name]])) {
    daily_only_data <- daily_only_data %>%
      left_join(daily_macro_list[[name]], by = "date")
  }
}

# Merge market data
for (name in names(market_data_list)) {
  if (!is.null(market_data_list[[name]])) {
    daily_only_data <- daily_only_data %>%
      left_join(market_data_list[[name]], by = "date")
  }
}

cat("Daily dataset dimensions:", dim(daily_only_data), "\n")

# Create Dataset 2: Weekly variables only
cat("Creating Dataset 2: Weekly variables only...\n")
weekly_only_data <- business_dates

for (name in names(weekly_data_list)) {
  if (!is.null(weekly_data_list[[name]])) {
    weekly_only_data <- weekly_only_data %>%
      left_join(weekly_data_list[[name]], by = "date")
  }
}

cat("Weekly dataset dimensions:", dim(weekly_only_data), "\n")

# Create Dataset 3: All data merged (with weekly data forward-filled)
cat("Creating Dataset 3: All variables merged with forward-filling...\n")
combined_data <- business_dates

# Add daily macro data
for (name in names(daily_macro_list)) {
  if (!is.null(daily_macro_list[[name]])) {
    combined_data <- combined_data %>%
      left_join(daily_macro_list[[name]], by = "date")
  }
}

# Add market data
for (name in names(market_data_list)) {
  if (!is.null(market_data_list[[name]])) {
    combined_data <- combined_data %>%
      left_join(market_data_list[[name]], by = "date")
  }
}

# Add weekly data with forward-filling
for (name in names(weekly_data_list)) {
  if (!is.null(weekly_data_list[[name]])) {
    # Get the release day for this indicator
    release_day <- weekly_macro_indicators[[name]]$release_day
    
    # Forward fill to daily frequency
    weekly_filled <- forward_fill_weekly_to_daily(
      weekly_data_list[[name]], 
      business_dates, 
      release_day
    )
    
    # Merge with combined data (excluding date column)
    value_col <- names(weekly_filled)[names(weekly_filled) != "date"]
    combined_data[[value_col]] <- weekly_filled[[value_col]]
  }
}

cat("Combined dataset dimensions:", dim(combined_data), "\n")

# ===== SAVE DATASETS =====

cat("\n=== SAVING DATASETS ===\n")

# Save Dataset 1: Daily variables only
daily_file <- file.path(config$output_dir, "daily_variables_only.csv")
write_csv(daily_only_data, daily_file)
cat("Saved daily variables dataset to:", daily_file, "\n")

# Save Dataset 2: Weekly variables only
weekly_file <- file.path(config$output_dir, "weekly_variables_only.csv")
write_csv(weekly_only_data, weekly_file)
cat("Saved weekly variables dataset to:", weekly_file, "\n")

# Save Dataset 3: Combined dataset
combined_file <- file.path(config$output_dir, "combined_all_variables.csv")
write_csv(combined_data, combined_file)
cat("Saved combined dataset to:", combined_file, "\n")

# ===== GENERATE ENHANCED SUMMARY REPORT =====

cat("\n=== GENERATING ENHANCED SUMMARY REPORT ===\n")

# Create summary statistics
summary_stats <- list(
  collection_date = Sys.Date(),
  date_range = paste(config$start_date, "to", config$end_date),
  business_days = nrow(business_dates),
  datasets_created = 3
)

# Add variable counts
summary_stats$daily_variables <- ncol(daily_only_data) - 1  # Excluding date column
summary_stats$weekly_variables <- ncol(weekly_only_data) - 1
summary_stats$combined_variables <- ncol(combined_data) - 1

# Create detailed variable lists
daily_vars <- names(daily_only_data)[names(daily_only_data) != "date"]
weekly_vars <- names(weekly_only_data)[names(weekly_only_data) != "date"]
combined_vars <- names(combined_data)[names(combined_data) != "date"]

# Calculate data completeness
calculate_completeness <- function(data) {
  data %>%
    summarise(across(-date, ~ sum(!is.na(.)) / length(.) * 100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "completeness_pct") %>%
    arrange(desc(completeness_pct))
}

daily_completeness <- calculate_completeness(daily_only_data)
weekly_completeness <- calculate_completeness(weekly_only_data)
combined_completeness <- calculate_completeness(combined_data)

# Check if MOVE index was successfully collected
move_success <- "MOVE" %in% names(combined_data)
claims_success <- all(c("ICSA", "CCSA") %in% names(combined_data))
sectors_success <- sum(c("XLF", "XLK", "XLV", "XLY", "XLP", "XLRE", "XLU", "XLB", "XLC", "XLE", "XLI") %in% names(combined_data))

# Save enhanced summary report
summary_file <- file.path(config$output_dir, "collection_summary_report_v2.txt")

sink(summary_file)
cat("=== MARKET & MACRO DATA COLLECTION SUMMARY V2.0 ===\n")
cat("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("NEW FEATURES IN V2.0:\n")
cat("✓ MOVE Index (^MOVE) - Bond Volatility Index\n")
cat("✓ S&P 500 Sector ETFs - All 11 SPDR Sector Funds\n")
cat("✓ Claims Data Fix - Saturday timestamps adjusted to Thursday releases\n")
cat("✓ Enhanced error handling and reporting\n\n")

cat("COLLECTION PARAMETERS:\n")
cat("- Start Date:", format(config$start_date, "%Y-%m-%d"), "\n")
cat("- End Date:", format(config$end_date, "%Y-%m-%d"), "\n")
cat("- Business Days:", summary_stats$business_days, "\n")
cat("- FRED API Key Used: Yes\n\n")

cat("DATASETS CREATED:\n")
cat("1. Daily Variables Only:", summary_stats$daily_variables, "variables\n")
cat("2. Weekly Variables Only:", summary_stats$weekly_variables, "variables\n")
cat("3. Combined Dataset:", summary_stats$combined_variables, "variables\n\n")

cat("NEW FEATURES STATUS:\n")
cat("- MOVE Index Collection:", ifelse(move_success, "✓ SUCCESS", "✗ FAILED"), "\n")
cat("- Claims Data Fix:", ifelse(claims_success, "✓ SUCCESS", "✗ FAILED"), "\n")
cat("- Sector ETFs Collected:", sectors_success, "out of 11\n\n")

cat("DAILY VARIABLES (", length(daily_vars), " total):\n")
for (var in daily_vars) {
  completeness <- daily_completeness$completeness_pct[daily_completeness$variable == var]
  if (length(completeness) > 0) {
    cat("- ", var, " (", round(completeness, 1), "% complete)\n", sep = "")
  }
}

cat("\nWEEKLY VARIABLES (", length(weekly_vars), " total):\n")
for (var in weekly_vars) {
  completeness <- weekly_completeness$completeness_pct[weekly_completeness$variable == var]
  if (length(completeness) > 0) {
    cat("- ", var, " (", round(completeness, 1), "% complete)\n", sep = "")
  }
}

cat("\nCOMBINED DATASET COMPLETENESS:\n")
print(combined_completeness)

cat("\nCLAIMS DATA DIAGNOSTIC:\n")
if (claims_success) {
  icsa_obs <- sum(!is.na(combined_data$ICSA))
  ccsa_obs <- sum(!is.na(combined_data$CCSA))
  cat("- ICSA (Initial Claims):", icsa_obs, "observations\n")
  cat("- CCSA (Continuing Claims):", ccsa_obs, "observations\n")
  cat("- Date adjustment applied: Saturday timestamps -> Thursday releases\n")
} else {
  cat("- Claims data collection failed\n")
}

cat("SECTOR ETFs DIAGNOSTIC:\n")
sector_etfs <- c("XLF", "XLK", "XLV", "XLY", "XLP", "XLRE", "XLU", "XLB", "XLC", "XLE", "XLI")
if (sectors_success > 0) {
  collected_sectors <- intersect(sector_etfs, names(combined_data))
  missing_sectors <- setdiff(sector_etfs, names(combined_data))
  
  cat("- Successfully collected sectors:\n")
  sector_names <- c(
    "XLF" = "Financials", "XLK" = "Technology", "XLV" = "Health Care",
    "XLY" = "Consumer Discretionary", "XLP" = "Consumer Staples", "XLRE" = "Real Estate",
    "XLU" = "Utilities", "XLB" = "Materials", "XLC" = "Communication Services",
    "XLE" = "Energy", "XLI" = "Industrials"
  )
  
  for (sector in collected_sectors) {
    sector_obs <- sum(!is.na(combined_data[[sector]]))
    cat("  ", sector, "(", sector_names[sector], "):", sector_obs, "observations\n")
  }
  
  if (length(missing_sectors) > 0) {
    cat("- Missing sectors:", paste(missing_sectors, collapse = ", "), "\n")
  }
} else {
  cat("- No sector ETFs collected successfully\n")
}

cat("\nMOVE INDEX DIAGNOSTIC:\n")
if (move_success) {
  move_obs <- sum(!is.na(combined_data$MOVE))
  cat("- MOVE Index observations:", move_obs, "\n")
  cat("- Successfully added bond volatility data\n")
} else {
  cat("- MOVE Index collection failed\n")
}

cat("\nNOTES:\n")
cat("- Weekly variables in the combined dataset are forward-filled to daily frequency\n")
cat("- Business days only (Monday-Friday) are included\n")
cat("- Claims data (ICSA/CCSA) timestamps adjusted from Saturday to Thursday\n")
cat("- MOVE index provides bond market volatility complement to equity VIX\n")

sink()

cat("Enhanced summary report saved to:", summary_file, "\n")

# Print final summary to console
cat("\n=== COLLECTION COMPLETE V2.0 ===\n")
cat("Successfully created 3 datasets:\n")
cat("1. Daily only (", summary_stats$daily_variables, " variables)\n")
cat("2. Weekly only (", summary_stats$weekly_variables, " variables)\n")
cat("3. Combined (", summary_stats$combined_variables, " variables)\n\n")


