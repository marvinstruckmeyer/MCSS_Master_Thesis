# Minimal Market Data Transformation Script
# Clean transformations with proper holiday handling

library(tidyverse)

cat("=== MARKET DATA TRANSFORMATIONS ===\n")

# Load data
raw_data <- read_csv("market_macro_data/combined_all_variables.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

cat("Loaded", nrow(raw_data), "observations from", min(raw_data$date), "to", max(raw_data$date), "\n")

# Define transformation categories
transformations <- list(
  # Market prices -> Log returns (%) - INCLUDING SECTOR ETFs
  log_returns = c("GSPC", "DJI", "IXIC", "COPPER", "GOLD", "USDOLLAR", "DCOILWTICO",
                  "XLF", "XLK", "XLV", "XLY", "XLP", "XLRE", "XLU", "XLB", "XLC", "XLE", "XLI"),
  
  # Volatility indices -> Absolute changes
  vol_changes = c("VIX", "VIXCLS", "MOVE"),
  
  # Financial conditions -> Keep original levels (already stationary)
  keep_levels = c("NFCI", "NFCICREDIT", "NFCILEVERAGE", "NFCIRISK"),
  
  # Economic data -> Absolute changes (non-stationary levels)
  econ_changes = c("ICSA", "CCSA", "WM2NS", "WTREGEN", "WRESBAL"),
  
  # Rates and spreads -> Basis point changes
  bps_changes = c("DGS2", "DGS5", "DGS10", "DGS30", "T5YIE", "T10YIE", "T5YIFR", 
                  "BAMLH0A0HYM2", "BAMLC0A0CM")
)

# Enhanced transformation functions that handle missing data properly
log_returns_proper <- function(x) {
  # Calculate log returns only for non-NA consecutive values
  n <- length(x)
  returns <- rep(NA, n)
  
  for (i in 2:n) {
    if (!is.na(x[i])) {
      # Find the most recent non-NA value before position i
      prev_idx <- max(which(!is.na(x[1:(i-1)])))
      if (length(prev_idx) > 0 && prev_idx >= 1) {
        if (x[prev_idx] > 0 && x[i] > 0) {
          returns[i] <- (log(x[i]) - log(x[prev_idx])) * 100
        }
      }
    }
  }
  return(returns)
}

abs_changes_proper <- function(x) {
  # Calculate changes only for non-NA consecutive values
  n <- length(x)
  changes <- rep(NA, n)
  
  for (i in 2:n) {
    if (!is.na(x[i])) {
      # Find the most recent non-NA value before position i
      prev_idx <- max(which(!is.na(x[1:(i-1)])))
      if (length(prev_idx) > 0 && prev_idx >= 1) {
        changes[i] <- x[i] - x[prev_idx]
      }
    }
  }
  return(changes)
}

bps_changes_proper <- function(x) {
  # Calculate basis point changes properly
  changes <- abs_changes_proper(x)
  return(changes * 100)
}

# Apply transformations
result <- raw_data %>% select(date)

# Log returns with proper holiday handling
for (var in intersect(transformations$log_returns, names(raw_data))) {
  result[[paste0(var, "_ret")]] <- log_returns_proper(raw_data[[var]])
  cat("✓", var, "-> log returns (holiday-adjusted)\n")
}

# Volatility changes with proper handling
for (var in intersect(transformations$vol_changes, names(raw_data))) {
  result[[paste0(var, "_chg")]] <- abs_changes_proper(raw_data[[var]])
  cat("✓", var, "-> absolute changes (holiday-adjusted)\n")
}

# Keep financial conditions levels (already stationary)
for (var in intersect(transformations$keep_levels, names(raw_data))) {
  result[[var]] <- raw_data[[var]]
  cat("✓", var, "-> keep original levels (stationary)\n")
}

# Economic data changes with proper handling
for (var in intersect(transformations$econ_changes, names(raw_data))) {
  result[[paste0(var, "_chg")]] <- abs_changes_proper(raw_data[[var]])
  cat("✓", var, "-> absolute changes (holiday-adjusted)\n")
}

# Basis point changes with proper handling
for (var in intersect(transformations$bps_changes, names(raw_data))) {
  result[[paste0(var, "_bps")]] <- bps_changes_proper(raw_data[[var]])
  cat("✓", var, "-> basis point changes (holiday-adjusted)\n")
}

# Clean up infinites and save
result <- result %>%
  mutate(across(-date, ~ ifelse(is.infinite(.), NA, .)))

write_csv(result, "market_macro_data/returns_and_changes.csv")

cat("\n✅ DONE! Saved", nrow(result), "rows,", ncol(result)-1, "variables\n")
cat("📁 File: market_macro_data/returns_and_changes.csv\n")
cat("📝 Holiday handling: Returns calculated using most recent available price\n")
cat("📊 Financial conditions indices kept as levels (stationary by design)\n")

# Quick diagnostic for holiday handling
cat("\n🔍 Holiday handling check:\n")
sample_dates <- c("2022-06-20", "2022-06-21", "2022-07-04", "2022-07-05")
if ("GSPC_ret" %in% names(result)) {
  check_data <- result %>% 
    filter(date %in% as.Date(sample_dates)) %>%
    select(date, GSPC_ret)
  if (nrow(check_data) > 0) {
    print(check_data)
  }
}

