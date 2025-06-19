# Fixed Strategic Feature Engineering Script v2.1
# Properly handles NAs in technical indicator calculations
# Focus: Momentum, Sector Rotation, Market Regimes, Cross-Asset Signals, Technical Analysis

library(tidyverse)
library(zoo)  # For rolling window functions
library(TTR) # For technical indicators (RSI)

cat("=== FIXED STRATEGIC FEATURE ENGINEERING V2.1 ===\n")

# Load transformed data (returns/changes)
cat("Loading returns and changes data...\n")
returns_data <- read_csv("market_macro_data/returns_and_changes.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Load original price data for technical indicators
cat("Loading original price data for technical indicators...\n")
price_data <- read_csv("market_macro_data/combined_all_variables.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  select(date, GSPC, VIX)  # Need S&P 500 and VIX prices for technical indicators

cat("Loaded", nrow(returns_data), "observations for returns data\n")
cat("Loaded", nrow(price_data), "observations for price data\n")

# Merge datasets by date
cat("Merging datasets...\n")
merged_data <- returns_data %>%
  left_join(price_data, by = "date", suffix = c("", "_price")) %>%
  arrange(date)

cat("Merged dataset has", nrow(merged_data), "observations\n")

# ===== IMPROVED TECHNICAL INDICATOR FUNCTIONS =====

# Function to calculate distance from moving average (handles NAs properly)
calc_ma_distance_robust <- function(prices, ma_period) {
  # Remove leading/trailing NAs but preserve structure
  if (all(is.na(prices))) {
    return(rep(NA, length(prices)))
  }
  
  # Calculate moving average using zoo::rollmean with na.rm=TRUE
  ma <- rollmean(prices, k = ma_period, fill = NA, align = "right", na.rm = TRUE)
  
  # Calculate percentage distance
  distance_pct <- ((prices - ma) / ma) * 100
  
  # Clean up infinites and NaNs
  distance_pct[is.infinite(distance_pct) | is.nan(distance_pct)] <- NA
  
  return(distance_pct)
}

# Function to calculate momentum (handles NAs properly)
calc_momentum_robust <- function(returns, window) {
  if (all(is.na(returns))) {
    return(rep(NA, length(returns)))
  }
  
  # Use rollmean with na.rm=TRUE to handle missing values
  momentum <- rollmean(returns, k = window, fill = NA, align = "right", na.rm = TRUE)
  
  return(momentum)
}

# Function to calculate RSI (simplified and robust)
calc_rsi_robust <- function(prices, n = 14) {
  if (all(is.na(prices)) || length(prices) < n + 1) {
    return(rep(NA, length(prices)))
  }
  
  # Calculate price changes
  price_changes <- diff(c(NA, prices))
  
  # Separate gains and losses
  gains <- ifelse(price_changes > 0, price_changes, 0)
  losses <- ifelse(price_changes < 0, -price_changes, 0)
  
  # Calculate average gains and losses using rollmean
  avg_gains <- rollmean(gains, k = n, fill = NA, align = "right", na.rm = TRUE)
  avg_losses <- rollmean(losses, k = n, fill = NA, align = "right", na.rm = TRUE)
  
  # Calculate RS and RSI
  rs <- avg_gains / avg_losses
  rsi <- 100 - (100 / (1 + rs))
  
  # Handle division by zero cases
  rsi[is.infinite(rsi) | is.nan(rsi)] <- NA
  rsi[avg_losses == 0 & avg_gains > 0] <- 100  # All gains, no losses
  rsi[avg_gains == 0 & avg_losses > 0] <- 0    # All losses, no gains
  
  return(rsi)
}

# Function to calculate rolling mean with proper NA handling
rollmean_robust <- function(x, k, ...) {
  result <- rollmean(x, k = k, fill = NA, align = "right", na.rm = TRUE, ...)
  # If there aren't enough non-NA values in the window, return NA
  result[is.nan(result)] <- NA
  return(result)
}

# ===== DIAGNOSTIC: CHECK DATA QUALITY =====
cat("\n=== DATA QUALITY DIAGNOSTIC ===\n")

# Check GSPC price data quality
gspc_na_count <- sum(is.na(merged_data$GSPC))
gspc_total <- nrow(merged_data)
cat("GSPC price data: ", gspc_na_count, " NAs out of ", gspc_total, " observations (", 
    round(gspc_na_count/gspc_total*100, 1), "%)\n", sep = "")

# Check GSPC return data quality
gspc_ret_na_count <- sum(is.na(merged_data$GSPC_ret))
cat("GSPC return data: ", gspc_ret_na_count, " NAs out of ", gspc_total, " observations (", 
    round(gspc_ret_na_count/gspc_total*100, 1), "%)\n", sep = "")

# Show some sample data
cat("\nSample of GSPC price and return data:\n")
sample_data <- merged_data %>% 
  select(date, GSPC, GSPC_ret) %>% 
  slice_head(n = 10)
print(sample_data)

# ===== ENGINEERED FEATURES WITH ROBUST CALCULATIONS =====
cat("\n=== CREATING ROBUST ENGINEERED FEATURES ===\n")

engineered_data <- merged_data %>%
  mutate(
    # ===== 1. MOMENTUM FEATURES (2 features) =====
    # Short-term momentum (5-day average returns) - ROBUST
    SP500_momentum_5d = calc_momentum_robust(GSPC_ret, 5),
    
    # Medium-term momentum (20-day average returns) - ROBUST  
    SP500_momentum_20d = calc_momentum_robust(GSPC_ret, 20),
    
    # ===== 2. TECHNICAL INDICATORS (2 features) =====
    # Distance from 50-day moving average - ROBUST
    SP500_distance_50d_MA = calc_ma_distance_robust(GSPC, 50),
    
    # S&P 500 RSI (70-day) - ROBUST
    SP500_RSI_14w = calc_rsi_robust(GSPC, n = 70),
    
    # ===== 3. SECTOR RELATIVE PERFORMANCE (3 features) =====
    # Tech vs Market (growth/momentum signal)
    Tech_vs_Market = ifelse(!is.na(XLK_ret) & !is.na(GSPC_ret), XLK_ret - GSPC_ret, NA),
    
    # Financials vs Market (rate sensitivity/economic cycle)
    Finance_vs_Market = ifelse(!is.na(XLF_ret) & !is.na(GSPC_ret), XLF_ret - GSPC_ret, NA),
    
    # Defensive vs Offensive sectors (robust calculation)
    Defensive_vs_Offensive = {
      # Calculate defensive average (utilities, staples, healthcare)
      defensive_avg <- rowMeans(cbind(XLU_ret, XLP_ret, XLV_ret), na.rm = TRUE)
      # Calculate offensive average (tech, discretionary, financials)
      offensive_avg <- rowMeans(cbind(XLK_ret, XLY_ret, XLF_ret), na.rm = TRUE)
      # Return difference, handling NAs
      ifelse(!is.na(defensive_avg) & !is.na(offensive_avg) & 
               !is.nan(defensive_avg) & !is.nan(offensive_avg), 
             defensive_avg - offensive_avg, NA)
    },
    
    # ===== 4. MARKET REGIME FEATURES (4 features) =====
    # Yield curve slope (recession predictor)
    Yield_Curve_Slope = ifelse(!is.na(DGS10_bps) & !is.na(DGS2_bps), 
                               DGS10_bps - DGS2_bps, NA),
    
    # Credit risk spread (risk appetite)
    Credit_Risk_Spread = ifelse(!is.na(BAMLH0A0HYM2_bps) & !is.na(BAMLC0A0CM_bps), 
                                BAMLH0A0HYM2_bps - BAMLC0A0CM_bps, NA),
    
    # Volatility regime (high/low vol environment) - ROBUST
    Vol_Regime = {
      vix_20d_avg <- rollmean_robust(VIX_chg, 20)
      ifelse(!is.na(VIX_chg) & !is.na(vix_20d_avg), VIX_chg - vix_20d_avg, NA)
    },
    
    # Flight to quality indicator
    Flight_to_Quality = ifelse(!is.na(GOLD_ret) & !is.na(GSPC_ret), 
                               GOLD_ret - GSPC_ret, NA),
    
    # ===== 5. CROSS-ASSET SIGNALS (3 features) =====
    # USD risk signal (safe haven vs risk assets)
    USD_Risk_Signal = ifelse(!is.na(USDOLLAR_ret) & !is.na(GSPC_ret), 
                             USDOLLAR_ret - GSPC_ret, NA),
    
    # Bond-equity divergence
    Bond_Equity_Divergence = ifelse(!is.na(DGS10_bps) & !is.na(GSPC_ret), 
                                    (DGS10_bps * -1) - GSPC_ret, NA),
    
    # Commodity momentum (combined signal) - ROBUST
    Commodity_Momentum = {
      comm_avg <- rowMeans(cbind(GOLD_ret, COPPER_ret, DCOILWTICO_ret), na.rm = TRUE)
      ifelse(!is.nan(comm_avg), comm_avg, NA)
    }
  ) %>%
  
  # Clean up any remaining infinites or NaNs in new features
  mutate(across(c(SP500_momentum_5d, SP500_momentum_20d, SP500_distance_50d_MA, SP500_RSI_14w,
                  Tech_vs_Market, Finance_vs_Market, Defensive_vs_Offensive,
                  Yield_Curve_Slope, Credit_Risk_Spread, Vol_Regime, Flight_to_Quality,
                  USD_Risk_Signal, Bond_Equity_Divergence, Commodity_Momentum), 
                ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

# ===== FEATURE QUALITY CHECK =====
new_features <- c(
  "SP500_momentum_5d", "SP500_momentum_20d", "SP500_distance_50d_MA", "SP500_RSI_14w",
  "Tech_vs_Market", "Finance_vs_Market", "Defensive_vs_Offensive",
  "Yield_Curve_Slope", "Credit_Risk_Spread", "Vol_Regime", "Flight_to_Quality",
  "USD_Risk_Signal", "Bond_Equity_Divergence", "Commodity_Momentum"
)

cat("\n=== FEATURE QUALITY CHECK (AFTER FIXES) ===\n")

# Simplified feature quality check
feature_quality <- data.frame(
  feature = new_features,
  missing = sapply(engineered_data[new_features], function(x) sum(is.na(x))),
  non_missing = sapply(engineered_data[new_features], function(x) sum(!is.na(x))),
  mean = sapply(engineered_data[new_features], function(x) mean(x, na.rm = TRUE)),
  sd = sapply(engineered_data[new_features], function(x) sd(x, na.rm = TRUE))
) %>%
  mutate(
    missing_pct = round(missing / nrow(engineered_data) * 100, 1),
    non_missing_pct = round(non_missing / nrow(engineered_data) * 100, 1)
  ) %>%
  arrange(desc(non_missing_pct))

print(feature_quality)

# ===== SPECIFIC DIAGNOSTIC FOR PROBLEMATIC FEATURES =====
cat("\n=== DIAGNOSTIC FOR PREVIOUSLY PROBLEMATIC FEATURES ===\n")

problematic_features <- c("SP500_momentum_5d", "SP500_momentum_20d", "SP500_distance_50d_MA", "SP500_RSI_14w")

for (feature in problematic_features) {
  if (feature %in% names(engineered_data)) {
    na_count <- sum(is.na(engineered_data[[feature]]))
    total_count <- nrow(engineered_data)
    na_pct <- round(na_count / total_count * 100, 1)
    
    cat(feature, ": ", na_count, " NAs out of ", total_count, " (", na_pct, "%)\n", sep = "")
    
    # Show first few non-NA values
    non_na_values <- engineered_data[[feature]][!is.na(engineered_data[[feature]])]
    if (length(non_na_values) > 0) {
      cat("  First few values: ", paste(round(head(non_na_values, 5), 3), collapse = ", "), "\n")
      cat("  Range: ", round(min(non_na_values, na.rm = TRUE), 3), " to ", 
          round(max(non_na_values, na.rm = TRUE), 3), "\n")
    } else {
      cat("  No non-NA values found!\n")
    }
  }
}

# ===== SAVE FIXED DATASET =====
write_csv(engineered_data, "market_macro_data/features_engineered_fixed.csv")

cat("\n✅ SAVED FIXED ENGINEERED DATASET\n")
cat("📁 File: market_macro_data/features_engineered_fixed.csv\n")
cat("📈 Total variables:", ncol(engineered_data) - 1, "\n")
cat("🔧 Fixed technical indicators with robust NA handling\n")

# ===== COMPARISON WITH ORIGINAL =====
if (file.exists("market_macro_data/features_engineered.csv")) {
  cat("\n=== COMPARISON WITH ORIGINAL ===\n")
  original_data <- read_csv("market_macro_data/features_engineered.csv", show_col_types = FALSE)
  
  for (feature in problematic_features) {
    if (feature %in% names(original_data) && feature %in% names(engineered_data)) {
      orig_na <- sum(is.na(original_data[[feature]]))
      fixed_na <- sum(is.na(engineered_data[[feature]]))
      improvement <- orig_na - fixed_na
      
      cat(feature, ":\n")
      cat("  Original NAs: ", orig_na, "\n")
      cat("  Fixed NAs: ", fixed_na, "\n")
      cat("  Improvement: ", improvement, " fewer NAs\n\n")
    }
  }
}

cat("🎯 TECHNICAL INDICATORS NOW PROPERLY CALCULATED!\n")
cat("🔧 Robust functions handle NAs in underlying price/return data\n")
cat("📊 Use 'features_engineered_fixed.csv' for your analysis\n")

