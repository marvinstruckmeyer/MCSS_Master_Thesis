# =============================================================================
# VIX CHANGE PREDICTION WITH SENTIMENT ANALYSIS & GARCH MODELS
# =============================================================================

# Load required libraries
library(tidyverse)
library(randomForest)
library(xgboost)
library(caret)
library(doParallel)
library(tictoc)
library(lubridate)
library(MLmetrics)
library(ggplot2)
library(plotly)
library(corrplot)
library(forecast)  # For Diebold-Mariano test
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(kableExtra)
library(rugarch)  # For GARCH models

# Set up parallel processing
n_cores <- detectCores() - 1
registerDoParallel(cores = n_cores)
cat("Using", n_cores, "cores for parallel processing\n")

# =============================================================================
# CONFIGURATION - UPDATED FOR VIX CHANGE PREDICTION
# =============================================================================

config <- list(
  # Data paths
  data_path = "03_ml_prediction/ml_data_final_complete_enhanced_BOND_FIXED.csv",
  output_dir = "VIX_sentiment_ablation_results_time_series_cross_validation",  
  checkpoint_dir = "VIX_sentiment_ablation_checkpoints_time_series_cross_validation",  
  
  # Target variable - VIX changes
  target_var = "VIX_chg",
  
  # Base predictors - REMOVED VIXCLS_chg to fix the warning
  base_predictors = c(
    # Same-day market returns (safe with shifted target)
    "GSPC_ret", "DJI_ret", "IXIC_ret", "COPPER_ret", "GOLD_ret", "USDOLLAR_ret", "DCOILWTICO_ret",
    
    # Sector returns
    "XLF_ret", "XLK_ret", "XLV_ret", "XLY_ret", "XLP_ret", 
    "XLRE_ret", "XLU_ret", "XLB_ret", "XLC_ret", "XLE_ret", "XLI_ret",
    
    # Volatility measures (excluding VIX-related to avoid target leakage)
    "MOVE_chg",  # Only keep bond volatility, remove VIXCLS_chg
    
    # Economic indicators
    "NFCI", "NFCICREDIT", "NFCILEVERAGE", "NFCIRISK",
    "ICSA_chg", "CCSA_chg", "WM2NS_chg", "WTREGEN_chg", "WRESBAL_chg",
    
    # Interest rates
    "DGS2_bps", "DGS5_bps", "DGS10_bps", "DGS30_bps", 
    "T5YIE_bps", "T10YIE_bps", "T5YIFR_bps", 
    "BAMLH0A0HYM2_bps", "BAMLC0A0CM_bps",
    
    # Technical indicators
    "SP500_momentum_5d", "SP500_momentum_20d", "SP500_distance_50d_MA", "SP500_RSI_14w",
    
    # Style factors (these are the new enhanced features!)
    "Tech_vs_Market", "Finance_vs_Market", "Defensive_vs_Offensive",
    "Yield_Curve_Slope", "Credit_Risk_Spread", "Vol_Regime", 
    "Flight_to_Quality", "USD_Risk_Signal", "Bond_Equity_Divergence", 
    "Commodity_Momentum",
    
    # Lagged variables
    "GSPC_ret_1D_lagged", "DGS10_bps_1D_lagged", "VIX_chg_1D_lagged"
  ),
  
  # Sentiment variables for ablation study
  sentiment_groups = list(
    basic = c("dict_sentiment_avg", "sentimentr_score_avg", "finbert_score_avg", 
              "ollama_score_avg", "aggregate_sentiment_avg"),
    
    momentum_3d = c("dict_sentiment_avg_momentum_3d", "sentimentr_score_avg_momentum_3d",
                    "finbert_score_avg_momentum_3d", "ollama_score_avg_momentum_3d",
                    "aggregate_sentiment_avg_momentum_3d"),
    
    momentum_5d = c("dict_sentiment_avg_momentum_5d", "sentimentr_score_avg_momentum_5d",
                    "finbert_score_avg_momentum_5d", "ollama_score_avg_momentum_5d", 
                    "aggregate_sentiment_avg_momentum_5d"),
    
    cross_sectional = c("sentiment_cross_sectional_std", "sentiment_cross_sectional_range",
                        "sentiment_cross_sectional_cv")
  ),
  
  # GARCH model specifications
  garch_config = list(
    specifications = list(
      "GARCH(1,1)" = list(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
      "EGARCH(1,1)" = list(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
      "GJR-GARCH(1,1)" = list(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
    ),
    distribution = "norm"
  ),
  
  # Time series cross-validation settings
  cv_config = list(
    initial_window = 200,
    horizon = 1,
    fixed_window = FALSE,
    skip = 0,
    n_folds = 10
  ),
  
  # Hyperparameter tuning settings
  tune_config = list(
    rf = list(
      ntree = c(500, 750, 1000, 1500),
      mtry = c(5, 8, 12, 15),
      nodesize = c(1, 3, 5),
      maxnodes = c(NULL, 100, 300)
    ),
    xgb = list(
      nrounds = c(100, 300),
      max_depth = c(4, 6),
      eta = c(0.05, 0.1),
      subsample = c(0.8),
      colsample_bytree = c(0.8),
      min_child_weight = c(1, 3)
    )
  ),
  
  # Cross-validation settings
  cv_folds = 5,
  cv_repeats = 2,
  
  # Random seed for reproducibility
  seed = 42,
  
  # Checkpoint settings
  save_after_each_model = TRUE,
  save_intermediate_results = TRUE,
  
  # Visualization settings
  create_plots = TRUE,
  plot_dpi = 300,
  plot_width = 12,
  plot_height = 8
)

# Create output directories
dir.create(config$output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(config$checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(config$output_dir, "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(config$output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(config$output_dir, "garch_models"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# DATA HANDLING FUNCTIONS
# =============================================================================

safe_complete_cases <- function(data_df) {
  if (nrow(data_df) == 0 || ncol(data_df) == 0) {
    return(logical(0))
  }
  complete_rows <- apply(data_df, 1, function(row) !any(is.na(row)))
  return(complete_rows)
}

prepare_model_data <- function(data, feature_cols, target_col) {
  required_cols <- c(feature_cols, target_col)
  existing_cols <- intersect(required_cols, names(data))
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  model_data <- data[, existing_cols, drop = FALSE]
  complete_rows <- safe_complete_cases(model_data)
  clean_data <- model_data[complete_rows, , drop = FALSE]
  
  if (nrow(clean_data) == 0) {
    return(list(X = NULL, y = NULL, n = 0, features = character(0)))
  }
  
  actual_features <- setdiff(existing_cols, target_col)
  X_clean <- clean_data[, actual_features, drop = FALSE]
  y_clean <- clean_data[[target_col]]
  
  return(list(
    X = X_clean,
    y = y_clean,
    n = nrow(clean_data),
    features = actual_features
  ))
}

# =============================================================================
# DATA LOADING - UPDATED FOR VIX CHANGES
# =============================================================================

load_and_prepare_data <- function() {
  cat("Loading and preparing data for VIX change prediction...\n")
  
  if (!file.exists(config$data_path)) {
    stop("Data file not found: ", config$data_path)
  }
  
  data <- read_csv(config$data_path, show_col_types = FALSE)
  cat("Loaded data with", nrow(data), "rows and", ncol(data), "columns\n")
  
  # Handle date column
  if ("date" %in% names(data)) {
    data <- data %>% mutate(date = as.Date(date))
  } else if ("Date" %in% names(data)) {
    data <- data %>% mutate(date = as.Date(Date)) %>% select(-Date)
  }
  
  # Sort by date and shift target
  # CRITICAL: Create properly shifted target (predict NEXT day's VIX change)
  data <- data %>%
    arrange(date) %>%
    mutate(
      VIX_chg_next_day = lead(!!sym(config$target_var), 1)  # Tomorrow's VIX change
    )
  
  cat("✓ Created next-day target variable: VIX_chg_next_day\n")
  
  # Check if target or related VIX variables are in base predictors
  vix_vars <- c("VIX_chg", "VIXCLS_chg", "VIX", "VIXCLS")
  problematic_vars <- intersect(vix_vars, config$base_predictors)
  if (length(problematic_vars) > 0) {
    warning("VIX-related variables found in base predictors: ", paste(problematic_vars, collapse = ", "))
    config$base_predictors <<- setdiff(config$base_predictors, problematic_vars)
    cat("Removed VIX-related variables to avoid lookahead bias\n")
  }
  
  # Check available variables
  available_base <- intersect(config$base_predictors, names(data))
  missing_base <- setdiff(config$base_predictors, names(data))
  
  # Check sentiment groups
  available_sentiment_groups <- list()
  total_sentiment_vars <- 0
  
  for (group_name in names(config$sentiment_groups)) {
    group_vars <- config$sentiment_groups[[group_name]]
    available_vars <- intersect(group_vars, names(data))
    
    cat("Sentiment group '", group_name, "': ", length(available_vars), "/", length(group_vars), " available\n")
    
    if (length(available_vars) > 0) {
      available_sentiment_groups[[group_name]] <- available_vars
      total_sentiment_vars <- total_sentiment_vars + length(available_vars)
    }
  }
  
  cat("Available base predictors:", length(available_base), "of", length(config$base_predictors), "\n")
  if (length(missing_base) > 0) {
    cat("Missing base predictors:", paste(missing_base, collapse = ", "), "\n")
  }
  
  cat("Total sentiment variables available:", total_sentiment_vars, "\n")
  
  # Update config
  config$base_predictors <<- available_base
  config$sentiment_groups <<- available_sentiment_groups
  
  # Create clean dataset
  all_sentiment_vars <- unlist(available_sentiment_groups, use.names = FALSE)
  
  data_clean <- data %>%
    filter(!is.na(VIX_chg_next_day)) %>%  # Remove last row (can't predict future)
    select(date, VIX_chg_next_day, all_of(available_base), all_of(all_sentiment_vars)) %>%
    filter(complete.cases(.))
  
  cat("After removing missing target values:", nrow(data_clean), "observations\n")
  cat("Date range:", min(data_clean$date), "to", max(data_clean$date), "\n")
  
  # Provide some descriptive statistics about the target variable
  target_stats <- summary(data_clean$VIX_chg_next_day)
  cat("\nVIX changes summary:\n")
  print(target_stats)
  cat("Standard deviation:", round(sd(data_clean$VIX_chg_next_day, na.rm = TRUE), 4), "\n")
  
  return(data_clean)
}

# =============================================================================
# GARCH MODEL FUNCTIONS WITH IMPROVED ERROR HANDLING
# =============================================================================

fit_garch_baseline <- function(train_data, target_col) {
  cat("Fitting GARCH baseline models for VIX changes...\n")
  
  # Prepare time series data
  ts_data <- train_data %>%
    arrange(date) %>%
    filter(!is.na(!!sym(target_col))) %>%
    pull(!!sym(target_col))
  
  if (length(ts_data) < 100) {
    cat("Insufficient data for GARCH model\n")
    return(NULL)
  }
  
  # Check for constant variance (causes convergence issues)
  if (var(ts_data, na.rm = TRUE) == 0) {
    cat("Zero variance in target series - cannot fit GARCH\n")
    return(NULL)
  }
  
  garch_results <- list()
  
  # Try different GARCH specifications with improved error handling
  for (spec_name in names(config$garch_config$specifications)) {
    cat("Fitting", spec_name, "...\n")
    
    spec <- config$garch_config$specifications[[spec_name]]
    
    tryCatch({
      # Create GARCH specification with more robust settings
      garch_spec <- ugarchspec(
        variance.model = spec$variance.model,
        mean.model = spec$mean.model,
        distribution.model = config$garch_config$distribution
      )
      
      # Fit GARCH model with solver control
      garch_fit <- ugarchfit(
        garch_spec, 
        ts_data,
        solver = "hybrid",  # More robust solver
        solver.control = list(trace = 0)
      )
      
      # Check convergence
      if (convergence(garch_fit) == 0) {
        # Store results only if converged
        garch_results[[spec_name]] <- list(
          spec = garch_spec,
          fit = garch_fit,
          aic = infocriteria(garch_fit)[1],
          bic = infocriteria(garch_fit)[2],
          loglik = likelihood(garch_fit)
        )
        
        cat("  AIC:", round(garch_results[[spec_name]]$aic, 4), 
            "BIC:", round(garch_results[[spec_name]]$bic, 4), "\n")
      } else {
        cat("  Failed to converge\n")
      }
      
    }, error = function(e) {
      cat("  Error fitting", spec_name, ":", e$message, "\n")
    })
  }
  
  # Select best model based on AIC
  if (length(garch_results) > 0) {
    aics <- sapply(garch_results, function(x) x$aic)
    best_model <- names(aics)[which.min(aics)]
    cat("Best GARCH model:", best_model, "with AIC =", round(min(aics), 4), "\n")
    
    return(list(
      models = garch_results,
      best_model = best_model,
      best_fit = garch_results[[best_model]]
    ))
  } else {
    cat("No GARCH models converged successfully\n")
    return(NULL)
  }
}

evaluate_garch_model <- function(garch_results, test_data, target_col) {
  if (is.null(garch_results)) {
    return(list(metrics = NULL, predictions = NULL))
  }
  
  # Prepare test data
  test_ts <- test_data %>%
    arrange(date) %>%
    filter(!is.na(!!sym(target_col))) %>%
    pull(!!sym(target_col))
  
  if (length(test_ts) == 0) {
    return(list(metrics = NULL, predictions = NULL))
  }
  
  best_fit <- garch_results$best_fit$fit
  
  tryCatch({
    # Forecast using the best GARCH model with improved settings
    n_ahead <- min(length(test_ts), 50)  # Limit forecast horizon
    forecasts <- ugarchforecast(best_fit, n.ahead = n_ahead)
    
    # Extract fitted values (point forecasts)
    predictions <- as.numeric(fitted(forecasts))
    
    # If we don't have enough forecasts, use the mean
    if (length(predictions) < length(test_ts)) {
      mean_forecast <- mean(predictions, na.rm = TRUE)
      if (is.na(mean_forecast)) {
        mean_forecast <- mean(test_ts, na.rm = TRUE)
      }
      predictions <- rep(mean_forecast, length(test_ts))
    } else if (length(predictions) > length(test_ts)) {
      predictions <- predictions[1:length(test_ts)]
    }
    
    # Handle constant predictions (causes correlation issues)
    if (var(predictions, na.rm = TRUE) == 0) {
      correlation <- 0
      r2 <- 0
    } else {
      correlation <- cor(test_ts, predictions, use = "complete.obs")
      if (is.na(correlation)) correlation <- 0
      
      # Calculate R² properly
      y_mean <- mean(test_ts, na.rm = TRUE)
      sse <- sum((test_ts - predictions)^2, na.rm = TRUE)
      tss <- sum((test_ts - y_mean)^2, na.rm = TRUE)
      r2 <- ifelse(tss == 0, 0, max(0, 1 - (sse / tss)))
    }
    
    # Calculate metrics
    metrics <- list(
      rmse = sqrt(mean((test_ts - predictions)^2, na.rm = TRUE)),
      mae = mean(abs(test_ts - predictions), na.rm = TRUE),
      r2 = r2,
      correlation = correlation,
      model_type = garch_results$best_model
    )
    
    return(list(
      metrics = metrics,
      predictions = predictions,
      actual = test_ts,
      model_info = garch_results$best_model
    ))
    
  }, error = function(e) {
    cat("Error in GARCH evaluation:", e$message, "\n")
    
    # Fallback: use simple mean forecast
    mean_forecast <- mean(test_ts, na.rm = TRUE)
    predictions <- rep(mean_forecast, length(test_ts))
    
    metrics <- list(
      rmse = sqrt(mean((test_ts - predictions)^2, na.rm = TRUE)),
      mae = mean(abs(test_ts - predictions), na.rm = TRUE),
      r2 = 0,
      correlation = 0,
      model_type = "Mean_Forecast"
    )
    
    return(list(
      metrics = metrics,
      predictions = predictions,
      actual = test_ts,
      model_info = "Mean_Forecast"
    ))
  })
}

# Enhanced GARCH with external regressors (GARCH-X) - with better error handling
fit_garch_with_sentiment <- function(train_data, feature_cols, target_col) {
  cat("Fitting GARCH-X models with sentiment variables...\n")
  
  # Prepare data
  model_data <- prepare_model_data(train_data, feature_cols, target_col)
  
  if (model_data$n < 100) {
    cat("Insufficient data for GARCH-X model\n")
    return(NULL)
  }
  
  # Check for constant variance
  if (var(model_data$y, na.rm = TRUE) == 0) {
    cat("Zero variance in target series - cannot fit GARCH-X\n")
    return(NULL)
  }
  
  # Create external regressor matrix (limit to most important features to avoid overfitting)
  if (ncol(model_data$X) > 10) {
    # Use only the first 10 features to avoid convergence issues
    external_regressors <- as.matrix(model_data$X[, 1:10])
    cat("  Using first 10 features to avoid convergence issues\n")
  } else {
    external_regressors <- as.matrix(model_data$X)
  }
  
  ts_data <- model_data$y
  
  garchx_results <- list()
  
  # Try GARCH(1,1) with external regressors (simplified to avoid convergence issues)
  for (spec_name in c("GARCH-X(1,1)")) {  # Only try standard GARCH-X
    cat("Fitting", spec_name, "with", ncol(external_regressors), "external regressors...\n")
    
    tryCatch({
      # Create specification with external regressors
      garch_spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, 
                          external.regressors = external_regressors),
        distribution.model = "norm"
      )
      
      # Fit model with robust settings
      garch_fit <- ugarchfit(
        garch_spec, 
        ts_data,
        solver = "hybrid",
        solver.control = list(trace = 0, maxfeval = 1000)
      )
      
      # Check convergence
      if (convergence(garch_fit) == 0) {
        # Store results
        garchx_results[[spec_name]] <- list(
          spec = garch_spec,
          fit = garch_fit,
          aic = infocriteria(garch_fit)[1],
          bic = infocriteria(garch_fit)[2],
          loglik = likelihood(garch_fit),
          features = colnames(external_regressors)
        )
        
        cat("  AIC:", round(garchx_results[[spec_name]]$aic, 4), 
            "BIC:", round(garchx_results[[spec_name]]$bic, 4), "\n")
      } else {
        cat("  Failed to converge\n")
      }
      
    }, error = function(e) {
      cat("  Error fitting", spec_name, ":", e$message, "\n")
    })
  }
  
  # Select best model
  if (length(garchx_results) > 0) {
    aics <- sapply(garchx_results, function(x) x$aic)
    best_model <- names(aics)[which.min(aics)]
    cat("Best GARCH-X model:", best_model, "with AIC =", round(min(aics), 4), "\n")
    
    return(list(
      models = garchx_results,
      best_model = best_model,
      best_fit = garchx_results[[best_model]]
    ))
  } else {
    cat("No GARCH-X models converged successfully\n")
    return(NULL)
  }
}

evaluate_garch_with_sentiment <- function(garchx_results, test_data, feature_cols, target_col) {
  if (is.null(garchx_results)) {
    return(list(metrics = NULL, predictions = NULL))
  }
  
  # Prepare test data
  model_data <- prepare_model_data(test_data, feature_cols, target_col)
  
  if (model_data$n == 0) {
    return(list(metrics = NULL, predictions = NULL))
  }
  
  best_fit <- garchx_results$best_fit$fit
  
  # Match the features used in training
  used_features <- garchx_results$best_fit$features
  test_regressors <- as.matrix(model_data$X[, used_features, drop = FALSE])
  test_ts <- model_data$y
  
  tryCatch({
    # Limit forecast horizon
    n_ahead <- min(nrow(test_regressors), 20)
    test_regressors_limited <- test_regressors[1:n_ahead, , drop = FALSE]
    
    # Forecast with external regressors
    forecasts <- ugarchforecast(
      best_fit, 
      n.ahead = n_ahead, 
      external.forecasts = list(mregfor = test_regressors_limited)
    )
    
    # Extract forecasts
    predictions <- as.numeric(fitted(forecasts))
    
    # Extend predictions to match test data length
    if (length(predictions) < length(test_ts)) {
      mean_pred <- mean(predictions, na.rm = TRUE)
      if (is.na(mean_pred)) mean_pred <- mean(test_ts, na.rm = TRUE)
      predictions <- c(predictions, rep(mean_pred, length(test_ts) - length(predictions)))
    } else if (length(predictions) > length(test_ts)) {
      predictions <- predictions[1:length(test_ts)]
    }
    
    # Handle constant predictions
    if (var(predictions, na.rm = TRUE) == 0) {
      correlation <- 0
      r2 <- 0
    } else {
      correlation <- cor(test_ts, predictions, use = "complete.obs")
      if (is.na(correlation)) correlation <- 0
      
      # Calculate R²
      y_mean <- mean(test_ts, na.rm = TRUE)
      sse <- sum((test_ts - predictions)^2, na.rm = TRUE)
      tss <- sum((test_ts - y_mean)^2, na.rm = TRUE)
      r2 <- ifelse(tss == 0, 0, max(0, 1 - (sse / tss)))
    }
    
    # Calculate metrics
    metrics <- list(
      rmse = sqrt(mean((test_ts - predictions)^2, na.rm = TRUE)),
      mae = mean(abs(test_ts - predictions), na.rm = TRUE),
      r2 = r2,
      correlation = correlation,
      model_type = garchx_results$best_model
    )
    
    return(list(
      metrics = metrics,
      predictions = predictions,
      actual = test_ts,
      model_info = garchx_results$best_model
    ))
    
  }, error = function(e) {
    cat("Error in GARCH-X evaluation:", e$message, "\n")
    
    # Fallback
    mean_forecast <- mean(test_ts, na.rm = TRUE)
    predictions <- rep(mean_forecast, length(test_ts))
    
    metrics <- list(
      rmse = sqrt(mean((test_ts - predictions)^2, na.rm = TRUE)),
      mae = mean(abs(test_ts - predictions), na.rm = TRUE),
      r2 = 0,
      correlation = 0,
      model_type = "Mean_Forecast"
    )
    
    return(list(
      metrics = metrics,
      predictions = predictions,
      actual = test_ts,
      model_info = "Mean_Forecast"
    ))
  })
}

# =============================================================================
# HYPERPARAMETER TUNING
# =============================================================================

tune_rf_simple <- function(train_data, feature_cols, target_col) {
  cat("Tuning Random Forest hyperparameters...\n")
  
  model_data <- prepare_model_data(train_data, feature_cols, target_col)
  if (model_data$n < 50) {
    cat("Insufficient data for tuning\n")
    return(list(best_params = list(ntree = 500, mtry = 5, nodesize = 1, maxnodes = NULL)))
  }
  
  best_oob_error <- Inf
  best_params <- NULL
  
  param_combinations <- expand.grid(
    ntree = config$tune_config$rf$ntree,
    mtry = pmin(config$tune_config$rf$mtry, ncol(model_data$X)),
    nodesize = config$tune_config$rf$nodesize,
    maxnodes = config$tune_config$rf$maxnodes,
    stringsAsFactors = FALSE
  )
  
  cat("Testing", nrow(param_combinations), "parameter combinations\n")
  
  for (i in 1:nrow(param_combinations)) {
    params <- param_combinations[i, ]
    
    tryCatch({
      rf_model <- randomForest(
        x = model_data$X,
        y = model_data$y,
        ntree = params$ntree,
        mtry = params$mtry,
        nodesize = params$nodesize,
        maxnodes = if(is.na(params$maxnodes)) NULL else params$maxnodes
      )
      
      oob_error <- tail(rf_model$mse, 1)
      
      if (oob_error < best_oob_error) {
        best_oob_error <- oob_error
        best_params <- params
      }
    }, error = function(e) {
      cat("Error with params", i, ":", e$message, "\n")
    })
    
    if (i %% 10 == 0) {
      cat("Completed", i, "of", nrow(param_combinations), "combinations\n")
    }
  }
  
  cat("Best RF OOB Error:", sqrt(best_oob_error), "\n")
  return(list(best_params = best_params, oob_error = best_oob_error))
}

tune_xgb_simple <- function(train_data, feature_cols, target_col) {
  cat("Tuning XGBoost hyperparameters...\n")
  
  model_data <- prepare_model_data(train_data, feature_cols, target_col)
  if (model_data$n < 50) {
    cat("Insufficient data for tuning\n")
    return(list(best_params = list(nrounds = 100, max_depth = 6, eta = 0.1, 
                                   subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 1)))
  }
  
  X_matrix <- as.matrix(model_data$X)
  
  best_test_error <- Inf
  best_params <- NULL
  
  param_combinations <- expand.grid(
    nrounds = config$tune_config$xgb$nrounds,
    max_depth = config$tune_config$xgb$max_depth,
    eta = config$tune_config$xgb$eta,
    subsample = config$tune_config$xgb$subsample,
    colsample_bytree = config$tune_config$xgb$colsample_bytree,
    min_child_weight = config$tune_config$xgb$min_child_weight,
    stringsAsFactors = FALSE
  )
  
  cat("Testing", nrow(param_combinations), "parameter combinations\n")
  
  for (i in 1:nrow(param_combinations)) {
    params <- param_combinations[i, ]
    
    tryCatch({
      cv_result <- xgb.cv(
        data = X_matrix,
        label = model_data$y,
        nrounds = params$nrounds,
        max_depth = params$max_depth,
        eta = params$eta,
        subsample = params$subsample,
        colsample_bytree = params$colsample_bytree,
        min_child_weight = params$min_child_weight,
        nfold = 5,
        verbose = 0,
        early_stopping_rounds = 10
      )
      
      test_error <- min(cv_result$evaluation_log$test_rmse_mean)
      
      if (test_error < best_test_error) {
        best_test_error <- test_error
        best_params <- params
      }
    }, error = function(e) {
      cat("Error with params", i, ":", e$message, "\n")
    })
    
    if (i %% 10 == 0) {
      cat("Completed", i, "of", nrow(param_combinations), "combinations\n")
    }
  }
  
  cat("Best XGB CV Error:", best_test_error, "\n")
  return(list(best_params = best_params, cv_error = best_test_error))
}

# =============================================================================
# MODEL TRAINING & EVALUATION
# =============================================================================

# Feature importance extraction function
extract_feature_importance <- function(model, model_type, feature_names) {
  if (is.null(model)) {
    return(NULL)
  }
  
  if (model_type == "rf") {
    tryCatch({
      importance_matrix <- importance(model)
      if (ncol(importance_matrix) >= 2) {
        # Use %IncMSE (increase in MSE when variable is permuted)
        importance_values <- importance_matrix[, "%IncMSE"]
      } else {
        importance_values <- importance_matrix[, 1]
      }
      
      feature_importance <- data.frame(
        Feature = names(importance_values),
        Importance = as.numeric(importance_values),
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Importance))
      
      return(feature_importance)
    }, error = function(e) {
      cat("Error extracting RF importance:", e$message, "\n")
      return(NULL)
    })
    
  } else if (model_type == "xgb") {
    tryCatch({
      importance_matrix <- xgb.importance(model = model)
      feature_importance <- data.frame(
        Feature = importance_matrix$Feature,
        Importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Importance))
      
      return(feature_importance)
    }, error = function(e) {
      cat("Error extracting XGBoost importance:", e$message, "\n")
      return(NULL)
    })
  }
  
  return(NULL)
}

# =============================================================================
# TIME SERIES CROSS-VALIDATION FUNCTIONS (ADD THESE)
# =============================================================================

# Quick TS CV configuration for VIX
quick_ts_cv_config <- list(
  initial_window = 200,     # 40% for initial training
  step_size = 30,          # Move forward by 30 obs each time
  n_splits = 6,            # 6 validation windows
  purge_length = 1,        # 1-day gap to prevent lookahead
  min_test_size = 20       # Minimum 20 observations for each test
)

# Generate CV splits
generate_cv_splits <- function(data, config) {
  n_obs <- nrow(data)
  splits <- list()
  
  current_start <- config$initial_window
  
  for (i in 1:config$n_splits) {
    # Rolling window: fixed size training window
    train_start <- max(1, current_start - config$initial_window + 1)
    train_end <- current_start
    
    # Test window
    test_start <- train_end + config$purge_length + 1
    test_end <- min(test_start + config$min_test_size - 1, n_obs)
    
    # Check if we have enough data
    if (test_end > test_start && train_end > train_start && test_end <= n_obs) {
      splits[[length(splits) + 1]] <- list(
        train_indices = train_start:train_end,
        test_indices = test_start:test_end,
        split_id = i
      )
    }
    
    # Move window forward
    current_start <- current_start + config$step_size
    
    # Stop if we can't create more valid splits
    if (current_start + config$min_test_size > n_obs) break
  }
  
  return(splits)
}

# Run single split for time series CV
# Run single split for time series CV with hyperparameter tuning
run_single_split_vix <- function(train_data, test_data, feature_cols, target_col) {
  
  results <- list()
  
  # Random Forest WITH HYPERPARAMETER TUNING
  tryCatch({
    cat("    Tuning RF hyperparameters...\n")
    rf_params <- tune_hyperparams_quick_vix(train_data, feature_cols, target_col, "rf", n_trials = 15)
    
    rf_result <- train_and_evaluate_quick_vix(train_data, test_data, feature_cols, 
                                              target_col, "rf", rf_params)
    results$rf <- rf_result
    
    if (!is.null(rf_result)) {
      cat("    RF best params: ntree =", rf_params$ntree, ", mtry =", rf_params$mtry, 
          ", nodesize =", rf_params$nodesize, "\n")
    }
    
  }, error = function(e) {
    cat("    RF error:", e$message, "\n")
    results$rf <- NULL
  })
  
  # XGBoost WITH HYPERPARAMETER TUNING
  tryCatch({
    cat("    Tuning XGB hyperparameters...\n")
    xgb_params <- tune_hyperparams_quick_vix(train_data, feature_cols, target_col, "xgb", n_trials = 15)
    
    xgb_result <- train_and_evaluate_quick_vix(train_data, test_data, feature_cols, 
                                               target_col, "xgb", xgb_params)
    results$xgb <- xgb_result
    
    if (!is.null(xgb_result)) {
      cat("    XGB best params: nrounds =", xgb_params$nrounds, ", max_depth =", xgb_params$max_depth, 
          ", eta =", xgb_params$eta, "\n")
    }
    
  }, error = function(e) {
    cat("    XGB error:", e$message, "\n")
    results$xgb <- NULL
  })
  
  return(results)
}

# Add the hyperparameter tuning function for VIX
tune_hyperparams_quick_vix <- function(train_data, feature_cols, target_col, model_type, n_trials = 15) {
  
  # Prepare data
  train_features <- train_data[, feature_cols, drop = FALSE]
  train_target <- train_data[[target_col]]
  
  # Remove missing values
  train_complete <- complete.cases(train_features, train_target)
  train_features <- train_features[train_complete, , drop = FALSE]
  train_target <- train_target[train_complete]
  
  if (nrow(train_features) < 50) {
    # Use default params for small datasets
    if (model_type == "rf") {
      return(list(ntree = 500, mtry = max(2, ncol(train_features) %/% 3), 
                  nodesize = 3, maxnodes = NULL))
    } else {
      return(list(nrounds = 100, max_depth = 6, eta = 0.1, 
                  subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 1))
    }
  }
  
  best_error <- Inf
  best_params <- NULL
  
  # Enhanced parameter grids
  if (model_type == "rf") {
    # Random Forest tuning
    param_grid <- expand.grid(
      ntree = c(300, 500, 750, 1000),                    # 4 options
      mtry = c(max(2, ncol(train_features) %/% 4),       # 3 options
               max(3, ncol(train_features) %/% 3),
               max(4, ncol(train_features) %/% 2)),
      nodesize = c(1, 3, 5),                             # 3 options  
      maxnodes = c(NULL, 50, 150, 300),                  # 4 options
      stringsAsFactors = FALSE
    )
    
    # Sample random combinations to keep it fast
    if (nrow(param_grid) > n_trials) {
      param_indices <- sample(nrow(param_grid), n_trials)
      param_grid <- param_grid[param_indices, ]
    }
    
    for (i in 1:nrow(param_grid)) {
      params <- param_grid[i, ]
      
      tryCatch({
        rf_model <- randomForest(
          x = train_features,
          y = train_target,
          ntree = params$ntree,
          mtry = params$mtry,
          nodesize = params$nodesize,
          maxnodes = if(is.null(params$maxnodes) || is.na(params$maxnodes)) NULL else params$maxnodes
        )
        
        oob_error <- tail(rf_model$mse, 1)
        
        if (oob_error < best_error) {
          best_error <- oob_error
          best_params <- params
        }
        
      }, error = function(e) {
        # Skip this parameter combination
      })
    }
    
  } else if (model_type == "xgb") {
    # XGBoost tuning
    param_grid <- expand.grid(
      nrounds = c(50, 100, 200, 300),                    # 4 options
      max_depth = c(3, 4, 6, 8),                         # 4 options
      eta = c(0.01, 0.05, 0.1, 0.2),                     # 4 options
      subsample = c(0.7, 0.8, 0.9),                      # 3 options
      colsample_bytree = c(0.7, 0.8, 0.9),               # 3 options
      min_child_weight = c(1, 3, 5),                     # 3 options
      stringsAsFactors = FALSE
    )
    
    # Sample random combinations
    if (nrow(param_grid) > n_trials) {
      param_indices <- sample(nrow(param_grid), n_trials)
      param_grid <- param_grid[param_indices, ]
    }
    
    for (i in 1:nrow(param_grid)) {
      params <- param_grid[i, ]
      
      tryCatch({
        cv_result <- xgb.cv(
          data = as.matrix(train_features),
          label = train_target,
          nrounds = params$nrounds,
          max_depth = params$max_depth,
          eta = params$eta,
          subsample = params$subsample,
          colsample_bytree = params$colsample_bytree,
          min_child_weight = params$min_child_weight,
          nfold = 3,  # Faster 3-fold CV
          verbose = 0,
          early_stopping_rounds = 10
        )
        
        test_error <- min(cv_result$evaluation_log$test_rmse_mean)
        
        if (test_error < best_error) {
          best_error <- test_error
          best_params <- params
          # Use early stopping result
          best_params$nrounds <- which.min(cv_result$evaluation_log$test_rmse_mean)
        }
        
      }, error = function(e) {
        # Skip this parameter combination
      })
    }
  }
  
  if (is.null(best_params)) {
    # Fallback to default
    if (model_type == "rf") {
      best_params <- list(ntree = 500, mtry = max(2, ncol(train_features) %/% 3), 
                          nodesize = 3, maxnodes = NULL)
    } else {
      best_params <- list(nrounds = 100, max_depth = 6, eta = 0.1, 
                          subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 1)
    }
  }
  
  return(best_params)
}

# Quick training and evaluation function for VIX
train_and_evaluate_quick_vix <- function(train_data, test_data, feature_cols, target_col, 
                                         model_type, params) {
  
  # Prepare data
  train_features <- train_data[, feature_cols, drop = FALSE]
  train_target <- train_data[[target_col]]
  
  test_features <- test_data[, feature_cols, drop = FALSE]
  test_target <- test_data[[target_col]]
  
  # Remove missing values
  train_complete <- complete.cases(train_features, train_target)
  test_complete <- complete.cases(test_features, test_target)
  
  train_features <- train_features[train_complete, , drop = FALSE]
  train_target <- train_target[train_complete]
  test_features <- test_features[test_complete, , drop = FALSE]
  test_target <- test_target[test_complete]
  
  if (nrow(train_features) < 20 || nrow(test_features) < 5) {
    return(NULL)
  }
  
  # Train model
  if (model_type == "rf") {
    model <- randomForest(
      x = train_features,
      y = train_target,
      ntree = params$ntree,
      mtry = params$mtry,
      nodesize = params$nodesize,
      maxnodes = params$maxnodes
    )
    predictions <- predict(model, test_features)
    
  } else if (model_type == "xgb") {
    model <- xgboost(
      data = as.matrix(train_features),
      label = train_target,
      nrounds = params$nrounds,
      max_depth = params$max_depth,
      eta = params$eta,
      subsample = params$subsample,
      colsample_bytree = params$colsample_bytree,
      min_child_weight = params$min_child_weight,
      verbose = 0
    )
    predictions <- predict(model, as.matrix(test_features))
  }
  
  # Calculate metrics
  valid_indices <- !is.na(test_target) & !is.na(predictions)
  actual_clean <- test_target[valid_indices]
  pred_clean <- predictions[valid_indices]
  
  if (length(actual_clean) == 0) {
    return(NULL)
  }
  
  # Calculate errors for Diebold-Mariano test
  prediction_errors <- actual_clean - pred_clean
  
  # Calculate standard metrics
  rmse <- sqrt(mean((actual_clean - pred_clean)^2))
  mae <- mean(abs(actual_clean - pred_clean))
  correlation <- cor(actual_clean, pred_clean, use = "complete.obs")
  
  # R² calculation
  y_mean <- mean(actual_clean)
  sse <- sum((actual_clean - pred_clean)^2)
  tss <- sum((actual_clean - y_mean)^2)
  r2 <- ifelse(tss == 0, ifelse(sse == 0, 1.0, 0.0), 1 - (sse / tss))
  
  # Directional accuracy
  actual_direction <- sign(actual_clean)
  pred_direction <- sign(pred_clean)
  directional_accuracy <- mean(actual_direction == pred_direction, na.rm = TRUE)
  
  return(list(
    metrics = list(
      rmse = rmse,
      mae = mae,
      r2 = r2,
      correlation = correlation,
      directional_accuracy = directional_accuracy
    ),
    predictions = predictions,
    actual = test_target,
    prediction_errors = prediction_errors,  # Store errors for DM test
    n_predictions = length(actual_clean)
  ))
}

# Aggregate CV results across splits
aggregate_cv_results_vix <- function(cv_results, experiment_name) {
  
  # Extract metrics for each model type
  rf_metrics <- data.frame()
  xgb_metrics <- data.frame()
  
  # Store all prediction errors for DM test
  rf_all_errors <- list()
  xgb_all_errors <- list()
  
  for (i in seq_along(cv_results)) {
    split_result <- cv_results[[i]]
    
    if (!is.null(split_result$rf) && !is.null(split_result$rf$metrics)) {
      rf_metrics <- rbind(rf_metrics, data.frame(
        split_id = i,
        split_result$rf$metrics,
        stringsAsFactors = FALSE
      ))
      
      # Store prediction errors
      if (!is.null(split_result$rf$prediction_errors)) {
        rf_all_errors[[i]] <- split_result$rf$prediction_errors
      }
    }
    
    if (!is.null(split_result$xgb) && !is.null(split_result$xgb$metrics)) {
      xgb_metrics <- rbind(xgb_metrics, data.frame(
        split_id = i,
        split_result$xgb$metrics,
        stringsAsFactors = FALSE
      ))
      
      # Store prediction errors
      if (!is.null(split_result$xgb$prediction_errors)) {
        xgb_all_errors[[i]] <- split_result$xgb$prediction_errors
      }
    }
  }
  
  # Calculate summary statistics with standard deviations
  rf_summary <- if(nrow(rf_metrics) > 0) {
    list(
      mean_rmse = mean(rf_metrics$rmse, na.rm = TRUE),
      std_rmse = sd(rf_metrics$rmse, na.rm = TRUE),  # ADD THIS
      mean_r2 = mean(rf_metrics$r2, na.rm = TRUE),
      std_r2 = sd(rf_metrics$r2, na.rm = TRUE),
      mean_correlation = mean(rf_metrics$correlation, na.rm = TRUE),
      mean_directional_accuracy = mean(rf_metrics$directional_accuracy, na.rm = TRUE),
      n_valid_splits = nrow(rf_metrics),
      all_prediction_errors = rf_all_errors  # ADD THIS
    )
  } else {
    list(mean_rmse = NA, std_rmse = NA, mean_r2 = NA, std_r2 = NA, 
         mean_correlation = NA, mean_directional_accuracy = NA, n_valid_splits = 0,
         all_prediction_errors = list())
  }
  
  xgb_summary <- if(nrow(xgb_metrics) > 0) {
    list(
      mean_rmse = mean(xgb_metrics$rmse, na.rm = TRUE),
      std_rmse = sd(xgb_metrics$rmse, na.rm = TRUE),  # ADD THIS
      mean_r2 = mean(xgb_metrics$r2, na.rm = TRUE),
      std_r2 = sd(xgb_metrics$r2, na.rm = TRUE),
      mean_correlation = mean(xgb_metrics$correlation, na.rm = TRUE),
      mean_directional_accuracy = mean(xgb_metrics$directional_accuracy, na.rm = TRUE),
      n_valid_splits = nrow(xgb_metrics),
      all_prediction_errors = xgb_all_errors  # ADD THIS
    )
  } else {
    list(mean_rmse = NA, std_rmse = NA, mean_r2 = NA, std_r2 = NA, 
         mean_correlation = NA, mean_directional_accuracy = NA, n_valid_splits = 0,
         all_prediction_errors = list())
  }
  
  return(list(
    experiment_name = experiment_name,
    rf_summary = rf_summary,
    xgb_summary = xgb_summary,
    rf_details = rf_metrics,
    xgb_details = xgb_metrics,
    cv_results = cv_results
  ))
}

# Create summary table with standard deviations
create_cv_summary_table_vix <- function(all_cv_results) {
  
  summary_df <- data.frame()
  
  for (exp_name in names(all_cv_results)) {
    exp_result <- all_cv_results[[exp_name]]
    
    # RF row
    if (!is.na(exp_result$rf_summary$mean_rmse)) {
      summary_df <- rbind(summary_df, data.frame(
        Experiment = exp_name,
        Model = "Random Forest",
        Mean_RMSE = exp_result$rf_summary$mean_rmse,
        Std_RMSE = exp_result$rf_summary$std_rmse,  # ADD THIS
        Mean_R2 = exp_result$rf_summary$mean_r2,
        Std_R2 = exp_result$rf_summary$std_r2,
        Mean_Correlation = exp_result$rf_summary$mean_correlation,
        Mean_Dir_Accuracy = exp_result$rf_summary$mean_directional_accuracy,
        N_Splits = exp_result$rf_summary$n_valid_splits,
        stringsAsFactors = FALSE
      ))
    }
    
    # XGB row
    if (!is.na(exp_result$xgb_summary$mean_rmse)) {
      summary_df <- rbind(summary_df, data.frame(
        Experiment = exp_name,
        Model = "XGBoost",
        Mean_RMSE = exp_result$xgb_summary$mean_rmse,
        Std_RMSE = exp_result$xgb_summary$std_rmse,  # ADD THIS
        Mean_R2 = exp_result$xgb_summary$mean_r2,
        Std_R2 = exp_result$xgb_summary$std_r2,
        Mean_Correlation = exp_result$xgb_summary$mean_correlation,
        Mean_Dir_Accuracy = exp_result$xgb_summary$mean_directional_accuracy,
        N_Splits = exp_result$xgb_summary$n_valid_splits,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(summary_df)
}

# Individual sentiment analysis function for VIX
analyze_individual_sentiment_results_vix <- function(all_results) {
  
  cat("Analyzing individual sentiment performance for VIX prediction...\n")
  
  # Extract individual sentiment results
  individual_experiments <- names(all_results)[grepl("^individual_", names(all_results))]
  
  if (length(individual_experiments) == 0) {
    cat("No individual sentiment experiments found.\n")
    return(NULL)
  }
  
  individual_summary <- data.frame()
  
  for (exp_name in individual_experiments) {
    exp_result <- all_results[[exp_name]]
    
    # Extract sentiment variable name
    sentiment_var <- gsub("^individual_", "", exp_name)
    
    # RF results
    if (!is.na(exp_result$rf_summary$mean_rmse)) {
      individual_summary <- rbind(individual_summary, data.frame(
        Sentiment_Variable = sentiment_var,
        Model = "Random Forest",
        Mean_RMSE = exp_result$rf_summary$mean_rmse,
        Std_RMSE = exp_result$rf_summary$std_rmse,
        Mean_R2 = exp_result$rf_summary$mean_r2,
        Std_R2 = exp_result$rf_summary$std_r2,
        Mean_Correlation = exp_result$rf_summary$mean_correlation,
        N_Splits = exp_result$rf_summary$n_valid_splits,
        stringsAsFactors = FALSE
      ))
    }
    
    # XGBoost results
    if (!is.na(exp_result$xgb_summary$mean_rmse)) {
      individual_summary <- rbind(individual_summary, data.frame(
        Sentiment_Variable = sentiment_var,
        Model = "XGBoost",
        Mean_RMSE = exp_result$xgb_summary$mean_rmse,
        Std_RMSE = exp_result$xgb_summary$std_rmse,
        Mean_R2 = exp_result$xgb_summary$mean_r2,
        Std_R2 = exp_result$xgb_summary$std_r2,
        Mean_Correlation = exp_result$xgb_summary$mean_correlation,
        N_Splits = exp_result$xgb_summary$n_valid_splits,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(individual_summary) > 0) {
    cat("\nINDIVIDUAL SENTIMENT VARIABLE RANKINGS FOR VIX:\n")
    cat(rep("=", 60), "\n")
    
    # Rank by RMSE (lower is better)
    individual_ranked <- individual_summary %>%
      dplyr::arrange(Mean_RMSE) %>%
      mutate(Rank = row_number())
    
    print(individual_ranked %>% 
            select(Rank, Sentiment_Variable, Model, Mean_RMSE, Std_RMSE, Mean_R2) %>%
            head(10))
    
    # Best performer by model type
    best_rf <- individual_ranked %>% 
      filter(Model == "Random Forest") %>% 
      slice_min(Mean_RMSE, n = 1)
    
    best_xgb <- individual_ranked %>% 
      filter(Model == "XGBoost") %>% 
      slice_min(Mean_RMSE, n = 1)
    
    cat("\n🏆 BEST INDIVIDUAL SENTIMENT VARIABLES FOR VIX:\n")
    if (nrow(best_rf) > 0) {
      cat("Random Forest:", best_rf$Sentiment_Variable[1], 
          "- RMSE:", round(best_rf$Mean_RMSE[1], 4), "\n")
    }
    if (nrow(best_xgb) > 0) {
      cat("XGBoost:", best_xgb$Sentiment_Variable[1], 
          "- RMSE:", round(best_xgb$Mean_RMSE[1], 4), "\n")
    }
    
    # Save detailed individual results
    write_csv(individual_ranked, file.path(config$output_dir, "individual_sentiment_rankings_vix.csv"))
    cat("\n📊 Detailed individual sentiment rankings saved to: individual_sentiment_rankings_vix.csv\n")
  }
  
  return(individual_summary)
}

train_model <- function(train_data, feature_cols, target_col, model_type, best_params) {
  model_data <- prepare_model_data(train_data, feature_cols, target_col)
  
  if (model_data$n < 10) {
    warning("Insufficient data for training")
    return(NULL)
  }
  
  if (model_type == "rf") {
    model <- randomForest(
      x = model_data$X,
      y = model_data$y,
      ntree = best_params$ntree,
      mtry = best_params$mtry,
      nodesize = best_params$nodesize,
      maxnodes = if(is.na(best_params$maxnodes)) NULL else best_params$maxnodes,
      importance = TRUE  # Enable importance calculation
    )
  } else if (model_type == "xgb") {
    model <- xgboost(
      data = as.matrix(model_data$X),
      label = model_data$y,
      nrounds = best_params$nrounds,
      max_depth = best_params$max_depth,
      eta = best_params$eta,
      subsample = best_params$subsample,
      colsample_bytree = best_params$colsample_bytree,
      min_child_weight = best_params$min_child_weight,
      verbose = 0
    )
  }
  
  return(model)
}

evaluate_model <- function(model, test_data, feature_cols, target_col, model_type) {
  if (is.null(model)) {
    return(list(metrics = NULL, predictions = NULL, importance = NULL))
  }
  
  model_data <- prepare_model_data(test_data, feature_cols, target_col)
  
  if (model_data$n == 0) {
    return(list(metrics = NULL, predictions = NULL, importance = NULL))
  }
  
  # Make predictions
  if (model_type == "rf") {
    predictions <- predict(model, model_data$X)
  } else if (model_type == "xgb") {
    predictions <- predict(model, as.matrix(model_data$X))
  }
  
  # Remove any NA values
  valid_indices <- !is.na(model_data$y) & !is.na(predictions)
  actual_clean <- model_data$y[valid_indices]
  pred_clean <- predictions[valid_indices]
  
  if (length(actual_clean) == 0) {
    return(list(metrics = NULL, predictions = NULL, importance = NULL))
  }
  
  # Calculate metrics
  y_mean <- mean(actual_clean)
  sse <- sum((actual_clean - pred_clean)^2)
  tss <- sum((actual_clean - y_mean)^2)
  
  # Handle case where TSS is zero
  if (tss == 0) {
    r2_correct <- ifelse(sse == 0, 1.0, 0.0)
  } else {
    r2_correct <- 1 - (sse / tss)
  }
  
  rmse <- sqrt(mean((actual_clean - pred_clean)^2))
  mae <- mean(abs(actual_clean - pred_clean))
  correlation <- cor(actual_clean, pred_clean, use = "complete.obs")
  if (is.na(correlation)) correlation <- 0
  
  # Directional accuracy
  actual_direction <- sign(actual_clean)
  pred_direction <- sign(pred_clean)
  directional_accuracy <- mean(actual_direction == pred_direction, na.rm = TRUE)
  
  metrics <- list(
    rmse = rmse,
    mae = mae,
    r2 = r2_correct,
    correlation = correlation,
    directional_accuracy = directional_accuracy,
    n_predictions = length(actual_clean)
  )
  
  # Extract feature importance
  importance <- extract_feature_importance(model, model_type, feature_cols)
  
  return(list(
    metrics = metrics,
    predictions = predictions,
    actual = model_data$y,
    importance = importance
  ))
}

# =============================================================================
# CHECKPOINT SYSTEM
# =============================================================================

save_checkpoint <- function(object, filename, checkpoint_dir = config$checkpoint_dir) {
  filepath <- file.path(checkpoint_dir, paste0(filename, "_", Sys.Date(), ".rds"))
  saveRDS(object, filepath)
  cat("Checkpoint saved:", filepath, "\n")
  return(filepath)
}

load_checkpoint <- function(filename, checkpoint_dir = config$checkpoint_dir) {
  pattern <- paste0(filename, "_.*\\.rds$")
  files <- list.files(checkpoint_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) > 0) {
    latest_file <- files[which.max(file.mtime(files))]
    cat("Loading checkpoint:", latest_file, "\n")
    return(readRDS(latest_file))
  } else {
    return(NULL)
  }
}

experiment_exists <- function(experiment_name) {
  checkpoint_file <- paste0("experiment_", experiment_name)
  result <- load_checkpoint(checkpoint_file)
  return(!is.null(result))
}

# =============================================================================
# EXPERIMENT EXECUTION
# =============================================================================

run_single_experiment <- function(experiment_name, train_data, test_data, feature_cols) {
  cat("\n", rep("=", 50), "\n")
  cat("VIX EXPERIMENT:", experiment_name, "\n")
  cat(rep("=", 50), "\n")
  
  # Check if experiment already exists
  if (experiment_exists(experiment_name)) {
    cat("Experiment already completed. Loading from checkpoint...\n")
    return(load_checkpoint(paste0("experiment_", experiment_name)))
  }
  
  # Handle NULL feature_cols for GARCH experiments
  if (is.null(feature_cols)) {
    cat("GARCH-only experiment\n")
  } else {
    cat("Using", length(feature_cols), "features\n")
  }
  
  # Initialize results
  experiment_results <- list(
    experiment_name = experiment_name,
    features = feature_cols,
    n_features = ifelse(is.null(feature_cols), 0, length(feature_cols))
  )
  
  # For GARCH experiments, fit GARCH models
  if (experiment_name == "garch_baseline") {
    cat("\nFitting GARCH baseline models...\n")
    tic("GARCH baseline")
    garch_results <- fit_garch_baseline(train_data, "VIX_chg_next_day")
    toc()
    
    if (!is.null(garch_results)) {
      garch_eval <- evaluate_garch_model(garch_results, test_data, "VIX_chg_next_day")
      experiment_results$garch <- list(
        models = garch_results,
        evaluation = garch_eval
      )
      
      if (!is.null(garch_eval$metrics)) {
        cat("GARCH Results - RMSE:", round(garch_eval$metrics$rmse, 6), 
            "R²:", round(garch_eval$metrics$r2, 4), 
            "Model:", garch_eval$model_info, "\n")
      }
      
      # Save GARCH models
      save_checkpoint(garch_results, paste0("garch_baseline_models"))
    }
    
    # Save experiment without ML models
    save_checkpoint(experiment_results, paste0("experiment_", experiment_name))
    return(experiment_results)
  }
  
  # For GARCH with sentiment
  if (experiment_name == "garch_with_sentiment") {
    cat("\nFitting GARCH-X models with sentiment...\n")
    tic("GARCH-X with sentiment")
    garchx_results <- fit_garch_with_sentiment(train_data, feature_cols, "VIX_chg_next_day")
    toc()
    
    if (!is.null(garchx_results)) {
      garchx_eval <- evaluate_garch_with_sentiment(garchx_results, test_data, feature_cols, "VIX_chg_next_day")
      experiment_results$garch <- list(
        models = garchx_results,
        evaluation = garchx_eval
      )
      
      if (!is.null(garchx_eval$metrics)) {
        cat("GARCH-X Results - RMSE:", round(garchx_eval$metrics$rmse, 6), 
            "R²:", round(garchx_eval$metrics$r2, 4), 
            "Model:", garchx_eval$model_info, "\n")
      }
      
      # Save GARCH-X models
      save_checkpoint(garchx_results, paste0("garchx_sentiment_models"))
    }
    
    # Save experiment
    save_checkpoint(experiment_results, paste0("experiment_", experiment_name))
    return(experiment_results)
  }
  
  # Standard ML experiments
  # Random Forest
  cat("\nTraining Random Forest...\n")
  tic("RF tuning")
  rf_tune <- tune_rf_simple(train_data, feature_cols, "VIX_chg_next_day")
  toc()
  
  tic("RF training")
  rf_model <- train_model(train_data, feature_cols, "VIX_chg_next_day", "rf", rf_tune$best_params)
  toc()
  
  if (!is.null(rf_model)) {
    rf_eval <- evaluate_model(rf_model, test_data, feature_cols, "VIX_chg_next_day", "rf")
    experiment_results$rf <- list(
      tune_results = rf_tune,
      model = if(config$save_after_each_model) rf_model else NULL,
      evaluation = rf_eval,
      results = rf_eval  # For compatibility with plotting functions
    )
    
    if (!is.null(rf_eval$metrics)) {
      cat("RF Results - RMSE:", round(rf_eval$metrics$rmse, 6), "R²:", round(rf_eval$metrics$r2, 4), "\n")
    }
    
    # Save RF checkpoint
    if (config$save_after_each_model) {
      save_checkpoint(experiment_results$rf, paste0(experiment_name, "_rf"))
    }
  }
  
  # XGBoost
  cat("\nTraining XGBoost...\n")
  tic("XGB tuning")
  xgb_tune <- tune_xgb_simple(train_data, feature_cols, "VIX_chg_next_day")
  toc()
  
  tic("XGB training")
  xgb_model <- train_model(train_data, feature_cols, "VIX_chg_next_day", "xgb", xgb_tune$best_params)
  toc()
  
  if (!is.null(xgb_model)) {
    xgb_eval <- evaluate_model(xgb_model, test_data, feature_cols, "VIX_chg_next_day", "xgb")
    experiment_results$xgb <- list(
      tune_results = xgb_tune,
      model = if(config$save_after_each_model) xgb_model else NULL,
      evaluation = xgb_eval,
      results = xgb_eval  # For compatibility with plotting functions
    )
    
    if (!is.null(xgb_eval$metrics)) {
      cat("XGB Results - RMSE:", round(xgb_eval$metrics$rmse, 6), "R²:", round(xgb_eval$metrics$r2, 4), "\n")
    }
    
    # Save XGB checkpoint
    if (config$save_after_each_model) {
      save_checkpoint(experiment_results$xgb, paste0(experiment_name, "_xgb"))
    }
  }
  
  # Save complete experiment
  save_checkpoint(experiment_results, paste0("experiment_", experiment_name))
  
  cat("Experiment", experiment_name, "completed and saved.\n")
  return(experiment_results)
}

# =============================================================================
# MAIN ABLATION STUDY
# =============================================================================

# =============================================================================
# MAIN ABLATION STUDY WITH TIME SERIES CV
# =============================================================================

# =============================================================================
# COMPREHENSIVE ABLATION STUDY WITH ALL EXPERIMENTS
# =============================================================================

run_ablation_study <- function() {
  cat("\n", rep("=", 60), "\n")
  cat("STARTING COMPREHENSIVE VIX SENTIMENT ABLATION STUDY WITH TIME SERIES CV\n")
  cat(rep("=", 60), "\n\n")
  
  # Load data
  data <- load_and_prepare_data()
  
  # Sort data by date for time series CV
  data <- data %>% dplyr::arrange(date)
  
  cat("Running Time Series CV with", nrow(data), "observations\n")
  cat("Config: Initial window =", quick_ts_cv_config$initial_window, 
      ", Step size =", quick_ts_cv_config$step_size, 
      ", Splits =", quick_ts_cv_config$n_splits, "\n\n")
  
  # Generate CV splits
  cv_splits <- generate_cv_splits(data, quick_ts_cv_config)
  cat("Generated", length(cv_splits), "CV splits\n\n")
  
  # Build comprehensive experiments with individual sentiment testing
  cat("Building comprehensive experiment list with individual sentiment testing...\n")
  
  experiments <- list()
  base_features <- config$base_predictors
  
  # Get all available sentiment variables
  all_sentiment_groups <- unlist(config$sentiment_groups, use.names = FALSE)
  available_sentiment <- intersect(all_sentiment_groups, names(data))
  
  cat("Available sentiment variables:", length(available_sentiment), "\n")
  if (length(available_sentiment) > 0) {
    cat("Sentiment variables:", paste(available_sentiment[1:min(5, length(available_sentiment))], collapse = ", "))
    if (length(available_sentiment) > 5) cat(", ...")
    cat("\n\n")
  }
  
  # Baseline experiments
  experiments$baseline <- base_features
  
  # Market only
  market_core <- c("GSPC_ret", "DJI_ret", "IXIC_ret", "MOVE_chg", 
                   "DGS10_bps", "GSPC_ret_1D_lagged", "VIX_chg_1D_lagged")
  experiments$market_only <- intersect(market_core, base_features)
  
  # Individual sentiment experiments
  cat("Creating individual sentiment experiments...\n")
  individual_sentiment_vars <- c(
    "dict_sentiment_avg",
    "sentimentr_score_avg", 
    "finbert_score_avg",
    "ollama_score_avg",
    "aggregate_sentiment_avg"
  )
  
  individual_count <- 0
  for (sentiment_var in individual_sentiment_vars) {
    if (sentiment_var %in% names(data)) {
      exp_name <- paste0("individual_", gsub("_avg$", "", sentiment_var))
      experiments[[exp_name]] <- c(base_features, sentiment_var)
      individual_count <- individual_count + 1
      cat("  Added:", exp_name, "\n")
    }
  }
  
  cat("Created", individual_count, "individual sentiment experiments\n\n")
  
  # Combined experiments
  basic_sentiment <- intersect(individual_sentiment_vars, names(data))
  if (length(basic_sentiment) > 0) {
    experiments$basic_sentiment <- c(base_features, basic_sentiment)
  }
  
  # Individual only (excluding aggregate)
  individual_only <- setdiff(individual_sentiment_vars, "aggregate_sentiment_avg")
  available_individual <- intersect(individual_only, names(data))
  if (length(available_individual) > 1) {
    experiments$combined_individual <- c(base_features, available_individual)
  }
  
  # Aggregate only
  if ("aggregate_sentiment_avg" %in% names(data)) {
    experiments$aggregate_only <- c(base_features, "aggregate_sentiment_avg")
  }
  
  # Combined experiments
  basic_sentiment <- intersect(individual_sentiment_vars, names(data))
  if (length(basic_sentiment) > 0) {
    experiments$basic_sentiment <- c(base_features, basic_sentiment)
  }
  
  # Store available_basic for later use
  available_basic <- basic_sentiment  # ADD THIS LINE
  
  # Individual only (excluding aggregate)
  individual_only <- setdiff(individual_sentiment_vars, "aggregate_sentiment_avg")
  available_individual <- intersect(individual_only, names(data))
  if (length(available_individual) > 1) {
    experiments$combined_individual <- c(base_features, available_individual)
  }
  
  # Aggregate only
  if ("aggregate_sentiment_avg" %in% names(data)) {
    experiments$aggregate_only <- c(base_features, "aggregate_sentiment_avg")
  }
  
  # Momentum features
  momentum_features <- c(unlist(config$sentiment_groups$momentum_3d, use.names = FALSE),
                         unlist(config$sentiment_groups$momentum_5d, use.names = FALSE))
  available_momentum <- intersect(momentum_features, names(data))
  if (length(available_momentum) > 0 && length(available_basic) > 0) {
    momentum_feature_set <- c(base_features, available_basic, available_momentum)
    experiments$momentum_features <- momentum_feature_set
  }
  
  # Cross-sectional features
  cross_features <- unlist(config$sentiment_groups$cross_sectional, use.names = FALSE)
  available_cross <- intersect(cross_features, names(data))
  if (length(available_cross) > 0 && length(available_basic) > 0) {
    cross_feature_set <- c(base_features, available_basic, available_cross)
    experiments$cross_sectional <- cross_feature_set
  }
  
  # All sentiment features
  if (length(available_sentiment) > 0) {
    experiments$all_sentiment <- c(base_features, available_sentiment)
  }
  
  # Filter experiments to ensure minimum features
  experiments_filtered <- list()
  min_features <- 5
  
  for (exp_name in names(experiments)) {
    feature_cols <- intersect(experiments[[exp_name]], names(data))
    
    if (length(feature_cols) >= min_features) {
      experiments_filtered[[exp_name]] <- feature_cols
    } else {
      cat("⚠️  Skipping", exp_name, ": only", length(feature_cols), "valid features\n")
    }
  }
  
  cat("\n✅ Created", length(experiments_filtered), "valid experiments\n")
  
  # Print experiment summary
  cat("\nEXPERIMENT SUMMARY:\n")
  cat(rep("-", 50), "\n")
  for (exp_name in names(experiments_filtered)) {
    n_features <- length(experiments_filtered[[exp_name]])
    n_sentiment <- sum(grepl("sentiment|finbert|ollama", experiments_filtered[[exp_name]]))
    cat(sprintf("%-25s: %2d features (%d sentiment)\n", exp_name, n_features, n_sentiment))
  }
  cat(rep("-", 50), "\n\n")
  
  # Store all results
  all_cv_results <- list()
  
  # Run each experiment with time series CV
  for (exp_name in names(experiments_filtered)) {
    cat("EXPERIMENT:", exp_name, "\n")
    cat(rep("-", 40), "\n")
    
    feature_cols <- experiments_filtered[[exp_name]]
    exp_cv_results <- list()
    
    # Run CV for this experiment
    for (split_idx in seq_along(cv_splits)) {
      split <- cv_splits[[split_idx]]
      
      train_data <- data[split$train_indices, ]
      test_data <- data[split$test_indices, ]
      
      cat("Split", split_idx, ": Train", length(split$train_indices), 
          "obs, Test", length(split$test_indices), "obs\n")
      
      # Run experiment for this split
      split_result <- run_single_split_vix(train_data, test_data, feature_cols, "VIX_chg_next_day")
      
      if (!is.null(split_result)) {
        split_result$split_info <- list(
          split_id = split_idx,
          train_period = c(min(train_data$date), max(train_data$date)),
          test_period = c(min(test_data$date), max(test_data$date)),
          n_train = nrow(train_data),
          n_test = nrow(test_data)
        )
        exp_cv_results[[split_idx]] <- split_result
      }
    }
    
    # Aggregate results for this experiment
    aggregated_results <- aggregate_cv_results_vix(exp_cv_results, exp_name)
    all_cv_results[[exp_name]] <- aggregated_results
    
    cat("Experiment", exp_name, "completed\n")
    if (!is.na(aggregated_results$rf_summary$mean_rmse)) {
      cat("Mean RMSE (RF):", round(aggregated_results$rf_summary$mean_rmse, 4), 
          "(±", round(aggregated_results$rf_summary$std_rmse, 4), ")\n")
    }
    if (!is.na(aggregated_results$xgb_summary$mean_rmse)) {
      cat("Mean RMSE (XGB):", round(aggregated_results$xgb_summary$mean_rmse, 4), 
          "(±", round(aggregated_results$xgb_summary$std_rmse, 4), ")\n")
    }
    cat("\n")
  }
  
  # Save prediction errors for Diebold-Mariano test
  prediction_errors_for_dm <- list()
  for (exp_name in names(all_cv_results)) {
    exp_result <- all_cv_results[[exp_name]]
    prediction_errors_for_dm[[exp_name]] <- list(
      rf_errors = exp_result$rf_summary$all_prediction_errors,
      xgb_errors = exp_result$xgb_summary$all_prediction_errors
    )
  }
  
  # Save prediction errors as RDS file
  saveRDS(prediction_errors_for_dm, file.path(config$output_dir, "prediction_errors_for_dm_test.rds"))
  cat("✅ Prediction errors saved for Diebold-Mariano test: prediction_errors_for_dm_test.rds\n")
  
  return(all_cv_results)
}

# =============================================================================
# STATISTICAL TESTING
# =============================================================================

# Diebold-Mariano test function
diebold_mariano_test <- function(errors1, errors2, h = 1) {
  # Diebold-Mariano test for equal predictive accuracy
  valid_indices <- !is.na(errors1) & !is.na(errors2)
  e1 <- errors1[valid_indices]
  e2 <- errors2[valid_indices]
  
  if (length(e1) < 10) {
    return(list(statistic = NA, p_value = NA, conclusion = "Insufficient data"))
  }
  
  d <- e1^2 - e2^2  # Loss differential (squared errors)
  d_bar <- mean(d)
  
  # Variance with potential autocorrelation correction
  var_d <- var(d) / length(d)
  
  # DM statistic
  dm_stat <- d_bar / sqrt(var_d)
  p_value <- 2 * (1 - pnorm(abs(dm_stat)))
  
  conclusion <- case_when(
    p_value < 0.01 ~ "Highly significant difference (p < 0.01)",
    p_value < 0.05 ~ "Significant difference (p < 0.05)",
    p_value < 0.10 ~ "Marginally significant difference (p < 0.10)",
    TRUE ~ "No significant difference (p >= 0.10)"
  )
  
  return(list(
    statistic = dm_stat,
    p_value = p_value,
    conclusion = conclusion,
    mean_loss_diff = d_bar
  ))
}

# Statistical testing function
perform_statistical_tests <- function(all_results) {
  if (length(all_results) < 2) {
    return(NULL)
  }
  
  baseline_name <- "baseline"
  if (!baseline_name %in% names(all_results)) {
    baseline_name <- names(all_results)[1]
  }
  
  baseline_data <- all_results[[baseline_name]]
  dm_results <- data.frame()
  
  for (experiment in names(all_results)) {
    if (experiment == baseline_name) next
    
    exp_data <- all_results[[experiment]]
    
    # Test Random Forest if both exist
    if (!is.null(baseline_data$rf) && !is.null(exp_data$rf) &&
        !is.null(baseline_data$rf$evaluation) && !is.null(exp_data$rf$evaluation) &&
        !is.null(baseline_data$rf$evaluation$predictions) && !is.null(exp_data$rf$evaluation$predictions)) {
      
      baseline_errors <- baseline_data$rf$evaluation$actual - baseline_data$rf$evaluation$predictions
      exp_errors <- exp_data$rf$evaluation$actual - exp_data$rf$evaluation$predictions
      
      # Align lengths
      min_len <- min(length(baseline_errors), length(exp_errors))
      if (min_len > 10) {
        dm_test <- diebold_mariano_test(baseline_errors[1:min_len], exp_errors[1:min_len])
        
        dm_results <- rbind(dm_results, data.frame(
          Experiment = experiment,
          Model = "Random Forest",
          DM_Statistic = dm_test$statistic,
          P_Value = dm_test$p_value,
          Conclusion = dm_test$conclusion,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Test XGBoost if both exist
    if (!is.null(baseline_data$xgb) && !is.null(exp_data$xgb) &&
        !is.null(baseline_data$xgb$evaluation) && !is.null(exp_data$xgb$evaluation) &&
        !is.null(baseline_data$xgb$evaluation$predictions) && !is.null(exp_data$xgb$evaluation$predictions)) {
      
      baseline_errors <- baseline_data$xgb$evaluation$actual - baseline_data$xgb$evaluation$predictions
      exp_errors <- exp_data$xgb$evaluation$actual - exp_data$xgb$evaluation$predictions
      
      # Align lengths
      min_len <- min(length(baseline_errors), length(exp_errors))
      if (min_len > 10) {
        dm_test <- diebold_mariano_test(baseline_errors[1:min_len], exp_errors[1:min_len])
        
        dm_results <- rbind(dm_results, data.frame(
          Experiment = experiment,
          Model = "XGBoost",
          DM_Statistic = dm_test$statistic,
          P_Value = dm_test$p_value,
          Conclusion = dm_test$conclusion,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(dm_results) > 0) {
    write_csv(dm_results, file.path(config$output_dir, "diebold_mariano_tests.csv"))
  }
  
  return(dm_results)
}

# =============================================================================
# RESULTS ANALYSIS
# =============================================================================

create_results_summary <- function(results) {
  summary_data <- data.frame()
  
  for (experiment in names(results)) {
    exp_data <- results[[experiment]]
    
    # Handle RF results from CV
    if (!is.null(exp_data$rf_summary) && !is.na(exp_data$rf_summary$mean_rmse)) {
      summary_data <- rbind(summary_data, data.frame(
        Experiment = experiment,
        Model = "Random Forest",
        RMSE = exp_data$rf_summary$mean_rmse,
        RMSE_Std = exp_data$rf_summary$std_rmse,  # ADD THIS
        MAE = NA,  # Calculate if needed
        R2 = exp_data$rf_summary$mean_r2,
        R2_Std = exp_data$rf_summary$std_r2,  # ADD THIS
        Correlation = exp_data$rf_summary$mean_correlation,
        Features = 0,
        stringsAsFactors = FALSE
      ))
    }
    
    # Handle XGBoost results from CV
    if (!is.null(exp_data$xgb_summary) && !is.na(exp_data$xgb_summary$mean_rmse)) {
      summary_data <- rbind(summary_data, data.frame(
        Experiment = experiment,
        Model = "XGBoost", 
        RMSE = exp_data$xgb_summary$mean_rmse,
        RMSE_Std = exp_data$xgb_summary$std_rmse,  # ADD THIS
        MAE = NA,  # Calculate if needed
        R2 = exp_data$xgb_summary$mean_r2,
        R2_Std = exp_data$xgb_summary$std_r2,  # ADD THIS
        Correlation = exp_data$xgb_summary$mean_correlation,
        Features = 0, 
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(summary_data)
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

create_performance_plots <- function(results_summary) {
  cat("Creating performance visualizations...\n")
  
  plots <- list()
  
  # 1. RMSE Comparison
  p1 <- ggplot(results_summary, aes(x = reorder(Experiment, -RMSE), y = RMSE, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.4f", RMSE)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    labs(
      title = "VIX Change Prediction: Model Performance Comparison (RMSE)",
      subtitle = "Lower values indicate better performance",
      x = "Experiment",
      y = "Root Mean Squared Error (RMSE)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  ggsave(file.path(config$output_dir, "plots", "rmse_comparison.png"), 
         p1, width = config$plot_width, height = config$plot_height, dpi = config$plot_dpi)
  plots$rmse <- p1
  
  # 2. R² Comparison
  p2 <- ggplot(results_summary, aes(x = reorder(Experiment, R2), y = R2, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.4f", R2)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    labs(
      title = "VIX Change Prediction: Model Performance Comparison (R²)",
      subtitle = "Higher values indicate better performance",
      x = "Experiment",
      y = "R-squared"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  ggsave(file.path(config$output_dir, "plots", "r2_comparison.png"), 
         p2, width = config$plot_width, height = config$plot_height, dpi = config$plot_dpi)
  plots$r2 <- p2
  
  # 3. Correlation Comparison
  p3 <- ggplot(results_summary, aes(x = reorder(Experiment, Correlation), y = Correlation, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.4f", Correlation)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    labs(
      title = "VIX Change Prediction: Model Performance Comparison (Correlation)",
      subtitle = "Higher values indicate better performance",
      x = "Experiment",
      y = "Correlation with Actual Values"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  ggsave(file.path(config$output_dir, "plots", "correlation_comparison.png"), 
         p3, width = config$plot_width, height = config$plot_height, dpi = config$plot_dpi)
  plots$correlation <- p3
  
  # 4. Feature Count vs Performance
  if (nrow(results_summary) > 3) {
    p4 <- ggplot(results_summary, aes(x = Features, y = R2, color = Model, shape = Experiment)) +
      geom_point(size = 4, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, alpha = 0.3) +
      labs(
        title = "VIX Prediction: Feature Count vs Model Performance",
        subtitle = "Relationship between number of features and R² performance",
        x = "Number of Features",
        y = "R-squared"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom"
      ) +
      scale_color_brewer(type = "qual", palette = "Set1")
    
    ggsave(file.path(config$output_dir, "plots", "features_vs_performance.png"), 
           p4, width = config$plot_width, height = config$plot_height, dpi = config$plot_dpi)
    plots$features_performance <- p4
  }
  
  return(plots)
}

create_feature_importance_plots <- function(all_results) {
  cat("Creating feature importance visualizations...\n")
  
  importance_plots <- list()
  
  for (experiment in names(all_results)) {
    exp_data <- all_results[[experiment]]
    
    # Random Forest importance
    if (!is.null(exp_data$rf) && !is.null(exp_data$rf$evaluation$importance)) {
      rf_importance <- exp_data$rf$evaluation$importance
      
      if (nrow(rf_importance) > 0) {
        # Take top 15 features
        top_features <- head(rf_importance, 15)
        
        p_rf <- ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
          geom_col(fill = "steelblue", alpha = 0.7) +
          coord_flip() +
          labs(
            title = paste("Random Forest Feature Importance:", experiment),
            subtitle = "Top 15 most important features",
            x = "Features",
            y = "Importance (%IncMSE)"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 10)
          )
        
        ggsave(file.path(config$output_dir, "plots", paste0("rf_importance_", experiment, ".png")), 
               p_rf, width = 10, height = 8, dpi = config$plot_dpi)
        importance_plots[[paste0(experiment, "_rf")]] <- p_rf
      }
    }
    
    # XGBoost importance
    if (!is.null(exp_data$xgb) && !is.null(exp_data$xgb$evaluation$importance)) {
      xgb_importance <- exp_data$xgb$evaluation$importance
      
      if (nrow(xgb_importance) > 0) {
        # Take top 15 features
        top_features <- head(xgb_importance, 15)
        
        p_xgb <- ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
          geom_col(fill = "darkorange", alpha = 0.7) +
          coord_flip() +
          labs(
            title = paste("XGBoost Feature Importance:", experiment),
            subtitle = "Top 15 most important features",
            x = "Features",
            y = "Importance (Gain)"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 10)
          )
        
        ggsave(file.path(config$output_dir, "plots", paste0("xgb_importance_", experiment, ".png")), 
               p_xgb, width = 10, height = 8, dpi = config$plot_dpi)
        importance_plots[[paste0(experiment, "_xgb")]] <- p_xgb
      }
    }
  }
  
  return(importance_plots)
}

create_prediction_plots <- function(all_results) {
  cat("Creating prediction vs actual plots...\n")
  
  prediction_plots <- list()
  
  for (experiment in names(all_results)) {
    exp_data <- all_results[[experiment]]
    
    # Random Forest predictions
    if (!is.null(exp_data$rf) && !is.null(exp_data$rf$evaluation) && 
        !is.null(exp_data$rf$evaluation$predictions) && !is.null(exp_data$rf$evaluation$actual)) {
      
      pred_data <- data.frame(
        Actual = exp_data$rf$evaluation$actual,
        Predicted = exp_data$rf$evaluation$predictions
      ) %>%
        filter(!is.na(Actual) & !is.na(Predicted))
      
      if (nrow(pred_data) > 0) {
        p_rf <- ggplot(pred_data, aes(x = Actual, y = Predicted)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
          geom_smooth(method = "lm", se = TRUE, color = "darkblue", alpha = 0.3) +
          labs(
            title = paste("Random Forest Predictions vs Actual:", experiment),
            subtitle = paste("Correlation:", round(cor(pred_data$Actual, pred_data$Predicted, use = "complete.obs"), 4)),
            x = "Actual VIX Changes",
            y = "Predicted VIX Changes"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold")
          )
        
        ggsave(file.path(config$output_dir, "plots", paste0("rf_predictions_", experiment, ".png")), 
               p_rf, width = 10, height = 8, dpi = config$plot_dpi)
        prediction_plots[[paste0(experiment, "_rf")]] <- p_rf
      }
    }
    
    # XGBoost predictions
    if (!is.null(exp_data$xgb) && !is.null(exp_data$xgb$evaluation) && 
        !is.null(exp_data$xgb$evaluation$predictions) && !is.null(exp_data$xgb$evaluation$actual)) {
      
      pred_data <- data.frame(
        Actual = exp_data$xgb$evaluation$actual,
        Predicted = exp_data$xgb$evaluation$predictions
      ) %>%
        filter(!is.na(Actual) & !is.na(Predicted))
      
      if (nrow(pred_data) > 0) {
        p_xgb <- ggplot(pred_data, aes(x = Actual, y = Predicted)) +
          geom_point(alpha = 0.6, color = "darkorange") +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
          geom_smooth(method = "lm", se = TRUE, color = "darkred", alpha = 0.3) +
          labs(
            title = paste("XGBoost Predictions vs Actual:", experiment),
            subtitle = paste("Correlation:", round(cor(pred_data$Actual, pred_data$Predicted, use = "complete.obs"), 4)),
            x = "Actual VIX Changes",
            y = "Predicted VIX Changes"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold")
          )
        
        ggsave(file.path(config$output_dir, "plots", paste0("xgb_predictions_", experiment, ".png")), 
               p_xgb, width = 10, height = 8, dpi = config$plot_dpi)
        prediction_plots[[paste0(experiment, "_xgb")]] <- p_xgb
      }
    }
  }
  
  return(prediction_plots)
}

# =============================================================================
# COMPREHENSIVE REPORTING
# =============================================================================

generate_comprehensive_report <- function(all_results) {
  cat("Generating comprehensive report...\n")
  
  # Create results summary
  results_summary <- create_results_summary(all_results)
  
  # Save summary table
  write_csv(results_summary, file.path(config$output_dir, "tables", "results_summary.csv"))
  
  # Perform statistical tests
  dm_results <- perform_statistical_tests(all_results)
  
  # Create visualizations
  if (config$create_plots) {
    performance_plots <- create_performance_plots(results_summary)
    importance_plots <- create_feature_importance_plots(all_results)
    prediction_plots <- create_prediction_plots(all_results)
  }
  
  # Generate markdown report
  report_content <- c(
    "# VIX Change Prediction: Sentiment Ablation Study Results",
    "",
    paste("**Analysis Date:**", Sys.Date()),
    paste("**Data Period:**", "See individual experiment logs"),
    "",
    "## Executive Summary",
    "",
    "This report presents the results of a comprehensive ablation study examining the impact of sentiment variables on VIX change prediction accuracy.",
    "",
    "## Model Performance Summary",
    "",
    "### Key Findings",
    "",
    if (nrow(results_summary) > 0) {
      best_experiment <- results_summary[which.max(results_summary$R2), ]
      paste("- **Best performing model:**", best_experiment$Model, "in", best_experiment$Experiment, "experiment")
    } else {
      "- No successful model runs to report"
    },
    "",
    if (nrow(results_summary) > 0) {
      paste("- **Highest R²:**", round(max(results_summary$R2, na.rm = TRUE), 4))
    } else {
      "- No R² values to report"
    },
    "",
    if (nrow(results_summary) > 0) {
      paste("- **Lowest RMSE:**", round(min(results_summary$RMSE, na.rm = TRUE), 6))
    } else {
      "- No RMSE values to report"
    },
    "",
    "### Detailed Results",
    "",
    "The following experiments were conducted:",
    ""
  )
  
  # Add experiment descriptions
  for (experiment in names(all_results)) {
    exp_data <- all_results[[experiment]]
    report_content <- c(report_content,
                        paste("**", experiment, "**"),
                        paste("- Features:", ifelse(is.null(exp_data$n_features), "GARCH only", exp_data$n_features)),
                        ""
    )
  }
  
  # Statistical significance section
  if (!is.null(dm_results) && nrow(dm_results) > 0) {
    report_content <- c(report_content,
                        "",
                        "## Statistical Significance Tests",
                        "",
                        "Diebold-Mariano tests for equal predictive accuracy:",
                        ""
    )
    
    for (i in 1:nrow(dm_results)) {
      result <- dm_results[i, ]
      report_content <- c(report_content,
                          paste("**", result$Experiment, "-", result$Model, "**"),
                          paste("- Test statistic:", round(result$DM_Statistic, 4)),
                          paste("- P-value:", round(result$P_Value, 4)),
                          paste("- Conclusion:", result$Conclusion),
                          ""
      )
    }
  }
  
  # Technical details
  report_content <- c(report_content,
                      "",
                      "## Technical Details",
                      "",
                      "### Data Preparation",
                      "- Target variable: Next-day VIX changes (properly time-shifted)",
                      "- Training/test split: 80/20",
                      "- Missing value handling: Complete case analysis",
                      "",
                      "### Model Specifications",
                      "- **Random Forest:** Tuned hyperparameters with OOB error optimization",
                      "- **XGBoost:** Tuned hyperparameters with cross-validation",
                      "- **GARCH:** Multiple specifications (GARCH, EGARCH, GJR-GARCH) with AIC selection",
                      "",
                      "### Sentiment Variables",
                      "- Basic sentiment: Dictionary, SentimentR, FinBERT, Ollama, Aggregate",
                      "- Momentum features: 3-day and 5-day momentum calculations",
                      "- Cross-sectional: Standard deviation, range, coefficient of variation",
                      "",
                      paste("**Report generated on:**", Sys.time())
  )
  
  # Write markdown report
  writeLines(report_content, file.path(config$output_dir, "VIX_Sentiment_Study_Report.md"))
  
  # Create HTML table for results
  if (nrow(results_summary) > 0) {
    html_table <- results_summary %>%
      kable("html", digits = 4, caption = "VIX Change Prediction Results") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    
    writeLines(as.character(html_table), file.path(config$output_dir, "tables", "results_table.html"))
  }
  
  cat("Comprehensive report generated successfully!\n")
  cat("Main report: ", file.path(config$output_dir, "VIX_Sentiment_Study_Report.md"), "\n")
  cat("Results summary: ", file.path(config$output_dir, "tables", "results_summary.csv"), "\n")
  
  return(list(
    summary = results_summary,
    dm_tests = dm_results,
    report_path = file.path(config$output_dir, "VIX_Sentiment_Study_Report.md")
  ))
}

# =============================================================================
# MAIN EXECUTION FUNCTION
# =============================================================================

# =============================================================================
# MAIN EXECUTION FUNCTION - UPDATED
# =============================================================================

main <- function() {
  cat("\n", rep("=", 70), "\n")
  cat("VIX CHANGE PREDICTION WITH SENTIMENT ANALYSIS - COMPREHENSIVE TIME SERIES CV\n")
  cat(rep("=", 70), "\n\n")
  
  # Set random seed for reproducibility
  set.seed(config$seed)
  
  # Record start time
  start_time <- Sys.time()
  cat("Analysis started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Run the comprehensive ablation study with time series CV
  tic("Total comprehensive ablation study with TS CV")
  all_results <- run_ablation_study()
  toc()
  
  # Create summary table with standard deviations
  results_summary <- create_cv_summary_table_vix(all_results)
  
  # Analyze individual sentiment performance
  individual_analysis <- analyze_individual_sentiment_results_vix(all_results)
  
  # Save results
  write_csv(results_summary, file.path(config$output_dir, "vix_cv_results_summary.csv"))
  saveRDS(all_results, file.path(config$output_dir, "vix_cv_detailed_results.rds"))
  
  cat("\n", rep("=", 80), "\n")
  cat("TIME SERIES CROSS-VALIDATION RESULTS SUMMARY\n")
  cat(rep("=", 80), "\n\n")
  
  print(results_summary %>% dplyr::arrange(Mean_RMSE))
  
  cat("\n📊 BEST PERFORMING MODELS (by Mean RMSE):\n")
  best_models <- results_summary %>% 
    dplyr::arrange(Mean_RMSE) %>% 
    head(5)
  
  for (i in 1:nrow(best_models)) {
    row <- best_models[i, ]
    cat(sprintf("%d. %s (%s): RMSE = %.4f (±%.4f), R² = %.4f (±%.4f), %d splits\n", 
                i, row$Experiment, row$Model, row$Mean_RMSE, row$Std_RMSE, 
                row$Mean_R2, row$Std_R2, row$N_Splits))
  }
  
  # Generate comprehensive report
  tic("Report generation")
  report_results <- generate_comprehensive_report(all_results)
  toc()
  
  # Record end time and duration
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  cat("\n", rep("=", 70), "\n")
  cat("COMPREHENSIVE VIX ANALYSIS WITH TIME SERIES CV COMPLETED!\n")
  cat(rep("=", 70), "\n")
  cat("Duration:  ", round(duration, 2), "minutes\n")
  cat("\nResults saved to:", config$output_dir, "\n")
  cat("Key files:\n")
  cat("- CV Results Summary:", file.path(config$output_dir, "vix_cv_results_summary.csv"), "\n")
  cat("- Individual Sentiment Rankings:", file.path(config$output_dir, "individual_sentiment_rankings_vix.csv"), "\n")
  cat("- Prediction Errors (for DM test):", file.path(config$output_dir, "prediction_errors_for_dm_test.rds"), "\n")
  cat("- Detailed Results:", file.path(config$output_dir, "vix_cv_detailed_results.rds"), "\n")
  
  cat("\n✅ Comprehensive time series CV completed successfully!\n")
  cat("Key advantages:\n")
  cat("- Tested model stability across", quick_ts_cv_config$n_splits, "time periods\n")
  cat("- Provided confidence intervals (±) for all metrics\n")
  cat("- Individual sentiment variable analysis included\n")
  cat("- Hyperparameter tuning for each split\n")
  cat("- Statistical rigor for thesis conclusions\n\n")
  
  return(list(
    results = all_results,
    summary = results_summary,
    individual_analysis = individual_analysis,
    report = report_results,
    duration_minutes = duration
  ))
}

# =============================================================================
# SCRIPT EXECUTION
# =============================================================================

final_results <- main()


