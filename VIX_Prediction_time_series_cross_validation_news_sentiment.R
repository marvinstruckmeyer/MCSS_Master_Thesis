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
  output_dir = "VIX_sentiment_ablation_results_news_sentiment",  
  checkpoint_dir = "VIX_sentiment_ablation_checkpoints_news_sentiment",  
  
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
    "GSPC_ret_1D_lagged", "DGS10_bps_1D_lagged", "VIX_chg_1D_lagged",
    
    "News Sentiment"
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

run_ablation_study <- function() {
  cat("\n", rep("=", 60), "\n")
  cat("STARTING COMPREHENSIVE VIX SENTIMENT ABLATION STUDY\n")
  cat(rep("=", 60), "\n\n")
  
  # Load data
  data <- load_and_prepare_data()
  
  # Split data
  n_train <- floor(nrow(data) * 0.8)
  train_data <- data[1:n_train, ]
  test_data <- data[(n_train + 1):nrow(data), ]
  
  cat("Training data:", nrow(train_data), "observations\n")
  cat("Test data:", nrow(test_data), "observations\n\n")
  
  # Initialize results storage
  all_results <- list()
  
  # Experiment 1: GARCH Baseline
  all_results$garch_baseline <- run_single_experiment("garch_baseline", train_data, test_data, NULL)
  
  # Experiment 2: ML Baseline (no sentiment)
  base_features <- config$base_predictors
  all_results$baseline <- run_single_experiment("baseline", train_data, test_data, base_features)
  
  # Experiment 3: Basic sentiment
  basic_sentiment <- unlist(config$sentiment_groups$basic, use.names = FALSE)
  available_basic <- intersect(basic_sentiment, names(data))
  if (length(available_basic) > 0) {
    basic_features <- c(base_features, available_basic)
    all_results$basic_sentiment <- run_single_experiment("basic_sentiment", train_data, test_data, basic_features)
  }
  
  # Experiment 4: Momentum features
  momentum_features <- c(unlist(config$sentiment_groups$momentum_3d, use.names = FALSE),
                         unlist(config$sentiment_groups$momentum_5d, use.names = FALSE))
  available_momentum <- intersect(momentum_features, names(data))
  if (length(available_momentum) > 0) {
    momentum_feature_set <- c(base_features, available_basic, available_momentum)
    all_results$momentum_features <- run_single_experiment("momentum_features", train_data, test_data, momentum_feature_set)
  }
  
  # Experiment 5: Cross-sectional features
  cross_features <- unlist(config$sentiment_groups$cross_sectional, use.names = FALSE)
  available_cross <- intersect(cross_features, names(data))
  if (length(available_cross) > 0) {
    cross_feature_set <- c(base_features, available_basic, available_cross)
    all_results$cross_sectional <- run_single_experiment("cross_sectional", train_data, test_data, cross_feature_set)
  }
  
  # Experiment 6: All sentiment features
  all_sentiment <- unlist(config$sentiment_groups, use.names = FALSE)
  available_all_sentiment <- intersect(all_sentiment, names(data))
  if (length(available_all_sentiment) > 0) {
    all_sentiment_features <- c(base_features, available_all_sentiment)
    all_results$all_sentiment <- run_single_experiment("all_sentiment", train_data, test_data, all_sentiment_features)
  }
  
  # Experiment 7: Market only (reduced base predictors for comparison)
  market_core <- c("GSPC_ret", "DJI_ret", "IXIC_ret", "MOVE_chg", 
                   "DGS10_bps", "GSPC_ret_1D_lagged", "VIX_chg_1D_lagged")
  market_only_features <- intersect(market_core, config$base_predictors)
  all_results$market_only <- run_single_experiment("market_only", train_data, test_data, market_only_features)
  
  # Experiment 8: GARCH with sentiment (GARCH-X)
  if (length(available_all_sentiment) > 0) {
    garch_sentiment_features <- c(base_features, available_all_sentiment)
    all_results$garch_with_sentiment <- run_single_experiment(
      "garch_with_sentiment", train_data, test_data, garch_sentiment_features
    )
  }
  
  # Save final results
  save_checkpoint(all_results, "final_results")
  
  return(all_results)
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
    
    # Handle GARCH results
    if (!is.null(exp_data$garch) && !is.null(exp_data$garch$evaluation$metrics)) {
      summary_data <- rbind(summary_data, data.frame(
        Experiment = experiment,
        Model = "GARCH",
        RMSE = exp_data$garch$evaluation$metrics$rmse,
        MAE = exp_data$garch$evaluation$metrics$mae,
        R2 = exp_data$garch$evaluation$metrics$r2,
        Correlation = exp_data$garch$evaluation$metrics$correlation,
        Features = ifelse(is.null(exp_data$n_features), 0, exp_data$n_features),
        stringsAsFactors = FALSE
      ))
    }
    
    # Handle RF results
    if (!is.null(exp_data$rf) && !is.null(exp_data$rf$evaluation$metrics)) {
      summary_data <- rbind(summary_data, data.frame(
        Experiment = experiment,
        Model = "Random Forest",
        RMSE = exp_data$rf$evaluation$metrics$rmse,
        MAE = exp_data$rf$evaluation$metrics$mae,
        R2 = exp_data$rf$evaluation$metrics$r2,
        Correlation = exp_data$rf$evaluation$metrics$correlation,
        Features = exp_data$n_features,
        stringsAsFactors = FALSE
      ))
    }
    
    # Handle XGBoost results
    if (!is.null(exp_data$xgb) && !is.null(exp_data$xgb$evaluation$metrics)) {
      summary_data <- rbind(summary_data, data.frame(
        Experiment = experiment,
        Model = "XGBoost",
        RMSE = exp_data$xgb$evaluation$metrics$rmse,
        MAE = exp_data$xgb$evaluation$metrics$mae,
        R2 = exp_data$xgb$evaluation$metrics$r2,
        Correlation = exp_data$xgb$evaluation$metrics$correlation,
        Features = exp_data$n_features,
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

main <- function() {
  cat("\n", rep("=", 70), "\n")
  cat("VIX CHANGE PREDICTION WITH SENTIMENT ANALYSIS - ABLATION STUDY\n")
  cat(rep("=", 70), "\n\n")
  
  # Set random seed for reproducibility
  set.seed(config$seed)
  
  # Record start time
  start_time <- Sys.time()
  cat("Analysis started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Check if final results already exist
  existing_results <- load_checkpoint("final_results")
  
  if (!is.null(existing_results)) {
    cat("Found existing results. Generating report from checkpoint...\n")
    all_results <- existing_results
  } else {
    cat("No existing results found. Running full ablation study...\n")
    
    # Run the ablation study
    tic("Total ablation study")
    all_results <- run_ablation_study()
    toc()
  }
  
  # Generate comprehensive report
  tic("Report generation")
  report_results <- generate_comprehensive_report(all_results)
  toc()
  
  # Record end time and duration
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  cat("\n", rep("=", 70), "\n")
  cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")
  cat(rep("=", 70), "\n")
  cat("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("End time:  ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Duration:  ", round(duration, 2), "minutes\n")
  cat("\nResults saved to:", config$output_dir, "\n")
  cat("Key files:\n")
  cat("- Main report:", file.path(config$output_dir, "VIX_Sentiment_Study_Report.md"), "\n")
  cat("- Results table:", file.path(config$output_dir, "tables", "results_summary.csv"), "\n")
  cat("- Plots folder:", file.path(config$output_dir, "plots"), "\n")
  cat("- Checkpoints:", config$checkpoint_dir, "\n")
  
  return(list(
    results = all_results,
    report = report_results,
    duration_minutes = duration
  ))
}

# =============================================================================
# SCRIPT EXECUTION
# =============================================================================

final_results <- main() 

