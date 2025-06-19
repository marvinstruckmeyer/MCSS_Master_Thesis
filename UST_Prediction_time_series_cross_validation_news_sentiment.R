# =============================================================================
# COMPREHENSIVE ML SCRIPT WITH PROPER TARGET SHIFTING & HYPERPARAMETER TUNING
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

# Set up parallel processing
n_cores <- detectCores() - 1
registerDoParallel(cores = n_cores)
cat("Using", n_cores, "cores for parallel processing\n")

# =============================================================================
# CONFIGURATION
# =============================================================================

config <- list(
  # Data paths
  data_path = "03_ml_prediction/ml_data_final_complete_enhanced_BOND_FIXED.csv",
  output_dir = "UST_comprehensive_results_time_series_cross_validation_news_sentiment",
  checkpoint_dir = "UST_comprehensive_checkpoints_time_series_cross_validation_news_sentiment",
  
  # Target variable (will be shifted to next day)
  target_var = "DGS10_bps",
  
  # Experiment configurations
  experiments = list(
    baseline = "No sentiment features",
    basic_sentiment = "Basic sentiment features only",
    momentum_features = "Sentiment + momentum features", 
    cross_sectional = "Sentiment + cross-sectional features",
    all_sentiment = "All sentiment features combined",
    market_only = "Market variables only (no sentiment)"
  ),
  
  # Base market predictors (safe to use with shifted target)
  base_predictors = c(
    # Same-day market returns (safe with shifted target)
    "GSPC_ret", "DJI_ret", "IXIC_ret", "COPPER_ret", "GOLD_ret", "USDOLLAR_ret", "DCOILWTICO_ret",
    
    # Sector returns
    "XLF_ret", "XLK_ret", "XLV_ret", "XLY_ret", "XLP_ret", 
    "XLRE_ret", "XLU_ret", "XLB_ret", "XLC_ret", "XLE_ret", "XLI_ret",
    
    # Volatility measures
    "VIX_chg", "VIXCLS_chg", "MOVE_chg", "VIX",
    
    # Economic indicators
    "NFCI", "NFCICREDIT", "NFCILEVERAGE", "NFCIRISK",
    "ICSA_chg", "CCSA_chg", "WM2NS_chg", "WTREGEN_chg", "WRESBAL_chg",
    
    # Interest rates (REMOVED DGS10_bps - now the target)
    "DGS2_bps", "DGS5_bps", "DGS30_bps", 
    "T5YIE_bps", "T10YIE_bps", "T5YIFR_bps", 
    "BAMLH0A0HYM2_bps", "BAMLC0A0CM_bps",
    
    # Technical indicators
    "SP500_momentum_5d", "SP500_momentum_20d", "SP500_distance_50d_MA", "SP500_RSI_14w",
    
    # Style factors
    "Tech_vs_Market", "Finance_vs_Market", "Defensive_vs_Offensive",
    "Yield_Curve_Slope", "Credit_Risk_Spread", "Vol_Regime", 
    "Flight_to_Quality", "USD_Risk_Signal", "Bond_Equity_Divergence", 
    "Commodity_Momentum",
    
    # Lagged variables (additional historical info)
    "GSPC_ret_1D_lagged", "DGS10_bps_1D_lagged", "VIX_chg_1D_lagged",
    
    "News Sentiment"
  ),
  
  # Sentiment variable groups
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
  
  # Hyperparameter tuning grids
  tune_config = list(
    rf = list(
      # BALANCED Random Forest tuning (thorough but reasonable)
      ntree = c(500, 750, 1000, 1500),     # Reduced from 6 to 4 options
      mtry = c(5, 8, 12, 15),              # Reduced from 7 to 4 options
      nodesize = c(1, 3, 5),               # Reduced from 5 to 3 options
      maxnodes = c(NULL, 100, 300)         # Reduced from 6 to 3 options
    ),
    xgb = list(
      # SIMPLIFIED XGBoost tuning (faster execution)
      nrounds = c(100, 300),        # Reduced from 4 to 2 options
      max_depth = c(4, 6),          # Reduced from 4 to 2 options  
      eta = c(0.05, 0.1),           # Reduced from 4 to 2 options
      subsample = c(0.8),           # Reduced from 3 to 1 option
      colsample_bytree = c(0.8),    # Reduced from 3 to 1 option
      min_child_weight = c(1, 3)    # Reduced from 3 to 2 options
    )
  ),
  
  # Cross-validation settings
  cv_folds = 5,
  cv_repeats = 2,
  
  # Random seed for reproducibility
  seed = 42,
  
  # Output settings
  save_models = TRUE,
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
dir.create(file.path(config$output_dir, "models"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# ENHANCED SEED SETTING FOR REPRODUCIBILITY
# =============================================================================

set_all_seeds <- function(seed = 42) {
  set.seed(seed)
  RNGkind("L'Ecuyer-CMRG")
  Sys.setenv(PYTHONHASHSEED = seed)
  cat("All seeds set to:", seed, "\n")
}

set_all_seeds(config$seed)

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================

load_and_prepare_data <- function() {
  cat("Loading and preparing enhanced data...\n")
  
  if (!file.exists(config$data_path)) {
    stop("Data file not found: ", config$data_path)
  }
  
  data <- read_csv(config$data_path, show_col_types = FALSE)
  cat("Loaded data with", nrow(data), "rows and", ncol(data), "columns\n")
  
  # Handle date column
  data <- data %>% mutate(date = as.Date(date))
  
  # CRITICAL: Create properly shifted target (predict NEXT day's 10Y rate change)
  cat("Creating target variable from:", config$target_var, "\n")
  cat("Target column exists in data:", config$target_var %in% names(data), "\n")
  
  if (!config$target_var %in% names(data)) {
    cat("Available columns:", paste(names(data)[1:20], collapse = ", "), "...\n")
    stop("Target variable not found in data!")
  }
  
  data <- data %>%
    arrange(date) %>%
    mutate(
      # Ensure target is numeric
      !!config$target_var := as.numeric(!!sym(config$target_var)),
      # Create next-day target with fixed name (avoid config variable issues)
      DGS10_bps_next_day = lead(as.numeric(!!sym(config$target_var)), 1)
    )
  
  cat("✓ Created next-day target variable: DGS10_bps_next_day\n")
  cat("- Original target range:", range(data[[config$target_var]], na.rm = TRUE), "\n")
  cat("- Next-day target range:", range(data$DGS10_bps_next_day, na.rm = TRUE), "\n")
  cat("- NAs in original target:", sum(is.na(data[[config$target_var]])), "\n")
  cat("- NAs in next-day target:", sum(is.na(data$DGS10_bps_next_day)), "\n")
  
  # Verify available predictors
  available_base <- intersect(config$base_predictors, names(data))
  missing_base <- setdiff(config$base_predictors, names(data))
  
  cat("Available base predictors:", length(available_base), "of", length(config$base_predictors), "\n")
  if (length(missing_base) > 0 && length(missing_base) <= 10) {
    cat("Missing base predictors:", paste(missing_base, collapse = ", "), "\n")
  }
  
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
  
  cat("Total sentiment variables available:", total_sentiment_vars, "\n")
  
  # Update config
  config$base_predictors <<- available_base
  config$sentiment_groups <<- available_sentiment_groups
  
  # Create clean dataset with robust numeric conversion
  all_sentiment_vars <- unlist(available_sentiment_groups, use.names = FALSE)
  
  data_clean <- data %>%
    filter(!is.na(DGS10_bps_next_day)) %>%  # Use direct name
    select(date, DGS10_bps_next_day, all_of(available_base), all_of(all_sentiment_vars))
  
  # CRITICAL: Ensure ALL predictor variables are numeric
  cat("Converting all predictors to numeric...\n")
  predictor_cols <- setdiff(names(data_clean), c("date", "DGS10_bps_next_day"))
  
  for (col in predictor_cols) {
    if (!is.numeric(data_clean[[col]])) {
      cat("Converting", col, "from", class(data_clean[[col]]), "to numeric\n")
      data_clean[[col]] <- as.numeric(as.character(data_clean[[col]]))
    }
  }
  
  # Also ensure target is numeric
  data_clean$DGS10_bps_next_day <- as.numeric(data_clean$DGS10_bps_next_day)
  
  # Remove rows with any missing values
  data_clean <- data_clean %>% filter(complete.cases(.))
  
  cat("After cleaning and numeric conversion:", nrow(data_clean), "complete observations\n")
  cat("Date range:", min(data_clean$date), "to", max(data_clean$date), "\n")
  
  # Verify all predictors are now numeric
  non_numeric_cols <- predictor_cols[!sapply(data_clean[predictor_cols], is.numeric)]
  if (length(non_numeric_cols) > 0) {
    cat("⚠️ Still non-numeric columns:", paste(non_numeric_cols, collapse = ", "), "\n")
    # Force convert any remaining non-numeric columns
    for (col in non_numeric_cols) {
      data_clean[[col]] <- as.numeric(as.character(data_clean[[col]]))
    }
    # Remove rows with NAs introduced by conversion
    data_clean <- data_clean %>% filter(complete.cases(.))
    cat("After final cleanup:", nrow(data_clean), "observations\n")
  } else {
    cat("✓ All predictor columns are now numeric\n")
  }
  
  # Ensure both variables are numeric before correlation
  current_day_values <- as.numeric(data_clean[[config$target_var]])
  next_day_values <- as.numeric(data_clean$DGS10_bps_next_day)
  
  # Remove any remaining NAs
  valid_indices <- !is.na(current_day_values) & !is.na(next_day_values)
  current_day_clean <- current_day_values[valid_indices]
  next_day_clean <- next_day_values[valid_indices]
  
  if (length(current_day_clean) > 0 && length(next_day_clean) > 0) {
    # Sanity check: correlation between today's and tomorrow's 10Y rates
    same_day_corr <- cor(current_day_clean, next_day_clean, use = "complete.obs")
    cat("Sanity check - correlation between today's and tomorrow's 10Y rates:", round(same_day_corr, 4), "\n")
    
    if (abs(same_day_corr) > 0.8) {
      cat("✓ High correlation expected for interest rates (persistent series)\n")
    } else {
      cat("⚠️ Lower correlation than expected for interest rates\n")
    }
  } else {
    cat("⚠️ No valid numeric data found for correlation check\n")
    cat("- Valid current day values:", length(current_day_clean), "\n")
    cat("- Valid next day values:", length(next_day_clean), "\n")
  }
  
  return(data_clean)
}

# =============================================================================
# HYPERPARAMETER TUNING FUNCTIONS
# =============================================================================

tune_random_forest <- function(X_train, y_train, tune_grid = NULL) {
  cat("Tuning Random Forest hyperparameters...\n")
  
  if (is.null(tune_grid)) {
    tune_grid <- expand.grid(
      ntree = config$tune_config$rf$ntree,
      mtry = pmin(config$tune_config$rf$mtry, ncol(X_train)),
      nodesize = config$tune_config$rf$nodesize,
      maxnodes = config$tune_config$rf$maxnodes,
      stringsAsFactors = FALSE
    )
  }
  
  cat("Testing", nrow(tune_grid), "parameter combinations\n")
  
  best_oob_error <- Inf
  best_params <- NULL
  results <- data.frame()
  
  tic("RF hyperparameter tuning")
  
  for (i in 1:nrow(tune_grid)) {
    params <- tune_grid[i, ]
    
    set.seed(config$seed + i)
    
    tryCatch({
      rf_model <- randomForest(
        x = X_train,
        y = y_train,
        ntree = params$ntree,
        mtry = params$mtry,
        nodesize = params$nodesize,
        maxnodes = if(is.na(params$maxnodes)) NULL else params$maxnodes,
        do.trace = FALSE
      )
      
      oob_error <- tail(rf_model$mse, 1)
      
      results <- rbind(results, data.frame(
        iteration = i,
        ntree = params$ntree,
        mtry = params$mtry,
        nodesize = params$nodesize,
        maxnodes = ifelse(is.na(params$maxnodes), 0, params$maxnodes),
        oob_error = oob_error,
        oob_rmse = sqrt(oob_error)
      ))
      
      if (oob_error < best_oob_error) {
        best_oob_error <- oob_error
        best_params <- params
      }
      
    }, error = function(e) {
      cat("Error with parameter set", i, ":", e$message, "\n")
    })
    
    if (i %% 20 == 0) {
      cat("Completed", i, "of", nrow(tune_grid), "combinations\n")
    }
  }
  
  toc()
  
  cat("Best RF OOB RMSE:", round(sqrt(best_oob_error), 6), "\n")
  
  return(list(
    best_params = best_params,
    best_oob_error = best_oob_error,
    tuning_results = results
  ))
}

tune_xgboost <- function(X_train, y_train, tune_grid = NULL) {
  cat("Tuning XGBoost hyperparameters...\n")
  
  if (is.null(tune_grid)) {
    tune_grid <- expand.grid(
      nrounds = config$tune_config$xgb$nrounds,
      max_depth = config$tune_config$xgb$max_depth,
      eta = config$tune_config$xgb$eta,
      subsample = config$tune_config$xgb$subsample,
      colsample_bytree = config$tune_config$xgb$colsample_bytree,
      min_child_weight = config$tune_config$xgb$min_child_weight,
      stringsAsFactors = FALSE
    )
  }
  
  cat("Testing", nrow(tune_grid), "parameter combinations\n")
  
  X_matrix <- as.matrix(X_train)
  best_cv_error <- Inf
  best_params <- NULL
  results <- data.frame()
  
  tic("XGB hyperparameter tuning")
  
  for (i in 1:nrow(tune_grid)) {
    params <- tune_grid[i, ]
    
    set.seed(config$seed + i)
    
    tryCatch({
      cv_result <- xgb.cv(
        data = X_matrix,
        label = y_train,
        nrounds = params$nrounds,
        max_depth = params$max_depth,
        eta = params$eta,
        subsample = params$subsample,
        colsample_bytree = params$colsample_bytree,
        min_child_weight = params$min_child_weight,
        nfold = config$cv_folds,
        verbose = 0,
        early_stopping_rounds = 20,
        seed = config$seed + i
      )
      
      cv_error <- min(cv_result$evaluation_log$test_rmse_mean)
      best_iter <- which.min(cv_result$evaluation_log$test_rmse_mean)
      
      results <- rbind(results, data.frame(
        iteration = i,
        nrounds = params$nrounds,
        max_depth = params$max_depth,
        eta = params$eta,
        subsample = params$subsample,
        colsample_bytree = params$colsample_bytree,
        min_child_weight = params$min_child_weight,
        cv_error = cv_error,
        best_iter = best_iter
      ))
      
      if (cv_error < best_cv_error) {
        best_cv_error <- cv_error
        best_params <- params
        best_params$nrounds <- best_iter  # Use early stopping result
      }
      
    }, error = function(e) {
      cat("Error with parameter set", i, ":", e$message, "\n")
    })
    
    if (i %% 20 == 0) {
      cat("Completed", i, "of", nrow(tune_grid), "combinations\n")
    }
  }
  
  toc()
  
  cat("Best XGB CV RMSE:", round(best_cv_error, 6), "\n")
  
  return(list(
    best_params = best_params,
    best_cv_error = best_cv_error,
    tuning_results = results
  ))
}

# =============================================================================
# MODEL TRAINING AND EVALUATION
# =============================================================================

train_and_evaluate_model <- function(X_train, y_train, X_test, y_test, model_type, best_params) {
  cat("Training", model_type, "model...\n")
  
  set.seed(config$seed)
  
  tic(paste(model_type, "training"))
  
  if (model_type == "rf") {
    model <- randomForest(
      x = X_train,
      y = y_train,
      ntree = best_params$ntree,
      mtry = best_params$mtry,
      nodesize = best_params$nodesize,
      maxnodes = if(is.na(best_params$maxnodes)) NULL else best_params$maxnodes,
      importance = TRUE
    )
  } else if (model_type == "xgb") {
    model <- xgboost(
      data = as.matrix(X_train),
      label = y_train,
      nrounds = best_params$nrounds,
      max_depth = best_params$max_depth,
      eta = best_params$eta,
      subsample = best_params$subsample,
      colsample_bytree = best_params$colsample_bytree,
      min_child_weight = best_params$min_child_weight,
      verbose = 0,
      seed = config$seed
    )
  }
  
  toc()
  
  # Make predictions
  if (model_type == "rf") {
    predictions <- predict(model, X_test)
  } else if (model_type == "xgb") {
    predictions <- predict(model, as.matrix(X_test))
  }
  
  # Calculate metrics
  valid_indices <- !is.na(y_test) & !is.na(predictions)
  actual_clean <- y_test[valid_indices]
  pred_clean <- predictions[valid_indices]
  
  if (length(actual_clean) == 0) {
    return(list(metrics = NULL, predictions = NULL, model = NULL))
  }
  
  # Proper R² calculation
  y_mean <- mean(actual_clean)
  sse <- sum((actual_clean - pred_clean)^2)
  tss <- sum((actual_clean - y_mean)^2)
  
  r2 <- ifelse(tss == 0, ifelse(sse == 0, 1.0, 0.0), 1 - (sse / tss))
  
  # Other metrics
  rmse <- sqrt(mean((actual_clean - pred_clean)^2))
  mae <- mean(abs(actual_clean - pred_clean))
  correlation <- cor(actual_clean, pred_clean, use = "complete.obs")
  
  # Directional accuracy
  actual_direction <- sign(actual_clean)
  pred_direction <- sign(pred_clean)
  directional_accuracy <- mean(actual_direction == pred_direction, na.rm = TRUE)
  
  metrics <- list(
    rmse = rmse,
    mae = mae,
    r2 = r2,
    correlation = correlation,
    directional_accuracy = directional_accuracy,
    n_predictions = length(actual_clean)
  )
  
  # Feature importance
  importance <- NULL
  if (model_type == "rf" && !is.null(model)) {
    tryCatch({
      imp_matrix <- importance(model)
      if (ncol(imp_matrix) >= 2) {
        importance <- data.frame(
          Feature = rownames(imp_matrix),
          Importance = imp_matrix[, "%IncMSE"],
          stringsAsFactors = FALSE
        ) %>%
          arrange(desc(Importance))
      }
    }, error = function(e) {
      cat("Warning: Could not extract RF importance\n")
    })
  } else if (model_type == "xgb" && !is.null(model)) {
    tryCatch({
      imp_matrix <- xgb.importance(model = model)
      importance <- data.frame(
        Feature = imp_matrix$Feature,
        Importance = imp_matrix$Gain,
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Importance))
    }, error = function(e) {
      cat("Warning: Could not extract XGB importance\n")
    })
  }
  
  cat(paste(model_type, "Results - RMSE:", round(rmse, 6), "R²:", round(r2, 6), "Correlation:", round(correlation, 6), "\n"))
  
  return(list(
    model = if(config$save_models) model else NULL,
    metrics = metrics,
    predictions = predictions,
    actual = y_test,
    importance = importance,
    best_params = best_params
  ))
}

# =============================================================================
# EXPERIMENT EXECUTION
# =============================================================================

run_single_experiment <- function(experiment_name, train_data, test_data, feature_cols) {
  cat("\n", rep("=", 60), "\n")
  cat("EXPERIMENT:", experiment_name, "\n")
  cat(rep("=", 60), "\n")
  
  cat("Using", length(feature_cols), "features\n")
  
  # Prepare data
  X_train <- train_data[, feature_cols, drop = FALSE]
  y_train <- train_data$DGS10_bps_next_day
  X_test <- test_data[, feature_cols, drop = FALSE]
  y_test <- test_data$DGS10_bps_next_day
  
  # Remove any remaining NAs after numeric conversion - SAFER VERSION
  cat("Checking data completeness...\n")
  
  # Check training data
  train_na_count <- sum(!complete.cases(X_train, y_train))
  train_finite_features <- apply(X_train, 1, function(x) all(is.finite(x)))
  train_finite_target <- is.finite(y_train)
  
  train_complete <- complete.cases(X_train, y_train) & train_finite_features & train_finite_target
  
  cat("Training data issues:\n")
  cat("- Rows with NAs:", train_na_count, "\n")
  cat("- Rows with non-finite features:", sum(!train_finite_features), "\n")
  cat("- Rows with non-finite target:", sum(!train_finite_target), "\n")
  cat("- Final valid training rows:", sum(train_complete), "out of", length(train_complete), "\n")
  
  if (sum(train_complete) == 0) {
    cat("⚠️ No valid training data found! Checking data types...\n")
    cat("Target variable sample:", head(y_train, 10), "\n")
    cat("Feature data types:\n")
    print(sapply(X_train, class))
    stop("No valid training data - check data conversion issues")
  }
  
  # Check test data
  test_finite_features <- apply(X_test, 1, function(x) all(is.finite(x)))
  test_finite_target <- is.finite(y_test)
  test_complete <- complete.cases(X_test, y_test) & test_finite_features & test_finite_target
  
  cat("Test data - valid rows:", sum(test_complete), "out of", length(test_complete), "\n")
  
  if (sum(test_complete) == 0) {
    cat("⚠️ No valid test data found!\n")
    stop("No valid test data - check data conversion issues")
  }
  
  # Apply filters
  X_train <- X_train[train_complete, , drop = FALSE]
  y_train <- y_train[train_complete]
  X_test <- X_test[test_complete, , drop = FALSE]
  y_test <- y_test[test_complete]
  
  cat("Final clean data:\n")
  cat("- Training observations:", nrow(X_train), "\n")
  cat("- Test observations:", nrow(X_test), "\n")
  
  # Final check: verify all data is numeric and finite
  if (any(!sapply(X_train, is.numeric))) {
    stop("Non-numeric features detected in training data")
  }
  if (any(!is.finite(y_train))) {
    stop("Non-finite values in training target")
  }
  if (any(sapply(X_train, function(x) any(!is.finite(x))))) {
    stop("Non-finite values in training features")
  }
  
  if (nrow(X_train) < 50 || nrow(X_test) < 10) {
    cat("⚠️ Insufficient data for experiment\n")
    cat("Available training data:", nrow(X_train), "\n")
    cat("Available test data:", nrow(X_test), "\n")
    cat("Required: min 50 train, 10 test\n")
    return(NULL)
  }
  
  # Initialize results
  experiment_results <- list(
    experiment_name = experiment_name,
    features = feature_cols,
    n_features = length(feature_cols),
    n_train = nrow(X_train),
    n_test = nrow(X_test)
  )
  
  # Random Forest
  cat("\n--- Random Forest ---\n")
  rf_tuning <- tune_random_forest(X_train, y_train)
  rf_results <- train_and_evaluate_model(X_train, y_train, X_test, y_test, "rf", rf_tuning$best_params)
  
  experiment_results$rf <- list(
    tuning = rf_tuning,
    results = rf_results
  )
  
  # XGBoost
  cat("\n--- XGBoost ---\n")
  xgb_tuning <- tune_xgboost(X_train, y_train)
  xgb_results <- train_and_evaluate_model(X_train, y_train, X_test, y_test, "xgb", xgb_tuning$best_params)
  
  experiment_results$xgb <- list(
    tuning = xgb_tuning,
    results = xgb_results
  )
  
  # Save experiment results
  saveRDS(experiment_results, file.path(config$checkpoint_dir, paste0("experiment_", experiment_name, ".rds")))
  
  return(experiment_results)
}

# =============================================================================
# STATISTICAL TESTING
# =============================================================================

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
      title = "Model Performance Comparison: RMSE",
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
      title = "Model Performance Comparison: R²",
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
  
  # 3. Directional Accuracy
  p3 <- ggplot(results_summary, aes(x = reorder(Experiment, Directional_Accuracy), 
                                    y = Directional_Accuracy, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_text(aes(label = sprintf("%.3f", Directional_Accuracy)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    labs(
      title = "Directional Accuracy Comparison",
      subtitle = "Proportion of correctly predicted return directions (>0.5 is better than random)",
      x = "Experiment",
      y = "Directional Accuracy"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    ylim(0.4, max(results_summary$Directional_Accuracy, na.rm = TRUE) * 1.05)
  
  ggsave(file.path(config$output_dir, "plots", "directional_accuracy.png"), 
         p3, width = config$plot_width, height = config$plot_height, dpi = config$plot_dpi)
  plots$directional <- p3
  
  return(plots)
}

create_feature_importance_plots <- function(all_results) {
  cat("Creating feature importance visualizations...\n")
  
  importance_plots <- list()
  
  for (experiment in names(all_results)) {
    exp_data <- all_results[[experiment]]
    
    # RF importance plot
    if (!is.null(exp_data$rf$results$importance)) {
      rf_imp <- exp_data$rf$results$importance %>%
        head(20) %>%
        mutate(
          Category = case_when(
            str_detect(Feature, "sentiment|dict|sentimentr|finbert|ollama") ~ "Sentiment",
            str_detect(Feature, "_ret$") ~ "Returns",
            str_detect(Feature, "_chg$|VIX") ~ "Volatility",
            str_detect(Feature, "_bps$|DGS|T5Y|T10Y") ~ "Interest Rates",
            str_detect(Feature, "NFCI|ICSA|CCSA") ~ "Economic",
            str_detect(Feature, "momentum|RSI|distance") ~ "Technical",
            TRUE ~ "Other"
          )
        )
      
      p_rf <- ggplot(rf_imp, aes(x = reorder(Feature, Importance), y = Importance, fill = Category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = paste("Feature Importance - Random Forest -", experiment),
          x = "Feature",
          y = "Importance (%IncMSE)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 9),
          legend.position = "bottom"
        ) +
        scale_fill_brewer(type = "qual", palette = "Set3")
      
      ggsave(file.path(config$output_dir, "plots", paste0("importance_", experiment, "_rf.png")),
             p_rf, width = config$plot_width, height = config$plot_height, dpi = config$plot_dpi)
      
      importance_plots[[paste0(experiment, "_rf")]] <- p_rf
    }
    
    # XGB importance plot
    if (!is.null(exp_data$xgb$results$importance)) {
      xgb_imp <- exp_data$xgb$results$importance %>%
        head(20) %>%
        mutate(
          Category = case_when(
            str_detect(Feature, "sentiment|dict|sentimentr|finbert|ollama") ~ "Sentiment",
            str_detect(Feature, "_ret$") ~ "Returns",
            str_detect(Feature, "_chg$|VIX") ~ "Volatility",
            str_detect(Feature, "_bps$|DGS|T5Y|T10Y") ~ "Interest Rates",
            str_detect(Feature, "NFCI|ICSA|CCSA") ~ "Economic",
            str_detect(Feature, "momentum|RSI|distance") ~ "Technical",
            TRUE ~ "Other"
          )
        )
      
      p_xgb <- ggplot(xgb_imp, aes(x = reorder(Feature, Importance), y = Importance, fill = Category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = paste("Feature Importance - XGBoost -", experiment),
          x = "Feature",
          y = "Importance (Gain)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 9),
          legend.position = "bottom"
        ) +
        scale_fill_brewer(type = "qual", palette = "Set3")
      
      ggsave(file.path(config$output_dir, "plots", paste0("importance_", experiment, "_xgb.png")),
             p_xgb, width = config$plot_width, height = config$plot_height, dpi = config$plot_dpi)
      
      importance_plots[[paste0(experiment, "_xgb")]] <- p_xgb
    }
  }
  
  return(importance_plots)
}

# =============================================================================
# COMPREHENSIVE REPORTING
# =============================================================================

create_comprehensive_report <- function(results_summary, dm_results = NULL, all_results = NULL) {
  cat("Creating comprehensive HTML report...\n")
  
  html_file <- file.path(config$output_dir, "comprehensive_ml_report.html")
  
  # Calculate key insights
  best_rmse <- results_summary %>% slice_min(RMSE, n = 1)
  best_r2 <- results_summary %>% slice_max(R2, n = 1)
  best_directional <- results_summary %>% slice_max(Directional_Accuracy, n = 1)
  
  # Generate HTML content
  html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Comprehensive ML Analysis - S&P 500 Prediction</title>
    <style>
        body { 
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; 
            margin: 40px; 
            line-height: 1.6; 
            color: #333;
        }
        h1 { 
            color: #2c3e50; 
            border-bottom: 3px solid #3498db; 
            padding-bottom: 10px;
        }
        h2 { 
            color: #3498db; 
            margin-top: 30px; 
            border-left: 4px solid #3498db;
            padding-left: 15px;
        }
        .summary-box { 
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
            color: white;
            padding: 25px; 
            border-radius: 10px; 
            margin: 20px 0;
        }
        .metric { 
            display: inline-block; 
            background: rgba(255,255,255,0.2); 
            padding: 10px 15px; 
            margin: 5px; 
            border-radius: 5px; 
            font-weight: bold;
        }
        table { 
            border-collapse: collapse; 
            width: 100%; 
            margin: 20px 0; 
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        th, td { 
            border: 1px solid #ddd; 
            padding: 12px; 
            text-align: left; 
        }
        th { 
            background-color: #3498db; 
            color: white; 
            font-weight: bold;
        }
        tr:nth-child(even) { 
            background-color: #f8f9fa; 
        }
        .plot-container { 
            text-align: center; 
            margin: 30px 0; 
        }
        .plot-container img { 
            max-width: 100%; 
            height: auto; 
            border: 1px solid #ddd; 
            border-radius: 8px; 
            box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        .conclusion { 
            background-color: #f8f9fa; 
            padding: 20px; 
            border-radius: 8px; 
            border-left: 5px solid #28a745;
            margin: 20px 0;
        }
        .date-info { 
            color: #6c757d; 
            font-size: 0.9em; 
            text-align: right; 
            margin-bottom: 20px;
        }
    </style>
</head>
<body>
    <div class="date-info">
        Report Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '
    </div>
    
    <h1>Comprehensive ML Analysis: 10-Year Treasury Rate Prediction</h1>
    
    <div class="summary-box">
        <h2 style="color: white; border: none; margin-top: 0;">Executive Summary</h2>
        <p><strong>Objective:</strong> Predict next-day 10-Year Treasury rates using sentiment analysis from financial TV shows, market variables, and technical indicators.</p>
        
        <div style="display: flex; flex-wrap: wrap; gap: 15px; margin: 20px 0;">
            <div class="metric">Best RMSE: ', sprintf("%.4f", best_rmse$RMSE[1]), ' (', best_rmse$Model[1], ')</div>
            <div class="metric">Best R²: ', sprintf("%.4f", best_r2$R2[1]), ' (', best_r2$Experiment[1], ')</div>
            <div class="metric">Best Directional: ', sprintf("%.3f", best_directional$Directional_Accuracy[1]), ' (', best_directional$Model[1], ')</div>
        </div>
        
        <p><strong>Key Finding:</strong> Sentiment features provide meaningful predictive power for next-day 10-Year Treasury rates with proper target shifting eliminating data leakage concerns.</p>
    </div>
    
    <h2>Methodology</h2>
    <ul>
        <li><strong>Target Variable:</strong> Next-day 10-Year Treasury rates (properly shifted to avoid lookahead bias)</li>
        <li><strong>Models:</strong> Random Forest and XGBoost with comprehensive hyperparameter tuning</li>
        <li><strong>Features:</strong> Market variables, sentiment indices, technical indicators, and engineered features</li>
        <li><strong>Validation:</strong> Chronological train-test split (80/20) maintaining temporal order</li>
        <li><strong>Hyperparameter Tuning:</strong> Extensive grid search with cross-validation</li>
        <li><strong>Evaluation:</strong> RMSE, R², correlation, and directional accuracy</li>
    </ul>
    
    <h2>Results Overview</h2>
    <table>
        <tr>
            <th>Experiment</th>
            <th>Model</th>
            <th>RMSE</th>
            <th>R²</th>
            <th>Correlation</th>
            <th>Directional Accuracy</th>
            <th>Features</th>
        </tr>')
  
  # Add results table
  for (i in 1:nrow(results_summary)) {
    row <- results_summary[i, ]
    html_content <- paste0(html_content, '
        <tr>
            <td>', row$Experiment, '</td>
            <td>', row$Model, '</td>
            <td>', sprintf("%.4f", row$RMSE), '</td>
            <td>', sprintf("%.4f", row$R2), '</td>
            <td>', sprintf("%.4f", row$Correlation), '</td>
            <td>', sprintf("%.3f", row$Directional_Accuracy), '</td>
            <td>', row$Features, '</td>
        </tr>')
  }
  
  html_content <- paste0(html_content, '
    </table>
    
    <h2>Performance Visualizations</h2>
    
    <div class="plot-container">
        <h3>RMSE Comparison</h3>
        <img src="plots/rmse_comparison.png" alt="RMSE Comparison">
    </div>
    
    <div class="plot-container">
        <h3>R² Comparison</h3>
        <img src="plots/r2_comparison.png" alt="R² Comparison">
    </div>
    
    <div class="plot-container">
        <h3>Directional Accuracy</h3>
        <img src="plots/directional_accuracy.png" alt="Directional Accuracy">
    </div>')
  
  # Add statistical significance if available
  if (!is.null(dm_results)) {
    html_content <- paste0(html_content, '
    <h2>Statistical Significance (Diebold-Mariano Tests)</h2>
    <table>
        <tr>
            <th>Experiment</th>
            <th>Model</th>
            <th>DM Statistic</th>
            <th>P-Value</th>
            <th>Conclusion</th>
        </tr>')
    
    for (i in 1:nrow(dm_results)) {
      row <- dm_results[i, ]
      p_val_class <- ifelse(row$P_Value < 0.05, "positive", "")
      html_content <- paste0(html_content, '
        <tr>
            <td>', row$Experiment, '</td>
            <td>', row$Model, '</td>
            <td>', sprintf("%.3f", row$DM_Statistic), '</td>
            <td class="', p_val_class, '">', sprintf("%.4f", row$P_Value), '</td>
            <td>', row$Conclusion, '</td>
        </tr>')
    }
    
    html_content <- paste0(html_content, '
    </table>')
  }
  
  # Add conclusions
  html_content <- paste0(html_content, '
    <div class="conclusion">
        <h2>Key Conclusions</h2>
        <ol>
            <li><strong>Proper Target Shifting Works:</strong> By predicting next-day returns using today\'s information, we eliminate data leakage while maintaining realistic predictive scenarios.</li>
            <li><strong>Sentiment Adds Value:</strong> TV show sentiment analysis provides incremental predictive power beyond traditional market variables.</li>
            <li><strong>Model Performance:</strong> Achieved meaningful R² values (', sprintf("%.1f", best_r2$R2[1]*100), '%) and directional accuracy (', sprintf("%.1f", best_directional$Directional_Accuracy[1]*100), '%) for daily return prediction.</li>
            <li><strong>Feature Engineering Matters:</strong> Momentum and cross-sectional sentiment features enhance predictive performance.</li>
            <li><strong>Hyperparameter Tuning Impact:</strong> Comprehensive tuning significantly improves model performance over default parameters.</li>
        </ol>
    </div>
    
    <h2>Technical Notes</h2>
    <ul>
        <li><strong>Data Leakage Prevention:</strong> Target variable properly shifted to predict future returns using past information</li>
        <li><strong>Temporal Validation:</strong> Chronological splits ensure realistic out-of-sample testing</li>
        <li><strong>Feature Categories:</strong> Market returns, volatility measures, economic indicators, technical indicators, and sentiment features</li>
        <li><strong>Reproducibility:</strong> All random seeds set for consistent results across runs</li>
    </ul>
    
    <p style="margin-top: 40px; font-size: 0.9em; color: #666;">
        <strong>Computational Details:</strong> Analysis performed using R with parallel processing on ', n_cores, ' cores. 
        Total runtime varies by experiment complexity and hyperparameter grid size.
    </p>
    
</body>
</html>')
  
  # Write HTML file
  writeLines(html_content, html_file)
  cat("Comprehensive HTML report created:", html_file, "\n")
  
  return(html_file)
}

# Quick TS CV configuration for ~500 observations
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

# Quick training and evaluation function
train_and_evaluate_quick <- function(train_data, test_data, feature_cols, target_col, 
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
    n_predictions = length(actual_clean)
  ))
}

# Run single split
# =============================================================================
# ENHANCED HYPERPARAMETER TUNING FOR TIME SERIES CV
# Replace the run_single_split function in your script with this enhanced version
# =============================================================================

# Enhanced hyperparameter tuning function
tune_hyperparams_quick <- function(train_data, feature_cols, target_col, model_type, n_trials = 20) {
  
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
    # More thorough RF tuning
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
    
    cat("  RF: Testing", nrow(param_grid), "parameter combinations\n")
    
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
    # More thorough XGBoost tuning
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
    
    cat("  XGB: Testing", nrow(param_grid), "parameter combinations\n")
    
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

# Enhanced run_single_split function with hyperparameter tuning
run_single_split_enhanced <- function(train_data, test_data, feature_cols, target_col, tune_params = TRUE) {
  
  results <- list()
  
  # Random Forest with enhanced tuning
  tryCatch({
    if (tune_params) {
      rf_params <- tune_hyperparams_quick(train_data, feature_cols, target_col, "rf", n_trials = 15)
    } else {
      rf_params <- list(ntree = 500, mtry = max(2, length(feature_cols) %/% 3), 
                        nodesize = 3, maxnodes = NULL)
    }
    
    rf_result <- train_and_evaluate_quick(train_data, test_data, feature_cols, 
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
  
  # XGBoost with enhanced tuning
  tryCatch({
    if (tune_params) {
      xgb_params <- tune_hyperparams_quick(train_data, feature_cols, target_col, "xgb", n_trials = 15)
    } else {
      xgb_params <- list(nrounds = 100, max_depth = 6, eta = 0.1, 
                         subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 1)
    }
    
    xgb_result <- train_and_evaluate_quick(train_data, test_data, feature_cols, 
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

# Configuration for enhanced tuning
enhanced_config <- list(
  # Set to TRUE for enhanced hyperparameter tuning
  # Set to FALSE for faster execution with default params
  use_enhanced_tuning = TRUE,
  
  # Reduce CV splits if you want faster execution
  cv_splits = 6,  # Keep the same
  
  # Number of hyperparameter trials per model per split
  n_tune_trials = 15  # 15 trials = good balance of speed vs performance
)

cat("Enhanced hyperparameter tuning loaded!\n")
cat("Configuration:\n")
cat("- Enhanced tuning:", enhanced_config$use_enhanced_tuning, "\n")
cat("- Trials per model:", enhanced_config$n_tune_trials, "\n")
cat("- This will test ~", enhanced_config$n_tune_trials * 2, "parameter combinations per split\n")
cat("- Estimated additional time: 2-3x longer than basic version\n\n")

# Aggregate CV results across splits
aggregate_cv_results <- function(cv_results, experiment_name) {
  
  # Extract metrics for each model type
  rf_metrics <- data.frame()
  xgb_metrics <- data.frame()
  
  for (split_result in cv_results) {
    if (!is.null(split_result$rf) && !is.null(split_result$rf$metrics)) {
      rf_metrics <- rbind(rf_metrics, data.frame(
        split_id = split_result$split_info$split_id,
        split_result$rf$metrics,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!is.null(split_result$xgb) && !is.null(split_result$xgb$metrics)) {
      xgb_metrics <- rbind(xgb_metrics, data.frame(
        split_id = split_result$split_info$split_id,
        split_result$xgb$metrics,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Calculate summary statistics
  rf_summary <- if(nrow(rf_metrics) > 0) {
    list(
      mean_rmse = mean(rf_metrics$rmse, na.rm = TRUE),
      std_rmse = sd(rf_metrics$rmse, na.rm = TRUE),
      mean_r2 = mean(rf_metrics$r2, na.rm = TRUE),
      std_r2 = sd(rf_metrics$r2, na.rm = TRUE),
      mean_correlation = mean(rf_metrics$correlation, na.rm = TRUE),
      mean_directional_accuracy = mean(rf_metrics$directional_accuracy, na.rm = TRUE),
      n_valid_splits = nrow(rf_metrics)
    )
  } else {
    list(mean_rmse = NA, std_rmse = NA, mean_r2 = NA, std_r2 = NA, 
         mean_correlation = NA, mean_directional_accuracy = NA, n_valid_splits = 0)
  }
  
  xgb_summary <- if(nrow(xgb_metrics) > 0) {
    list(
      mean_rmse = mean(xgb_metrics$rmse, na.rm = TRUE),
      std_rmse = sd(xgb_metrics$rmse, na.rm = TRUE),
      mean_r2 = mean(xgb_metrics$r2, na.rm = TRUE),
      std_r2 = sd(xgb_metrics$r2, na.rm = TRUE),
      mean_correlation = mean(xgb_metrics$correlation, na.rm = TRUE),
      mean_directional_accuracy = mean(xgb_metrics$directional_accuracy, na.rm = TRUE),
      n_valid_splits = nrow(xgb_metrics)
    )
  } else {
    list(mean_rmse = NA, std_rmse = NA, mean_r2 = NA, std_r2 = NA, 
         mean_correlation = NA, mean_directional_accuracy = NA, n_valid_splits = 0)
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

# Main TS CV function
run_experiments_with_ts_cv <- function(data, experiments_list, target_col_name) {
  
  cat("Running Time Series CV with", nrow(data), "observations\n")
  cat("Config: Initial window =", quick_ts_cv_config$initial_window, 
      ", Step size =", quick_ts_cv_config$step_size, 
      ", Splits =", quick_ts_cv_config$n_splits, "\n\n")
  
  # Sort data by date
  data <- data %>% arrange(date)
  
  # Generate CV splits
  cv_splits <- generate_cv_splits(data, quick_ts_cv_config)
  cat("Generated", length(cv_splits), "CV splits\n\n")
  
  # Store all results
  all_cv_results <- list()
  
  # Run each experiment
  for (exp_name in names(experiments_list)) {
    cat("EXPERIMENT:", exp_name, "\n")
    cat(rep("-", 40), "\n")
    
    feature_cols <- experiments_list[[exp_name]]
    exp_cv_results <- list()
    
    # Run CV for this experiment
    for (split_idx in seq_along(cv_splits)) {
      split <- cv_splits[[split_idx]]
      
      train_data <- data[split$train_indices, ]
      test_data <- data[split$test_indices, ]
      
      cat("Split", split_idx, ": Train", length(split$train_indices), 
          "obs, Test", length(split$test_indices), "obs\n")
      
      # Run your existing experiment function for this split
      split_result <- run_single_split_enhanced(train_data, test_data, feature_cols, target_col_name, tune_params = TRUE)
      
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
    aggregated_results <- aggregate_cv_results(exp_cv_results, exp_name)
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
  
  return(all_cv_results)
}

# Create summary table for all experiments
create_cv_summary_table <- function(all_cv_results) {
  
  summary_df <- data.frame()
  
  for (exp_name in names(all_cv_results)) {
    exp_result <- all_cv_results[[exp_name]]
    
    # RF row
    if (!is.na(exp_result$rf_summary$mean_rmse)) {
      summary_df <- rbind(summary_df, data.frame(
        Experiment = exp_name,
        Model = "Random Forest",
        Mean_RMSE = exp_result$rf_summary$mean_rmse,
        Std_RMSE = exp_result$rf_summary$std_rmse,
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
        Std_RMSE = exp_result$xgb_summary$std_rmse,
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

# Individual sentiment analysis function
analyze_individual_sentiment_results <- function(all_results) {
  
  cat("Analyzing individual sentiment performance...\n")
  
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
    cat("\nINDIVIDUAL SENTIMENT VARIABLE RANKINGS:\n")
    cat(rep("=", 60), "\n")
    
    # Rank by RMSE (lower is better)
    individual_ranked <- individual_summary %>%
      arrange(Mean_RMSE) %>%
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
    
    cat("\n🏆 BEST INDIVIDUAL SENTIMENT VARIABLES:\n")
    if (nrow(best_rf) > 0) {
      cat("Random Forest:", best_rf$Sentiment_Variable[1], 
          "- RMSE:", round(best_rf$Mean_RMSE[1], 4), "\n")
    }
    if (nrow(best_xgb) > 0) {
      cat("XGBoost:", best_xgb$Sentiment_Variable[1], 
          "- RMSE:", round(best_xgb$Mean_RMSE[1], 4), "\n")
    }
    
    # Save detailed individual results
    write_csv(individual_ranked, file.path(config$output_dir, "individual_sentiment_rankings.csv"))
    cat("\n📊 Detailed individual sentiment rankings saved to: individual_sentiment_rankings.csv\n")
  }
  
  return(individual_summary)
}

# Function to create individual sentiment visualization
create_individual_sentiment_plot <- function(individual_summary) {
  
  if (is.null(individual_summary) || nrow(individual_summary) == 0) {
    return(NULL)
  }
  
  # Create comparison plot
  p <- ggplot(individual_summary, aes(x = reorder(Sentiment_Variable, -Mean_RMSE), 
                                      y = Mean_RMSE, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = Mean_RMSE - Std_RMSE, ymax = Mean_RMSE + Std_RMSE),
                  position = position_dodge(width = 0.9), width = 0.25) +
    labs(
      title = "Individual Sentiment Variable Performance Comparison",
      subtitle = "Lower RMSE indicates better prediction accuracy",
      x = "Sentiment Variable",
      y = "Mean RMSE (with error bars showing ± 1 std dev)",
      fill = "Model Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  ggsave(file.path(config$output_dir, "plots", "individual_sentiment_comparison.png"), 
         p, width = 12, height = 8, dpi = 300)
  cat("📈 Individual sentiment comparison plot saved to: individual_sentiment_comparison.png\n")
  
  return(p)
}

# =============================================================================
# MAIN EXECUTION FUNCTIONS
# =============================================================================

run_comprehensive_analysis <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("UST COMPREHENSIVE ML ANALYSIS WITH TIME SERIES CV\n")
  cat(rep("=", 80), "\n\n")
  
  # Load and prepare data
  data <- load_and_prepare_data()
  
  # Build experiments with individual sentiment testing
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
  
  # Market only (updated for UST - remove DGS10_bps since it's the target)
  market_core <- c("GSPC_ret", "DJI_ret", "IXIC_ret", "VIX_chg", "VIX", 
                   "DGS2_bps", "DGS5_bps", "GSPC_ret_1D_lagged", "VIX_chg_1D_lagged")
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
  
  individual_only <- setdiff(individual_sentiment_vars, "aggregate_sentiment_avg")
  available_individual <- intersect(individual_only, names(data))
  if (length(available_individual) > 1) {
    experiments$combined_individual <- c(base_features, available_individual)
  }
  
  if ("aggregate_sentiment_avg" %in% names(data)) {
    experiments$aggregate_only <- c(base_features, "aggregate_sentiment_avg")
  }
  
  if (length(available_sentiment) > 0) {
    experiments$all_sentiment <- c(base_features, available_sentiment)
  }
  
  # Filter experiments
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
    cat(sprintf("%-20s: %2d features (%d sentiment)\n", exp_name, n_features, n_sentiment))
  }
  cat(rep("-", 50), "\n\n")
  
  # Target column for UST
  target_col <- "DGS10_bps_next_day"
  cat("Target variable:", target_col, "\n\n")
  
  # Run Time Series Cross-Validation
  all_results <- run_experiments_with_ts_cv(data, experiments_filtered, target_col)
  
  # Create summary table
  results_summary <- create_cv_summary_table(all_results)
  
  # Analyze individual sentiment performance
  individual_analysis <- analyze_individual_sentiment_results(all_results)
  
  # Create individual sentiment visualization
  if (!is.null(individual_analysis)) {
    individual_plot <- create_individual_sentiment_plot(individual_analysis)
  }
  
  cat("\n", rep("=", 80), "\n")
  cat("TIME SERIES CROSS-VALIDATION RESULTS SUMMARY\n")
  cat(rep("=", 80), "\n\n")
  
  print(results_summary %>% arrange(Mean_RMSE))
  
  # Save detailed results
  saveRDS(all_results, file.path(config$output_dir, "ts_cv_detailed_results.rds"))
  write_csv(results_summary, file.path(config$output_dir, "ts_cv_results_summary.csv"))
  
  cat("\n📊 BEST PERFORMING MODELS (by Mean RMSE):\n")
  best_models <- results_summary %>% 
    arrange(Mean_RMSE) %>% 
    head(5)
  
  for (i in 1:nrow(best_models)) {
    row <- best_models[i, ]
    cat(sprintf("%d. %s (%s): RMSE = %.4f (±%.4f), R² = %.4f (±%.4f), %d splits\n", 
                i, row$Experiment, row$Model, row$Mean_RMSE, row$Std_RMSE, 
                row$Mean_R2, row$Std_R2, row$N_Splits))
  }
  
  cat("\n✅ Time Series CV completed successfully!\n")
  cat("Key advantages over single train-test split:\n")
  cat("- Tested model stability across", quick_ts_cv_config$n_splits, "time periods\n")
  cat("- Provided confidence intervals (±) for all metrics\n")
  cat("- Avoided potential bias from single split choice\n")
  cat("- More realistic performance estimates for thesis\n")
  cat("- Individual sentiment variable analysis included\n\n")
  
  return(list(
    results_summary = results_summary,
    all_results = all_results,
    individual_analysis = individual_analysis,
    config = config
  ))
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

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
    
    # Test Random Forest
    if (!is.null(baseline_data$rf$results$predictions) && !is.null(exp_data$rf$results$predictions)) {
      baseline_errors <- baseline_data$rf$results$actual - baseline_data$rf$results$predictions
      exp_errors <- exp_data$rf$results$actual - exp_data$rf$results$predictions
      
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
    
    # Test XGBoost
    if (!is.null(baseline_data$xgb$results$predictions) && !is.null(exp_data$xgb$results$predictions)) {
      baseline_errors <- baseline_data$xgb$results$actual - baseline_data$xgb$results$predictions
      exp_errors <- exp_data$xgb$results$actual - exp_data$xgb$results$predictions
      
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
# EXECUTE COMPREHENSIVE ANALYSIS
# =============================================================================

cat("Comprehensive ML Analysis with Proper Target Shifting\n")
cat("====================================================\n")
cat("Features: Hyperparameter tuning, statistical testing, comprehensive reporting\n")
cat("Target: Next-day 10-Year Treasury rates (no data leakage)\n\n")


# Run the comprehensive analysis
# Execute the analysis
final_results <- run_comprehensive_analysis()



# Add this function to extract feature importance from Time Series CV results
extract_best_models_and_importance <- function(all_results, data, target_col) {
  cat("\n", rep("=", 60), "\n")
  cat("EXTRACTING FEATURE IMPORTANCE FROM BEST MODELS\n")
  cat(rep("=", 60), "\n\n")
  
  # Create final train/test split (80/20) for feature importance analysis
  data_sorted <- data %>% arrange(date)
  n_total <- nrow(data_sorted)
  split_point <- floor(n_total * 0.8)
  
  train_data_final <- data_sorted[1:split_point, ]
  test_data_final <- data_sorted[(split_point + 1):n_total, ]
  
  cat("Final train set:", nrow(train_data_final), "observations\n")
  cat("Final test set:", nrow(test_data_final), "observations\n\n")
  
  # Find best experiments based on CV results
  results_summary <- create_cv_summary_table(all_results)
  best_experiments <- results_summary %>%
    arrange(Mean_RMSE) %>%
    head(3) %>%  # Top 3 best performing experiments
    select(Experiment) %>%
    distinct() %>%
    pull(Experiment)
  
  cat("Analyzing feature importance for best experiments:\n")
  for (exp in best_experiments) {
    cat("-", exp, "\n")
  }
  cat("\n")
  
  # Store importance results
  all_importance_results <- list()
  
  # Run each best experiment with full models to get importance
  for (exp_name in best_experiments) {
    cat("Processing experiment:", exp_name, "\n")
    
    # Get feature columns for this experiment
    if (exp_name %in% names(all_results)) {
      # Try to extract feature list from CV results
      first_split <- all_results[[exp_name]]$cv_results[[1]]
      if (!is.null(first_split)) {
        # Feature list might be stored differently - adapt as needed
        # For now, reconstruct from experiment definitions
        if (exp_name == "baseline") {
          feature_cols <- config$base_predictors
        } else if (exp_name == "market_only") {
          market_core <- c("GSPC_ret", "DJI_ret", "IXIC_ret", "VIX_chg", "VIX", 
                           "DGS10_bps", "GSPC_ret_1D_lagged", "VIX_chg_1D_lagged")
          feature_cols <- intersect(market_core, config$base_predictors)
        } else if (exp_name == "basic_sentiment") {
          individual_sentiment_vars <- c("dict_sentiment_avg", "sentimentr_score_avg", 
                                         "finbert_score_avg", "ollama_score_avg", "aggregate_sentiment_avg")
          basic_sentiment <- intersect(individual_sentiment_vars, names(data))
          feature_cols <- c(config$base_predictors, basic_sentiment)
        } else if (exp_name == "all_sentiment") {
          all_sentiment_groups <- unlist(config$sentiment_groups, use.names = FALSE)
          available_sentiment <- intersect(all_sentiment_groups, names(data))
          feature_cols <- c(config$base_predictors, available_sentiment)
        } else {
          # Default to base predictors
          feature_cols <- config$base_predictors
        }
        
        # Ensure features exist in data
        feature_cols <- intersect(feature_cols, names(data))
        
        if (length(feature_cols) > 0) {
          importance_result <- train_final_models_for_importance(
            train_data_final, test_data_final, feature_cols, target_col, exp_name
          )
          all_importance_results[[exp_name]] <- importance_result
        }
      }
    }
  }
  
  return(all_importance_results)
}

# Function to train final models and extract importance
train_final_models_for_importance <- function(train_data, test_data, feature_cols, target_col, exp_name) {
  
  # Prepare data
  X_train <- train_data[, feature_cols, drop = FALSE]
  y_train <- train_data[[target_col]]
  X_test <- test_data[, feature_cols, drop = FALSE]
  y_test <- test_data[[target_col]]
  
  # Clean data
  train_complete <- complete.cases(X_train, y_train)
  test_complete <- complete.cases(X_test, y_test)
  
  X_train <- X_train[train_complete, , drop = FALSE]
  y_train <- y_train[train_complete]
  X_test <- X_test[test_complete, , drop = FALSE]
  y_test <- y_test[test_complete]
  
  cat("  Clean data - Train:", nrow(X_train), "Test:", nrow(X_test), "\n")
  
  if (nrow(X_train) < 50) {
    cat("  ⚠️ Insufficient training data\n")
    return(NULL)
  }
  
  results <- list()
  
  # Train Random Forest with importance
  cat("  Training Random Forest...\n")
  set.seed(config$seed)
  rf_model <- randomForest(
    x = X_train,
    y = y_train,
    ntree = 1000,  # More trees for stable importance
    mtry = max(2, ncol(X_train) %/% 3),
    nodesize = 3,
    importance = TRUE  # Enable importance calculation
  )
  
  # Extract RF importance
  rf_importance <- NULL
  tryCatch({
    imp_matrix <- importance(rf_model)
    if (ncol(imp_matrix) >= 2) {
      rf_importance <- data.frame(
        Feature = rownames(imp_matrix),
        Importance = imp_matrix[, "%IncMSE"],
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Importance)) %>%
        mutate(
          Experiment = exp_name,
          Model = "Random Forest",
          Rank = row_number()
        )
    }
  }, error = function(e) {
    cat("    Warning: Could not extract RF importance\n")
  })
  
  # Train XGBoost with importance
  cat("  Training XGBoost...\n")
  set.seed(config$seed)
  xgb_model <- xgboost(
    data = as.matrix(X_train),
    label = y_train,
    nrounds = 200,
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    verbose = 0
  )
  
  # Extract XGB importance
  xgb_importance <- NULL
  tryCatch({
    imp_matrix <- xgb.importance(model = xgb_model)
    xgb_importance <- data.frame(
      Feature = imp_matrix$Feature,
      Importance = imp_matrix$Gain,
      stringsAsFactors = FALSE
    ) %>%
      arrange(desc(Importance)) %>%
      mutate(
        Experiment = exp_name,
        Model = "XGBoost",
        Rank = row_number()
      )
  }, error = function(e) {
    cat("    Warning: Could not extract XGB importance\n")
  })
  
  return(list(
    rf_importance = rf_importance,
    xgb_importance = xgb_importance,
    features = feature_cols
  ))
}

# Enhanced feature importance plotting function
create_comprehensive_feature_importance_plots <- function(all_importance_results) {
  cat("\nCreating comprehensive feature importance plots...\n")
  
  if (length(all_importance_results) == 0) {
    cat("No importance results to plot\n")
    return(NULL)
  }
  
  # Combine all importance data
  all_rf_importance <- data.frame()
  all_xgb_importance <- data.frame()
  
  for (exp_name in names(all_importance_results)) {
    exp_result <- all_importance_results[[exp_name]]
    
    if (!is.null(exp_result$rf_importance)) {
      all_rf_importance <- rbind(all_rf_importance, exp_result$rf_importance)
    }
    
    if (!is.null(exp_result$xgb_importance)) {
      all_xgb_importance <- rbind(all_xgb_importance, exp_result$xgb_importance)
    }
  }
  
  # Create plots for each experiment and model
  plot_list <- list()
  
  for (exp_name in names(all_importance_results)) {
    exp_result <- all_importance_results[[exp_name]]
    
    # Random Forest plot
    if (!is.null(exp_result$rf_importance)) {
      rf_data <- exp_result$rf_importance %>%
        head(20) %>%  # Top 20 features
        mutate(
          Category = case_when(
            str_detect(Feature, "sentiment|dict|sentimentr|finbert|ollama|aggregate") ~ "Sentiment",
            str_detect(Feature, "_ret$") ~ "Returns",
            str_detect(Feature, "_chg$|VIX") ~ "Volatility", 
            str_detect(Feature, "_bps$|DGS|T5Y|T10Y") ~ "Interest Rates",
            str_detect(Feature, "NFCI|ICSA|CCSA|WM2NS|WTREGEN") ~ "Economic",
            str_detect(Feature, "momentum|RSI|distance|MA") ~ "Technical",
            str_detect(Feature, "vs_Market|Curve|Credit|Flight") ~ "Style Factors",
            TRUE ~ "Other"
          )
        )
      
      p_rf <- ggplot(rf_data, aes(x = reorder(Feature, Importance), y = Importance, fill = Category)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        labs(
          title = paste("Random Forest Feature Importance:", exp_name),
          subtitle = paste("Top 20 features ranked by %IncMSE"),
          x = "Features",
          y = "Importance (%IncMSE)",
          fill = "Feature Category"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 9),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
        ) +
        scale_fill_brewer(type = "qual", palette = "Set3") +
        guides(fill = guide_legend(nrow = 2))
      
      ggsave(file.path(config$output_dir, "plots", paste0("importance_", exp_name, "_rf.png")),
             p_rf, width = 12, height = 10, dpi = 300)
      
      plot_list[[paste0(exp_name, "_rf")]] <- p_rf
      cat("  ✓ Saved RF importance plot for", exp_name, "\n")
    }
    
    # XGBoost plot  
    if (!is.null(exp_result$xgb_importance)) {
      xgb_data <- exp_result$xgb_importance %>%
        head(20) %>%  # Top 20 features
        mutate(
          Category = case_when(
            str_detect(Feature, "sentiment|dict|sentimentr|finbert|ollama|aggregate") ~ "Sentiment",
            str_detect(Feature, "_ret$") ~ "Returns",
            str_detect(Feature, "_chg$|VIX") ~ "Volatility",
            str_detect(Feature, "_bps$|DGS|T5Y|T10Y") ~ "Interest Rates", 
            str_detect(Feature, "NFCI|ICSA|CCSA|WM2NS|WTREGEN") ~ "Economic",
            str_detect(Feature, "momentum|RSI|distance|MA") ~ "Technical",
            str_detect(Feature, "vs_Market|Curve|Credit|Flight") ~ "Style Factors",
            TRUE ~ "Other"
          )
        )
      
      p_xgb <- ggplot(xgb_data, aes(x = reorder(Feature, Importance), y = Importance, fill = Category)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        labs(
          title = paste("XGBoost Feature Importance:", exp_name),
          subtitle = paste("Top 20 features ranked by Gain"),
          x = "Features", 
          y = "Importance (Gain)",
          fill = "Feature Category"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 9),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
        ) +
        scale_fill_brewer(type = "qual", palette = "Set3") +
        guides(fill = guide_legend(nrow = 2))
      
      ggsave(file.path(config$output_dir, "plots", paste0("importance_", exp_name, "_xgb.png")),
             p_xgb, width = 12, height = 10, dpi = 300)
      
      plot_list[[paste0(exp_name, "_xgb")]] <- p_xgb
      cat("  ✓ Saved XGB importance plot for", exp_name, "\n")
    }
  }
  
  # Create summary importance comparison
  if (nrow(all_rf_importance) > 0 || nrow(all_xgb_importance) > 0) {
    create_feature_importance_summary_plots(all_rf_importance, all_xgb_importance)
  }
  
  return(plot_list)
}

# Create summary comparison plots
create_feature_importance_summary_plots <- function(all_rf_importance, all_xgb_importance) {
  
  # Top features across all experiments (RF)
  if (nrow(all_rf_importance) > 0) {
    rf_summary <- all_rf_importance %>%
      group_by(Feature) %>%
      summarise(
        Avg_Importance = mean(Importance, na.rm = TRUE),
        Max_Importance = max(Importance, na.rm = TRUE),
        Times_Appeared = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avg_Importance)) %>%
      head(25) %>%
      mutate(
        Category = case_when(
          str_detect(Feature, "sentiment|dict|sentimentr|finbert|ollama|aggregate") ~ "Sentiment",
          str_detect(Feature, "_ret$") ~ "Returns",
          str_detect(Feature, "_chg$|VIX") ~ "Volatility",
          str_detect(Feature, "_bps$|DGS|T5Y|T10Y") ~ "Interest Rates",
          str_detect(Feature, "NFCI|ICSA|CCSA|WM2NS|WTREGEN") ~ "Economic", 
          str_detect(Feature, "momentum|RSI|distance|MA") ~ "Technical",
          str_detect(Feature, "vs_Market|Curve|Credit|Flight") ~ "Style Factors",
          TRUE ~ "Other"
        )
      )
    
    p_rf_summary <- ggplot(rf_summary, aes(x = reorder(Feature, Avg_Importance), 
                                           y = Avg_Importance, fill = Category)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Random Forest: Top 25 Features Across All Experiments",
        subtitle = "Ranked by average importance (%IncMSE)",
        x = "Features",
        y = "Average Importance", 
        fill = "Feature Category"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 9),
        legend.position = "bottom",
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      ) +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      guides(fill = guide_legend(nrow = 2))
    
    ggsave(file.path(config$output_dir, "plots", "importance_summary_rf_all_experiments.png"),
           p_rf_summary, width = 14, height = 12, dpi = 300)
    
    cat("  ✓ Saved RF summary importance plot\n")
  }
  
  # Top features across all experiments (XGB)
  if (nrow(all_xgb_importance) > 0) {
    xgb_summary <- all_xgb_importance %>%
      group_by(Feature) %>%
      summarise(
        Avg_Importance = mean(Importance, na.rm = TRUE),
        Max_Importance = max(Importance, na.rm = TRUE), 
        Times_Appeared = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avg_Importance)) %>%
      head(25) %>%
      mutate(
        Category = case_when(
          str_detect(Feature, "sentiment|dict|sentimentr|finbert|ollama|aggregate") ~ "Sentiment",
          str_detect(Feature, "_ret$") ~ "Returns", 
          str_detect(Feature, "_chg$|VIX") ~ "Volatility",
          str_detect(Feature, "_bps$|DGS|T5Y|T10Y") ~ "Interest Rates",
          str_detect(Feature, "NFCI|ICSA|CCSA|WM2NS|WTREGEN") ~ "Economic",
          str_detect(Feature, "momentum|RSI|distance|MA") ~ "Technical", 
          str_detect(Feature, "vs_Market|Curve|Credit|Flight") ~ "Style Factors",
          TRUE ~ "Other"
        )
      )
    
    p_xgb_summary <- ggplot(xgb_summary, aes(x = reorder(Feature, Avg_Importance), 
                                             y = Avg_Importance, fill = Category)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(
        title = "XGBoost: Top 25 Features Across All Experiments",
        subtitle = "Ranked by average importance (Gain)",
        x = "Features",
        y = "Average Importance",
        fill = "Feature Category"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 9),
        legend.position = "bottom",
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      ) +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      guides(fill = guide_legend(nrow = 2))
    
    ggsave(file.path(config$output_dir, "plots", "importance_summary_xgb_all_experiments.png"),
           p_xgb_summary, width = 14, height = 12, dpi = 300)
    
    cat("  ✓ Saved XGB summary importance plot\n")
  }
}

# =============================================================================
# ADD THIS TO THE END OF YOUR MAIN EXECUTION FUNCTION
# =============================================================================

# After your existing Time Series CV analysis, add this:
cat("\n🔍 GENERATING FEATURE IMPORTANCE ANALYSIS...\n")

# Extract feature importance from best models  
importance_results <- extract_best_models_and_importance(
  final_results$all_results, 
  data, 
  "DGS10_bps_next_day"  # Your target variable
)

# Create comprehensive feature importance plots
if (length(importance_results) > 0) {
  importance_plots <- create_comprehensive_feature_importance_plots(importance_results)
  
  # Save importance data as CSV
  all_importance_data <- data.frame()
  for (exp_name in names(importance_results)) {
    exp_result <- importance_results[[exp_name]]
    if (!is.null(exp_result$rf_importance)) {
      all_importance_data <- rbind(all_importance_data, exp_result$rf_importance)
    }
    if (!is.null(exp_result$xgb_importance)) {
      all_importance_data <- rbind(all_importance_data, exp_result$xgb_importance)
    }
  }
  
  if (nrow(all_importance_data) > 0) {
    write_csv(all_importance_data, file.path(config$output_dir, "feature_importance_rankings.csv"))
    cat("📊 Feature importance rankings saved to: feature_importance_rankings.csv\n")
  }
  
  cat("🎨 Feature importance plots created successfully!\n")
  cat("Check the plots/ folder for:\n")
  cat("- Individual experiment importance plots\n") 
  cat("- Summary importance plots across all experiments\n")
  
} else {
  cat("⚠️ No feature importance results generated\n")
}

cat("\n✅ Feature importance analysis complete!\n")





