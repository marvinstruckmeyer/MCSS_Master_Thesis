# =============================================================================
# QUICK TIME SERIES CV REPLACEMENT FOR YOUR THREE SCRIPTS
# Optimized for ~500 observations and thesis deadline constraints
# =============================================================================

# CONFIGURATION FOR 500 OBSERVATIONS
quick_ts_cv_config <- list(
  initial_window = 200,     # 40% for initial training (200 obs)
  step_size = 30,          # Move forward by 30 obs each time
  n_splits = 6,            # 6 validation windows (manageable number)
  purge_length = 1,        # 1-day gap to prevent lookahead
  min_test_size = 20       # Minimum 20 observations for each test
)

# =============================================================================
# SINGLE FUNCTION TO REPLACE YOUR TRAIN-TEST SPLIT IN ALL THREE SCRIPTS
# =============================================================================

# This replaces the data splitting section in ALL your scripts
run_experiments_with_ts_cv <- function(data, experiments_list, target_col_name) {
  
  cat("Running Time Series CV with", nrow(data), "observations\n")
  cat("Config: Initial window =", quick_ts_cv_config$initial_window, 
      ", Step size =", quick_ts_cv_config$step_size, 
      ", Splits =", quick_ts_cv_config$n_splits, "\n\n")
  
  # Sort data by date
  data <- data %>% arrange(date)
  n_obs <- nrow(data)
  
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
      split_result <- run_single_split(train_data, test_data, feature_cols, target_col_name)
      
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
    cat("Mean RMSE (RF):", round(aggregated_results$rf_summary$mean_rmse, 4), 
        "(±", round(aggregated_results$rf_summary$std_rmse, 4), ")\n")
    cat("Mean RMSE (XGB):", round(aggregated_results$xgb_summary$mean_rmse, 4), 
        "(±", round(aggregated_results$xgb_summary$std_rmse, 4), ")\n\n")
  }
  
  return(all_cv_results)
}

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

# Run single split (uses your existing model training logic)
run_single_split <- function(train_data, test_data, feature_cols, target_col) {
  
  # This is simplified hyperparameter tuning for speed
  quick_rf_params <- list(ntree = 500, mtry = max(2, length(feature_cols) %/% 3), 
                          nodesize = 3, maxnodes = NULL)
  quick_xgb_params <- list(nrounds = 100, max_depth = 6, eta = 0.1, 
                           subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 1)
  
  results <- list()
  
  # Random Forest
  tryCatch({
    rf_result <- train_and_evaluate_quick(train_data, test_data, feature_cols, 
                                          target_col, "rf", quick_rf_params)
    results$rf <- rf_result
  }, error = function(e) {
    cat("RF error:", e$message, "\n")
    results$rf <- NULL
  })
  
  # XGBoost
  tryCatch({
    xgb_result <- train_and_evaluate_quick(train_data, test_data, feature_cols, 
                                           target_col, "xgb", quick_xgb_params)
    results$xgb <- xgb_result
  }, error = function(e) {
    cat("XGB error:", e$message, "\n")
    results$xgb <- NULL
  })
  
  return(results)
}

# Quick training and evaluation (simplified from your existing code)
train_and_evaluate_quick <- function(train_data, test_data, feature_cols, target_col, 
                                     model_type, params) {
  
  # Use your existing prepare_model_data function or similar
  # Prepare training data
  train_features <- train_data[, feature_cols, drop = FALSE]
  train_target <- train_data[[target_col]]
  
  # Prepare test data  
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
  
  # Calculate metrics (using your existing logic)
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

cat("Quick Time Series CV framework loaded!\n")
cat("Optimized for ~500 observations and thesis deadlines\n")
cat("Configuration: 200 initial window, 30 step size, 6 splits\n")
cat("This will give you robust validation without excessive computation time.\n\n")

cat("USAGE:\n")
cat("1. Replace your data splitting section with this function\n")
cat("2. Define experiments_list with your feature combinations\n")
cat("3. Run: results <- run_experiments_with_ts_cv(data, experiments_list, target_col)\n")
cat("4. Get summary: summary_table <- create_cv_summary_table(results)\n")