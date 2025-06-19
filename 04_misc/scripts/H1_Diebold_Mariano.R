# =============================================================================
# EXTRACT PREDICTION ERRORS FROM SAVED RESULTS FOR DIEBOLD-MARIANO TEST
# =============================================================================

library(dplyr)
library(forecast) # for dm.test

# =============================================================================
# FUNCTION TO EXTRACT ERRORS FROM SAVED TS CV RESULTS
# =============================================================================

extract_prediction_errors_from_cv <- function(cv_results_path, baseline_exp = "baseline", 
                                              target_experiments = NULL) {
  
  cat("Loading saved CV results from:", cv_results_path, "\n")
  
  # Load the saved results
  if (!file.exists(cv_results_path)) {
    stop("Results file not found: ", cv_results_path)
  }
  
  all_results <- readRDS(cv_results_path)
  cat("Loaded results for", length(all_results), "experiments\n")
  
  # Print available experiments
  cat("Available experiments:\n")
  for (exp_name in names(all_results)) {
    cat("-", exp_name, "\n")
  }
  
  # Extract prediction errors for each experiment
  prediction_errors <- list()
  
  for (exp_name in names(all_results)) {
    exp_result <- all_results[[exp_name]]
    
    cat("\nProcessing experiment:", exp_name, "\n")
    
    # Extract errors from each CV split
    rf_errors_all <- c()
    xgb_errors_all <- c()
    
    if (!is.null(exp_result$cv_results)) {
      for (split_idx in seq_along(exp_result$cv_results)) {
        split_result <- exp_result$cv_results[[split_idx]]
        
        # Random Forest errors
        if (!is.null(split_result$rf) && !is.null(split_result$rf$predictions) && 
            !is.null(split_result$rf$actual)) {
          
          actual <- split_result$rf$actual
          pred <- split_result$rf$predictions
          
          # Only use valid predictions
          valid_idx <- !is.na(actual) & !is.na(pred)
          if (sum(valid_idx) > 0) {
            errors <- actual[valid_idx] - pred[valid_idx]
            rf_errors_all <- c(rf_errors_all, errors)
          }
        }
        
        # XGBoost errors
        if (!is.null(split_result$xgb) && !is.null(split_result$xgb$predictions) && 
            !is.null(split_result$xgb$actual)) {
          
          actual <- split_result$xgb$actual
          pred <- split_result$xgb$predictions
          
          # Only use valid predictions
          valid_idx <- !is.na(actual) & !is.na(pred)
          if (sum(valid_idx) > 0) {
            errors <- actual[valid_idx] - pred[valid_idx]
            xgb_errors_all <- c(xgb_errors_all, errors)
          }
        }
      }
    }
    
    prediction_errors[[exp_name]] <- list(
      rf_errors = if(length(rf_errors_all) > 0) rf_errors_all else NULL,
      xgb_errors = if(length(xgb_errors_all) > 0) xgb_errors_all else NULL
    )
    
    cat("  RF errors:", length(rf_errors_all), "observations\n")
    cat("  XGB errors:", length(xgb_errors_all), "observations\n")
  }
  
  return(prediction_errors)
}

# =============================================================================
# PROPER DIEBOLD-MARIANO TEST IMPLEMENTATION
# =============================================================================

run_diebold_mariano_tests <- function(prediction_errors, baseline_exp = "baseline") {
  
  cat("\n", rep("=", 60), "\n")
  cat("RUNNING DIEBOLD-MARIANO TESTS\n")
  cat(rep("=", 60), "\n\n")
  
  if (!baseline_exp %in% names(prediction_errors)) {
    stop("Baseline experiment '", baseline_exp, "' not found in results")
  }
  
  baseline_errors <- prediction_errors[[baseline_exp]]
  dm_results <- data.frame()
  
  # Test each experiment against baseline
  for (exp_name in names(prediction_errors)) {
    if (exp_name == baseline_exp) next
    
    exp_errors <- prediction_errors[[exp_name]]
    
    cat("Testing:", exp_name, "vs", baseline_exp, "\n")
    
    # Test Random Forest
    if (!is.null(baseline_errors$rf_errors) && !is.null(exp_errors$rf_errors)) {
      
      # Ensure equal length (take minimum)
      min_len <- min(length(baseline_errors$rf_errors), length(exp_errors$rf_errors))
      
      if (min_len >= 30) {  # Need sufficient observations for DM test
        base_err <- baseline_errors$rf_errors[1:min_len]
        exp_err <- exp_errors$rf_errors[1:min_len]
        
        # Run DM test using forecast package
        tryCatch({
          dm_result <- dm.test(base_err, exp_err, h = 1, power = 2)
          
          dm_results <- rbind(dm_results, data.frame(
            Experiment = exp_name,
            Model = "Random Forest",
            DM_Statistic = as.numeric(dm_result$statistic),
            P_Value = as.numeric(dm_result$p.value),
            N_Obs = min_len,
            Conclusion = case_when(
              dm_result$p.value < 0.001 ~ "Highly significant (p < 0.001)",
              dm_result$p.value < 0.01 ~ "Very significant (p < 0.01)",
              dm_result$p.value < 0.05 ~ "Significant (p < 0.05)",
              dm_result$p.value < 0.10 ~ "Marginally significant (p < 0.10)",
              TRUE ~ "Not significant (p >= 0.10)"
            ),
            stringsAsFactors = FALSE
          ))
          
          cat("  RF: DM =", round(as.numeric(dm_result$statistic), 3), 
              ", p =", round(as.numeric(dm_result$p.value), 4), "\n")
          
        }, error = function(e) {
          cat("  RF: Error in DM test -", e$message, "\n")
        })
        
      } else {
        cat("  RF: Insufficient observations (", min_len, ")\n")
      }
    }
    
    # Test XGBoost
    if (!is.null(baseline_errors$xgb_errors) && !is.null(exp_errors$xgb_errors)) {
      
      # Ensure equal length (take minimum)
      min_len <- min(length(baseline_errors$xgb_errors), length(exp_errors$xgb_errors))
      
      if (min_len >= 30) {  # Need sufficient observations for DM test
        base_err <- baseline_errors$xgb_errors[1:min_len]
        exp_err <- exp_errors$xgb_errors[1:min_len]
        
        # Run DM test using forecast package
        tryCatch({
          dm_result <- dm.test(base_err, exp_err, h = 1, power = 2)
          
          dm_results <- rbind(dm_results, data.frame(
            Experiment = exp_name,
            Model = "XGBoost", 
            DM_Statistic = as.numeric(dm_result$statistic),
            P_Value = as.numeric(dm_result$p.value),
            N_Obs = min_len,
            Conclusion = case_when(
              dm_result$p.value < 0.001 ~ "Highly significant (p < 0.001)",
              dm_result$p.value < 0.01 ~ "Very significant (p < 0.01)",
              dm_result$p.value < 0.05 ~ "Significant (p < 0.05)",
              dm_result$p.value < 0.10 ~ "Marginally significant (p < 0.10)",
              TRUE ~ "Not significant (p >= 0.10)"
            ),
            stringsAsFactors = FALSE
          ))
          
          cat("  XGB: DM =", round(as.numeric(dm_result$statistic), 3), 
              ", p =", round(as.numeric(dm_result$p.value), 4), "\n")
          
        }, error = function(e) {
          cat("  XGB: Error in DM test -", e$message, "\n")
        })
        
      } else {
        cat("  XGB: Insufficient observations (", min_len, ")\n")
      }
    }
  }
  
  return(dm_results)
}

# =============================================================================
# ENHANCED H1 ANALYSIS WITH PROPER DM TESTS
# =============================================================================

enhanced_h1_analysis_with_dm <- function(sp500_cv_path, ust_cv_path = NULL) {
  
  cat("ENHANCED H1 ANALYSIS WITH PROPER DIEBOLD-MARIANO TESTS\n")
  cat(rep("=", 70), "\n\n")
  
  # Extract prediction errors from S&P 500 results
  cat("Extracting S&P 500 prediction errors...\n")
  sp500_errors <- extract_prediction_errors_from_cv(sp500_cv_path)
  
  # Run DM tests for S&P 500
  cat("\nRunning DM tests for S&P 500...\n")
  sp500_dm_results <- run_diebold_mariano_tests(sp500_errors, "baseline")
  
  # Process UST if provided
  ust_dm_results <- NULL
  if (!is.null(ust_cv_path) && file.exists(ust_cv_path)) {
    cat("\nExtracting UST prediction errors...\n")
    ust_errors <- extract_prediction_errors_from_cv(ust_cv_path)
    
    cat("\nRunning DM tests for UST...\n")
    ust_dm_results <- run_diebold_mariano_tests(ust_errors, "baseline")
  }
  
  # Combine results and create comprehensive summary
  all_dm_results <- sp500_dm_results
  if (!is.null(ust_dm_results)) {
    ust_dm_results$Target <- "10Y UST"
    sp500_dm_results$Target <- "S&P 500"
    all_dm_results$Target <- "S&P 500"
    all_dm_results <- rbind(all_dm_results, ust_dm_results)
  } else {
    all_dm_results$Target <- "S&P 500"
  }
  
  # Display results
  cat("\n", rep("=", 80), "\n")
  cat("DIEBOLD-MARIANO TEST RESULTS SUMMARY\n")
  cat(rep("=", 80), "\n\n")
  
  if (nrow(all_dm_results) > 0) {
    # Format for display
    display_results <- all_dm_results %>%
      mutate(
        DM_Statistic = round(DM_Statistic, 3),
        P_Value = round(P_Value, 4),
        Significance = case_when(
          P_Value < 0.001 ~ "***",
          P_Value < 0.01 ~ "**",
          P_Value < 0.05 ~ "*",
          P_Value < 0.10 ~ ".",
          TRUE ~ ""
        )
      ) %>%
      select(Target, Experiment, Model, DM_Statistic, P_Value, Significance, N_Obs, Conclusion)
    
    print(kable(display_results, format = "simple"))
    
    # Statistical summary
    cat("\n", rep("=", 60), "\n")
    cat("STATISTICAL SIGNIFICANCE SUMMARY\n")
    cat(rep("=", 60), "\n\n")
    
    sig_count <- sum(all_dm_results$P_Value < 0.05, na.rm = TRUE)
    marginal_count <- sum(all_dm_results$P_Value >= 0.05 & all_dm_results$P_Value < 0.10, na.rm = TRUE)
    total_tests <- nrow(all_dm_results)
    
    cat("Total tests performed:", total_tests, "\n")
    cat("Significant (p < 0.05):", sig_count, "out of", total_tests, 
        "(", round(sig_count/total_tests*100, 1), "%)\n")
    cat("Marginally significant (0.05 ≤ p < 0.10):", marginal_count, "out of", total_tests,
        "(", round(marginal_count/total_tests*100, 1), "%)\n")
    cat("Not significant (p ≥ 0.10):", total_tests - sig_count - marginal_count, "out of", total_tests,
        "(", round((total_tests - sig_count - marginal_count)/total_tests*100, 1), "%)\n")
    
    # H1 Conclusion
    cat("\n", rep("=", 60), "\n")
    cat("HYPOTHESIS 1 CONCLUSION WITH PROPER STATISTICAL TESTING\n")
    cat(rep("=", 60), "\n\n")
    
    if (sig_count >= total_tests/2) {
      cat("✓ STRONG STATISTICAL SUPPORT FOR H1:\n")
      cat("  Majority of sentiment-enhanced models show statistically\n")
      cat("  significant improvements over baseline using rigorous DM tests.\n")
    } else if (sig_count + marginal_count >= total_tests/2) {
      cat("~ MODERATE STATISTICAL SUPPORT FOR H1:\n")
      cat("  Majority of sentiment-enhanced models show significant or\n")
      cat("  marginally significant improvements using rigorous DM tests.\n")
    } else {
      cat("? LIMITED STATISTICAL SUPPORT FOR H1:\n")
      cat("  While improvements are observed, statistical significance\n")
      cat("  is limited when using rigorous DM tests.\n")
    }
    
  } else {
    cat("No DM test results generated\n")
  }
  
  return(list(
    sp500_errors = sp500_errors,
    ust_errors = if(!is.null(ust_cv_path)) ust_errors else NULL,
    dm_results = all_dm_results,
    summary_stats = list(
      total_tests = total_tests,
      significant = sig_count,
      marginally_significant = marginal_count
    )
  ))
}

# =============================================================================
# EXECUTION EXAMPLE
# =============================================================================

sp500_results_path <- "SP_500_comprehensive_results_time_series_cross_validation/ts_cv_detailed_results.rds"
ust_results_path <- "UST_comprehensive_results_time_series_cross_validation/ts_cv_detailed_results.rds"

dm_analysis <- enhanced_h1_analysis_with_dm(sp500_results_path, ust_results_path)

