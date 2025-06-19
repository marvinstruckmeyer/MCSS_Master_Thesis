# =============================================================================
# Hypothesis 1 Analysis: Sentiment Features Predictive Power
# Testing whether sentiment features improve prediction of asset returns
# =============================================================================

# Load required libraries
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================

# Function to load and clean data from Excel files
load_results_data <- function(file_path) {
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Clean column names (remove spaces and special characters)
  colnames(data) <- gsub(" ", "_", colnames(data))
  colnames(data) <- gsub("[^A-Za-z0-9_]", "", colnames(data))
  
  # Ensure numeric columns are properly formatted
  numeric_cols <- c("Mean_RMSE", "Std_RMSE", "Mean_R2", "Std_R2", 
                    "Mean_Correlation", "Mean_Dir_Accuracy")
  
  for(col in numeric_cols) {
    if(col %in% colnames(data)) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  return(data)
}

# Load the results data
# Note: Replace these paths with your actual file paths
sp500_results <- read_csv("SP_500_comprehensive_results_time_series_cross_validation/ts_cv_results_summary_SP_500.csv")
ust_results <- read_csv("UST_comprehensive_results_time_series_cross_validation/ts_cv_results_summary_UST.csv")

# =============================================================================
# HELPER FUNCTIONS FOR H1 ANALYSIS
# =============================================================================

# Function to filter sentiment models (exclude baseline and market_only)
filter_sentiment_models <- function(data) {
  sentiment_models <- data %>%
    filter(!Experiment %in% c("baseline", "market_only"))
  return(sentiment_models)
}

# Function to get baseline results
get_baseline_results <- function(data) {
  baseline <- data %>%
    filter(Experiment == "baseline")
  return(baseline)
}

# Function to calculate performance improvements
calculate_improvements <- function(sentiment_results, baseline_results, model_type) {
  # Filter for specific model type
  sent_model <- sentiment_results %>% filter(Model == model_type)
  base_model <- baseline_results %>% filter(Model == model_type)
  
  if(nrow(base_model) == 0 || nrow(sent_model) == 0) {
    return(NULL)
  }
  
  # Calculate mean RMSE across all sentiment models
  mean_sent_rmse <- mean(sent_model$Mean_RMSE, na.rm = TRUE)
  baseline_rmse <- base_model$Mean_RMSE[1]
  
  # Find best performing sentiment model
  best_model_idx <- which.min(sent_model$Mean_RMSE)
  best_model_rmse <- sent_model$Mean_RMSE[best_model_idx]
  best_model_name <- sent_model$Experiment[best_model_idx]
  
  # Calculate improvements
  mean_improvement_pct <- ((baseline_rmse - mean_sent_rmse) / baseline_rmse) * 100
  best_improvement_pct <- ((baseline_rmse - best_model_rmse) / baseline_rmse) * 100
  
  # Calculate absolute improvements
  mean_improvement_abs <- baseline_rmse - mean_sent_rmse
  best_improvement_abs <- baseline_rmse - best_model_rmse
  
  return(list(
    model_type = model_type,
    baseline_rmse = baseline_rmse,
    mean_sentiment_rmse = mean_sent_rmse,
    best_sentiment_rmse = best_model_rmse,
    best_model_name = best_model_name,
    mean_improvement_pct = mean_improvement_pct,
    best_improvement_pct = best_improvement_pct,
    mean_improvement_abs = mean_improvement_abs,
    best_improvement_abs = best_improvement_abs,
    n_sentiment_models = nrow(sent_model)
  ))
}

# =============================================================================
# H1 ANALYSIS FUNCTION
# =============================================================================

analyze_h1_hypothesis <- function(results_data, target_name) {
  cat("\n", rep("=", 60), "\n")
  cat("HYPOTHESIS 1 ANALYSIS:", target_name, "\n")
  cat(rep("=", 60), "\n")
  
  # Get baseline and sentiment model results
  baseline_results <- get_baseline_results(results_data)
  sentiment_results <- filter_sentiment_models(results_data)
  
  # Print basic summary
  cat("\nBasic Summary:\n")
  cat("- Total experiments:", nrow(results_data), "\n")
  cat("- Baseline experiments:", nrow(baseline_results), "\n")
  cat("- Sentiment-enhanced experiments:", nrow(sentiment_results), "\n")
  cat("- Models tested:", paste(unique(results_data$Model), collapse = ", "), "\n")
  
  # Analyze for each model type
  model_types <- unique(results_data$Model)
  h1_results <- list()
  
  for(model_type in model_types) {
    cat("\n", rep("-", 40), "\n")
    cat("ANALYSIS FOR", model_type, "\n")
    cat(rep("-", 40), "\n")
    
    improvement_results <- calculate_improvements(sentiment_results, baseline_results, model_type)
    
    if(is.null(improvement_results)) {
      cat("No results available for", model_type, "\n")
      next
    }
    
    h1_results[[model_type]] <- improvement_results
    
    # Print detailed results
    cat("\nBaseline RMSE:", sprintf("%.6f", improvement_results$baseline_rmse), "\n")
    cat("Mean Sentiment RMSE:", sprintf("%.6f", improvement_results$mean_sentiment_rmse), "\n")
    cat("Best Sentiment RMSE:", sprintf("%.6f", improvement_results$best_sentiment_rmse), "\n")
    cat("Best Model:", improvement_results$best_model_name, "\n")
    cat("\nPerformance Improvements:\n")
    cat("- Mean sentiment vs baseline:", sprintf("%.2f%%", improvement_results$mean_improvement_pct), 
        sprintf("(%.6f)", improvement_results$mean_improvement_abs), "\n")
    cat("- Best sentiment vs baseline:", sprintf("%.2f%%", improvement_results$best_improvement_pct), 
        sprintf("(%.6f)", improvement_results$best_improvement_abs), "\n")
    
    # Interpretation
    cat("\nInterpretation:\n")
    if(improvement_results$mean_improvement_pct > 0) {
      cat("✓ Sentiment features provide average improvement over baseline\n")
    } else {
      cat("✗ Sentiment features do not provide average improvement over baseline\n")
    }
    
    if(improvement_results$best_improvement_pct > 0) {
      cat("✓ Best sentiment model outperforms baseline\n")
    } else {
      cat("✗ Best sentiment model does not outperform baseline\n")
    }
  }
  
  return(h1_results)
}

# Function to create summary table for H1 results
create_h1_summary_table <- function(h1_results_sp500, h1_results_ust) {
  # Combine results into a summary table
  summary_data <- data.frame()
  
  # SP500 results
  for(model in names(h1_results_sp500)) {
    if(!is.null(h1_results_sp500[[model]])) {
      summary_data <- rbind(summary_data, data.frame(
        Target = "S&P 500 Returns",
        Model = model,
        Baseline_RMSE = h1_results_sp500[[model]]$baseline_rmse,
        Mean_Sentiment_RMSE = h1_results_sp500[[model]]$mean_sentiment_rmse,
        Best_Sentiment_RMSE = h1_results_sp500[[model]]$best_sentiment_rmse,
        Best_Model = h1_results_sp500[[model]]$best_model_name,
        Mean_Improvement_Pct = h1_results_sp500[[model]]$mean_improvement_pct,
        Best_Improvement_Pct = h1_results_sp500[[model]]$best_improvement_pct,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # UST results
  for(model in names(h1_results_ust)) {
    if(!is.null(h1_results_ust[[model]])) {
      summary_data <- rbind(summary_data, data.frame(
        Target = "10Y UST Changes",
        Model = model,
        Baseline_RMSE = h1_results_ust[[model]]$baseline_rmse,
        Mean_Sentiment_RMSE = h1_results_ust[[model]]$mean_sentiment_rmse,
        Best_Sentiment_RMSE = h1_results_ust[[model]]$best_sentiment_rmse,
        Best_Model = h1_results_ust[[model]]$best_model_name,
        Mean_Improvement_Pct = h1_results_ust[[model]]$mean_improvement_pct,
        Best_Improvement_Pct = h1_results_ust[[model]]$best_improvement_pct,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(summary_data)
}

# =============================================================================
# MAIN ANALYSIS EXECUTION
# =============================================================================

# Run H1 analysis for both targets
h1_results_sp500 <- analyze_h1_hypothesis(sp500_results, "S&P 500 Daily Returns")
h1_results_ust <- analyze_h1_hypothesis(ust_results, "10-Year UST Daily Changes")

# Create comprehensive summary table
h1_summary_table <- create_h1_summary_table(h1_results_sp500, h1_results_ust)

# Display summary table
cat("\n", rep("=", 80), "\n")
cat("HYPOTHESIS 1 - COMPREHENSIVE SUMMARY TABLE\n")
cat(rep("=", 80), "\n")

# Print formatted table
if(nrow(h1_summary_table) > 0) {
  # Round numerical columns for display
  h1_summary_table$Baseline_RMSE <- round(h1_summary_table$Baseline_RMSE, 6)
  h1_summary_table$Mean_Sentiment_RMSE <- round(h1_summary_table$Mean_Sentiment_RMSE, 6)
  h1_summary_table$Best_Sentiment_RMSE <- round(h1_summary_table$Best_Sentiment_RMSE, 6)
  h1_summary_table$Mean_Improvement_Pct <- round(h1_summary_table$Mean_Improvement_Pct, 2)
  h1_summary_table$Best_Improvement_Pct <- round(h1_summary_table$Best_Improvement_Pct, 2)
  
  print(kable(h1_summary_table, 
              caption = "Hypothesis 1 Results: Sentiment Features Predictive Power",
              format = "simple"))
}

# =============================================================================
# H1 CONCLUSION
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("HYPOTHESIS 1 - OVERALL CONCLUSION\n")
cat(rep("=", 60), "\n")

# Count positive improvements
positive_improvements <- sum(h1_summary_table$Best_Improvement_Pct > 0, na.rm = TRUE)
total_models <- nrow(h1_summary_table)

cat("\nSummary of Evidence for H1:\n")
cat("- Models showing improvement:", positive_improvements, "out of", total_models, "\n")
cat("- Proportion with improvement:", round(positive_improvements/total_models * 100, 1), "%\n")

if(positive_improvements/total_models >= 0.5) {
  cat("\n✓ HYPOTHESIS 1 SUPPORTED: Sentiment features generally improve prediction accuracy\n")
} else {
  cat("\n✗ HYPOTHESIS 1 NOT SUPPORTED: Sentiment features do not consistently improve prediction accuracy\n")
}

# Additional insights
cat("\nKey Insights:\n")
best_overall <- h1_summary_table[which.max(h1_summary_table$Best_Improvement_Pct), ]
if(nrow(best_overall) > 0) {
  cat("- Best performing combination:", best_overall$Model, "for", best_overall$Target, "\n")
  cat("- Maximum improvement achieved:", round(best_overall$Best_Improvement_Pct, 2), "%\n")
  cat("- Best sentiment model:", best_overall$Best_Model, "\n")
}

cat("\n", rep("=", 60), "\n")

# Return results for further analysis
list(
  sp500_results = h1_results_sp500,
  ust_results = h1_results_ust,
  summary_table = h1_summary_table
)

##
# =============================================================================
# Statistical Significance Testing for H1 Results
# =============================================================================

library(dplyr)

# =============================================================================
# METHOD 1: SIMPLIFIED T-TEST USING AVAILABLE DATA
# =============================================================================

# Function to perform approximate t-test using mean and std of RMSE across folds
approximate_significance_test <- function(baseline_rmse, baseline_std, 
                                          sentiment_rmse, sentiment_std, 
                                          n_folds = 6) {
  
  # Calculate the difference in means
  mean_diff <- baseline_rmse - sentiment_rmse
  
  # Calculate pooled standard error
  se_diff <- sqrt((baseline_std^2 + sentiment_std^2) / n_folds)
  
  # T-statistic
  t_stat <- mean_diff / se_diff
  
  # Degrees of freedom (conservative estimate)
  df <- n_folds - 1
  
  # Two-tailed p-value
  p_value <- 2 * pt(-abs(t_stat), df)
  
  # Effect size (Cohen's d approximation)
  pooled_sd <- sqrt((baseline_std^2 + sentiment_std^2) / 2)
  cohens_d <- mean_diff / pooled_sd
  
  # Significance level
  significance <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**", 
    p_value < 0.05 ~ "*",
    p_value < 0.10 ~ ".",
    TRUE ~ ""
  )
  
  return(list(
    mean_diff = mean_diff,
    t_statistic = t_stat,
    p_value = p_value,
    significance = significance,
    cohens_d = cohens_d,
    conclusion = case_when(
      p_value < 0.001 ~ "Highly significant (p < 0.001)",
      p_value < 0.01 ~ "Very significant (p < 0.01)",
      p_value < 0.05 ~ "Significant (p < 0.05)",
      p_value < 0.10 ~ "Marginally significant (p < 0.10)",
      TRUE ~ "Not significant (p >= 0.10)"
    )
  ))
}

# =============================================================================
# APPLY STATISTICAL TESTS TO H1 RESULTS
# =============================================================================

test_h1_significance <- function(sp500_data, ust_data) {
  
  cat("\n", rep("=", 70), "\n")
  cat("STATISTICAL SIGNIFICANCE TESTING FOR HYPOTHESIS 1\n")
  cat(rep("=", 70), "\n")
  
  # Initialize results data frame
  test_results <- data.frame()
  
  # Test for S&P 500
  cat("\n--- S&P 500 RETURNS ---\n")
  
  # Get baseline results
  sp500_baseline_rf <- sp500_data %>% filter(Experiment == "baseline", Model == "Random Forest")
  sp500_baseline_xgb <- sp500_data %>% filter(Experiment == "baseline", Model == "XGBoost")
  
  # Get best sentiment models results
  sp500_best_rf <- sp500_data %>% filter(Experiment == "individual_sentimentr_score", Model == "Random Forest")
  sp500_best_xgb <- sp500_data %>% filter(Experiment == "all_sentiment", Model == "XGBoost")
  
  # Test Random Forest
  if(nrow(sp500_baseline_rf) > 0 && nrow(sp500_best_rf) > 0) {
    rf_test <- approximate_significance_test(
      sp500_baseline_rf$Mean_RMSE, sp500_baseline_rf$Std_RMSE,
      sp500_best_rf$Mean_RMSE, sp500_best_rf$Std_RMSE
    )
    
    cat("\nRandom Forest (vs individual_sentimentr_score):\n")
    cat("- Improvement:", sprintf("%.6f", rf_test$mean_diff), "\n")
    cat("- t-statistic:", sprintf("%.3f", rf_test$t_statistic), "\n")
    cat("- p-value:", sprintf("%.4f", rf_test$p_value), rf_test$significance, "\n")
    cat("- Effect size (Cohen's d):", sprintf("%.3f", rf_test$cohens_d), "\n")
    cat("- Conclusion:", rf_test$conclusion, "\n")
    
    test_results <- rbind(test_results, data.frame(
      Target = "S&P 500",
      Model = "Random Forest",
      Best_Sentiment_Model = "individual_sentimentr_score",
      Improvement = rf_test$mean_diff,
      T_Statistic = rf_test$t_statistic,
      P_Value = rf_test$p_value,
      Significance = rf_test$significance,
      Cohens_D = rf_test$cohens_d,
      Conclusion = rf_test$conclusion,
      stringsAsFactors = FALSE
    ))
  }
  
  # Test XGBoost
  if(nrow(sp500_baseline_xgb) > 0 && nrow(sp500_best_xgb) > 0) {
    xgb_test <- approximate_significance_test(
      sp500_baseline_xgb$Mean_RMSE, sp500_baseline_xgb$Std_RMSE,
      sp500_best_xgb$Mean_RMSE, sp500_best_xgb$Std_RMSE
    )
    
    cat("\nXGBoost (vs all_sentiment):\n")
    cat("- Improvement:", sprintf("%.6f", xgb_test$mean_diff), "\n")
    cat("- t-statistic:", sprintf("%.3f", xgb_test$t_statistic), "\n")
    cat("- p-value:", sprintf("%.4f", xgb_test$p_value), xgb_test$significance, "\n")
    cat("- Effect size (Cohen's d):", sprintf("%.3f", xgb_test$cohens_d), "\n")
    cat("- Conclusion:", xgb_test$conclusion, "\n")
    
    test_results <- rbind(test_results, data.frame(
      Target = "S&P 500",
      Model = "XGBoost", 
      Best_Sentiment_Model = "all_sentiment",
      Improvement = xgb_test$mean_diff,
      T_Statistic = xgb_test$t_statistic,
      P_Value = xgb_test$p_value,
      Significance = xgb_test$significance,
      Cohens_D = xgb_test$cohens_d,
      Conclusion = xgb_test$conclusion,
      stringsAsFactors = FALSE
    ))
  }
  
  # Test for UST
  cat("\n--- 10-YEAR UST CHANGES ---\n")
  
  # Get baseline results
  ust_baseline_rf <- ust_data %>% filter(Experiment == "baseline", Model == "Random Forest")
  ust_baseline_xgb <- ust_data %>% filter(Experiment == "baseline", Model == "XGBoost")
  
  # Get best sentiment models results
  ust_best_rf <- ust_data %>% filter(Experiment == "basic_sentiment", Model == "Random Forest")
  ust_best_xgb <- ust_data %>% filter(Experiment == "individual_finbert_score", Model == "XGBoost")
  
  # Test Random Forest
  if(nrow(ust_baseline_rf) > 0 && nrow(ust_best_rf) > 0) {
    rf_test <- approximate_significance_test(
      ust_baseline_rf$Mean_RMSE, ust_baseline_rf$Std_RMSE,
      ust_best_rf$Mean_RMSE, ust_best_rf$Std_RMSE
    )
    
    cat("\nRandom Forest (vs basic_sentiment):\n")
    cat("- Improvement:", sprintf("%.6f", rf_test$mean_diff), "\n")
    cat("- t-statistic:", sprintf("%.3f", rf_test$t_statistic), "\n")
    cat("- p-value:", sprintf("%.4f", rf_test$p_value), rf_test$significance, "\n")
    cat("- Effect size (Cohen's d):", sprintf("%.3f", rf_test$cohens_d), "\n")
    cat("- Conclusion:", rf_test$conclusion, "\n")
    
    test_results <- rbind(test_results, data.frame(
      Target = "10Y UST",
      Model = "Random Forest",
      Best_Sentiment_Model = "basic_sentiment", 
      Improvement = rf_test$mean_diff,
      T_Statistic = rf_test$t_statistic,
      P_Value = rf_test$p_value,
      Significance = rf_test$significance,
      Cohens_D = rf_test$cohens_d,
      Conclusion = rf_test$conclusion,
      stringsAsFactors = FALSE
    ))
  }
  
  # Test XGBoost
  if(nrow(ust_baseline_xgb) > 0 && nrow(ust_best_xgb) > 0) {
    xgb_test <- approximate_significance_test(
      ust_baseline_xgb$Mean_RMSE, ust_baseline_xgb$Std_RMSE,
      ust_best_xgb$Mean_RMSE, ust_best_xgb$Std_RMSE
    )
    
    cat("\nXGBoost (vs individual_finbert_score):\n")
    cat("- Improvement:", sprintf("%.6f", xgb_test$mean_diff), "\n")
    cat("- t-statistic:", sprintf("%.3f", xgb_test$t_statistic), "\n")
    cat("- p-value:", sprintf("%.4f", xgb_test$p_value), xgb_test$significance, "\n")
    cat("- Effect size (Cohen's d):", sprintf("%.3f", xgb_test$cohens_d), "\n")
    cat("- Conclusion:", xgb_test$conclusion, "\n")
    
    test_results <- rbind(test_results, data.frame(
      Target = "10Y UST",
      Model = "XGBoost",
      Best_Sentiment_Model = "individual_finbert_score",
      Improvement = xgb_test$mean_diff,
      T_Statistic = xgb_test$t_statistic,
      P_Value = xgb_test$p_value,
      Significance = xgb_test$significance,
      Cohens_D = xgb_test$cohens_d,
      Conclusion = xgb_test$conclusion,
      stringsAsFactors = FALSE
    ))
  }
  
  return(test_results)
}

# =============================================================================
# ENHANCED H1 ANALYSIS WITH STATISTICAL TESTS
# =============================================================================

enhanced_h1_analysis <- function(sp500_results, ust_results) {
  
  # Run statistical tests
  significance_results <- test_h1_significance(sp500_results, ust_results)
  
  # Create enhanced summary table
  cat("\n", rep("=", 80), "\n")
  cat("ENHANCED H1 SUMMARY WITH STATISTICAL SIGNIFICANCE\n")
  cat(rep("=", 80), "\n")
  
  # Format table for display
  significance_results$P_Value <- round(significance_results$P_Value, 4)
  significance_results$T_Statistic <- round(significance_results$T_Statistic, 3)
  significance_results$Improvement <- round(significance_results$Improvement, 6)
  significance_results$Cohens_D <- round(significance_results$Cohens_D, 3)
  
  print(kable(significance_results[, c("Target", "Model", "Best_Sentiment_Model", 
                                       "Improvement", "T_Statistic", "P_Value", 
                                       "Significance", "Conclusion")],
              caption = "Statistical Significance of Sentiment Feature Improvements",
              format = "simple"))
  
  # Summary statistics
  cat("\n", rep("=", 60), "\n")
  cat("STATISTICAL SUMMARY\n")
  cat(rep("=", 60), "\n")
  
  significant_count <- sum(significance_results$P_Value < 0.05, na.rm = TRUE)
  marginal_count <- sum(significance_results$P_Value >= 0.05 & significance_results$P_Value < 0.10, na.rm = TRUE)
  total_tests <- nrow(significance_results)
  
  cat("\nSignificance Summary:\n")
  cat("- Significant (p < 0.05):", significant_count, "out of", total_tests, "\n")
  cat("- Marginally significant (0.05 ≤ p < 0.10):", marginal_count, "out of", total_tests, "\n")
  cat("- Not significant (p ≥ 0.10):", total_tests - significant_count - marginal_count, "out of", total_tests, "\n")
  
  # Effect sizes
  large_effects <- sum(abs(significance_results$Cohens_D) >= 0.8, na.rm = TRUE)
  medium_effects <- sum(abs(significance_results$Cohens_D) >= 0.5 & abs(significance_results$Cohens_D) < 0.8, na.rm = TRUE)
  small_effects <- sum(abs(significance_results$Cohens_D) >= 0.2 & abs(significance_results$Cohens_D) < 0.5, na.rm = TRUE)
  
  cat("\nEffect Size Summary (Cohen's d):\n")
  cat("- Large effects (|d| ≥ 0.8):", large_effects, "out of", total_tests, "\n")
  cat("- Medium effects (0.5 ≤ |d| < 0.8):", medium_effects, "out of", total_tests, "\n")
  cat("- Small effects (0.2 ≤ |d| < 0.5):", small_effects, "out of", total_tests, "\n")
  
  # Overall conclusion
  cat("\n", rep("=", 60), "\n")
  cat("OVERALL H1 CONCLUSION WITH STATISTICAL EVIDENCE\n")
  cat(rep("=", 60), "\n")
  
  if(significant_count >= total_tests/2) {
    cat("\n✓ STRONG STATISTICAL SUPPORT FOR H1:\n")
    cat("  Sentiment features show statistically significant improvements\n")
    cat("  in prediction accuracy for the majority of model-target combinations.\n")
  } else if(significant_count + marginal_count >= total_tests/2) {
    cat("\n~ MODERATE STATISTICAL SUPPORT FOR H1:\n") 
    cat("  Sentiment features show significant or marginally significant\n")
    cat("  improvements for the majority of model-target combinations.\n")
  } else {
    cat("\n? LIMITED STATISTICAL SUPPORT FOR H1:\n")
    cat("  While improvements are observed, statistical significance\n")
    cat("  is limited across model-target combinations.\n")
  }
  
  return(significance_results)
}

# =============================================================================
# EXECUTION
# =============================================================================

sp500_results <- load_results_data("ts_cv_results_summary_SP_500.xlsx")
ust_results <- load_results_data("ts_cv_results_summary_UST.xlsx")

statistical_results <- enhanced_h1_analysis(sp500_results, ust_results)
