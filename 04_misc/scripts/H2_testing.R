# =============================================================================
# HYPOTHESIS 2 TESTING: SENTIMENT BETTER FOR VOLATILITY THAN RETURNS
# =============================================================================

library(dplyr)
library(knitr)

# =============================================================================
# APPROACH 1: DIRECT COMPARISON OF IMPROVEMENT MAGNITUDES
# =============================================================================

test_h2_improvement_comparison <- function(sp500_results, vix_results) {
  
  cat("\n", rep("=", 70), "\n")
  cat("HYPOTHESIS 2 TESTING: VOLATILITY vs RETURNS PREDICTIVE POWER\n")
  cat(rep("=", 70), "\n\n")
  
  cat("H2: Sentiment features have stronger predictive power for volatility than for returns\n\n")
  
  # Calculate percentage improvements for each target
  h2_results <- data.frame()
  
  # Get baseline results for both targets
  sp500_baseline_rf <- sp500_results %>% filter(Experiment == "baseline", Model == "Random Forest")
  sp500_baseline_xgb <- sp500_results %>% filter(Experiment == "baseline", Model == "XGBoost")
  vix_baseline_rf <- vix_results %>% filter(Experiment == "baseline", Model == "Random Forest")
  vix_baseline_xgb <- vix_results %>% filter(Experiment == "baseline", Model == "XGBoost")
  
  # Define sentiment experiments to test
  sentiment_experiments <- c("individual_dict_sentiment", "individual_sentimentr_score", 
                             "individual_finbert_score", "basic_sentiment", "all_sentiment")
  
  # Calculate improvements for each sentiment experiment
  for (exp_name in sentiment_experiments) {
    
    # S&P 500 (Returns) improvements
    sp500_exp_rf <- sp500_results %>% filter(Experiment == exp_name, Model == "Random Forest")
    sp500_exp_xgb <- sp500_results %>% filter(Experiment == exp_name, Model == "XGBoost")
    
    # VIX (Volatility) improvements  
    vix_exp_rf <- vix_results %>% filter(Experiment == exp_name, Model == "Random Forest")
    vix_exp_xgb <- vix_results %>% filter(Experiment == exp_name, Model == "XGBoost")
    
    # Calculate percentage improvements for Random Forest
    if (nrow(sp500_baseline_rf) > 0 && nrow(sp500_exp_rf) > 0 && 
        nrow(vix_baseline_rf) > 0 && nrow(vix_exp_rf) > 0) {
      
      sp500_improvement_rf <- ((sp500_baseline_rf$Mean_RMSE - sp500_exp_rf$Mean_RMSE) / sp500_baseline_rf$Mean_RMSE) * 100
      vix_improvement_rf <- ((vix_baseline_rf$Mean_RMSE - vix_exp_rf$Mean_RMSE) / vix_baseline_rf$Mean_RMSE) * 100
      
      h2_results <- rbind(h2_results, data.frame(
        Experiment = exp_name,
        Model = "Random Forest",
        SP500_Improvement_Pct = sp500_improvement_rf,
        VIX_Improvement_Pct = vix_improvement_rf,
        Difference = vix_improvement_rf - sp500_improvement_rf,
        H2_Supported = vix_improvement_rf > sp500_improvement_rf,
        stringsAsFactors = FALSE
      ))
    }
    
    # Calculate percentage improvements for XGBoost
    if (nrow(sp500_baseline_xgb) > 0 && nrow(sp500_exp_xgb) > 0 && 
        nrow(vix_baseline_xgb) > 0 && nrow(vix_exp_xgb) > 0) {
      
      sp500_improvement_xgb <- ((sp500_baseline_xgb$Mean_RMSE - sp500_exp_xgb$Mean_RMSE) / sp500_baseline_xgb$Mean_RMSE) * 100
      vix_improvement_xgb <- ((vix_baseline_xgb$Mean_RMSE - vix_exp_xgb$Mean_RMSE) / vix_baseline_xgb$Mean_RMSE) * 100
      
      h2_results <- rbind(h2_results, data.frame(
        Experiment = exp_name,
        Model = "XGBoost",
        SP500_Improvement_Pct = sp500_improvement_xgb,
        VIX_Improvement_Pct = vix_improvement_xgb,
        Difference = vix_improvement_xgb - sp500_improvement_xgb,
        H2_Supported = vix_improvement_xgb > sp500_improvement_xgb,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(h2_results)
}

# =============================================================================
# APPROACH 2: STATISTICAL TEST OF IMPROVEMENT DIFFERENCES
# =============================================================================

test_h2_statistical_significance <- function(h2_results, sp500_results, vix_results) {
  
  cat("\n", rep("=", 60), "\n")
  cat("H2 STATISTICAL SIGNIFICANCE TESTING\n")
  cat(rep("=", 60), "\n\n")
  
  # Test if the differences are statistically significant
  # Using paired t-test on the improvement differences
  
  if (nrow(h2_results) > 0) {
    
    # Extract improvement differences
    improvement_differences <- h2_results$Difference
    
    # One-sample t-test against zero (H0: no difference, H1: VIX > SP500)
    t_test_result <- t.test(improvement_differences, mu = 0, alternative = "greater")
    
    cat("Statistical Test Results:\n")
    cat("H0: No difference in sentiment predictive power between volatility and returns\n")
    cat("H1: Sentiment has greater predictive power for volatility than returns\n\n")
    
    cat("Sample size:", length(improvement_differences), "model-experiment combinations\n")
    cat("Mean improvement difference (VIX - SP500):", sprintf("%.3f%%", mean(improvement_differences)), "\n")
    cat("Standard deviation:", sprintf("%.3f%%", sd(improvement_differences)), "\n")
    cat("t-statistic:", sprintf("%.3f", t_test_result$statistic), "\n")
    cat("p-value:", sprintf("%.4f", t_test_result$p.value), "\n")
    
    # Interpretation
    significance_level <- case_when(
      t_test_result$p.value < 0.001 ~ "Highly significant (p < 0.001)",
      t_test_result$p.value < 0.01 ~ "Very significant (p < 0.01)",
      t_test_result$p.value < 0.05 ~ "Significant (p < 0.05)",
      t_test_result$p.value < 0.10 ~ "Marginally significant (p < 0.10)",
      TRUE ~ "Not significant (p >= 0.10)"
    )
    
    cat("Conclusion:", significance_level, "\n\n")
    
    # Effect size (Cohen's d)
    cohens_d <- mean(improvement_differences) / sd(improvement_differences)
    cat("Effect size (Cohen's d):", sprintf("%.3f", cohens_d), "\n")
    
    effect_interpretation <- case_when(
      abs(cohens_d) >= 0.8 ~ "Large effect",
      abs(cohens_d) >= 0.5 ~ "Medium effect", 
      abs(cohens_d) >= 0.2 ~ "Small effect",
      TRUE ~ "Negligible effect"
    )
    
    cat("Effect size interpretation:", effect_interpretation, "\n")
    
    return(list(
      t_test = t_test_result,
      cohens_d = cohens_d,
      significance = significance_level,
      effect_interpretation = effect_interpretation
    ))
    
  } else {
    cat("No data available for statistical testing\n")
    return(NULL)
  }
}

# =============================================================================
# APPROACH 3: SIGN TEST (NON-PARAMETRIC ALTERNATIVE)
# =============================================================================

test_h2_sign_test <- function(h2_results) {
  
  cat("\n", rep("=", 50), "\n")
  cat("H2 SIGN TEST (NON-PARAMETRIC)\n")
  cat(rep("=", 50), "\n\n")
  
  if (nrow(h2_results) > 0) {
    
    # Count how many experiments favor volatility vs returns
    n_total <- nrow(h2_results)
    n_vix_better <- sum(h2_results$H2_Supported)
    n_sp500_better <- n_total - n_vix_better
    
    cat("Results breakdown:\n")
    cat("- VIX (volatility) shows better improvement:", n_vix_better, "out of", n_total, 
        sprintf("(%.1f%%)", n_vix_better/n_total*100), "\n")
    cat("- S&P 500 (returns) shows better improvement:", n_sp500_better, "out of", n_total, 
        sprintf("(%.1f%%)", n_sp500_better/n_total*100), "\n\n")
    
    # Binomial test (sign test)
    # H0: P(VIX better) = 0.5, H1: P(VIX better) > 0.5
    binom_test <- binom.test(n_vix_better, n_total, p = 0.5, alternative = "greater")
    
    cat("Sign Test Results:\n")
    cat("H0: Equal probability of VIX vs SP500 being better (p = 0.5)\n")
    cat("H1: VIX improvements are more likely (p > 0.5)\n\n")
    
    cat("Successes (VIX better):", n_vix_better, "\n")
    cat("Total trials:", n_total, "\n")
    cat("Success probability:", sprintf("%.3f", n_vix_better/n_total), "\n")
    cat("p-value:", sprintf("%.4f", binom_test$p.value), "\n")
    
    sign_significance <- case_when(
      binom_test$p.value < 0.001 ~ "Highly significant (p < 0.001)",
      binom_test$p.value < 0.01 ~ "Very significant (p < 0.01)",
      binom_test$p.value < 0.05 ~ "Significant (p < 0.05)",
      binom_test$p.value < 0.10 ~ "Marginally significant (p < 0.10)",
      TRUE ~ "Not significant (p >= 0.10)"
    )
    
    cat("Conclusion:", sign_significance, "\n")
    
    return(list(
      binom_test = binom_test,
      n_vix_better = n_vix_better,
      n_total = n_total,
      success_rate = n_vix_better/n_total,
      significance = sign_significance
    ))
    
  } else {
    cat("No data available for sign testing\n")
    return(NULL)
  }
}

# =============================================================================
# COMPREHENSIVE H2 ANALYSIS FUNCTION
# =============================================================================

comprehensive_h2_analysis <- function(sp500_results, vix_results) {
  
  cat("COMPREHENSIVE HYPOTHESIS 2 ANALYSIS\n")
  cat("Testing whether sentiment features have stronger predictive power for volatility than returns\n")
  cat(rep("=", 80), "\n\n")
  
  # Step 1: Calculate improvement comparisons
  h2_results <- test_h2_improvement_comparison(sp500_results, vix_results)
  
  if (nrow(h2_results) > 0) {
    
    # Display detailed results table
    cat("DETAILED COMPARISON TABLE:\n")
    cat(rep("-", 80), "\n")
    
    display_table <- h2_results %>%
      mutate(
        SP500_Improvement_Pct = round(SP500_Improvement_Pct, 2),
        VIX_Improvement_Pct = round(VIX_Improvement_Pct, 2),
        Difference = round(Difference, 2),
        H2_Support = ifelse(H2_Supported, "✓", "✗")
      ) %>%
      select(Experiment, Model, SP500_Improvement_Pct, VIX_Improvement_Pct, 
             Difference, H2_Support)
    
    colnames(display_table) <- c("Experiment", "Model", "SP500_Improv_%", 
                                 "VIX_Improv_%", "Difference", "H2_Support")
    
    print(kable(display_table, format = "simple"))
    
    # Step 2: Statistical significance testing
    stat_results <- test_h2_statistical_significance(h2_results, sp500_results, vix_results)
    
    # Step 3: Non-parametric sign test
    sign_results <- test_h2_sign_test(h2_results)
    
    # Step 4: Overall H2 conclusion
    cat("\n", rep("=", 60), "\n")
    cat("OVERALL HYPOTHESIS 2 CONCLUSION\n")
    cat(rep("=", 60), "\n\n")
    
    n_supporting <- sum(h2_results$H2_Supported)
    n_total <- nrow(h2_results)
    support_pct <- n_supporting / n_total * 100
    
    cat("Summary of Evidence for H2:\n")
    cat("- Experiments supporting H2 (VIX > SP500):", n_supporting, "out of", n_total, 
        sprintf("(%.1f%%)", support_pct), "\n")
    
    if (!is.null(stat_results)) {
      cat("- T-test p-value:", sprintf("%.4f", stat_results$t_test$p.value), "\n")
    }
    
    if (!is.null(sign_results)) {
      cat("- Sign test p-value:", sprintf("%.4f", sign_results$binom_test$p.value), "\n")
    }
    
    # Final conclusion
    cat("\n")
    if (support_pct >= 70 && !is.null(stat_results) && stat_results$t_test$p.value < 0.05) {
      cat("✓ HYPOTHESIS 2 STRONGLY SUPPORTED:\n")
      cat("  Sentiment features consistently show greater predictive power for volatility than returns,\n")
      cat("  with statistically significant evidence.\n")
    } else if (support_pct >= 60) {
      cat("~ HYPOTHESIS 2 MODERATELY SUPPORTED:\n")
      cat("  Sentiment features show a tendency toward greater predictive power for volatility,\n")
      cat("  though statistical evidence may be limited.\n")
    } else {
      cat("✗ HYPOTHESIS 2 NOT SUPPORTED:\n")
      cat("  No clear evidence that sentiment features have stronger predictive power for volatility\n")
      cat("  compared to returns.\n")
    }
    
    return(list(
      h2_results = h2_results,
      statistical_results = stat_results,
      sign_test_results = sign_results,
      summary = list(
        n_supporting = n_supporting,
        n_total = n_total,
        support_percentage = support_pct
      )
    ))
    
  } else {
    cat("No valid comparison data available for H2 testing\n")
    return(NULL)
  }
}

# =============================================================================
# EXECUTION EXAMPLE
# =============================================================================

sp500_data <- read_csv("SP_500_comprehensive_results_time_series_cross_validation/ts_cv_results_summary_SP_500.csv")
vix_data <- read_csv("VIX_sentiment_ablation_results_time_series_cross_validation/vix_cv_results_summary.csv")
 
h2_analysis <- comprehensive_h2_analysis(sp500_data, vix_data)

cat("H2 TESTING FRAMEWORK READY!\n")
cat("===========================\n")
cat("This approach:\n")
cat("1. Compares improvement magnitudes between volatility and returns\n")
cat("2. Tests statistical significance using t-test and sign test\n")
cat("3. Provides comprehensive evidence evaluation for H2\n")
cat("4. Handles multiple sentiment experiments and models\n")

