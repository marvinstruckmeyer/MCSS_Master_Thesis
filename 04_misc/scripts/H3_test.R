# =============================================================================
# HYPOTHESIS 3 ANALYSIS: INCREMENTAL VALUE OF TV SENTIMENT BEYOND NEWS SENTIMENT
# =============================================================================

library(dplyr)
library(knitr)

# =============================================================================
# H3 ANALYSIS FUNCTIONS
# =============================================================================

analyze_h3_hypothesis <- function(results_data, target_name) {
  cat("\n", rep("=", 70), "\n")
  cat("HYPOTHESIS 3 ANALYSIS:", target_name, "\n")
  cat(rep("=", 70), "\n")
  cat("Testing: Do TV sentiment features add incremental value beyond news sentiment?\n\n")
  
  # Get baseline (news sentiment) and TV sentiment model results
  # Note: In H3, "baseline" includes traditional news sentiment
  news_baseline_results <- results_data %>% filter(Experiment == "baseline")
  tv_sentiment_results <- results_data %>% filter(!Experiment %in% c("baseline", "market_only"))
  
  # Print basic summary
  cat("Basic Summary:\n")
  cat("- Total experiments:", nrow(results_data), "\n")
  cat("- News sentiment baseline experiments:", nrow(news_baseline_results), "\n")
  cat("- TV sentiment-enhanced experiments:", nrow(tv_sentiment_results), "\n")
  cat("- Models tested:", paste(unique(results_data$Model), collapse = ", "), "\n\n")
  
  cat("H3 Testing Framework:\n")
  cat("- Baseline: Market/macro variables + traditional news sentiment\n")
  cat("- Treatment: Baseline + TV sentiment features\n")
  cat("- Question: Does TV sentiment add incremental predictive power?\n\n")
  
  # Analyze for each model type
  model_types <- unique(results_data$Model)
  h3_results <- list()
  
  for(model_type in model_types) {
    cat(rep("-", 50), "\n")
    cat("ANALYSIS FOR", model_type, "\n")
    cat(rep("-", 50), "\n")
    
    # Filter for specific model type
    tv_sent_model <- tv_sentiment_results %>% filter(Model == model_type)
    news_base_model <- news_baseline_results %>% filter(Model == model_type)
    
    if(nrow(news_base_model) == 0 || nrow(tv_sent_model) == 0) {
      cat("No results available for", model_type, "\n\n")
      next
    }
    
    # Calculate incremental improvements
    news_baseline_rmse <- news_base_model$Mean_RMSE[1]
    
    # Mean TV sentiment improvement over news baseline
    mean_tv_rmse <- mean(tv_sent_model$Mean_RMSE, na.rm = TRUE)
    mean_improvement_pct <- ((news_baseline_rmse - mean_tv_rmse) / news_baseline_rmse) * 100
    mean_improvement_abs <- news_baseline_rmse - mean_tv_rmse
    
    # Best TV sentiment model vs news baseline
    best_model_idx <- which.min(tv_sent_model$Mean_RMSE)
    best_tv_rmse <- tv_sent_model$Mean_RMSE[best_model_idx]
    best_model_name <- tv_sent_model$Experiment[best_model_idx]
    best_improvement_pct <- ((news_baseline_rmse - best_tv_rmse) / news_baseline_rmse) * 100
    best_improvement_abs <- news_baseline_rmse - best_tv_rmse
    
    # Store results
    h3_results[[model_type]] <- list(
      model_type = model_type,
      news_baseline_rmse = news_baseline_rmse,
      mean_tv_sentiment_rmse = mean_tv_rmse,
      best_tv_sentiment_rmse = best_tv_rmse,
      best_model_name = best_model_name,
      mean_improvement_pct = mean_improvement_pct,
      best_improvement_pct = best_improvement_pct,
      mean_improvement_abs = mean_improvement_abs,
      best_improvement_abs = best_improvement_abs,
      n_tv_sentiment_models = nrow(tv_sent_model)
    )
    
    # Print detailed results
    cat("News Sentiment Baseline RMSE:", sprintf("%.6f", news_baseline_rmse), "\n")
    cat("Mean TV Sentiment RMSE:", sprintf("%.6f", mean_tv_rmse), "\n")
    cat("Best TV Sentiment RMSE:", sprintf("%.6f", best_tv_rmse), "\n")
    cat("Best TV Sentiment Model:", best_model_name, "\n\n")
    
    cat("Incremental Improvements (TV beyond News):\n")
    cat("- Mean TV sentiment vs news baseline:", sprintf("%.2f%%", mean_improvement_pct), 
        sprintf("(%.6f)", mean_improvement_abs), "\n")
    cat("- Best TV sentiment vs news baseline:", sprintf("%.2f%%", best_improvement_pct), 
        sprintf("(%.6f)", best_improvement_abs), "\n\n")
    
    # Interpretation
    cat("Interpretation:\n")
    if(mean_improvement_pct > 0) {
      cat("✓ TV sentiment features provide average incremental value beyond news sentiment\n")
    } else {
      cat("✗ TV sentiment features do not provide average incremental value beyond news sentiment\n")
    }
    
    if(best_improvement_pct > 0) {
      cat("✓ Best TV sentiment model outperforms news sentiment baseline\n")
    } else {
      cat("✗ Best TV sentiment model does not outperform news sentiment baseline\n")
    }
    cat("\n")
  }
  
  return(h3_results)
}

# =============================================================================
# H3 STATISTICAL SIGNIFICANCE TESTING
# =============================================================================

test_h3_significance <- function(sp500_news_results, ust_news_results) {
  
  cat("\n", rep("=", 80), "\n")
  cat("H3 STATISTICAL SIGNIFICANCE TESTING\n")
  cat(rep("=", 80), "\n\n")
  
  # Function to perform significance test for one target
  test_single_target <- function(results_data, target_name) {
    
    cat("--- ", target_name, " ---\n")
    
    # Get news baseline and best TV sentiment models
    news_baseline_rf <- results_data %>% filter(Experiment == "baseline", Model == "Random Forest")
    news_baseline_xgb <- results_data %>% filter(Experiment == "baseline", Model == "XGBoost")
    
    # Find best TV sentiment models
    tv_sentiment_results <- results_data %>% filter(!Experiment %in% c("baseline", "market_only"))
    
    test_results <- data.frame()
    
    # Test Random Forest
    if(nrow(news_baseline_rf) > 0 && nrow(tv_sentiment_results) > 0) {
      
      tv_sentiment_rf <- tv_sentiment_results %>% filter(Model == "Random Forest")
      
      if(nrow(tv_sentiment_rf) > 0) {
        best_tv_rf <- tv_sentiment_rf %>% slice_min(Mean_RMSE, n = 1)
        
        cat("Random Forest (vs", best_tv_rf$Experiment[1], "):\n")
        
        # Calculate improvement and significance test
        baseline_rmse <- news_baseline_rf$Mean_RMSE
        baseline_std <- news_baseline_rf$Std_RMSE
        best_rmse <- best_tv_rf$Mean_RMSE
        best_std <- best_tv_rf$Std_RMSE
        
        improvement <- baseline_rmse - best_rmse
        improvement_pct <- (improvement / baseline_rmse) * 100
        
        # Standard error of difference (assuming 6 CV folds)
        se_diff <- sqrt((baseline_std^2 + best_std^2) / 6)
        
        # T-statistic and p-value
        t_stat <- improvement / se_diff
        p_value <- pt(-abs(t_stat), df = 5)  # One-tailed test
        
        # Effect size
        pooled_sd <- sqrt((baseline_std^2 + best_std^2) / 2)
        cohens_d <- improvement / pooled_sd
        
        # Significance levels
        significance <- case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          p_value < 0.10 ~ ".",
          TRUE ~ ""
        )
        
        conclusion <- case_when(
          p_value < 0.001 ~ "Highly significant (p < 0.001)",
          p_value < 0.01 ~ "Very significant (p < 0.01)",
          p_value < 0.05 ~ "Significant (p < 0.05)",
          p_value < 0.10 ~ "Marginally significant (p < 0.10)",
          TRUE ~ "Not significant (p >= 0.10)"
        )
        
        test_results <- rbind(test_results, data.frame(
          Target = target_name,
          Model = "Random Forest",
          Best_TV_Sentiment_Model = best_tv_rf$Experiment[1],
          Improvement_Pct = improvement_pct,
          T_Statistic = t_stat,
          P_Value = p_value,
          Significance = significance,
          Cohens_D = cohens_d,
          Conclusion = conclusion,
          stringsAsFactors = FALSE
        ))
        
        cat("- Incremental improvement:", sprintf("%.2f%%", improvement_pct), "\n")
        cat("- t-statistic:", sprintf("%.3f", t_stat), "\n")
        cat("- p-value:", sprintf("%.4f", p_value), significance, "\n")
        cat("- Effect size (Cohen's d):", sprintf("%.3f", cohens_d), "\n")
        cat("- Conclusion:", conclusion, "\n\n")
      }
    }
    
    # Test XGBoost
    if(nrow(news_baseline_xgb) > 0 && nrow(tv_sentiment_results) > 0) {
      
      tv_sentiment_xgb <- tv_sentiment_results %>% filter(Model == "XGBoost")
      
      if(nrow(tv_sentiment_xgb) > 0) {
        best_tv_xgb <- tv_sentiment_xgb %>% slice_min(Mean_RMSE, n = 1)
        
        cat("XGBoost (vs", best_tv_xgb$Experiment[1], "):\n")
        
        # Calculate improvement and significance test
        baseline_rmse <- news_baseline_xgb$Mean_RMSE
        baseline_std <- news_baseline_xgb$Std_RMSE
        best_rmse <- best_tv_xgb$Mean_RMSE
        best_std <- best_tv_xgb$Std_RMSE
        
        improvement <- baseline_rmse - best_rmse
        improvement_pct <- (improvement / baseline_rmse) * 100
        
        # Standard error of difference
        se_diff <- sqrt((baseline_std^2 + best_std^2) / 6)
        
        # T-statistic and p-value
        t_stat <- improvement / se_diff
        p_value <- pt(-abs(t_stat), df = 5)
        
        # Effect size
        pooled_sd <- sqrt((baseline_std^2 + best_std^2) / 2)
        cohens_d <- improvement / pooled_sd
        
        # Significance levels
        significance <- case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          p_value < 0.10 ~ ".",
          TRUE ~ ""
        )
        
        conclusion <- case_when(
          p_value < 0.001 ~ "Highly significant (p < 0.001)",
          p_value < 0.01 ~ "Very significant (p < 0.01)",
          p_value < 0.05 ~ "Significant (p < 0.05)",
          p_value < 0.10 ~ "Marginally significant (p < 0.10)",
          TRUE ~ "Not significant (p >= 0.10)"
        )
        
        test_results <- rbind(test_results, data.frame(
          Target = target_name,
          Model = "XGBoost",
          Best_TV_Sentiment_Model = best_tv_xgb$Experiment[1],
          Improvement_Pct = improvement_pct,
          T_Statistic = t_stat,
          P_Value = p_value,
          Significance = significance,
          Cohens_D = cohens_d,
          Conclusion = conclusion,
          stringsAsFactors = FALSE
        ))
        
        cat("- Incremental improvement:", sprintf("%.2f%%", improvement_pct), "\n")
        cat("- t-statistic:", sprintf("%.3f", t_stat), "\n")
        cat("- p-value:", sprintf("%.4f", p_value), significance, "\n")
        cat("- Effect size (Cohen's d):", sprintf("%.3f", cohens_d), "\n")
        cat("- Conclusion:", conclusion, "\n\n")
      }
    }
    
    return(test_results)
  }
  
  # Test both targets
  sp500_tests <- test_single_target(sp500_news_results, "S&P 500")
  ust_tests <- test_single_target(ust_news_results, "10Y UST")
  
  # Combine results
  all_h3_tests <- rbind(sp500_tests, ust_tests)
  
  return(all_h3_tests)
}

# =============================================================================
# COMPREHENSIVE H3 ANALYSIS
# =============================================================================

comprehensive_h3_analysis <- function(sp500_news_results, ust_news_results) {
  
  cat("COMPREHENSIVE HYPOTHESIS 3 ANALYSIS\n")
  cat("Testing incremental value of TV sentiment beyond traditional news sentiment\n")
  cat(rep("=", 90), "\n\n")
  
  # Run H3 analysis for both targets
  h3_sp500_results <- analyze_h3_hypothesis(sp500_news_results, "S&P 500 Daily Returns")
  h3_ust_results <- analyze_h3_hypothesis(ust_news_results, "10-Year UST Daily Changes")
  
  # Run statistical significance testing
  h3_statistical_results <- test_h3_significance(sp500_news_results, ust_news_results)
  
  # Create comprehensive summary
  if(nrow(h3_statistical_results) > 0) {
    cat(rep("=", 90), "\n")
    cat("H3 COMPREHENSIVE SUMMARY TABLE\n")
    cat(rep("=", 90), "\n\n")
    
    # Format table for display
    display_table <- h3_statistical_results %>%
      mutate(
        Improvement_Pct = round(Improvement_Pct, 2),
        T_Statistic = round(T_Statistic, 3),
        P_Value = round(P_Value, 4),
        Cohens_D = round(Cohens_D, 3)
      ) %>%
      select(Target, Model, Best_TV_Sentiment_Model, Improvement_Pct, 
             T_Statistic, P_Value, Significance, Conclusion)
    
    colnames(display_table) <- c("Target", "Model", "Best_TV_Model", "Improvement_%", 
                                 "T_Stat", "P_Value", "Sig", "Conclusion")
    
    print(kable(display_table, 
                caption = "H3 Results: Incremental Value of TV Sentiment Beyond News Sentiment",
                format = "simple"))
    
    # Statistical summary
    cat("\n", rep("=", 70), "\n")
    cat("H3 STATISTICAL SUMMARY\n")
    cat(rep("=", 70), "\n\n")
    
    significant_count <- sum(h3_statistical_results$P_Value < 0.05, na.rm = TRUE)
    marginal_count <- sum(h3_statistical_results$P_Value >= 0.05 & h3_statistical_results$P_Value < 0.10, na.rm = TRUE)
    total_tests <- nrow(h3_statistical_results)
    
    cat("Summary of Evidence for H3:\n")
    cat("- Tests performed:", total_tests, "\n")
    cat("- Significant (p < 0.05):", significant_count, "out of", total_tests, 
        sprintf("(%.1f%%)", significant_count/total_tests*100), "\n")
    cat("- Marginally significant (0.05 ≤ p < 0.10):", marginal_count, "out of", total_tests,
        sprintf("(%.1f%%)", marginal_count/total_tests*100), "\n")
    cat("- Not significant (p ≥ 0.10):", total_tests - significant_count - marginal_count, 
        "out of", total_tests, sprintf("(%.1f%%)", (total_tests - significant_count - marginal_count)/total_tests*100), "\n\n")
    
    # Effect sizes
    large_effects <- sum(abs(h3_statistical_results$Cohens_D) >= 0.8, na.rm = TRUE)
    medium_effects <- sum(abs(h3_statistical_results$Cohens_D) >= 0.5 & abs(h3_statistical_results$Cohens_D) < 0.8, na.rm = TRUE)
    small_effects <- sum(abs(h3_statistical_results$Cohens_D) >= 0.2 & abs(h3_statistical_results$Cohens_D) < 0.5, na.rm = TRUE)
    
    cat("Effect Size Summary (Cohen's d):\n")
    cat("- Large effects (|d| ≥ 0.8):", large_effects, "out of", total_tests, "\n")
    cat("- Medium effects (0.5 ≤ |d| < 0.8):", medium_effects, "out of", total_tests, "\n")
    cat("- Small effects (0.2 ≤ |d| < 0.5):", small_effects, "out of", total_tests, "\n\n")
    
    # Best improvement
    if(total_tests > 0) {
      best_improvement <- max(h3_statistical_results$Improvement_Pct, na.rm = TRUE)
      best_result <- h3_statistical_results[which.max(h3_statistical_results$Improvement_Pct), ]
      
      cat("Best Incremental Improvement:\n")
      cat("- Maximum improvement:", sprintf("%.2f%%", best_improvement), "\n")
      cat("- Best combination:", best_result$Model[1], "for", best_result$Target[1], "\n")
      cat("- TV sentiment model:", best_result$Best_TV_Sentiment_Model[1], "\n")
      cat("- Statistical significance:", best_result$Conclusion[1], "\n\n")
    }
    
    # Overall H3 conclusion
    cat(rep("=", 70), "\n")
    cat("OVERALL H3 CONCLUSION\n")
    cat(rep("=", 70), "\n\n")
    
    if(significant_count >= total_tests/2) {
      cat("✓ STRONG SUPPORT FOR H3:\n")
      cat("  TV sentiment features provide statistically significant incremental\n")
      cat("  predictive value beyond traditional news sentiment indices.\n")
    } else if(significant_count + marginal_count >= total_tests/2) {
      cat("~ MODERATE SUPPORT FOR H3:\n")
      cat("  TV sentiment features show significant or marginally significant\n")
      cat("  incremental value beyond traditional news sentiment.\n")
    } else {
      cat("? LIMITED SUPPORT FOR H3:\n")
      cat("  While some incremental improvements are observed, statistical\n")
      cat("  significance is limited for the incremental value hypothesis.\n")
    }
    
    cat("\nKey Insight:\n")
    cat("H3 tests the most stringent requirement - whether TV sentiment adds value\n")
    cat("BEYOND existing news sentiment measures. This is a higher bar than H1.\n")
    
  } else {
    cat("No statistical test results available for H3\n")
  }
  
  return(list(
    sp500_analysis = h3_sp500_results,
    ust_analysis = h3_ust_results,
    statistical_results = h3_statistical_results
  ))
}

# =============================================================================
# EXECUTION EXAMPLE
# =============================================================================
sp500_news_data <- read_csv("SP_500_comprehensive_results_news_sentiment/ts_cv_results_summary_SP500_news.csv")
ust_news_data <- read_csv("UST_comprehensive_results_time_series_cross_validation_news_sentiment/ts_cv_results_summary_UST_news.csv") 

#
h3_analysis <- comprehensive_h3_analysis(sp500_news_data, ust_news_data)

cat("H3 TESTING FRAMEWORK READY!\n")
cat("============================\n")
cat("This approach:\n")
cat("1. Tests incremental value of TV sentiment beyond news sentiment\n")
cat("2. Uses same statistical methodology as H1 for consistency\n")
cat("3. Provides comprehensive evidence evaluation across targets\n")
cat("4. Tests the most stringent requirement for TV sentiment value\n")