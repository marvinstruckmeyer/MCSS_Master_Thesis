# =============================================================================
# PUBLICATION-READY TABLES FOR HYPOTHESIS TESTING RESULTS 
# =============================================================================

library(kableExtra)
library(dplyr)

# =============================================================================
# TABLE 1: HYPOTHESIS 1 RESULTS
# =============================================================================

create_h1_table <- function() {
  
  # Create H1 results data
  h1_data <- data.frame(
    `Target Variable` = c("S&P 500 returns", "S&P 500 returns", 
                          "10Y UST changes", "10Y UST changes"),
    Model = c("Random forest", "XGBoost", "Random forest", "XGBoost"),
    `Best Sentiment Model` = c("sentimentr", "All sentiment features",
                               "All sentiment indices", "FinBERT"),
    `RMSE Improvement` = c("0.82%", "1.34%", "0.34%", "0.34%"),
    `T-Statistic` = c("0.037", "0.061", "0.034", "0.043"),
    `P-Value` = c("0.972", "0.953", "0.975", "0.968"),
    Conclusion = c("Not significant", "Not significant", 
                   "Not significant", "Not significant"),
    check.names = FALSE
  )
  
  # Create formatted table
  h1_table <- h1_data %>%
    kbl(caption = NULL,
        align = "lllcccc",
        booktabs = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE,
                  font_size = 12) %>%
    column_spec(1, bold = TRUE, width = "2.5cm") %>%
    column_spec(2, width = "2cm") %>%
    column_spec(3, width = "3cm") %>%
    column_spec(4, width = "1.5cm") %>%
    column_spec(5, width = "1.5cm") %>%
    column_spec(6, width = "1.5cm") %>%
    column_spec(7, width = "2cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    #add_footnote("Key Finding: Sentiment features provide consistent improvements but lack statistical significance (all p > 0.90).",
     #            notation = "alphabet")  # Changed from "general" to "alphabet"
  
  return(h1_table)
}

# =============================================================================
# TABLE 2A: HYPOTHESIS 2 DETAILED COMPARISON TABLE
# =============================================================================

create_h2_detailed_table <- function() {
  
  # Create H2 detailed comparison data
  h2_detailed_data <- data.frame(
    Experiment = c("Loughran-McDonald", "Loughran-McDonald",
                   "sentimentr", "sentimentr", 
                   "FinBERT", "FinBERT",
                   "All sentiment indices", "All sentiment indices",
                   "All sentiment features", "All sentiment features"),
    Model = c("Random forest", "XGBoost", "Random forest", "XGBoost",
              "Random forest", "XGBoost", "Random forest", "XGBoost",
              "Random forest", "XGBoost"),
    `SP500 Improv %` = c("0.60", "-1.06", "0.82", "-3.23", "-0.11", "-1.80",
                         "0.24", "0.06", "0.47", "1.34"),
    `VIX Improv %` = c("0.36", "1.06", "0.77", "-0.75", "1.60", "1.46",
                       "1.62", "0.05", "0.73", "1.77"),
    Difference = c("-0.24", "2.12", "-0.05", "2.48", "1.71", "3.26",
                   "1.38", "-0.01", "0.26", "0.43"),
    `Support for H2` = c("✗", "✓", "✗", "✓", "✓", "✓", "✓", "✗", "✓", "✓"),
    check.names = FALSE
  )
  
  # Create formatted table
  h2_detailed_table <- h2_detailed_data %>%
    kbl(caption = NULL,
        align = "llcccc",
        booktabs = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE,
                  font_size = 11) %>%
    column_spec(1, bold = TRUE, width = "3.5cm") %>%
    column_spec(2, width = "2.2cm") %>%
    column_spec(3, width = "1.8cm") %>%
    column_spec(4, width = "1.8cm") %>%
    column_spec(5, width = "1.5cm") %>%
    column_spec(6, width = "1.5cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    # Highlight rows where VIX shows large advantage
    row_spec(c(2, 4, 6), background = "#e8f5e8") %>%  # Large positive differences
    add_footnote(c("✓ indicates VIX improvement > S&P 500 improvement (H2 supported)",
                   "Highlighted rows show substantial VIX advantages (difference > 2.0%)"),
                 notation = "alphabet")  # Changed from "general" to "alphabet"
  
  return(h2_detailed_table)
}

# =============================================================================
# TABLE 2B: HYPOTHESIS 2 STATISTICAL SUMMARY
# =============================================================================

create_h2_summary_table <- function() {
  
  # Create H2 statistical summary data
  h2_summary_data <- data.frame(
    `Comparison metric` = c("Model-experiment combinations tested",
                            "Cases favoring volatility (VIX)",
                            "Mean improvement difference (VIX - S&P 500)",
                            "Effect size (Cohen's d)",
                            "Statistical significance (t-test)"),
    Value = c("10", "7 out of 10 (70%)", "+1.135%", "0.925 (Large)", 
              "p = 0.008**"),
    Interpretation = c("Total comparisons", "Directional evidence", 
                       "Volatility advantage", "Substantial effect",
                       "Highly significant"),
    check.names = FALSE)
  
  # Create formatted table
  h2_summary_table <- h2_summary_data %>%
    kbl(caption = NULL,
        align = "lcc",
        booktabs = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE,
                  font_size = 12) %>%
    column_spec(1, bold = TRUE, width = "5cm") %>%
    column_spec(2, width = "3cm") %>%
    column_spec(3, width = "3cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    row_spec(5, bold = TRUE, color = "darkgreen")
  
  return(h2_summary_table)
}

# =============================================================================
# TABLE 3: HYPOTHESIS 3 RESULTS
# =============================================================================

create_h3_table <- function() {
  
  # Create H3 results data
  h3_data <- data.frame(
    `Target Variable` = c("S&P 500 returns", "S&P 500 returns",
                          "10Y UST changes", "10Y UST changes"),
    Model = c("Random forest", "XGBoost", "Random forest", "XGBoost"),
    `Best TV Sentiment Model` = c("Loughran-McDonald", "All sentiment features",
                                  "Average sentiment score", "Loughran-McDonald"),
    `Incremental Improvement` = c("0.42%", "5.78%", "1.13%", "1.46%"),
    `T-Statistic` = c("0.019", "0.254", "0.105", "0.179"),
    `P-Value` = c("0.493", "0.405", "0.460", "0.433"),
    Conclusion = c("Not significant", "Not significant", 
                   "Not significant", "Not significant"),
    check.names = FALSE
  )
  
  # Create formatted table
  h3_table <- h3_data %>%
    kbl(caption = NULL,
        align = "lllcccc",
        booktabs = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE,
                  font_size = 12) %>%
    column_spec(1, bold = TRUE, width = "2.5cm") %>%
    column_spec(2, width = "2cm") %>%
    column_spec(3, width = "3.5cm") %>%
    column_spec(4, width = "2cm") %>%
    column_spec(5, width = "1.5cm") %>%
    column_spec(6, width = "1.5cm") %>%
    column_spec(7, width = "2cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    row_spec(2, bold = TRUE, color = "darkblue")
  
  return(h3_table)
}

# =============================================================================
# SUMMARY TABLE ACROSS ALL HYPOTHESES
# =============================================================================

create_summary_table <- function() {
  
  # Create summary data
  summary_data <- data.frame(
    Hypothesis = c("H1: Sentiment Predictive Power", 
                   "H2: Volatility vs Returns", 
                   "H3: Incremental Value"),
    `Primary Finding` = c("Consistent improvements (0.34-1.34%)",
                          "Volatility prediction superior (+1.135%)",
                          "TV adds value beyond news (up to 5.78%)"),
    `Statistical Support` = c("Limited (all p > 0.90)", 
                              "Strong (p = 0.008**)",
                              "Limited (all p > 0.40)"),
    `Economic Significance` = c("Modest", "Substantial", "Moderate"),
    check.names = FALSE
  )
  
  # Create formatted table
  summary_table <- summary_data %>%
    kbl(caption = "Table 4: Summary of Results Across All Hypotheses",
        align = "llcc",
        booktabs = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE,
                  font_size = 12) %>%
    column_spec(1, bold = TRUE, width = "3cm") %>%
    column_spec(2, width = "4cm") %>%
    column_spec(3, width = "3cm") %>%
    column_spec(4, width = "2.5cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    row_spec(2, bold = TRUE, color = "darkgreen") %>%  # Highlight H2 success
    add_footnote("** p < 0.01; Overall: Sentiment analysis shows greatest promise for volatility prediction.",
                 notation = "alphabet")  # Changed from "general" to "alphabet"
  
  return(summary_table)
}

# =============================================================================
# FUNCTION TO CREATE ALL TABLES WITH PROPER SPACING
# =============================================================================

create_all_hypothesis_tables <- function() {
  
  cat("Creating publication-ready tables for hypothesis testing results...\n\n")
  
  # Create individual tables
  h1_table <- create_h1_table()
  h2_detailed_table <- create_h2_detailed_table()
  h2_summary_table <- create_h2_summary_table()
  h3_table <- create_h3_table()
  summary_table <- create_summary_table()
  
  # Display tables with spacing
  cat("=== HYPOTHESIS 1 TABLE ===\n")
  print(h1_table)
  cat("\n\n")
  
  cat("=== HYPOTHESIS 2 DETAILED TABLE ===\n") 
  print(h2_detailed_table)
  cat("\n\n")
  
  cat("=== HYPOTHESIS 2 STATISTICAL SUMMARY ===\n")
  print(h2_summary_table)
  cat("\n\n")
  
  cat("=== HYPOTHESIS 3 TABLE ===\n")
  print(h3_table)
  cat("\n\n")
  
  cat("=== SUMMARY TABLE ===\n")
  print(summary_table)
  cat("\n\n")
  
  cat("✅ All tables created successfully!\n")
  cat("📋 To copy to Word:\n")
  cat("   1. Run each create_hX_table() function individually\n")
  cat("   2. The output will render as HTML in RStudio Viewer\n")
  cat("   3. Right-click and 'Copy' from the Viewer\n")
  cat("   4. Paste directly into Word - formatting preserved!\n\n")
  
  return(list(h1 = h1_table, 
              h2_detailed = h2_detailed_table, 
              h2_summary = h2_summary_table,
              h3 = h3_table, 
              summary = summary_table))
}

# =============================================================================
# ALTERNATIVE: WORD-COMPATIBLE TABLE FUNCTION
# =============================================================================

create_word_compatible_tables <- function() {
  
  # For direct Word compatibility, use this simpler approach
  library(flextable)
  
  # H1 Table with flextable
  h1_data <- data.frame(
    Target = c("S&P 500", "S&P 500", "10Y UST", "10Y UST"),
    Model = c("Random Forest", "XGBoost", "Random Forest", "XGBoost"),
    Best_Model = c("sentimentr_score", "all_sentiment", "basic_sentiment", "finbert_score"),
    Improvement = c("0.82%", "1.34%", "0.34%", "0.34%"),
    P_Value = c("0.972", "0.953", "0.975", "0.968"),
    Conclusion = c("Not sig.", "Not sig.", "Not sig.", "Not sig.")
  )
  
  h1_flex <- flextable(h1_data) %>%
    set_caption("Table 1: Hypothesis 1 Results") %>%
    theme_box() %>%
    autofit()
  
  return(h1_flex)
}

# Run all tables
all_tables <- create_all_hypothesis_tables()

# run individual tables:
h1_table <- create_h1_table()
h2_detailed_table <- create_h2_detailed_table() 
h2_summary_table <- create_h2_summary_table()
h3_table <- create_h3_table()
summary_table <- create_summary_table()


### for the appendix
# =============================================================================
# WIDE FORMAT PROFESSIONAL TABLES - SPACE EFFICIENT
# =============================================================================

library(kableExtra)
library(dplyr)

# =============================================================================
# TABLE 1: UST NEWS RESULTS - WIDE FORMAT
# =============================================================================

create_ust_news_table_wide <- function() {
  
  # Create wide format data - experiments as rows, models as columns
  ust_news_wide <- data.frame(
    `Experiment` = c("Baseline", "Market variables only", "Loughran-McDonald", 
                     "sentimentr", "FinBERT", "Ollama", "Average sentiment score",
                     "All sentiment indices", "All individual indices, but not average one", 
                     "All sentiment features"),
    `RF Mean RMSE` = c(0.1536, 0.1522, 0.1522, 0.1532, 0.1521, 0.1532, 
                       0.1518, 0.1532, 0.1520, 0.1535),
    `XGB Mean RMSE` = c(0.1489, 0.1486, 0.1467, 0.1491, 0.1474, 0.1470,
                        0.1478, 0.1490, 0.1472, 0.1476),
    `RF Std RMSE` = c(0.0295, 0.0253, 0.0208, 0.0285, 0.0272, 0.0280,
                      0.0275, 0.0278, 0.0280, 0.0267),
    `XGB Std RMSE` = c(0.0210, 0.0250, 0.0250, 0.0189, 0.0212, 0.0184,
                       0.0194, 0.0191, 0.0182, 0.0203),
    `RF Mean R²` = c(-0.1268, -0.1298, -0.1091, -0.1226, -0.1102, -0.1242,
                     -0.1050, -0.1244, -0.1049, -0.1310),
    `XGB Mean R²` = c(-0.0588, -0.0549, -0.0382, -0.0758, -0.0494, -0.0470,
                      -0.0566, -0.0746, -0.0513, -0.0524),
    check.names = FALSE
  )
  
  # Create formatted table
  ust_news_table_wide <- ust_news_wide %>%
    kbl(caption = NULL,
        align = "lcccccc",
        booktabs = TRUE,
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE,
                  font_size = 11) %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2:7, width = "1.8cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    #add_header_above(c(" " = 1, "Random Forest" = 3, "XGBoost" = 3)) %>%
    add_header_above(c(" " = 1, "RMSE Metrics" = 6)) %>%
    pack_rows("Baseline Models", 1, 2, label_row_css = "background-color: #e8f5e8; color: #333;") %>%
    pack_rows("Individual Sentiment Models", 3, 7, label_row_css = "background-color: #e8f4f8; color: #333;") %>%
    pack_rows("Combined Sentiment Models", 8, 10, label_row_css = "background-color: #f8f4e8; color: #333;")
  
  return(ust_news_table_wide)
}

# =============================================================================
# TABLE 2: SP500 RESULTS - WIDE FORMAT
# =============================================================================

create_sp500_table_wide <- function() {
  
  # Create wide format data - experiments as rows, models as columns
  sp500_wide <- data.frame(
    `Experiment` = c("Baseline", "Market variables only", "Loughran-McDonald", 
                     "sentimentr", "FinBERT", "Ollama", "Average sentiment score",
                     "All sentiment indices", "All individual indices, but not average one", 
                     "All sentiment features"),
    `RF Mean RMSE` = c(0.8426, 0.8608, 0.8375, 0.8357, 0.8435, 0.8491, 
                       0.8426, 0.8406, 0.8366, 0.8386),
    `XGB Mean RMSE` = c(0.8423, 0.8524, 0.8513, 0.8696, 0.8575, 0.8376,
                        0.8407, 0.8418, 0.8611, 0.8311),
    `RF Std RMSE` = c(0.3241, 0.3529, 0.3082, 0.3165, 0.3304, 0.3146,
                      0.3255, 0.3155, 0.3245, 0.3160),
    `XGB Std RMSE` = c(0.3204, 0.3493, 0.3448, 0.3000, 0.3425, 0.3315,
                       0.3142, 0.3385, 0.3000, 0.3140),
    `RF Mean R²` = c(-0.0889, -0.1233, -0.0860, -0.0744, -0.0870, -0.1160,
                     -0.1012, -0.0898, -0.0713, -0.0825),
    `XGB Mean R²` = c(-0.0928, -0.1048, -0.1069, -0.2006, -0.1220, -0.0730,
                      -0.0978, -0.0776, -0.1698, -0.0675),
    check.names = FALSE
  )
  
  # Create formatted table
  sp500_table_wide <- sp500_wide %>%
    kbl(caption = NULL,
        align = "lcccccc",
        booktabs = TRUE,
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE,
                  font_size = 11) %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2:7, width = "1.8cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    #add_header_above(c(" " = 1, "Random Forest" = 3, "XGBoost" = 3)) %>%
    add_header_above(c(" " = 1, "RMSE Metrics" = 6)) %>%
    pack_rows("Baseline Models", 1, 2, label_row_css = "background-color: #e8f5e8; color: #333;") %>%
    pack_rows("Individual Sentiment Models", 3, 7, label_row_css = "background-color: #e8f4f8; color: #333;") %>%
    pack_rows("Combined Sentiment Models", 8, 10, label_row_css = "background-color: #f8f4e8; color: #333;")
  
  return(sp500_table_wide)
}

# =============================================================================
# TABLE 3: SP500 NEWS RESULTS - WIDE FORMAT
# =============================================================================

create_sp500_news_table_wide <- function() {
  
  # Create wide format data - experiments as rows, models as columns
  sp500_news_wide <- data.frame(
    `Experiment` = c("Baseline", "Market variables only", "Loughran-McDonald", 
                     "sentimentr", "FinBERT", "Ollama", "Average sentiment score",
                     "All sentiment indices", "All individual indices, but not average one", 
                     "All sentiment features"),
    `RF Mean RMSE` = c(0.8386, 0.8575, 0.8351, 0.8477, 0.8441, 0.8435, 
                       0.8512, 0.8429, 0.8371, 0.8361),
    `XGB Mean RMSE` = c(0.8702, 0.8484, 0.8419, 0.8506, 0.8458, 0.8616,
                        0.8331, 0.8434, 0.8347, 0.8200),
    `RF Std RMSE` = c(0.3234, 0.3571, 0.3183, 0.3219, 0.3138, 0.3249,
                      0.3230, 0.3248, 0.3223, 0.3178),
    `XGB Std RMSE` = c(0.3716, 0.3303, 0.3223, 0.3323, 0.3087, 0.3406,
                       0.3264, 0.3399, 0.3229, 0.3105),
    `RF Mean R²` = c(-0.0753, -0.1103, -0.0703, -0.1054, -0.1009, -0.0891,
                     -0.1131, -0.0885, -0.0735, -0.0757),
    `XGB Mean R²` = c(-0.1471, -0.1008, -0.0912, -0.1040, -0.1161, -0.1338,
                      -0.0636, -0.0833, -0.0732, -0.0350),
    check.names = FALSE
  )
  
  # Create formatted table
  sp500_news_table_wide <- sp500_news_wide %>%
    kbl(caption = NULL,
        align = "lcccccc",
        booktabs = TRUE,
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE,
                  font_size = 11) %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2:7, width = "1.8cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    #add_header_above(c(" " = 1, "Random Forest" = 3, "XGBoost" = 3)) %>%
    add_header_above(c(" " = 1, "RMSE Metrics" = 6)) %>%
    pack_rows("Baseline Models", 1, 2, label_row_css = "background-color: #e8f5e8; color: #333;") %>%
    pack_rows("Individual Sentiment Models", 3, 7, label_row_css = "background-color: #e8f4f8; color: #333;") %>%
    pack_rows("Combined Sentiment Models", 8, 10, label_row_css = "background-color: #f8f4e8; color: #333;")
  
  return(sp500_news_table_wide)
}

# =============================================================================
# TABLE 4: UST RESULTS - WIDE FORMAT
# =============================================================================

create_ust_table_wide <- function() {
  
  # Create wide format data - experiments as rows, models as columns
  ust_wide <- data.frame(
    `Experiment` = c("Baseline", "Market variables only", "Loughran-McDonald", 
                     "sentimentr", "FinBERT", "Ollama", "Average sentiment score",
                     "All sentiment indices", "All individual indices, but not average one", 
                     "All sentiment features"),
    `RF Mean RMSE` = c(0.1524, 0.1511, 0.1521, 0.1520, 0.1551, 0.1524, 
                       0.1527, 0.1519, 0.1527, 0.1528),
    `XGB Mean RMSE` = c(0.1479, 0.1489, 0.1476, 0.1487, 0.1475, 0.1484,
                        0.1476, 0.1503, 0.1475, 0.1486),
    `RF Std RMSE` = c(0.0266, 0.0250, 0.0280, 0.0274, 0.0259, 0.0276,
                      0.0269, 0.0274, 0.0263, 0.0247),
    `XGB Std RMSE` = c(0.0199, 0.0213, 0.0199, 0.0215, 0.0204, 0.0190,
                       0.0196, 0.0191, 0.0202, 0.0206),
    `RF Mean R²` = c(-0.1144, -0.1132, -0.1087, -0.1075, -0.1230, -0.1120,
                     -0.1186, -0.1061, -0.1192, -0.1232),
    `XGB Mean R²` = c(-0.0587, -0.0706, -0.0519, -0.0661, -0.0481, -0.0660,
                      -0.0542, -0.0944, -0.0505, -0.0673),
    check.names = FALSE
  )
  
  # Create formatted table
  ust_table_wide <- ust_wide %>%
    kbl(caption = NULL,
        align = "lcccccc",
        booktabs = TRUE,
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE,
                  font_size = 11) %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2:7, width = "1.8cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    #add_header_above(c(" " = 1, "Random Forest" = 3, "XGBoost" = 3)) %>%
    add_header_above(c(" " = 1, "RMSE Metrics" = 6)) %>%
    pack_rows("Baseline Models", 1, 2, label_row_css = "background-color: #e8f5e8; color: #333;") %>%
    pack_rows("Individual Sentiment Models", 3, 7, label_row_css = "background-color: #e8f4f8; color: #333;") %>%
    pack_rows("Combined Sentiment Models", 8, 10, label_row_css = "background-color: #f8f4e8; color: #333;")
  
  return(ust_table_wide)
}


####
create_vix_table_wide <- function() {
  
  # Create VIX wide format data - experiments as rows, models as columns
  vix_wide <- data.frame(
    `Experiment` = c("Baseline", "Market variables only", "Loughran-McDonald", 
                     "sentimentr", "FinBERT", "Ollama", "Average sentiment score",
                     "All sentiment indices", "All individual indices, but not average one", 
                     "All sentiment features", "Momentum features", "Cross sectional"),
    `RF Mean RMSE` = c(0.0594, 0.0588, 0.0591, 0.0589, 0.0584, 0.0589, 0.0589, 
                       0.0584, 0.0586, 0.0589, 0.0587, 0.0586),
    `XGB Mean RMSE` = c(0.0605, 0.0587, 0.0599, 0.0610, 0.0596, 0.0618, 0.0606,
                        0.0605, 0.0586, 0.0598, 0.0584, 0.0590),
    `RF Std RMSE` = c(0.0462, 0.0482, 0.0462, 0.0461, 0.0458, 0.0463, 0.0462,
                      0.0459, 0.0461, 0.0461, 0.0464, 0.0464),
    `XGB Std RMSE` = c(0.0481, 0.0480, 0.0478, 0.0476, 0.0478, 0.0499, 0.0476,
                       0.0469, 0.0463, 0.0470, 0.0455, 0.0478),
    `RF Mean R²` = c(-0.1684, -0.0889, -0.1463, -0.1382, -0.1208, -0.1343, -0.1396,
                     -0.1164, -0.1285, -0.1459, -0.1193, -0.1126),
    `XGB Mean R²` = c(-0.1674, -0.1038, -0.1488, -0.2137, -0.1169, -0.2078, -0.1851,
                      -0.2135, -0.1240, -0.1249, -0.1582, -0.1009),
    check.names = FALSE
  )
  
  # Create formatted table
  vix_table_wide <- vix_wide %>%
    kbl(caption = NULL,
        align = "lcccccc",
        booktabs = TRUE,
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE,
                  font_size = 11) %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2:7, width = "1.8cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    #add_header_above(c(" " = 1, "Random Forest" = 3, "XGBoost" = 3)) %>%
    add_header_above(c(" " = 1, "RMSE Metrics" = 6)) %>%
    pack_rows("Baseline Models", 1, 2, label_row_css = "background-color: #e8f5e8; color: #333;") %>%
    pack_rows("Individual Sentiment Models", 3, 7, label_row_css = "background-color: #e8f4f8; color: #333;") %>%
    pack_rows("Combined Sentiment Models", 8, 10, label_row_css = "background-color: #f8f4e8; color: #333;") %>%
    pack_rows("Advanced Features", 11, 12, label_row_css = "background-color: #fff2e8; color: #333;")
  
  return(vix_table_wide)
}

###

# =============================================================================
# COMPACT VERSION - RMSE ONLY FOR MAXIMUM SPACE EFFICIENCY
# =============================================================================

create_compact_comparison_table <- function() {
  
  # Ultra-compact version with just RMSE comparisons
  compact_data <- data.frame(
    `Experiment` = c("Baseline", "Market variables only", "Loughran-McDonald", 
                     "sentimentr", "FinBERT", "Ollama", "Average sentiment score",
                     "All sentiment indices", "All individual indices", "All sentiment features"),
    `UST News RF` = c(0.1536, 0.1522, 0.1522, 0.1532, 0.1521, 0.1532, 0.1518, 0.1532, 0.1520, 0.1535),
    `UST News XGB` = c(0.1489, 0.1486, 0.1467, 0.1491, 0.1474, 0.1470, 0.1478, 0.1490, 0.1472, 0.1476),
    `SP500 RF` = c(0.8426, 0.8608, 0.8375, 0.8357, 0.8435, 0.8491, 0.8426, 0.8406, 0.8366, 0.8386),
    `SP500 XGB` = c(0.8423, 0.8524, 0.8513, 0.8696, 0.8575, 0.8376, 0.8407, 0.8418, 0.8611, 0.8311),
    `SP500 News RF` = c(0.8386, 0.8575, 0.8351, 0.8477, 0.8441, 0.8435, 0.8512, 0.8429, 0.8371, 0.8361),
    `SP500 News XGB` = c(0.8702, 0.8484, 0.8419, 0.8506, 0.8458, 0.8616, 0.8331, 0.8434, 0.8347, 0.8200),
    `UST RF` = c(0.1524, 0.1511, 0.1521, 0.1520, 0.1551, 0.1524, 0.1527, 0.1519, 0.1527, 0.1528),
    `UST XGB` = c(0.1479, 0.1489, 0.1476, 0.1487, 0.1475, 0.1484, 0.1476, 0.1503, 0.1475, 0.1486),
    check.names = FALSE
  )
  
  # Create ultra-compact table
  compact_table <- compact_data %>%
    kbl(caption = NULL,
        align = "lcccccccc",
        booktabs = TRUE,
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE,
                  font_size = 10) %>%
    column_spec(1, bold = TRUE, width = "3.5cm") %>%
    column_spec(2:9, width = "1.4cm") %>%
    row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
    add_header_above(c(" " = 1, "UST News" = 2, "S&P 500" = 2, "S&P 500 News" = 2, "UST" = 2)) %>%
    add_header_above(c(" " = 1, "Mean RMSE by Dataset and Model" = 8)) %>%
    pack_rows("Baseline", 1, 2, label_row_css = "background-color: #e8f5e8; color: #333;") %>%
    pack_rows("Individual Sentiment", 3, 7, label_row_css = "background-color: #e8f4f8; color: #333;") %>%
    pack_rows("Combined Sentiment", 8, 10, label_row_css = "background-color: #f8f4e8; color: #333;")
  
  return(compact_table)
}

# =============================================================================
# FUNCTION TO CREATE ALL WIDE FORMAT TABLES
# =============================================================================

create_all_wide_tables <- function() {
  
  cat("Creating space-efficient wide format tables...\n\n")
  
  # Create individual wide tables
  ust_news_wide <- create_ust_news_table_wide()
  sp500_wide <- create_sp500_table_wide()
  sp500_news_wide <- create_sp500_news_table_wide()
  ust_wide <- create_ust_table_wide()
  compact_comparison <- create_compact_comparison_table()
  
  # Display tables
  cat("=== TABLE 1: UST NEWS (WIDE FORMAT) ===\n")
  print(ust_news_wide)
  cat("\n\n")
  
  cat("=== TABLE 2: S&P 500 (WIDE FORMAT) ===\n") 
  print(sp500_wide)
  cat("\n\n")
  
  cat("=== TABLE 3: S&P 500 NEWS (WIDE FORMAT) ===\n")
  print(sp500_news_wide)
  cat("\n\n")
  
  cat("=== TABLE 4: UST (WIDE FORMAT) ===\n")
  print(ust_wide)
  cat("\n\n")
  
  cat("=== TABLE 5: COMPACT COMPARISON (ALL DATASETS) ===\n")
  print(compact_comparison)
  cat("\n\n")
  
  cat("✅ All wide format tables created successfully!\n")
  cat("📏 Benefits of wide format:\n")
  cat("   • Much shorter tables (10 rows vs 20)\n")
  cat("   • Easier to compare models side-by-side\n")
  cat("   • Fits better on standard page sizes\n")
  cat("   • Table 5 shows ALL results in one compact view\n\n")
  
  return(list(ust_news_wide = ust_news_wide, 
              sp500_wide = sp500_wide, 
              sp500_news_wide = sp500_news_wide,
              ust_wide = ust_wide,
              compact_comparison = compact_comparison))
}

# =============================================================================
# EXECUTE - RUN WIDE FORMAT TABLES
# =============================================================================

# Run all wide format tables
all_wide_tables <- create_all_wide_tables()

# Run individual wide tables:
ust_news_wide <- create_ust_news_table_wide()
sp500_wide <- create_sp500_table_wide() 
sp500_news_wide <- create_sp500_news_table_wide()
ust_wide <- create_ust_table_wide()
compact_comparison <- create_compact_comparison_table()



