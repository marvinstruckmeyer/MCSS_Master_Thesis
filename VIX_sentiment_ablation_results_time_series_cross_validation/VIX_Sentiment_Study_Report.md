# VIX Change Prediction: Sentiment Ablation Study Results

**Analysis Date:** 2025-06-17
**Data Period:** See individual experiment logs

## Executive Summary

This report presents the results of a comprehensive ablation study examining the impact of sentiment variables on VIX change prediction accuracy.

## Model Performance Summary

### Key Findings

- **Best performing model:** Random Forest in market_only experiment

- **Highest R²:** -0.0886

- **Lowest RMSE:** 0.058385

### Detailed Results

The following experiments were conducted:

** baseline **
- Features: GARCH only

** market_only **
- Features: GARCH only

** individual_dict_sentiment **
- Features: GARCH only

** individual_sentimentr_score **
- Features: GARCH only

** individual_finbert_score **
- Features: GARCH only

** individual_ollama_score **
- Features: GARCH only

** individual_aggregate_sentiment **
- Features: GARCH only

** basic_sentiment **
- Features: GARCH only

** combined_individual **
- Features: GARCH only

** aggregate_only **
- Features: GARCH only

** momentum_features **
- Features: GARCH only

** cross_sectional **
- Features: GARCH only

** all_sentiment **
- Features: GARCH only


## Technical Details

### Data Preparation
- Target variable: Next-day VIX changes (properly time-shifted)
- Training/test split: 80/20
- Missing value handling: Complete case analysis

### Model Specifications
- **Random Forest:** Tuned hyperparameters with OOB error optimization
- **XGBoost:** Tuned hyperparameters with cross-validation
- **GARCH:** Multiple specifications (GARCH, EGARCH, GJR-GARCH) with AIC selection

### Sentiment Variables
- Basic sentiment: Dictionary, SentimentR, FinBERT, Ollama, Aggregate
- Momentum features: 3-day and 5-day momentum calculations
- Cross-sectional: Standard deviation, range, coefficient of variation

**Report generated on:** 2025-06-17 02:55:19.576843
