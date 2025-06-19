# Sentiment analysis of financial TV shows and predictive modelling

**Master's Thesis in Computational Social Sciences**  
Universidad Carlos III de Madrid (UC3M)

**Author:** Marvin-Julian Struckmeyer  
**Email:** 100540594@alumnos.uc3m.es  

## Abstract

This thesis studies the relationship between sentiment extracted from Bloomberg TV shows and market movements. To this end, I use multiple sentiment analysis methods (dictionary-based, FInBERT, and Ollama) to construct daily sentiment indices. Comprehensive ablation studies with random forest and XGBoost are then used to evaluate whether these sentiment indices have predictive power for daily returns for the S&P 500, and daily changes in 10-Year US Treasury yields and the VIX.

## Repository structure

```
MCSS_Master_Thesis/
│
├── 01_data_retrieval/          # YouTube API data collection
│   ├── scripts/
│   └── data/
│
├── 02_sentiment_analysis/      # Sentiment analysis pipeline
│   ├── scripts/
│   └── data/
│
├── 03_ml_prediction/          # Machine learning models
│   ├── scripts/
│   └── data/
│
├── 04_misc/                   # Utilities and plotting
│   └── scripts/
│
├── market_macro_data/         # Economic indicators
│
├── config/                    # Configuration files
│   └── config.yaml
│
└── *_results_*/              # Model results and outputs
```

## 📊 Large datasets (Zenodo)

Due to GitHub's file size limitations, I host the the main transcript datasets on Zenodo:

🔗 **[Download Large Datasets from Zenodo](https://zenodo.org/records/15698311)**

The following files should be downloaded and placed in `01_data_retrieval/data/combined/`:
- `bloomberg_transcripts_part_1_of_5.csv` (199 MB)
- `bloomberg_transcripts_part_2_of_5.csv` (199 MB)
- `bloomberg_transcripts_part_3_of_5.csv` (200 MB)
- `bloomberg_transcripts_part_4_of_5.csv` (198 MB)
- `bloomberg_transcripts_part_5_of_5.csv` (56 MB)

## 🔧 Setup and reproduction

### Prerequisites
- R (>= 4.0.0)
- Python (>= 3.8) - for FinBERT sentiment analysis
- Required R packages (will be installed automatically by scripts)

### API keys 

To replicate this work, you need to generate API keys and replace the current placeholders in the scripts. Here's how:

#### 1. YouTube Data API Key
1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Enable "YouTube Data API v3"
3. Create an API Key
4. **Replace in these files:**
   - `01_data_retrieval/scripts/02_date_correction.R`: 
     ```r
     YOUTUBE_API_KEY <- "YOUR_KEY_HERE"
     ```
   - `config/config.yaml`:
     ```yaml
     api_key: YOUR_KEY_HERE
     ```

#### 2. FRED API Key
1. Get free key at [FRED API](https://fred.stlouisfed.org/docs/api/api_key.html)
2. **Replace in:**
   - `03_ml_prediction/scripts/01_database_raw_data.R`:
     ```r
     fred_api_key = "YOUR_KEY_HERE"
     ```

#### 3. Ollama setup (optional)
For LLM-based sentiment analysis:
1. Install Ollama: `curl -fsSL https://ollama.com/install.sh | sh`
2. Start server: `ollama serve`
3. **Replace in:**
   - `02_sentiment_analysis/scripts/06_01_basic_sentiment_server_solution_Ollama.R`
   - Update `ollama_url` variable

### Files to update checklist
- [ ] `01_data_retrieval/scripts/02_date_correction.R`
- [ ] `03_ml_prediction/scripts/01_database_raw_data.R`
- [ ] `02_sentiment_analysis/scripts/06_01_basic_sentiment_server_solution_Ollama.R`
- [ ] `config/config.yaml`

## 🚀 Running the analysis

### 1. Data collection
```r
# Collect Bloomberg TV video metadata
source("01_data_retrieval/scripts/01_video_collection.R")

# Correct publication dates
source("01_data_retrieval/scripts/02_date_correction.R")

# Download transcripts
source("01_data_retrieval/scripts/03_transcript_collection.R")
```

### 2. Sentiment analysis
```r
# Basic sentiment analysis
source("02_sentiment_analysis/scripts/01_basic_sentiment.R")

# Advanced methods (FinBERT, Ollama)
source("02_sentiment_analysis/scripts/06_01_basic_sentiment_server_solution_Ollama.R")
source("02_sentiment_analysis/scripts/06_02_basic_sentiment_server_solution_FinBERT.R")

# Merge all methods
source("02_sentiment_analysis/scripts/09_final_merge_all_methods.R")
```

### 3. Machine learning pipeline
```r
# Feature engineering
source("02_sentiment_analysis/scripts/12_ml_complete_prep.R")

# Market data collection
source("03_ml_prediction/scripts/01_database_raw_data.R")

# Run predictions
source("SP500_Prediction_time_series_cross_validation.R")
source("VIX_Prediction_time_series_cross_validation.R")
source("UST_Prediction_time_series_cross_validation.R")
```

## 📈 Key results

- **4 sentiment analysis methods** compared: Dictionary-based, SentimentR, FinBERT, and Ollama
- **Time-series cross-validation** on 3 financial indicators
- **Statistical significance testing** using simple t-tests and more sophisticated Diebold-Mariano tests

## 📚 Methods

### Sentiment Analysis
- **Loughran-McDonald Dictionary**: finance-specific dictionary
- **SentimentR**: valence-aware dictionary-based approach
- **FinBERT**: financial domain fine-tuned BERT model
- **Ollama LLM**: large language model sentiment classification

### Machine Learning
- **Random forest** and **XGBoost** models
- **Time-series cross-validation** with expanding window
- **Feature engineering** including lags and rolling statistics
- **Multiple target variables**: daily returns of S&P 500, daily changes of 10-Year US Treasury and VIX


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


---
*This repository contains the complete replication materials for the master's thesis "Sentiment Analysis of Financial TV Shows and Predictive Modelling" submitted to UC3M in 2025.*
