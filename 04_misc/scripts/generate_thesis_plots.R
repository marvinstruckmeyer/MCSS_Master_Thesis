# Load required libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(gridExtra)

source("academic_theme.R")

# Load data
input_file <- "02_sentiment_analysis/data/combined_sentiment_results_with_finbert.csv"
sentiment_data <- read_csv(input_file, show_col_types = FALSE)

# Filter to complete cases
complete_data <- sentiment_data %>%
  filter(!is.na(dict_sentiment) & !is.na(sentimentr_score) & 
           !is.na(ollama_score) & !is.na(finbert_score))

# Create output directory for thesis plots
thesis_plots_dir <- "thesis_plots"
dir.create(thesis_plots_dir, showWarnings = FALSE, recursive = TRUE)

# ===== PLOT 1: COMPREHENSIVE PAIRWISE ANALYSIS =====
cat("Creating Plot 1: Comprehensive Pairwise Analysis...\n")

# Prepare data
pair_plot_data <- complete_data %>%
  select(dict_sentiment, sentimentr_score, ollama_score, finbert_score) %>%
  rename(
    "Loughran-McDonald" = dict_sentiment,
    "sentimentr" = sentimentr_score,
    "Ollama" = ollama_score,
    "FinBERT" = finbert_score
  )

# Create pairwise plot with academic styling
pairwise_plot <- ggpairs(
  pair_plot_data,
  title = NULL,
  upper = list(continuous = wrap("cor", size = 3.5, color = "black")),
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.5, color = "gray30")),
  diag = list(continuous = wrap("densityDiag", alpha = 0.7, fill = "gray70"))
) +
  theme_academic

# Save as high-resolution PNG (good for Word)
ggsave(file.path(thesis_plots_dir, "figure1_pairwise_analysis.png"), 
       pairwise_plot, width = 10, height = 8, dpi = 300, bg = "white")

# Also save as PDF (vector format, best quality)
ggsave(file.path(thesis_plots_dir, "figure1_pairwise_analysis.pdf"), 
       pairwise_plot, width = 10, height = 8, bg = "white")

cat("✓ Plot 1 saved\n")

# ===== PLOT 2: THRESHOLD ANALYSIS =====
cat("Creating Plot 2: Threshold Analysis...\n")

# Recreate threshold analysis (simplified version from your script)
calculate_agreement <- function(data, thresholds) {
  results <- data.frame()
  
  for (thresh in thresholds) {
    dict_bin <- data$dict_sentiment > thresh
    sentimentr_bin <- data$sentimentr_score > thresh  
    ollama_bin <- data$ollama_score > thresh
    finbert_bin <- data$finbert_score > thresh
    
    all_four <- mean((dict_bin == sentimentr_bin) & 
                       (sentimentr_bin == ollama_bin) & 
                       (ollama_bin == finbert_bin)) * 100
    
    dict_sentimentr <- mean(dict_bin == sentimentr_bin) * 100
    dict_finbert <- mean(dict_bin == finbert_bin) * 100
    sentimentr_finbert <- mean(sentimentr_bin == finbert_bin) * 100
    
    results <- rbind(results, data.frame(
      threshold = thresh,
      all_four = all_four,
      dict_sentimentr = dict_sentimentr,
      dict_finbert = dict_finbert,
      sentimentr_finbert = sentimentr_finbert
    ))
  }
  return(results)
}

# Calculate thresholds
thresholds <- seq(-0.5, 0.5, by = 0.1)
threshold_results <- calculate_agreement(complete_data, thresholds)

# Create threshold plot
threshold_plot_data <- threshold_results %>%
  pivot_longer(cols = -threshold, names_to = "comparison", values_to = "agreement_rate") %>%
  mutate(
    comparison = case_when(
      comparison == "all_four" ~ "All four Methods",
      comparison == "dict_sentimentr" ~ "Loughran-McDonald vs sentimentr",
      comparison == "dict_finbert" ~ "Loughran-McDonald vs FinBERT",
      comparison == "sentimentr_finbert" ~ "sentimentr vs FinBERT",
      TRUE ~ comparison
    ),
    line_type = ifelse(comparison == "All Four Methods", "All Four", "Pairwise")
  )

threshold_plot <- ggplot(threshold_plot_data, 
                         aes(x = threshold, y = agreement_rate, 
                             color = comparison, linetype = line_type)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = c(
    "All four Methods" = "black",
    "Loughran-McDonald vs sentimentr" = "#2ca02c",
    "Loughran-McDonald vs FinBERT" = "#d62728", 
    "sentimentr vs FinBERT" = "#1f77b4"
  )) +
  scale_linetype_manual(values = c("All four" = "solid", "Pairwise" = "dashed")) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Classification Threshold",
    y = "Agreement Rate (%)",
    color = "Method Comparison",
    linetype = "Type"
  ) +
  theme_academic +
  guides(color = guide_legend(ncol = 2), linetype = guide_legend(ncol = 2))

# Save threshold plot
ggsave(file.path(thesis_plots_dir, "figure2_threshold_analysis.png"), 
       threshold_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(thesis_plots_dir, "figure2_threshold_analysis.pdf"), 
       threshold_plot, width = 10, height = 6, bg = "white")

cat("✓ Plot 2 saved\n")

# ===== PLOT 3: FINBERT DISAGREEMENT =====
cat("Creating Plot 3: FinBERT Disagreement Analysis...\n")

# Calculate FinBERT disagreements
finbert_disagreement <- complete_data %>%
  mutate(
    finbert_vs_dict_diff = abs(finbert_score - dict_sentiment),
    finbert_vs_sentimentr_diff = abs(finbert_score - sentimentr_score),
    finbert_vs_ollama_diff = abs(finbert_score - ollama_score)
  )

# Create disagreement plot
plot_data <- finbert_disagreement %>%
  select(finbert_vs_dict_diff, finbert_vs_sentimentr_diff, finbert_vs_ollama_diff) %>%
  pivot_longer(everything(), names_to = "comparison", values_to = "difference") %>%
  mutate(
    comparison = case_when(
      comparison == "finbert_vs_dict_diff" ~ "FinBERT vs Dictionary",
      comparison == "finbert_vs_sentimentr_diff" ~ "FinBERT vs SentimentR",
      comparison == "finbert_vs_ollama_diff" ~ "FinBERT vs Ollama",
      TRUE ~ comparison
    )
  )

finbert_plot <- ggplot(plot_data, aes(x = comparison, y = difference)) +
  geom_boxplot(alpha = 0.7, fill = "gray80", color = "black", 
               outlier.alpha = 0.5, outlier.size = 1) +
  labs(
    title = "FinBERT Disagreement Magnitude with Other Methods",
    subtitle = "Absolute differences in sentiment scores",
    x = "Method Comparison",
    y = "Absolute Difference"
  ) +
  theme_academic +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Save FinBERT plot
ggsave(file.path(thesis_plots_dir, "figure3_finbert_disagreement.png"), 
       finbert_plot, width = 8, height = 6, dpi = 300, bg = "white")
ggsave(file.path(thesis_plots_dir, "figure3_finbert_disagreement.pdf"), 
       finbert_plot, width = 8, height = 6, bg = "white")

cat("✓ Plot 3 saved\n")

# ===== PLOT 4: AGREEMENT PATTERNS =====
cat("Creating Plot 4: Agreement Patterns...\n")

# Calculate agreement patterns
agreement_analysis <- complete_data %>%
  mutate(
    dict_binary = dict_sentiment > median(dict_sentiment),
    sentimentr_binary = sentimentr_score > median(sentimentr_score),
    ollama_binary = ollama_score > median(ollama_score),
    finbert_binary = finbert_score > median(finbert_score),
    positive_votes = dict_binary + sentimentr_binary + ollama_binary + finbert_binary
  )

vote_distribution <- agreement_analysis %>%
  count(positive_votes) %>%
  mutate(
    percentage = n / sum(n) * 100,
    vote_label = case_when(
      positive_votes == 0 ~ "All Negative",
      positive_votes == 1 ~ "3 Negative, 1 Positive", 
      positive_votes == 2 ~ "Split Decision (2-2)",
      positive_votes == 3 ~ "3 Positive, 1 Negative",
      positive_votes == 4 ~ "All Positive"
    )
  )

agreement_plot <- ggplot(vote_distribution, aes(x = factor(positive_votes), y = n)) +
  geom_bar(stat = "identity", alpha = 0.8, fill = "gray70", color = "black") +
  geom_text(aes(label = paste0(n, "\n(", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 3) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Number of methods voting 'Positive'",
    y = "Number of videos"
  ) +
  theme_academic +
  ylim(0, max(vote_distribution$n) * 1.12) 

# Save agreement plot
ggsave(file.path(thesis_plots_dir, "figure4_agreement_patterns.png"), 
       agreement_plot, width = 8, height = 6, dpi = 300, bg = "white")
ggsave(file.path(thesis_plots_dir, "figure4_agreement_patterns.pdf"), 
       agreement_plot, width = 8, height = 6, bg = "white")


# ===== SUMMARY =====
cat("\n" + rep("=", 50) + "\n")
cat("ALL THESIS PLOTS GENERATED SUCCESSFULLY!\n")
cat(rep("=", 50) + "\n")
cat("Files saved in:", thesis_plots_dir, "\n")

cat("Files created:\n")
cat("1. figure1_pairwise_analysis.png/.pdf\n")
cat("2. figure2_threshold_analysis.png/.pdf\n") 
cat("3. figure3_finbert_disagreement.png/.pdf\n")
cat("4. figure4_agreement_patterns.png/.pdf\n\n")


