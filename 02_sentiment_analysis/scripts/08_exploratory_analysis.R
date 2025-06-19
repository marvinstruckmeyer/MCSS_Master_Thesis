# Comprehensive analysis of Dictionary, SentimentR, Ollama, and FinBERT methods

library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(viridis)
library(gridExtra)
library(scales)

# Setup
input_file <- "02_sentiment_analysis/data/combined_sentiment_results_with_finbert.csv"
output_dir <- "02_sentiment_analysis/four_method_analysis"

# Create output directory
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Custom theme
theme_sentiment <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Color palette for four methods
method_colors <- c(
  "Dictionary" = "steelblue",
  "SentimentR" = "forestgreen", 
  "Ollama" = "darkorange",
  "FinBERT" = "purple"
)

# Read data
cat("Loading 4-method sentiment data...\n")
sentiment_data <- read_csv(input_file, show_col_types = FALSE)

# Filter to complete cases for main analysis
complete_data <- sentiment_data %>%
  filter(!is.na(dict_sentiment) & !is.na(sentimentr_score) & 
           !is.na(ollama_score) & !is.na(finbert_score))

cat("Total videos:", nrow(sentiment_data), "\n")
cat("Videos with all four sentiment scores:", nrow(complete_data), "\n")
cat("Completion rate:", round(nrow(complete_data)/nrow(sentiment_data)*100, 1), "%\n")

# ===== 1. INDIVIDUAL METHOD DISTRIBUTIONS =====

cat("\n=== CREATING INDIVIDUAL METHOD VISUALIZATIONS ===\n")

# Individual density plots
create_density_plot <- function(data, column, title, color, mean_val) {
  ggplot(data, aes(x = !!sym(column))) +
    geom_density(fill = color, alpha = 0.7, color = "white", size = 1) +
    geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed", size = 1) +
    labs(
      title = title,
      subtitle = paste("Mean:", round(mean_val, 3)),
      x = "Sentiment Score",
      y = "Density"
    ) +
    theme_sentiment
}

# Create individual density plots
d1 <- create_density_plot(complete_data, "dict_sentiment", "Dictionary-Based Sentiment", 
                          method_colors["Dictionary"], mean(complete_data$dict_sentiment))
d2 <- create_density_plot(complete_data, "sentimentr_score", "SentimentR Score", 
                          method_colors["SentimentR"], mean(complete_data$sentimentr_score))
d3 <- create_density_plot(complete_data, "ollama_score", "Ollama API Sentiment", 
                          method_colors["Ollama"], mean(complete_data$ollama_score))
d4 <- create_density_plot(complete_data, "finbert_score", "FinBERT Sentiment", 
                          method_colors["FinBERT"], mean(complete_data$finbert_score))

# Combine density plots
density_grid <- grid.arrange(d1, d2, d3, d4, ncol = 2)
ggsave(file.path(output_dir, "four_method_density_plots.png"), 
       density_grid, width = 12, height = 10, dpi = 300)

# Box plots comparison
sentiment_long <- complete_data %>%
  select(video_id, dict_sentiment, sentimentr_score, ollama_score, finbert_score) %>%
  pivot_longer(cols = -video_id, names_to = "method", values_to = "score") %>%
  mutate(method = case_when(
    method == "dict_sentiment" ~ "Dictionary",
    method == "sentimentr_score" ~ "SentimentR",
    method == "ollama_score" ~ "Ollama",
    method == "finbert_score" ~ "FinBERT",
    TRUE ~ method
  )) %>%
  mutate(method = factor(method, levels = c("Dictionary", "SentimentR", "Ollama", "FinBERT")))

box_plot <- ggplot(sentiment_long, aes(x = method, y = score, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_violin(alpha = 0.3) +
  scale_fill_manual(values = method_colors) +
  labs(
    title = "Four-Method Sentiment Score Distributions",
    subtitle = "Box plots with violin plots showing distribution shapes",
    x = "Sentiment Analysis Method",
    y = "Sentiment Score",
    fill = "Method"
  ) +
  theme_sentiment +
  theme(legend.position = "none")

ggsave(file.path(output_dir, "four_method_boxplots.png"), 
       box_plot, width = 12, height = 8, dpi = 300)

# ===== 2. CORRELATION ANALYSIS =====

cat("\n=== ANALYZING CORRELATIONS BETWEEN FOUR METHODS ===\n")

# Calculate correlation matrix
correlation_data <- complete_data %>%
  select(dict_sentiment, sentimentr_score, ollama_score, finbert_score)

cor_matrix <- cor(correlation_data, use = "complete.obs")
cat("Four-method correlation matrix:\n")
print(round(cor_matrix, 3))

# Enhanced correlation plot
png(file.path(output_dir, "four_method_correlation_matrix.png"), 
    width = 10, height = 8, units = "in", res = 300)
corrplot(cor_matrix, 
         method = "circle", 
         type = "upper",
         order = "hclust",
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         title = "Four-Method Sentiment Correlation Matrix",
         mar = c(0,0,2,0),
         col = colorRampPalette(c("red", "white", "blue"))(100))
dev.off()

# ===== 3. COMPREHENSIVE PAIRWISE ANALYSIS =====

cat("\n=== CREATING COMPREHENSIVE PAIRWISE ANALYSIS ===\n")

# Rename for cleaner pair plot
pair_plot_data <- complete_data %>%
  select(dict_sentiment, sentimentr_score, ollama_score, finbert_score) %>%
  rename(
    "Dictionary" = dict_sentiment,
    "SentimentR" = sentimentr_score,
    "Ollama" = ollama_score,
    "FinBERT" = finbert_score
  )

# Create comprehensive pair plot
if (require(GGally, quietly = TRUE)) {
  pair_plot <- ggpairs(
    pair_plot_data,
    title = "Comprehensive Four-Method Pairwise Analysis",
    upper = list(continuous = wrap("cor", size = 4, color = "black")),
    lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
    diag = list(continuous = wrap("densityDiag", alpha = 0.7))
  ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "four_method_pairwise_analysis.png"), 
         pair_plot, width = 14, height = 12, dpi = 300)
}

# Individual scatter plots for key relationships
create_scatter_plot <- function(data, x_col, y_col, x_name, y_name, color) {
  ggplot(data, aes(x = !!sym(x_col), y = !!sym(y_col))) +
    geom_point(alpha = 0.6, color = color, size = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "red", size = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed", size = 1) +
    labs(
      title = paste(x_name, "vs", y_name),
      subtitle = paste("Correlation:", round(cor(data[[x_col]], data[[y_col]]), 3)),
      x = paste(x_name, "Score"),
      y = paste(y_name, "Score")
    ) +
    theme_sentiment
}

# Create key scatter plots
s1 <- create_scatter_plot(complete_data, "dict_sentiment", "finbert_score", 
                          "Dictionary", "FinBERT", method_colors["FinBERT"])
s2 <- create_scatter_plot(complete_data, "dict_sentiment", "sentimentr_score", 
                          "Dictionary", "SentimentR", method_colors["SentimentR"])
s3 <- create_scatter_plot(complete_data, "sentimentr_score", "finbert_score", 
                          "SentimentR", "FinBERT", method_colors["FinBERT"])
s4 <- create_scatter_plot(complete_data, "ollama_score", "finbert_score", 
                          "Ollama", "FinBERT", method_colors["FinBERT"])

scatter_grid <- grid.arrange(s1, s2, s3, s4, ncol = 2)
ggsave(file.path(output_dir, "key_method_scatter_plots.png"), 
       scatter_grid, width = 12, height = 10, dpi = 300)

# ===== 4. AGREEMENT ANALYSIS =====

cat("\n=== ANALYZING FOUR-METHOD AGREEMENT PATTERNS ===\n")

# Create comprehensive agreement analysis
agreement_analysis <- complete_data %>%
  mutate(
    # Binary classifications using median as threshold
    dict_binary = dict_sentiment > median(dict_sentiment),
    sentimentr_binary = sentimentr_score > median(sentimentr_score),
    ollama_binary = ollama_score > median(ollama_score),
    finbert_binary = finbert_score > median(finbert_score),
    
    # Count how many methods agree on positive
    positive_votes = dict_binary + sentimentr_binary + ollama_binary + finbert_binary,
    
    # Agreement patterns
    all_four_agree = (dict_binary == sentimentr_binary) & 
      (sentimentr_binary == ollama_binary) & 
      (ollama_binary == finbert_binary),
    
    # Three-method agreements
    dict_sentimentr_ollama = (dict_binary == sentimentr_binary) & 
      (sentimentr_binary == ollama_binary),
    dict_sentimentr_finbert = (dict_binary == sentimentr_binary) & 
      (sentimentr_binary == finbert_binary),
    dict_ollama_finbert = (dict_binary == ollama_binary) & 
      (ollama_binary == finbert_binary),
    sentimentr_ollama_finbert = (sentimentr_binary == ollama_binary) & 
      (ollama_binary == finbert_binary),
    
    # Disagreement strength (4D distance)
    disagreement_strength = sqrt(
      (dict_sentiment - sentimentr_score)^2 + 
        (dict_sentiment - ollama_score)^2 + 
        (dict_sentiment - finbert_score)^2 + 
        (sentimentr_score - ollama_score)^2 + 
        (sentimentr_score - finbert_score)^2 + 
        (ollama_score - finbert_score)^2
    )
  )

# Agreement statistics
agreement_stats <- agreement_analysis %>%
  summarise(
    total_videos = n(),
    all_four_agree = sum(all_four_agree),
    all_four_agree_pct = mean(all_four_agree) * 100,
    three_or_more_agree = sum(positive_votes %in% c(0, 3, 4)),
    three_or_more_pct = mean(positive_votes %in% c(0, 3, 4)) * 100,
    split_decision = sum(positive_votes == 2),
    split_decision_pct = mean(positive_votes == 2) * 100,
    avg_disagreement = mean(disagreement_strength)
  )

cat("Four-method agreement statistics:\n")
print(agreement_stats)

# Visualize agreement patterns
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

vote_plot <- ggplot(vote_distribution, aes(x = factor(positive_votes), y = n, fill = factor(positive_votes))) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_viridis_d(name = "Positive Votes", labels = vote_distribution$vote_label) +
  labs(
    title = "Four-Method Agreement Patterns",
    subtitle = "How many methods classify each video as positive?",
    x = "Number of Methods Voting 'Positive'",
    y = "Number of Videos"
  ) +
  theme_sentiment

ggsave(file.path(output_dir, "four_method_agreement_patterns.png"), 
       vote_plot, width = 10, height = 6, dpi = 300)

# ===== 5. SHOW-SPECIFIC ANALYSIS =====

cat("\n=== ANALYZING SENTIMENT PATTERNS BY SHOW ===\n")

# Show-level analysis with four methods
show_analysis <- agreement_analysis %>%
  group_by(playlist_name) %>%
  filter(n() >= 10) %>%  # Shows with at least 10 videos
  summarise(
    video_count = n(),
    
    # Mean sentiment scores
    dict_mean = mean(dict_sentiment),
    sentimentr_mean = mean(sentimentr_score),
    ollama_mean = mean(ollama_score),
    finbert_mean = mean(finbert_score),
    
    # Standard deviations
    dict_sd = sd(dict_sentiment),
    sentimentr_sd = sd(sentimentr_score),
    ollama_sd = sd(ollama_score),
    finbert_sd = sd(finbert_score),
    
    # Agreement rates
    four_method_agreement = mean(all_four_agree) * 100,
    avg_disagreement = mean(disagreement_strength),
    
    # Positive rates
    dict_positive_rate = mean(dict_binary) * 100,
    sentimentr_positive_rate = mean(sentimentr_binary) * 100,
    ollama_positive_rate = mean(ollama_binary) * 100,
    finbert_positive_rate = mean(finbert_binary) * 100,
    
    .groups = "drop"
  ) %>%
  arrange(desc(video_count))

cat("Show-level four-method analysis:\n")
print(show_analysis)

# Save show analysis
write_csv(show_analysis, file.path(output_dir, "four_method_show_analysis.csv"))

# Visualize show differences
show_sentiment_plot <- show_analysis %>%
  select(playlist_name, dict_mean, sentimentr_mean, ollama_mean, finbert_mean, video_count) %>%
  pivot_longer(cols = c(dict_mean, sentimentr_mean, ollama_mean, finbert_mean), 
               names_to = "method", values_to = "mean_sentiment") %>%
  mutate(
    method = case_when(
      method == "dict_mean" ~ "Dictionary",
      method == "sentimentr_mean" ~ "SentimentR", 
      method == "ollama_mean" ~ "Ollama",
      method == "finbert_mean" ~ "FinBERT",
      TRUE ~ method
    ),
    method = factor(method, levels = c("Dictionary", "SentimentR", "Ollama", "FinBERT")),
    playlist_short = str_trunc(playlist_name, 25)
  ) %>%
  ggplot(aes(x = reorder(playlist_short, video_count), y = mean_sentiment, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = method_colors) +
  labs(
    title = "Average Sentiment by Bloomberg Show - Four Methods",
    subtitle = "Shows ordered by number of videos (minimum 10 videos)",
    x = "Show",
    y = "Average Sentiment Score",
    fill = "Method"
  ) +
  theme_sentiment

ggsave(file.path(output_dir, "four_method_sentiment_by_show.png"), 
       show_sentiment_plot, width = 14, height = 8, dpi = 300)

# ===== 6. THRESHOLD ANALYSIS =====

cat("\n=== PERFORMING FOUR-METHOD THRESHOLD ANALYSIS ===\n")

# Function to calculate four-method agreement at different thresholds
calculate_four_method_agreement <- function(data, thresholds) {
  results <- data.frame()
  
  for (thresh in thresholds) {
    dict_bin <- data$dict_sentiment > thresh
    sentimentr_bin <- data$sentimentr_score > thresh  
    ollama_bin <- data$ollama_score > thresh
    finbert_bin <- data$finbert_score > thresh
    
    # Calculate agreements
    all_four <- mean((dict_bin == sentimentr_bin) & 
                       (sentimentr_bin == ollama_bin) & 
                       (ollama_bin == finbert_bin)) * 100
    
    # Pairwise agreements
    dict_sentimentr <- mean(dict_bin == sentimentr_bin) * 100
    dict_ollama <- mean(dict_bin == ollama_bin) * 100
    dict_finbert <- mean(dict_bin == finbert_bin) * 100
    sentimentr_ollama <- mean(sentimentr_bin == ollama_bin) * 100
    sentimentr_finbert <- mean(sentimentr_bin == finbert_bin) * 100
    ollama_finbert <- mean(ollama_bin == finbert_bin) * 100
    
    results <- rbind(results, data.frame(
      threshold = thresh,
      all_four = all_four,
      dict_sentimentr = dict_sentimentr,
      dict_ollama = dict_ollama,
      dict_finbert = dict_finbert,
      sentimentr_ollama = sentimentr_ollama,
      sentimentr_finbert = sentimentr_finbert,
      ollama_finbert = ollama_finbert
    ))
  }
  
  return(results)
}

# Test different thresholds
thresholds <- seq(-0.5, 0.5, by = 0.1)
threshold_results <- calculate_four_method_agreement(agreement_analysis, thresholds)

# Plot threshold analysis
threshold_plot_data <- threshold_results %>%
  pivot_longer(cols = -threshold, names_to = "comparison", values_to = "agreement_rate") %>%
  mutate(
    comparison = case_when(
      comparison == "all_four" ~ "All Four Methods",
      comparison == "dict_sentimentr" ~ "Dictionary vs SentimentR",
      comparison == "dict_ollama" ~ "Dictionary vs Ollama",
      comparison == "dict_finbert" ~ "Dictionary vs FinBERT",
      comparison == "sentimentr_ollama" ~ "SentimentR vs Ollama",
      comparison == "sentimentr_finbert" ~ "SentimentR vs FinBERT",
      comparison == "ollama_finbert" ~ "Ollama vs FinBERT",
      TRUE ~ comparison
    ),
    line_type = ifelse(comparison == "All Four Methods", "All Four", "Pairwise")
  )

threshold_plot <- ggplot(threshold_plot_data, aes(x = threshold, y = agreement_rate, 
                                                  color = comparison, linetype = line_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_viridis_d() +
  scale_linetype_manual(values = c("All Four" = "solid", "Pairwise" = "dashed")) +
  labs(
    title = "Four-Method Agreement Rates at Different Thresholds",
    subtitle = "How threshold choice affects method agreement",
    x = "Classification Threshold",
    y = "Agreement Rate (%)",
    color = "Method Comparison",
    linetype = "Type"
  ) +
  theme_sentiment

ggsave(file.path(output_dir, "four_method_threshold_analysis.png"), 
       threshold_plot, width = 12, height = 8, dpi = 300)

# ===== 7. FINBERT SPECIAL ANALYSIS =====

cat("\n=== SPECIAL ANALYSIS: FINBERT VS OTHERS ===\n")

# Analyze where FinBERT disagrees most with others
finbert_disagreement <- agreement_analysis %>%
  mutate(
    finbert_vs_dict_diff = abs(finbert_score - dict_sentiment),
    finbert_vs_sentimentr_diff = abs(finbert_score - sentimentr_score),
    finbert_vs_ollama_diff = abs(finbert_score - ollama_score),
    finbert_vs_others_avg = (finbert_vs_dict_diff + finbert_vs_sentimentr_diff + finbert_vs_ollama_diff) / 3,
    
    # FinBERT extreme disagreement
    finbert_extreme_disagreement = finbert_vs_others_avg > quantile(finbert_vs_others_avg, 0.95)
  )

# Where does FinBERT disagree most?
finbert_disagreement_summary <- finbert_disagreement %>%
  summarise(
    avg_dict_diff = mean(finbert_vs_dict_diff),
    avg_sentimentr_diff = mean(finbert_vs_sentimentr_diff),
    avg_ollama_diff = mean(finbert_vs_ollama_diff),
    extreme_disagreement_count = sum(finbert_extreme_disagreement),
    extreme_disagreement_pct = mean(finbert_extreme_disagreement) * 100
  )

cat("FinBERT disagreement analysis:\n")
print(finbert_disagreement_summary)

# Plot FinBERT disagreement patterns
finbert_disagreement_plot <- finbert_disagreement %>%
  select(finbert_vs_dict_diff, finbert_vs_sentimentr_diff, finbert_vs_ollama_diff) %>%
  pivot_longer(everything(), names_to = "comparison", values_to = "difference") %>%
  mutate(
    comparison = case_when(
      comparison == "finbert_vs_dict_diff" ~ "FinBERT vs Dictionary",
      comparison == "finbert_vs_sentimentr_diff" ~ "FinBERT vs SentimentR",
      comparison == "finbert_vs_ollama_diff" ~ "FinBERT vs Ollama",
      TRUE ~ comparison
    )
  ) %>%
  ggplot(aes(x = comparison, y = difference, fill = comparison)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("FinBERT vs Dictionary" = method_colors["Dictionary"],
                               "FinBERT vs SentimentR" = method_colors["SentimentR"],
                               "FinBERT vs Ollama" = method_colors["Ollama"])) +
  labs(
    title = "FinBERT Disagreement Magnitude with Other Methods",
    subtitle = "Absolute differences in sentiment scores",
    x = "Method Comparison",
    y = "Absolute Difference",
    fill = "Comparison"
  ) +
  theme_sentiment +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "finbert_disagreement_analysis.png"), 
       finbert_disagreement_plot, width = 10, height = 6, dpi = 300)

# ===== 8. MAJOR DISAGREEMENTS ANALYSIS =====

cat("\n=== IDENTIFYING MAJOR FOUR-METHOD DISAGREEMENTS ===\n")

# Find videos with highest disagreement across all four methods
major_disagreements <- agreement_analysis %>%
  filter(disagreement_strength > quantile(disagreement_strength, 0.95)) %>%
  select(video_id, title, playlist_name, 
         dict_sentiment, sentimentr_score, ollama_score, finbert_score,
         disagreement_strength) %>%
  arrange(desc(disagreement_strength))

cat("Top 10 videos with highest four-method disagreement:\n")
print(head(major_disagreements, 10))

# Save major disagreements
write_csv(major_disagreements, file.path(output_dir, "four_method_major_disagreements.csv"))

# ===== 9. COMPREHENSIVE STATISTICS SUMMARY =====

cat("\n=== GENERATING COMPREHENSIVE STATISTICS ===\n")

# Calculate comprehensive statistics for all four methods
comprehensive_stats <- complete_data %>%
  summarise(
    across(c(dict_sentiment, sentimentr_score, ollama_score, finbert_score),
           list(
             count = ~sum(!is.na(.)),
             mean = ~mean(., na.rm = TRUE),
             median = ~median(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE),
             min = ~min(., na.rm = TRUE),
             max = ~max(., na.rm = TRUE),
             q25 = ~quantile(., 0.25, na.rm = TRUE),
             q75 = ~quantile(., 0.75, na.rm = TRUE),
             skewness = ~e1071::skewness(., na.rm = TRUE),
             kurtosis = ~e1071::kurtosis(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("method", "statistic"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(method = case_when(
    method == "dict_sentiment" ~ "Dictionary",
    method == "sentimentr_score" ~ "SentimentR",
    method == "ollama_score" ~ "Ollama",
    method == "finbert_score" ~ "FinBERT",
    TRUE ~ method
  ))

# Save comprehensive statistics
write_csv(comprehensive_stats, file.path(output_dir, "four_method_comprehensive_stats.csv"))

# ===== 10. FINAL SUMMARY REPORT =====

cat("\n" + rep("=", 60))
cat("\nFOUR-METHOD SENTIMENT ANALYSIS COMPLETE!")
cat("\n" + rep("=", 60))

# Create summary report
summary_report <- list(
  total_videos = nrow(complete_data),
  
  # Agreement statistics
  all_four_agreement_rate = agreement_stats$all_four_agree_pct,
  split_decision_rate = agreement_stats$split_decision_pct,
  
  # Correlation insights
  strongest_positive_correlation = max(cor_matrix[cor_matrix < 1]),
  strongest_negative_correlation = min(cor_matrix),
  
  # Method characteristics
  most_positive_method = comprehensive_stats$method[which.max(comprehensive_stats$mean)],
  most_negative_method = comprehensive_stats$method[which.min(comprehensive_stats$mean)],
  most_volatile_method = comprehensive_stats$method[which.max(comprehensive_stats$sd)],
  
  # Disagreement insights
  major_disagreements_count = nrow(major_disagreements),
  avg_disagreement_strength = agreement_stats$avg_disagreement
)

# Save summary
saveRDS(summary_report, file.path(output_dir, "four_method_analysis_summary.rds"))

# Print key findings
cat("\n🎯 KEY FINDINGS:")
cat("\n📊 Dataset: ", summary_report$total_videos, " videos with all 4 methods")
cat("\n🤝 Agreement: All four methods agree ", round(summary_report$all_four_agreement_rate, 1), "% of the time")
cat("\n⚖️ Split Decisions: ", round(summary_report$split_decision_rate, 1), "% are 2-2 splits")
cat("\n📈 Most Positive: ", summary_report$most_positive_method)
cat("\n📉 Most Negative: ", summary_report$most_negative_method)
cat("\n🌊 Most Volatile: ", summary_report$most_volatile_method)
cat("\n⚠️ Major Disagreements: ", summary_report$major_disagreements_count, " cases")

cat("\n\n📁 FILES CREATED:")
cat("\n1. four_method_density_plots.png - Individual distributions")
cat("\n2. four_method_boxplots.png - Comparative distributions") 
cat("\n3. four_method_correlation_matrix.png - Correlation analysis")
cat("\n4. four_method_pairwise_analysis.png - Comprehensive pairs")
cat("\n5. key_method_scatter_plots.png - Key relationships")
cat("\n6. four_method_agreement_patterns.png - Agreement analysis")
cat("\n7. four_method_sentiment_by_show.png - Show-specific patterns")
cat("\n8. four_method_threshold_analysis.png - Threshold sensitivity")
cat("\n9. finbert_disagreement_analysis.png - FinBERT special analysis")
cat("\n10. four_method_show_analysis.csv - Show-level statistics")
cat("\n11. four_method_major_disagreements.csv - Extreme disagreements")
cat("\n12. four_method_comprehensive_stats.csv - Complete statistics")

cat("\n\n🎉 MEGA FOUR-METHOD ANALYSIS COMPLETE! 🎉\n")

# ===== 11. SPECIAL INSIGHTS SECTION =====

cat("\n=== GENERATING SPECIAL INSIGHTS ===\n")

# Correlation ranking
cor_pairs <- data.frame(
  pair = c("Dictionary-SentimentR", "Dictionary-Ollama", "Dictionary-FinBERT",
           "SentimentR-Ollama", "SentimentR-FinBERT", "Ollama-FinBERT"),
  correlation = c(cor_matrix[1,2], cor_matrix[1,3], cor_matrix[1,4],
                  cor_matrix[2,3], cor_matrix[2,4], cor_matrix[3,4])
) %>%
  arrange(desc(abs(correlation)))

cat("\nCorrelation rankings (by absolute value):\n")
print(cor_pairs)

# Method pair agreement rates at optimal threshold
optimal_threshold <- threshold_results$threshold[which.max(threshold_results$all_four)]
cat("\nOptimal threshold for four-method agreement:", optimal_threshold, "\n")

# Range analysis
range_analysis <- comprehensive_stats %>%
  mutate(range = max - min) %>%
  select(method, min, max, range) %>%
  arrange(desc(range))

cat("\nSentiment range analysis:\n")
print(range_analysis)

# Distribution types
distribution_types <- comprehensive_stats %>%
  mutate(
    distribution_type = case_when(
      abs(skewness) < 0.5 & abs(kurtosis) < 3 ~ "Normal-like",
      skewness > 0.5 ~ "Right-skewed", 
      skewness < -0.5 ~ "Left-skewed",
      abs(kurtosis) > 3 ~ "Heavy-tailed",
      TRUE ~ "Other"
    )
  ) %>%
  select(method, mean, skewness, kurtosis, distribution_type)

cat("\nDistribution characteristics:\n")
print(distribution_types)

# Create final insights visualization
insights_text <- paste0(
  "FOUR-METHOD SENTIMENT ANALYSIS INSIGHTS\n\n",
  "🏆 STRONGEST CORRELATION: ", cor_pairs$pair[1], " (", round(cor_pairs$correlation[1], 3), ")\n",
  "⚡ MOST SURPRISING: Dictionary-FinBERT negative correlation (", round(cor_matrix[1,4], 3), ")\n",
  "🎯 OPTIMAL THRESHOLD: ", optimal_threshold, " for maximum four-method agreement\n",
  "📊 AGREEMENT RATE: ", round(summary_report$all_four_agreement_rate, 1), "% all four methods agree\n",
  "🌊 WIDEST RANGE: ", range_analysis$method[1], " (", round(range_analysis$range[1], 3), " units)\n",
  "📈 MOST POSITIVE: ", summary_report$most_positive_method, "\n",
  "📉 MOST NEGATIVE: ", summary_report$most_negative_method, "\n",
  "⚠️ MAJOR DISAGREEMENTS: ", summary_report$major_disagreements_count, " videos (top 5%)\n\n",
  "KEY TAKEAWAYS:\n",
  "• FinBERT shows fundamentally different patterns from traditional methods\n",
  "• Dictionary and SentimentR have strongest agreement (linguistic similarity)\n", 
  "• Ollama's quantization creates unique discrete patterns\n",
  "• Financial context (FinBERT) vs general sentiment creates interesting tensions\n",
  "• Show-specific patterns suggest content type affects sentiment consistency"
)

# Save insights as text file
writeLines(insights_text, file.path(output_dir, "key_insights_summary.txt"))

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All visualizations, statistics, and insights saved to:", output_dir, "\n")
cat("\nThis comprehensive analysis reveals fascinating patterns in how different\n")
cat("sentiment methods interpret Bloomberg financial content!\n")

