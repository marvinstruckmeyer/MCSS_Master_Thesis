# ===== STANDARDIZED ACADEMIC THEME FOR THESIS PLOTS =====
# Load required libraries
library(ggplot2)
library(RColorBrewer)

# Define academic theme - SIMPLIFIED VERSION WITHOUT MARGIN ISSUES
theme_academic <- theme_minimal() +
  theme(
    # Plot titles and labels
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    
    # Axis titles and text
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    
    # Legend
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    
    # Panel and grid
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    
    # Strip text for facets
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    strip.background = element_blank(),
    
    # Overall plot appearance
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Academic color palettes (grayscale-friendly and colorblind-friendly)
academic_colors <- list(
  # Grayscale palette for main elements
  grayscale = c("gray20", "gray40", "gray60", "gray80"),
  
  # Conservative color palette (colorblind-friendly)
  categorical = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
  
  # For your four methods specifically
  methods = c(
    "Dictionary" = "#1f77b4",      # Blue
    "SentimentR" = "#2ca02c",      # Green  
    "Ollama" = "#ff7f0e",          # Orange
    "FinBERT" = "#d62728"          # Red
  ),
  
  # Agreement/disagreement colors
  agreement = c(
    "All Negative" = "#d62728",           # Red
    "3 Negative, 1 Positive" = "#ff7f0e", # Orange
    "Split Decision (2-2)" = "#ffdd44",    # Yellow
    "3 Positive, 1 Negative" = "#98df8a", # Light Green
    "All Positive" = "#2ca02c"            # Green
  )
)

# ===== UPDATED PLOT FUNCTIONS =====

# 1. Updated pairwise analysis plot (keep your existing one, just add theme)
create_academic_pairwise_plot <- function(data) {
  # Load required library for this function
  if (!require(GGally, quietly = TRUE)) {
    stop("GGally package is required for pairwise plots. Please install it with: install.packages('GGally')")
  }
  
  pair_plot_data <- data %>%
    select(dict_sentiment, sentimentr_score, ollama_score, finbert_score) %>%
    rename(
      "Dictionary" = dict_sentiment,
      "SentimentR" = sentimentr_score,
      "Ollama" = ollama_score,
      "FinBERT" = finbert_score
    )
  
  # Keep your existing pairwise plot but apply academic theme
  pair_plot <- ggpairs(
    pair_plot_data,
    title = "Comprehensive Four-Method Pairwise Analysis",
    upper = list(continuous = wrap("cor", size = 3.5, color = "black")),
    lower = list(continuous = wrap("points", alpha = 0.4, size = 0.5, color = "gray30")),
    diag = list(continuous = wrap("densityDiag", alpha = 0.7, fill = "gray70"))
  ) +
    theme_academic
  
  return(pair_plot)
}

# 2. Academic threshold analysis plot
create_academic_threshold_plot <- function(threshold_results) {
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
  
  ggplot(threshold_plot_data, aes(x = threshold, y = agreement_rate, 
                                  color = comparison, linetype = line_type)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.5, alpha = 0.8) +
    scale_color_manual(values = c(
      "All Four Methods" = "black",
      "Dictionary vs SentimentR" = academic_colors$methods["SentimentR"],
      "Dictionary vs Ollama" = academic_colors$methods["Ollama"],
      "Dictionary vs FinBERT" = academic_colors$methods["FinBERT"],
      "SentimentR vs Ollama" = "gray50",
      "SentimentR vs FinBERT" = "gray60",
      "Ollama vs FinBERT" = "gray70"
    )) +
    scale_linetype_manual(values = c("All Four" = "solid", "Pairwise" = "dashed")) +
    labs(
      title = "Four-Method Agreement Rates at Different Thresholds",
      subtitle = "How threshold choice affects method agreement",
      x = "Classification Threshold",
      y = "Agreement Rate (%)",
      color = "Method Comparison",
      linetype = "Type"
    ) +
    theme_academic +
    guides(
      color = guide_legend(ncol = 2),
      linetype = guide_legend(ncol = 2)
    )
}

# 3. Academic FinBERT disagreement plot
create_academic_finbert_plot <- function(finbert_disagreement) {
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
  
  ggplot(plot_data, aes(x = comparison, y = difference)) +
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
}

# 4. Academic agreement patterns plot
create_academic_agreement_plot <- function(agreement_analysis) {
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
  
  ggplot(vote_distribution, aes(x = factor(positive_votes), y = n)) +
    geom_bar(stat = "identity", alpha = 0.8, fill = "gray70", color = "black") +
    geom_text(aes(label = paste0(n, "\n(", round(percentage, 1), "%)")), 
              vjust = -0.5, size = 3) +
    labs(
      title = "Four-Method Agreement Patterns",
      subtitle = "How many methods classify each video as positive?",
      x = "Number of Methods Voting 'Positive'",
      y = "Number of Videos"
    ) +
    theme_academic
}

# existing_plot + theme_academic

# For new plots, use the academic color palette:
# scale_fill_manual(values = academic_colors$methods)
# scale_color_manual(values = academic_colors$categorical)
