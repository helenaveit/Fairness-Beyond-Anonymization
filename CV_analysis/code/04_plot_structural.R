library(dplyr)
library(tidyr)
library(ggplot2)

structural <- read.csv("data/structural_features.csv", check.names = FALSE)
plot_dir <- "plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

if (!"model" %in% names(structural)) {
  structural$model <- "All CVs"
}

section_vars <- grep("_num_words$", names(structural), value = TRUE)
section_vars <- setdiff(section_vars, "cv_total_words")

section_plot_data <- structural %>%
  select(model, all_of(section_vars)) %>%
  pivot_longer(cols = all_of(section_vars), names_to = "section", values_to = "words") %>%
  group_by(model, section) %>%
  summarize(mean_words = mean(words, na.rm = TRUE), .groups = "drop")

section_plot <- ggplot(section_plot_data, aes(x = section, y = mean_words, fill = model)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.85) +
  coord_flip() +
  labs(x = NULL, y = "Mean words", fill = "Model") +
  theme_bw()

ggsave(file.path(plot_dir, "structural_section_words.png"), section_plot, width = 10, height = 7, dpi = 300)

if (all(c("gender", "cv_total_words") %in% names(structural))) {
  gender_plot <- ggplot(structural, aes(x = gender, y = cv_total_words, fill = model)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
    facet_wrap(~ model) +
    labs(x = NULL, y = "Total words") +
    theme_bw() +
    theme(legend.position = "none")

  ggsave(file.path(plot_dir, "structural_total_words_by_gender.png"), gender_plot, width = 9, height = 5, dpi = 300)
}

if (all(c("ethnicity", "cv_total_words") %in% names(structural))) {
  ethnicity_plot <- ggplot(structural, aes(x = ethnicity, y = cv_total_words, fill = model)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
    facet_wrap(~ model) +
    labs(x = NULL, y = "Total words") +
    theme_bw() +
    theme(legend.position = "none")

  ggsave(file.path(plot_dir, "structural_total_words_by_ethnicity.png"), ethnicity_plot, width = 9, height = 5, dpi = 300)
}

