library(dplyr)
library(tidyr)
library(ggplot2)

lexical <- read.csv("data/lexical_counts.csv", check.names = FALSE)
plot_dir <- "plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

if (!"model" %in% names(lexical)) {
  lexical$model <- "All CVs"
}

plot_data <- lexical %>%
  select(any_of(c("model", "gender", "ethnicity")), agentic_count, communal_count) %>%
  pivot_longer(
    cols = c(agentic_count, communal_count),
    names_to = "lexical_category",
    values_to = "count"
  )

overall_plot <- ggplot(plot_data, aes(x = lexical_category, y = count, fill = model)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
  coord_flip() +
  labs(x = NULL, y = "Word count", fill = "Model") +
  theme_bw()

ggsave(file.path(plot_dir, "lexical_counts_overall.png"), overall_plot, width = 8, height = 5, dpi = 300)

if ("gender" %in% names(plot_data)) {
  gender_plot <- ggplot(plot_data, aes(x = gender, y = count, fill = lexical_category)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
    facet_wrap(~ model) +
    labs(x = NULL, y = "Word count", fill = "Lexical category") +
    theme_bw()

  ggsave(file.path(plot_dir, "lexical_counts_by_gender.png"), gender_plot, width = 10, height = 5, dpi = 300)
}

if ("ethnicity" %in% names(plot_data)) {
  ethnicity_plot <- ggplot(plot_data, aes(x = ethnicity, y = count, fill = lexical_category)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
    facet_wrap(~ model) +
    labs(x = NULL, y = "Word count", fill = "Lexical category") +
    theme_bw()

  ggsave(file.path(plot_dir, "lexical_counts_by_ethnicity.png"), ethnicity_plot, width = 10, height = 5, dpi = 300)
}
