library(dplyr)
library(tidyr)
library(ggplot2)

find_cv_root <- function() {
  candidates <- c(
    "CV_analysis",
    ".",
    file.path("..", "..")
  )
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "data", "full_count_data", "count_dat_all_models.csv"))) {
      return(normalizePath(candidate))
    }
  }
  stop("Could not find CV_analysis root with data/full_count_data/count_dat_all_models.csv")
}

cv_root <- find_cv_root()
plot_dir <- file.path(cv_root, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

count_dat <- read.csv(
  file.path(cv_root, "data", "full_count_data", "count_dat_all_models.csv"),
  check.names = FALSE
) %>%
  mutate(
    Model = factor(
      model_label,
      levels = c("ChatGPT", "Gemini", "Qwen 4B", "Qwen 8B")
    ),
    gender = factor(gender, levels = c("female", "male")),
    ethnicity = factor(ethnicity, levels = c("german", "turkish"))
  )

struct_vars <- c(
  "cv_total_words",
  "k01_persoenliche_daten_num_words",
  "k02_profil_num_words",
  "k03_faehigkeiten_num_words",
  "k04_berufserfahrung_num_words",
  "k05_ausbildung_num_words",
  "k06_skills_num_words",
  "k07_sprachen_num_words",
  "k08_interessen_num_words",
  "k09_angestrebte_position_num_words",
  "k10_cover_letter_snippet_num_words"
)

var_labels <- c(
  "cv_total_words" = "Total",
  "k01_persoenliche_daten_num_words" = "1. Pers. Data",
  "k02_profil_num_words" = "2. Profile",
  "k03_faehigkeiten_num_words" = "3. Competences",
  "k04_berufserfahrung_num_words" = "4. Work Experience",
  "k05_ausbildung_num_words" = "5. Education",
  "k06_skills_num_words" = "6. Skills",
  "k07_sprachen_num_words" = "7. Languages",
  "k08_interessen_num_words" = "8. Interests",
  "k09_angestrebte_position_num_words" = "9. Position",
  "k10_cover_letter_snippet_num_words" = "10. Cover Letter"
)

model_colors <- c(
  "ChatGPT" = "#018571",
  "Gemini" = "#a6611a",
  "Qwen 4B" = "#5e3c99",
  "Qwen 8B" = "#e66101"
)

gender_colors <- c(
  "female" = "#cc6677",
  "male" = "#44aa99"
)

ethnicity_colors <- c(
  "german" = "#117733",
  "turkish" = "#ddcc77"
)

legend_theme <- theme(
  legend.position = "bottom",
  legend.box = "vertical",
  legend.text = element_text(size = 11),
  legend.title = element_text(size = 11),
  legend.margin = margin(t = 6, r = 0, b = 6, l = 0),
  plot.margin = margin(t = 12, r = 18, b = 18, l = 12)
)

make_struct_long <- function(data, group_cols = character()) {
  data %>%
    select(Model, all_of(group_cols), all_of(struct_vars)) %>%
    pivot_longer(
      cols = all_of(struct_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      variable = factor(
        variable,
        levels = names(var_labels),
        labels = unname(var_labels)
      )
    )
}

df_long <- make_struct_long(count_dat)

df_summary <- df_long %>%
  group_by(variable, Model) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

barplot_sections_model <- ggplot(df_summary, aes(x = variable, y = mean_value, fill = Model)) +
  geom_col(position = position_dodge(width = 0.85), alpha = 0.85) +
  geom_text(
    aes(label = round(mean_value, 1)),
    position = position_dodge(width = 0.85),
    hjust = -0.08,
    size = 3.2
  ) +
  scale_fill_manual(values = model_colors, drop = FALSE) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "Avg. number of words", fill = "Model") +
  guides(fill = guide_legend(title = "Model", nrow = 1)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13)
  ) +
  legend_theme

ggsave(
  filename = file.path(plot_dir, "barplot_sections_model.png"),
  plot = barplot_sections_model,
  width = 12,
  height = 7,
  dpi = 300
)

df_gender_summary <- make_struct_long(count_dat, group_cols = "gender") %>%
  group_by(variable, Model, gender) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

barplot_sections_gender_model <- ggplot(df_gender_summary, aes(x = variable, y = mean_value)) +
  geom_col(
    aes(fill = gender, color = Model),
    position = position_dodge(width = 0.85),
    alpha = 0.9,
    linewidth = 0.35
  ) +
  geom_text(
    aes(label = round(mean_value, 1), group = gender),
    position = position_dodge(width = 0.85),
    hjust = -0.08,
    size = 2.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = gender_colors,
    name = "Gender",
    labels = c("female" = "Female", "male" = "Male")
  ) +
  scale_color_manual(values = model_colors, name = "Model", drop = FALSE) +
  facet_wrap(~ Model, ncol = 2) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "Avg. number of words") +
  guides(
    color = guide_legend(order = 1, nrow = 1, override.aes = list(fill = NA, linewidth = 0.8)),
    fill = guide_legend(order = 2, nrow = 1)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12)
  ) +
  legend_theme

ggsave(
  filename = file.path(plot_dir, "barplot_sections_gender_model.png"),
  plot = barplot_sections_gender_model,
  width = 12,
  height = 9,
  dpi = 300
)

df_ethnicity_summary <- make_struct_long(count_dat, group_cols = "ethnicity") %>%
  group_by(variable, Model, ethnicity) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

barplot_sections_ethnicity_model <- ggplot(df_ethnicity_summary, aes(x = variable, y = mean_value)) +
  geom_col(
    aes(fill = ethnicity, color = Model),
    position = position_dodge(width = 0.85),
    alpha = 0.9,
    linewidth = 0.35
  ) +
  geom_text(
    aes(label = round(mean_value, 1), group = ethnicity),
    position = position_dodge(width = 0.85),
    hjust = -0.08,
    size = 2.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = ethnicity_colors,
    name = "Ethnicity",
    labels = c("german" = "German", "turkish" = "Turkish")
  ) +
  scale_color_manual(values = model_colors, name = "Model", drop = FALSE) +
  facet_wrap(~ Model, ncol = 2) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "Avg. number of words") +
  guides(
    color = guide_legend(order = 1, nrow = 1, override.aes = list(fill = NA, linewidth = 0.8)),
    fill = guide_legend(order = 2, nrow = 1)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12)
  ) +
  legend_theme

ggsave(
  filename = file.path(plot_dir, "barplot_sections_ethnicity_model.png"),
  plot = barplot_sections_ethnicity_model,
  width = 12,
  height = 9,
  dpi = 300
)

cat("Saved structural plots to:", plot_dir, "\n")
print(count_dat %>% count(Model))
