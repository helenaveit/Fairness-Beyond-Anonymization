library(dplyr)
library(tidyr)
library(ggplot2)

base_dir <- "CV_analysis"
out_dir <- file.path(base_dir, "data", "full_count_data")
plot_dir <- file.path(base_dir, "plots", "model_comparisons")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

models <- tibble::tribble(
  ~model_key, ~model_label, ~pos_file, ~structural_file, ~word_file, ~legacy_file,
  "openai", "ChatGPT", "openai_cv_pos_counts.csv", "openai_structural_features.csv", "openai_agentic_communal_tentative_certain.csv", "count_dat_openai.csv",
  "gemini", "Gemini", "gemini_cv_pos_counts.csv", "gemini_structural_features.csv", "gemini_agentic_communal_tentative_certain.csv", "count_dat_google.csv",
  "qwen4B", "Qwen 4B", "qwen4B_cv_pos_counts.csv", "qwen4B_structural_features.csv", "qwen4B_agentic_communal_tentative_certain.csv", "count_dat_qwen4B.csv",
  "qwen8B", "Qwen 8B", "qwen8B_cv_pos_counts.csv", "qwen8B_structural_features.csv", "qwen8B_agentic_communal_tentative_certain.csv", "count_dat_qwen8B.csv"
)

join_keys <- c("profile_id", "first_name", "last_name", "gender", "ethnicity", "name_ID")
model_join_keys <- c(join_keys, "provider", "model")

read_model_counts <- function(model_row) {
  pos_path <- file.path(base_dir, "data", "pos_counts", model_row$pos_file)
  structural_path <- file.path(base_dir, "data", "structural", model_row$structural_file)
  word_path <- file.path(base_dir, "data", "word_counts", model_row$word_file)

  pos <- read.csv(pos_path, check.names = FALSE) %>%
    select(-any_of(c(
      "num_other", "nouns_per_1k", "verbs_per_1k", "adjectives_per_1k",
      "adverbs_per_1k", "pronouns_per_1k", "numerals_per_1k", "other_per_1k"
    )))

  structural <- read.csv(structural_path, check.names = FALSE) %>%
    select(-any_of(c(
      "k02_profil_num_items", "k02_profil_num_subkeys",
      "k04_berufserfahrung_num_subkeys", "k05_ausbildung_num_subkeys",
      "k08_interessen_num_subkeys", "k10_cover_letter_snippet_num_subkeys"
    )))

  lexical <- read.csv(word_path, check.names = FALSE) %>%
    select(any_of(c(
      join_keys,
      "agentic_count", "communal_count", "certainty_count", "tentative_count"
    )))

  count_dat <- pos %>%
    full_join(structural, by = model_join_keys) %>%
    full_join(lexical, by = join_keys) %>%
    mutate(
      model_key = model_row$model_key,
      model_label = model_row$model_label,
      name_type = paste0(first_name, "_", last_name)
    ) %>%
    select(
      model_key, model_label, profile_id, name_ID, name_type,
      first_name, last_name, gender, ethnicity, provider, model, everything()
    )

  write.csv(count_dat, file.path(out_dir, model_row$legacy_file), row.names = FALSE)
  count_dat
}

count_data_list <- lapply(seq_len(nrow(models)), function(i) read_model_counts(models[i, ]))
count_dat_all <- bind_rows(count_data_list)
write.csv(count_dat_all, file.path(out_dir, "count_dat_all_models.csv"), row.names = FALSE)

feature_cols <- count_dat_all %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("profile_id", "name_ID", "total_tokens"))) %>%
  names()

compute_ate <- function(dat, group_var, high_value, low_value, effect_label) {
  dat %>%
    group_by(model_key, model_label, profile_id) %>%
    summarise(
      across(
        all_of(feature_cols),
        list(
          high = ~ mean(.x[.data[[group_var]] == high_value], na.rm = TRUE),
          low = ~ mean(.x[.data[[group_var]] == low_value], na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -c(model_key, model_label, profile_id),
      names_to = c("feature", ".value"),
      names_sep = "__"
    ) %>%
    mutate(
      effect = effect_label,
      ATE = high - low
    ) %>%
    select(model_key, model_label, profile_id, effect, feature, high, low, ATE)
}

summarise_ate <- function(ate_df) {
  ate_df %>%
    group_by(model_key, model_label, effect, feature) %>%
    summarise(
      avg_high = mean(high, na.rm = TRUE),
      avg_low = mean(low, na.rm = TRUE),
      avg_ATE = mean(ATE, na.rm = TRUE),
      med_ATE = median(ATE, na.rm = TRUE),
      q25_ATE = quantile(ATE, 0.25, na.rm = TRUE),
      q75_ATE = quantile(ATE, 0.75, na.rm = TRUE),
      sd_ATE = sd(ATE, na.rm = TRUE),
      n_profiles = n_distinct(profile_id),
      .groups = "drop"
    )
}

model_differences <- function(summary_df, reference_model = "openai") {
  ref <- summary_df %>%
    filter(model_key == reference_model) %>%
    select(effect, feature, reference_avg_ATE = avg_ATE)

  summary_df %>%
    left_join(ref, by = c("effect", "feature")) %>%
    mutate(diff_vs_chatgpt = avg_ATE - reference_avg_ATE) %>%
    arrange(effect, feature, model_key)
}

ate_gender <- compute_ate(
  count_dat_all,
  group_var = "gender",
  high_value = "male",
  low_value = "female",
  effect_label = "gender_male_minus_female"
)
ate_ethnicity <- compute_ate(
  count_dat_all,
  group_var = "ethnicity",
  high_value = "german",
  low_value = "turkish",
  effect_label = "ethnicity_german_minus_turkish"
)

ate_gender_summary <- summarise_ate(ate_gender)
ate_ethnicity_summary <- summarise_ate(ate_ethnicity)
ate_gender_diff <- model_differences(ate_gender_summary)
ate_ethnicity_diff <- model_differences(ate_ethnicity_summary)

write.csv(ate_gender, file.path(out_dir, "ate_gender_by_model.csv"), row.names = FALSE)
write.csv(ate_ethnicity, file.path(out_dir, "ate_ethnicity_by_model.csv"), row.names = FALSE)
write.csv(ate_gender_summary, file.path(out_dir, "ate_gender_summary_by_model.csv"), row.names = FALSE)
write.csv(ate_ethnicity_summary, file.path(out_dir, "ate_ethnicity_summary_by_model.csv"), row.names = FALSE)
write.csv(ate_gender_diff, file.path(out_dir, "ate_gender_model_differences_vs_chatgpt.csv"), row.names = FALSE)
write.csv(ate_ethnicity_diff, file.path(out_dir, "ate_ethnicity_model_differences_vs_chatgpt.csv"), row.names = FALSE)

plot_feature_overview <- function(summary_df, filename, title) {
  top_features <- summary_df %>%
    group_by(feature) %>%
    summarise(max_abs_ate = max(abs(avg_ATE), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(max_abs_ate)) %>%
    slice_head(n = 20) %>%
    pull(feature)

  p <- summary_df %>%
    filter(feature %in% top_features) %>%
    mutate(feature = factor(feature, levels = rev(top_features))) %>%
    ggplot(aes(x = avg_ATE, y = feature, color = model_label)) +
    geom_vline(xintercept = 0, linewidth = 0.3, color = "grey55") +
    geom_point(position = position_dodge(width = 0.55), size = 2) +
    labs(title = title, x = "Average ATE", y = NULL, color = "Model") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ggsave(file.path(plot_dir, filename), p, width = 9, height = 7, dpi = 300)
}

plot_feature_overview(
  ate_gender_summary,
  "ate_gender_top_features_by_model.png",
  "Gender ATE by model: male - female"
)
plot_feature_overview(
  ate_ethnicity_summary,
  "ate_ethnicity_top_features_by_model.png",
  "Ethnicity ATE by model: German - Turkish"
)

cat("Saved full count data for models:\n")
print(count_dat_all %>% count(model_key, model_label))
cat("\nSaved ATE summaries and model comparison files to:", out_dir, "\n")
cat("Saved overview plots to:", plot_dir, "\n")
