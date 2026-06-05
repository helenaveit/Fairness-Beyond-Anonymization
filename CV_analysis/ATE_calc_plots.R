library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

############################################
## ATE calculation and plots for all models
############################################

find_cv_root <- function() {
  candidates <- c("CV_analysis", ".", "..")
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "data", "full_count_data", "count_dat_openai.csv"))) {
      return(normalizePath(candidate))
    }
  }
  stop("Could not find CV_analysis root with data/full_count_data/count_dat_openai.csv")
}

cv_root <- find_cv_root()
data_dir <- file.path(cv_root, "data", "full_count_data")
plot_dir <- file.path(cv_root, "plots")
comparison_plot_dir <- file.path(plot_dir, "model_comparisons")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(comparison_plot_dir, recursive = TRUE, showWarnings = FALSE)

model_files <- data.frame(
  model_key = c("openai", "gemini", "qwen4B", "qwen8B"),
  model_label = c("ChatGPT", "Gemini", "Qwen 4B", "Qwen 8B"),
  file = c(
    "count_dat_openai.csv",
    "count_dat_google.csv",
    "count_dat_qwen4B.csv",
    "count_dat_qwen8B.csv"
  ),
  stringsAsFactors = FALSE
)

model_colors <- c(
  "ChatGPT" = "#018571",
  "Gemini" = "#a6611a",
  "Qwen 4B" = "#5e3c99",
  "Qwen 8B" = "#e66101"
)

read_model_count <- function(model_key, model_label, file) {
  read.csv(file.path(data_dir, file), check.names = FALSE) %>%
    mutate(
      model_key = model_key,
      model_label = model_label,
      gender = factor(gender, levels = c("female", "male")),
      ethnicity = factor(ethnicity, levels = c("turkish", "german"))
    )
}

count_dat_all <- bind_rows(
  lapply(seq_len(nrow(model_files)), function(i) {
    read_model_count(
      model_files$model_key[i],
      model_files$model_label[i],
      model_files$file[i]
    )
  })
)

excluded_feature_cols <- c(
  "profile_id", "name_ID", "total_tokens", "num_top_keys",
  "k01_persoenliche_daten_num_items", "k01_persoenliche_daten_num_subkeys",
  "k03_faehigkeiten_num_items", "k03_faehigkeiten_num_subkeys",
  "k04_berufserfahrung_num_items",
  "k05_ausbildung_num_items",
  "k06_skills_num_items", "k06_skills_num_subkeys",
  "k07_sprachen_num_items", "k07_sprachen_num_subkeys",
  "k08_interessen_num_items",
  "k09_angestrebte_position_num_items", "k09_angestrebte_position_num_subkeys",
  "k10_cover_letter_snippet_num_items"
)

feat_cols <- count_dat_all %>%
  select(where(is.numeric)) %>%
  select(-any_of(excluded_feature_cols)) %>%
  names()

compute_ate <- function(dat, group_var, high_value, low_value, effect_label) {
  dat %>%
    group_by(model_key, model_label, profile_id) %>%
    summarise(
      across(
        all_of(feat_cols),
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

ate_gender_all <- compute_ate(
  count_dat_all,
  group_var = "gender",
  high_value = "male",
  low_value = "female",
  effect_label = "gender_male_minus_female"
)

ate_ethnicity_all <- compute_ate(
  count_dat_all,
  group_var = "ethnicity",
  high_value = "german",
  low_value = "turkish",
  effect_label = "ethnicity_german_minus_turkish"
)

ate_gender_summary <- summarise_ate(ate_gender_all)
ate_ethnicity_summary <- summarise_ate(ate_ethnicity_all)
ate_gender_diff <- model_differences(ate_gender_summary)
ate_ethnicity_diff <- model_differences(ate_ethnicity_summary)

write.csv(ate_gender_all, file.path(data_dir, "ate_gender_by_model.csv"), row.names = FALSE)
write.csv(ate_ethnicity_all, file.path(data_dir, "ate_ethnicity_by_model.csv"), row.names = FALSE)
write.csv(ate_gender_summary, file.path(data_dir, "ate_gender_summary_by_model.csv"), row.names = FALSE)
write.csv(ate_ethnicity_summary, file.path(data_dir, "ate_ethnicity_summary_by_model.csv"), row.names = FALSE)
write.csv(ate_gender_diff, file.path(data_dir, "ate_gender_model_differences_vs_chatgpt.csv"), row.names = FALSE)
write.csv(ate_ethnicity_diff, file.path(data_dir, "ate_ethnicity_model_differences_vs_chatgpt.csv"), row.names = FALSE)

ate_long_gender_chat <- ate_gender_all %>% filter(model_key == "openai")
ate_long_gender_gem <- ate_gender_all %>% filter(model_key == "gemini")
ate_long_gender_qwen4B <- ate_gender_all %>% filter(model_key == "qwen4B")
ate_long_gender_qwen8B <- ate_gender_all %>% filter(model_key == "qwen8B")

ate_long_eth_chat <- ate_ethnicity_all %>% filter(model_key == "openai")
ate_long_eth_gem <- ate_ethnicity_all %>% filter(model_key == "gemini")
ate_long_eth_qwen4B <- ate_ethnicity_all %>% filter(model_key == "qwen4B")
ate_long_eth_qwen8B <- ate_ethnicity_all %>% filter(model_key == "qwen8B")

feat_avgs_g_chat <- ate_gender_summary %>% filter(model_key == "openai")
feat_avgs_g_gem <- ate_gender_summary %>% filter(model_key == "gemini")
feat_avgs_g_qwen4B <- ate_gender_summary %>% filter(model_key == "qwen4B")
feat_avgs_g_qwen8B <- ate_gender_summary %>% filter(model_key == "qwen8B")

feat_avgs_e_chat <- ate_ethnicity_summary %>% filter(model_key == "openai")
feat_avgs_e_gem <- ate_ethnicity_summary %>% filter(model_key == "gemini")
feat_avgs_e_qwen4B <- ate_ethnicity_summary %>% filter(model_key == "qwen4B")
feat_avgs_e_qwen8B <- ate_ethnicity_summary %>% filter(model_key == "qwen8B")

############################################
## Feature names
############################################

lexica_feat <- c("agentic_count", "communal_count", "certainty_count", "tentative_count")
feature_names_lex <- c(
  "agentic_count" = "Agentic",
  "communal_count" = "Communal",
  "certainty_count" = "Certainty",
  "tentative_count" = "Tentative"
)

agentic_communal_feat <- c("agentic_count", "communal_count")
feature_names_agentic_communal <- c(
  "agentic_count" = "Agentic",
  "communal_count" = "Communal"
)

cert_tent_feat <- c("certainty_count", "tentative_count")
feature_names_cert_tent <- c(
  "certainty_count" = "Certainty",
  "tentative_count" = "Tentative"
)

pos_feat <- c("num_verbs", "num_adjectives", "num_numerals", "num_nouns")
feature_names_pos <- c(
  "num_verbs" = "Verbs",
  "num_adjectives" = "Adjectives",
  "num_numerals" = "Numerals",
  "num_nouns" = "Nouns"
)

struc_feat <- c(
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

feature_names_struc <- c(
  "cv_total_words" = "Total words",
  "k01_persoenliche_daten_num_words" = "Personal",
  "k02_profil_num_words" = "Profile",
  "k03_faehigkeiten_num_words" = "Competences",
  "k04_berufserfahrung_num_words" = "Work experience",
  "k05_ausbildung_num_words" = "Education",
  "k06_skills_num_words" = "Skills",
  "k07_sprachen_num_words" = "Languages",
  "k08_interessen_num_words" = "Interests",
  "k09_angestrebte_position_num_words" = "Position",
  "k10_cover_letter_snippet_num_words" = "Cover Letter"
)

struc_feat_min <- c(
  "cv_total_words",
  "k01_persoenliche_daten_num_words",
  "k02_profil_num_words",
  "k05_ausbildung_num_words",
  "k08_interessen_num_words",
  "k10_cover_letter_snippet_num_words"
)

feature_names_struc_min <- feature_names_struc[struc_feat_min]
feat_name_total_w <- c("cv_total_words" = "Total words")

############################################
## Plot helpers
############################################

label_features <- function(df, feature_names = NULL) {
  if (is.null(feature_names)) {
    return(df)
  }

  df %>%
    mutate(feature = recode(feature, !!!feature_names))
}

boxplot_ate_grid_models <- function(
    ate_df,
    ate_col = "ATE",
    x_label = "ATE",
    features = NULL,
    feature_names = NULL,
    limits = NULL,
    filename = NULL,
    width = 14,
    height = 8
) {
  if (is.null(features)) {
    features <- unique(ate_df$feature)
  }

  plot_df <- ate_df %>%
    filter(feature %in% features) %>%
    label_features(feature_names) %>%
    mutate(
      model_label = factor(model_label, levels = model_files$model_label),
      feature = factor(feature, levels = rev(unname(feature_names[features] %||% features)))
    )

  if (is.null(limits)) {
    rng <- range(plot_df[[ate_col]], na.rm = TRUE)
    max_abs <- max(abs(rng))
    limits <- c(-max_abs, max_abs)
  }

  p <- ggplot(plot_df, aes(x = .data[[ate_col]], y = "")) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey45") +
    geom_boxplot(aes(fill = model_label), alpha = 0.72, outlier.alpha = 0.5) +
    facet_grid(feature ~ model_label, switch = "y") +
    scale_fill_manual(values = model_colors, name = "Model", drop = FALSE) +
    scale_x_continuous(breaks = pretty(limits, n = 5)) +
    coord_cartesian(xlim = limits) +
    labs(x = x_label, y = NULL) +
    theme_bw() +
    theme(
      strip.placement = "outside",
      strip.text.x = element_text(size = 13),
      strip.text.y.left = element_text(angle = 90, size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 12),
      panel.spacing.y = unit(0.18, "lines"),
      panel.spacing.x = unit(0.55, "lines"),
      legend.position = "bottom"
    )

  if (!is.null(filename)) {
    ggsave(file.path(plot_dir, filename), p, width = width, height = height, dpi = 300)
  }

  p
}

plot_feature_overview <- function(summary_df, filename, title) {
  top_features <- summary_df %>%
    group_by(feature) %>%
    summarise(max_abs_ate = max(abs(avg_ATE), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(max_abs_ate)) %>%
    slice_head(n = 20) %>%
    pull(feature)

  p <- summary_df %>%
    filter(feature %in% top_features) %>%
    mutate(
      feature = factor(feature, levels = rev(top_features)),
      model_label = factor(model_label, levels = model_files$model_label)
    ) %>%
    ggplot(aes(x = avg_ATE, y = feature, color = model_label)) +
    geom_vline(xintercept = 0, linewidth = 0.3, color = "grey55") +
    geom_point(position = position_dodge(width = 0.55), size = 2) +
    scale_color_manual(values = model_colors, name = "Model", drop = FALSE) +
    labs(title = title, x = "Average ATE", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ggsave(file.path(comparison_plot_dir, filename), p, width = 9, height = 7, dpi = 300)
  p
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

############################################
## Save plots
############################################

boxplot_ate_grid_models(
  ate_gender_all,
  x_label = "Avg. gender differences (male - female) per profile",
  features = "cv_total_words",
  feature_names = feat_name_total_w,
  limits = c(-20, 20),
  filename = "boxplot_ate_length_gender.png",
  width = 16,
  height = 4
)

boxplot_ate_grid_models(
  ate_ethnicity_all,
  x_label = "Avg. ethnicity differences (German - Turkish) per profile",
  features = "cv_total_words",
  feature_names = feat_name_total_w,
  limits = c(-20, 20),
  filename = "boxplot_ate_length_ethnicity.png",
  width = 16,
  height = 4
)

boxplot_ate_grid_models(
  ate_gender_all,
  x_label = "ATE (male - female)",
  features = pos_feat,
  feature_names = feature_names_pos,
  limits = c(-4, 4),
  filename = "boxplot_ate_pos_gender.png",
  width = 16,
  height = 8
)

boxplot_ate_grid_models(
  ate_ethnicity_all,
  x_label = "ATE (German - Turkish)",
  features = pos_feat,
  feature_names = feature_names_pos,
  limits = c(-4, 4),
  filename = "boxplot_ate_pos_ethnicity.png",
  width = 16,
  height = 8
)

boxplot_ate_grid_models(
  ate_gender_all,
  x_label = "Avg. gender differences (male - female) per profile",
  features = agentic_communal_feat,
  feature_names = feature_names_agentic_communal,
  limits = c(-4, 4),
  filename = "boxplot_ate_agentic_communal_gender.png",
  width = 16,
  height = 6
)

boxplot_ate_grid_models(
  ate_ethnicity_all,
  x_label = "Avg. ethnicity differences (German - Turkish) per profile",
  features = agentic_communal_feat,
  feature_names = feature_names_agentic_communal,
  limits = c(-4, 4),
  filename = "boxplot_ate_agentic_communal_ethnicity.png",
  width = 16,
  height = 6
)

boxplot_ate_grid_models(
  ate_gender_all,
  x_label = "Avg. gender differences (male - female) per profile",
  features = cert_tent_feat,
  feature_names = feature_names_cert_tent,
  limits = c(-2, 2),
  filename = "boxplot_ate_cert_tent_gender.png",
  width = 16,
  height = 6
)

boxplot_ate_grid_models(
  ate_ethnicity_all,
  x_label = "Avg. ethnicity differences (German - Turkish) per profile",
  features = cert_tent_feat,
  feature_names = feature_names_cert_tent,
  limits = c(-2, 2),
  filename = "boxplot_ate_cert_tent_ethnicity.png",
  width = 16,
  height = 6
)

boxplot_ate_grid_models(
  ate_gender_all,
  x_label = "ATE (male - female)",
  features = struc_feat_min,
  feature_names = feature_names_struc_min,
  limits = c(-10, 10),
  filename = "boxplot_ate_struc_gender.png",
  width = 16,
  height = 10
)

boxplot_ate_grid_models(
  ate_ethnicity_all,
  x_label = "ATE (German - Turkish)",
  features = struc_feat_min,
  feature_names = feature_names_struc_min,
  limits = c(-10, 10),
  filename = "boxplot_ate_struc_ethnicity.png",
  width = 16,
  height = 10
)

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

cat("Saved ATE files to:", data_dir, "\n")
cat("Saved ATE plots to:", plot_dir, "\n")
print(count_dat_all %>% count(model_key, model_label))
