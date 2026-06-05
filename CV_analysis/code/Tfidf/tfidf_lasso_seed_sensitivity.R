################################################################################
# TF-IDF Lasso Seed Sensitivity
#
# Repeats the TF-IDF lasso workflow across multiple random seeds.
################################################################################


################################################################################
# Packages
################################################################################

library(dplyr)
library(glmnet)
library(pROC)
library(tibble)


################################################################################
# Model Inputs and Seed Grid
################################################################################

MODEL_SPECS <- tribble(
  ~model_key,        ~model_label,          ~path,
  "chatgpt",         "ChatGPT",             "CV_analysis/data/raw_tfidf_traintest_chatgpt.csv",
  "gemini",          "Gemini",              "CV_analysis/data/raw_tfidf_traintest_gem.csv",
  "qwen4B",          "Qwen 4B",             "CV_analysis/data/raw_tfidf_traintest_qwen4B.csv",
  "qwen8B",          "Qwen 8B",             "CV_analysis/data/raw_tfidf_traintest_qwen8B.csv",
  "qwen14B",         "Qwen 14B",            "CV_analysis/data/raw_tfidf_traintest_qwen14B.csv",
  "qwen4B_topk64",   "Qwen 4B top_k=64",    "CV_analysis/data/raw_tfidf_traintest_qwen4B_topk64.csv",
  "qwen8B_topk64",   "Qwen 8B top_k=64",    "CV_analysis/data/raw_tfidf_traintest_qwen8B_topk64.csv",
  "qwen14B_topk64",  "Qwen 14B top_k=64",   "CV_analysis/data/raw_tfidf_traintest_qwen14B_topk64.csv"
)

SEEDS <- c(
  1, 7, 13, 21, 42, 84, 101, 123, 222, 333,
  444, 555, 777, 999, 1001, 1234, 1729, 2024, 2025, 3141,
  4242, 5151, 6789, 9001, 12345, 27182, 31415, 42424, 65537, 8675309
)
N_FOLDS <- 3
OUT_DIR <- "CV_analysis/data/lasso_results/seed_sensitivity"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


################################################################################
# Data Loading and Matrix Helpers
################################################################################

read_tfidf_data <- function(path) {
  read.csv(path, check.names = FALSE)
}


make_x <- function(df) {
  df %>%
    select(-profile_id, -gender, -ethnicity, -name_ID) %>%
    as.matrix()
}


################################################################################
# Coefficient Extraction
################################################################################

make_active_coef_df <- function(coef_mat, model_key, model_label, target, seed) {
  active <- coef_mat[coef_mat[, 1] != 0, , drop = FALSE]

  data.frame(
    seed = seed,
    model_key = model_key,
    model_label = model_label,
    target = target,
    term = rownames(active),
    coefficient = as.numeric(active[, 1]),
    row.names = NULL
  ) %>%
    arrange(desc(abs(coefficient)))
}


################################################################################
# Fit One Lasso Target for One Seed
################################################################################

fit_lasso_target <- function(
  train_df,
  test_df,
  x_train,
  x_test,
  foldid,
  model_key,
  model_label,
  target,
  seed
) {
  if (target == "gender") {
    y_train <- ifelse(train_df$gender == "male", 1, 0)
    y_test <- ifelse(test_df$gender == "male", 1, 0)
    positive_class <- "male"
  } else {
    y_train <- ifelse(train_df$ethnicity == "german", 1, 0)
    y_test <- ifelse(test_df$ethnicity == "german", 1, 0)
    positive_class <- "german"
  }

  cv_fit <- cv.glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = 1,
    foldid = foldid
  )

  lambda_min <- cv_fit$lambda.min
  lambda_1se <- cv_fit$lambda.1se

  lasso <- glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = 1,
    lambda = lambda_1se
  )

  pred_probs <- as.numeric(predict(lasso, newx = x_test, type = "response"))
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)

  acc <- mean(pred_class == y_test)
  roc_obj <- roc(response = y_test, predictor = pred_probs, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))

  active_coefs <- make_active_coef_df(
    coef(lasso),
    model_key = model_key,
    model_label = model_label,
    target = target,
    seed = seed
  )

  list(
    lambda_min = lambda_min,
    lambda_1se = lambda_1se,
    acc = acc,
    auc = auc_value,
    active = active_coefs,
    positive_class = positive_class
  )
}


################################################################################
# Fit Both Targets for One Model Version
################################################################################

fit_lasso_for_model <- function(df, model_key, model_label, train_ids, fold_map, seed) {
  train_df <- df %>% filter(profile_id %in% train_ids)
  test_df <- df %>% filter(!profile_id %in% train_ids)

  x_train <- make_x(train_df)
  x_test <- make_x(test_df)

  foldid <- as.numeric(fold_map[as.character(train_df$profile_id)])

  gender_fit <- fit_lasso_target(
    train_df = train_df,
    test_df = test_df,
    x_train = x_train,
    x_test = x_test,
    foldid = foldid,
    model_key = model_key,
    model_label = model_label,
    target = "gender",
    seed = seed
  )

  ethnicity_fit <- fit_lasso_target(
    train_df = train_df,
    test_df = test_df,
    x_train = x_train,
    x_test = x_test,
    foldid = foldid,
    model_key = model_key,
    model_label = model_label,
    target = "ethnicity",
    seed = seed
  )

  list(gender = gender_fit, ethnicity = ethnicity_fit)
}


################################################################################
# Per-Seed Metric Summary
################################################################################

summarize_metrics_for_seed <- function(seed, lasso_results) {
  bind_rows(lapply(MODEL_SPECS$model_key, function(model_key) {
    spec <- MODEL_SPECS %>% filter(.data$model_key == .env$model_key)
    result <- lasso_results[[model_key]]

    bind_rows(
      tibble(
        seed = seed,
        model_key = model_key,
        model_label = spec$model_label,
        target = "gender",
        positive_class = result$gender$positive_class,
        lambda_min = result$gender$lambda_min,
        lambda_1se = result$gender$lambda_1se,
        active_terms = nrow(result$gender$active),
        accuracy = result$gender$acc,
        auc = result$gender$auc
      ),
      tibble(
        seed = seed,
        model_key = model_key,
        model_label = spec$model_label,
        target = "ethnicity",
        positive_class = result$ethnicity$positive_class,
        lambda_min = result$ethnicity$lambda_min,
        lambda_1se = result$ethnicity$lambda_1se,
        active_terms = nrow(result$ethnicity$active),
        accuracy = result$ethnicity$acc,
        auc = result$ethnicity$auc
      )
    )
  }))
}


################################################################################
# Load Data and Run All Seeds
################################################################################

tfidf_data <- setNames(
  lapply(MODEL_SPECS$path, read_tfidf_data),
  MODEL_SPECS$model_key
)

profiles <- unique(tfidf_data$chatgpt$profile_id)

all_metrics <- list()
all_active_coefficients <- list()

for (seed in SEEDS) {
  message("Running seed ", seed, "...")
  set.seed(seed)

  train_ids <- sample(profiles, size = floor(0.8 * length(profiles)))
  profile_fold <- sample(rep(seq_len(N_FOLDS), length.out = length(profiles)))
  fold_map <- setNames(profile_fold, profiles)

  lasso_results <- list()
  for (i in seq_len(nrow(MODEL_SPECS))) {
    spec <- MODEL_SPECS[i, ]
    lasso_results[[spec$model_key]] <- fit_lasso_for_model(
      df = tfidf_data[[spec$model_key]],
      model_key = spec$model_key,
      model_label = spec$model_label,
      train_ids = train_ids,
      fold_map = fold_map,
      seed = seed
    )
  }

  all_metrics[[as.character(seed)]] <- summarize_metrics_for_seed(seed, lasso_results)
  all_active_coefficients[[as.character(seed)]] <- bind_rows(lapply(MODEL_SPECS$model_key, function(model_key) {
    bind_rows(
      lasso_results[[model_key]]$gender$active,
      lasso_results[[model_key]]$ethnicity$active
    )
  }))
}


################################################################################
# Combine Per-Seed Outputs
################################################################################

seed_metrics <- bind_rows(all_metrics)
seed_active_coefficients <- bind_rows(all_active_coefficients)


################################################################################
# Summarize Metric Stability Across Seeds
################################################################################

metric_summary <- seed_metrics %>%
  group_by(model_key, model_label, target, positive_class) %>%
  summarize(
    n_seeds = n(),
    auc_mean = mean(auc),
    auc_sd = sd(auc),
    auc_se = auc_sd / sqrt(n_seeds),
    auc_ci95_low = auc_mean - qt(0.975, df = n_seeds - 1) * auc_se,
    auc_ci95_high = auc_mean + qt(0.975, df = n_seeds - 1) * auc_se,
    auc_min = min(auc),
    auc_max = max(auc),
    accuracy_mean = mean(accuracy),
    accuracy_sd = sd(accuracy),
    accuracy_se = accuracy_sd / sqrt(n_seeds),
    accuracy_ci95_low = accuracy_mean - qt(0.975, df = n_seeds - 1) * accuracy_se,
    accuracy_ci95_high = accuracy_mean + qt(0.975, df = n_seeds - 1) * accuracy_se,
    accuracy_min = min(accuracy),
    accuracy_max = max(accuracy),
    active_terms_mean = mean(active_terms),
    active_terms_sd = sd(active_terms),
    active_terms_se = active_terms_sd / sqrt(n_seeds),
    active_terms_ci95_low = active_terms_mean - qt(0.975, df = n_seeds - 1) * active_terms_se,
    active_terms_ci95_high = active_terms_mean + qt(0.975, df = n_seeds - 1) * active_terms_se,
    active_terms_min = min(active_terms),
    active_terms_max = max(active_terms),
    .groups = "drop"
  )


################################################################################
# Summarize Coefficient Selection Stability Across Seeds
################################################################################

coef_selection_frequency <- seed_active_coefficients %>%
  filter(term != "(Intercept)") %>%
  group_by(model_key, model_label, target, term) %>%
  summarize(
    selected_n = n_distinct(seed),
    selected_share = selected_n / length(SEEDS),
    coefficient_mean = mean(coefficient),
    coefficient_sd = sd(coefficient),
    coefficient_min = min(coefficient),
    coefficient_max = max(coefficient),
    .groups = "drop"
  ) %>%
  arrange(model_key, target, desc(selected_share), desc(abs(coefficient_mean)))


################################################################################
# Save Outputs
################################################################################

write.csv(
  seed_metrics,
  file.path(OUT_DIR, "tfidf_lasso_seed_metrics.csv"),
  row.names = FALSE
)

write.csv(
  metric_summary,
  file.path(OUT_DIR, "tfidf_lasso_seed_metric_summary.csv"),
  row.names = FALSE
)

write.csv(
  seed_active_coefficients,
  file.path(OUT_DIR, "tfidf_lasso_seed_active_coefficients.csv"),
  row.names = FALSE
)

write.csv(
  coef_selection_frequency,
  file.path(OUT_DIR, "tfidf_lasso_seed_coefficient_frequency.csv"),
  row.names = FALSE
)


################################################################################
# Console Summary
################################################################################

print(metric_summary)
