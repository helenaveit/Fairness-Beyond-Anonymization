library(dplyr)
library(glmnet)
library(pROC)
library(tibble)

model_specs <- tribble(
  ~model_key, ~model_label, ~path,
  "chatgpt", "ChatGPT", "data/raw_tfidf_traintest_chatgpt.csv",
  "gemini", "Gemini", "data/raw_tfidf_traintest_gemini.csv",
  "qwen4B", "Qwen 4B", "data/raw_tfidf_traintest_qwen4B.csv",
  "qwen8B", "Qwen 8B", "data/raw_tfidf_traintest_qwen8B.csv",
  "qwen14B", "Qwen 14B", "data/raw_tfidf_traintest_qwen14B.csv",
  "qwen4B_topk64", "Qwen 4B top_k=64", "data/raw_tfidf_traintest_qwen4B_topk64.csv",
  "qwen8B_topk64", "Qwen 8B top_k=64", "data/raw_tfidf_traintest_qwen8B_topk64.csv",
  "qwen14B_topk64", "Qwen 14B top_k=64", "data/raw_tfidf_traintest_qwen14B_topk64.csv"
) %>%
  filter(file.exists(path))

seeds <- c(1, 7, 13, 21, 42, 84, 101, 123, 222, 333, 444, 555, 777, 999, 2024, 2025)
out_dir <- "data/lasso_results/seed_sensitivity"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

make_x <- function(df) {
  drop_cols <- intersect(c("profile_id", "gender", "ethnicity", "name_ID"), names(df))
  df %>% select(-all_of(drop_cols)) %>% as.matrix()
}

fit_target <- function(train_df, test_df, x_train, x_test, foldid, model_key, model_label, target, seed) {
  if (target == "gender") {
    y_train <- ifelse(train_df$gender == "male", 1, 0)
    y_test <- ifelse(test_df$gender == "male", 1, 0)
    positive_class <- "male"
  } else {
    y_train <- ifelse(train_df$ethnicity == "german", 1, 0)
    y_test <- ifelse(test_df$ethnicity == "german", 1, 0)
    positive_class <- "german"
  }

  cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, foldid = foldid)
  fit <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = cv_fit$lambda.1se)
  probs <- as.numeric(predict(fit, newx = x_test, type = "response"))
  pred <- ifelse(probs > 0.5, 1, 0)
  active <- coef(fit)
  active <- active[active[, 1] != 0, , drop = FALSE]

  list(
    metrics = tibble(
      seed = seed,
      model_key = model_key,
      model_label = model_label,
      target = target,
      positive_class = positive_class,
      lambda_min = cv_fit$lambda.min,
      lambda_1se = cv_fit$lambda.1se,
      active_terms = nrow(active) - 1,
      accuracy = mean(pred == y_test),
      auc = as.numeric(auc(roc(y_test, probs, quiet = TRUE)))
    ),
    active = data.frame(
      seed = seed,
      model_key = model_key,
      model_label = model_label,
      target = target,
      term = rownames(active),
      coefficient = as.numeric(active[, 1]),
      row.names = NULL
    ) %>% filter(term != "(Intercept)")
  )
}

tfidf_data <- setNames(lapply(model_specs$path, read.csv, check.names = FALSE), model_specs$model_key)
profiles <- unique(tfidf_data[[1]]$profile_id)
all_metrics <- list()
all_active <- list()

for (seed in seeds) {
  set.seed(seed)
  train_ids <- sample(profiles, size = floor(0.8 * length(profiles)))
  profile_fold <- sample(rep(1:3, length.out = length(profiles)))
  fold_map <- setNames(profile_fold, profiles)

  for (i in seq_len(nrow(model_specs))) {
    spec <- model_specs[i, ]
    df <- tfidf_data[[spec$model_key]]
    train_df <- df %>% filter(profile_id %in% train_ids)
    test_df <- df %>% filter(!profile_id %in% train_ids)
    x_train <- make_x(train_df)
    x_test <- make_x(test_df)
    foldid <- as.numeric(fold_map[as.character(train_df$profile_id)])

    for (target in c("gender", "ethnicity")) {
      result <- fit_target(train_df, test_df, x_train, x_test, foldid, spec$model_key, spec$model_label, target, seed)
      key <- paste(seed, spec$model_key, target, sep = "_")
      all_metrics[[key]] <- result$metrics
      all_active[[key]] <- result$active
    }
  }
}

seed_metrics <- bind_rows(all_metrics)
seed_active <- bind_rows(all_active)

metric_summary <- seed_metrics %>%
  group_by(model_key, model_label, target, positive_class) %>%
  summarize(
    n_seeds = n(),
    auc_mean = mean(auc),
    auc_sd = sd(auc),
    accuracy_mean = mean(accuracy),
    accuracy_sd = sd(accuracy),
    active_terms_mean = mean(active_terms),
    active_terms_sd = sd(active_terms),
    .groups = "drop"
  )

coef_frequency <- seed_active %>%
  group_by(model_key, model_label, target, term) %>%
  summarize(
    selected_n = n_distinct(seed),
    selected_share = selected_n / length(seeds),
    coefficient_mean = mean(coefficient),
    coefficient_sd = sd(coefficient),
    .groups = "drop"
  ) %>%
  arrange(model_key, target, desc(selected_share), desc(abs(coefficient_mean)))

write.csv(seed_metrics, file.path(out_dir, "tfidf_lasso_seed_metrics.csv"), row.names = FALSE)
write.csv(metric_summary, file.path(out_dir, "tfidf_lasso_seed_metric_summary.csv"), row.names = FALSE)
write.csv(seed_active, file.path(out_dir, "tfidf_lasso_seed_active_coefficients.csv"), row.names = FALSE)
write.csv(coef_frequency, file.path(out_dir, "tfidf_lasso_seed_coefficient_frequency.csv"), row.names = FALSE)
print(metric_summary)
