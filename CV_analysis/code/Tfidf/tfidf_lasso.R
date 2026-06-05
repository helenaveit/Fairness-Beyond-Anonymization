################################################################################
# TF-IDF Lasso Models
#
# Fits gender and ethnicity lasso models for each TF-IDF model version.
# Splits and CV folds are assigned at the profile_id level.
################################################################################


################################################################################
# Packages
################################################################################

library(dplyr)
library(glmnet)
library(pROC)
library(tibble)


################################################################################
# Model Inputs and Output Directory
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

OUT_DIR <- "CV_analysis/data/lasso_results"
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

make_active_coef_df <- function(coef_mat, model_key, model_label, target) {
  active <- coef_mat[coef_mat[, 1] != 0, , drop = FALSE]

  data.frame(
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
# Fit One Lasso Target
################################################################################

fit_lasso_target <- function(
  train_df,
  test_df,
  x_train,
  x_test,
  foldid,
  model_key,
  model_label,
  target
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
    target = target
  )

  list(
    cv_fit = cv_fit,
    lasso = lasso,
    lambda_min = lambda_min,
    lambda_1se = lambda_1se,
    acc = acc,
    auc = auc_value,
    active = active_coefs,
    positive_class = positive_class,
    pred_probs = pred_probs,
    pred_class = pred_class,
    y_test = y_test
  )
}


################################################################################
# Fit Both Targets for One Model Version
################################################################################

fit_lasso_for_model <- function(df, model_key, model_label, train_ids, fold_map) {
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
    target = "gender"
  )

  ethnicity_fit <- fit_lasso_target(
    train_df = train_df,
    test_df = test_df,
    x_train = x_train,
    x_test = x_test,
    foldid = foldid,
    model_key = model_key,
    model_label = model_label,
    target = "ethnicity"
  )

  list(
    train_df = train_df,
    test_df = test_df,
    x_train = x_train,
    x_test = x_test,
    foldid = foldid,
    gender = gender_fit,
    ethnicity = ethnicity_fit
  )
}


################################################################################
# Load TF-IDF Data and Create Profile-Level Splits
################################################################################

tfidf_data <- setNames(
  lapply(MODEL_SPECS$path, read_tfidf_data),
  MODEL_SPECS$model_key
)

# Same profile-level split for every model version.
set.seed(42)
profiles <- unique(tfidf_data$chatgpt$profile_id)
train_ids <- sample(profiles, size = floor(0.8 * length(profiles)))

profile_fold <- sample(rep(1:3, length.out = length(profiles)))
fold_map <- setNames(profile_fold, profiles)


################################################################################
# Run Lasso Models
################################################################################

lasso_results <- list()

for (i in seq_len(nrow(MODEL_SPECS))) {
  spec <- MODEL_SPECS[i, ]
  message("Fitting ", spec$model_label, "...")

  lasso_results[[spec$model_key]] <- fit_lasso_for_model(
    df = tfidf_data[[spec$model_key]],
    model_key = spec$model_key,
    model_label = spec$model_label,
    train_ids = train_ids,
    fold_map = fold_map
  )
}


################################################################################
# Summarize Metrics and Selected Coefficients
################################################################################

metrics <- bind_rows(lapply(MODEL_SPECS$model_key, function(model_key) {
  spec <- MODEL_SPECS %>% filter(.data$model_key == .env$model_key)
  result <- lasso_results[[model_key]]

  bind_rows(
    tibble(
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

active_coefficients <- bind_rows(lapply(MODEL_SPECS$model_key, function(model_key) {
  bind_rows(
    lasso_results[[model_key]]$gender$active,
    lasso_results[[model_key]]$ethnicity$active
  )
}))


################################################################################
# Save Outputs
################################################################################

write.csv(
  metrics,
  file.path(OUT_DIR, "tfidf_lasso_metrics.csv"),
  row.names = FALSE
)

write.csv(
  active_coefficients,
  file.path(OUT_DIR, "tfidf_lasso_active_coefficients.csv"),
  row.names = FALSE
)

saveRDS(
  lasso_results,
  file.path(OUT_DIR, "tfidf_lasso_results.rds")
)


################################################################################
# Backwards-Compatible Aliases for Interactive Work
################################################################################

# Backwards-compatible aliases for the original model names.
df_tfidf_chatgpt <- tfidf_data$chatgpt
df_tfidf_gemini <- tfidf_data$gemini
df_tfidf_qwen4 <- tfidf_data$qwen4B
df_tfidf_qwen8 <- tfidf_data$qwen8B
df_tfidf_qwen14 <- tfidf_data$qwen14B

active_g_tf_chat <- lasso_results$chatgpt$gender$active
active_e_chat_tf <- lasso_results$chatgpt$ethnicity$active
acc_g_tf_chat <- lasso_results$chatgpt$gender$acc
acc_e_chat_tf <- lasso_results$chatgpt$ethnicity$acc
auc_g_tf_chat <- lasso_results$chatgpt$gender$auc
auc_e_chat_tf <- lasso_results$chatgpt$ethnicity$auc

active_g_gem_tf <- lasso_results$gemini$gender$active
active_e_gem_tf <- lasso_results$gemini$ethnicity$active
acc_g_gem_tf <- lasso_results$gemini$gender$acc
acc_e_gem_tf <- lasso_results$gemini$ethnicity$acc
auc_g_gem_tf <- lasso_results$gemini$gender$auc
auc_e_gem_tf <- lasso_results$gemini$ethnicity$auc

active_g_qwen4_tf <- lasso_results$qwen4B$gender$active
active_e_qwen4_tf <- lasso_results$qwen4B$ethnicity$active
acc_g_qwen4_tf <- lasso_results$qwen4B$gender$acc
acc_e_qwen4_tf <- lasso_results$qwen4B$ethnicity$acc
auc_g_qwen4_tf <- lasso_results$qwen4B$gender$auc
auc_e_qwen4_tf <- lasso_results$qwen4B$ethnicity$auc

active_g_qwen8_tf <- lasso_results$qwen8B$gender$active
active_e_qwen8_tf <- lasso_results$qwen8B$ethnicity$active
acc_g_qwen8_tf <- lasso_results$qwen8B$gender$acc
acc_e_qwen8_tf <- lasso_results$qwen8B$ethnicity$acc
auc_g_qwen8_tf <- lasso_results$qwen8B$gender$auc
auc_e_qwen8_tf <- lasso_results$qwen8B$ethnicity$auc

active_g_qwen14_tf <- lasso_results$qwen14B$gender$active
active_e_qwen14_tf <- lasso_results$qwen14B$ethnicity$active
acc_g_qwen14_tf <- lasso_results$qwen14B$gender$acc
acc_e_qwen14_tf <- lasso_results$qwen14B$ethnicity$acc
auc_g_qwen14_tf <- lasso_results$qwen14B$gender$auc
auc_e_qwen14_tf <- lasso_results$qwen14B$ethnicity$auc


################################################################################
# Console Summary
################################################################################

print(metrics)
