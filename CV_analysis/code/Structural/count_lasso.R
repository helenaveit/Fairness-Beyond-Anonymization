## Count-based LASSO models for ChatGPT, Gemini, Qwen 4B, and Qwen 8B

library(dplyr)
library(tidyr)
library(glmnet)
library(pROC)

find_cv_root <- function() {
  candidates <- c("CV_analysis", ".", file.path("..", ".."))
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "data", "full_count_data", "count_dat_openai.csv"))) {
      return(normalizePath(candidate))
    }
  }
  stop("Could not find CV_analysis root with data/full_count_data/count_dat_openai.csv")
}

cv_root <- find_cv_root()
count_dir <- file.path(cv_root, "data", "full_count_data")
out_dir <- file.path(cv_root, "data", "lasso_results")
plot_dir <- file.path(cv_root, "plots", "lasso_count")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

models <- tibble::tribble(
  ~model_key, ~model_label, ~file,
  "chat", "ChatGPT", "count_dat_openai.csv",
  "gem", "Gemini", "count_dat_google.csv",
  "qwen4B", "Qwen 4B", "count_dat_qwen4B.csv",
  "qwen8B", "Qwen 8B", "count_dat_qwen8B.csv"
)

## Feature columns: lexical, POS, and structural counts.
feat_cols_count <- c(
  "profile_id",
  "gender",
  "ethnicity",
  "agentic_count",
  "communal_count",
  "certainty_count",
  "tentative_count",
  "num_verbs",
  "num_adjectives",
  "num_adverbs",
  # "num_numerals",
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

read_count_data <- function(file) {
  read.csv(file.path(count_dir, file), check.names = FALSE) %>%
    select(all_of(feat_cols_count)) %>%
    mutate(
      gender = factor(gender, levels = c("female", "male")),
      ethnicity = factor(ethnicity, levels = c("turkish", "german")),
      across(where(is.numeric), ~ replace_na(.x, 0))
    )
}

count_data <- setNames(
  lapply(models$file, read_count_data),
  models$model_key
)

df_openai <- count_data$chat
df_gemini <- count_data$gem
df_qwen4B <- count_data$qwen4B
df_qwen8B <- count_data$qwen8B

set.seed(42)
profiles <- unique(df_openai$profile_id)
train_ids <- sample(profiles, size = floor(0.8 * length(profiles)))

profile_fold <- sample(rep(1:3, length.out = length(profiles)))
fold_map <- setNames(profile_fold, profiles)

make_xy <- function(df, train_ids, outcome, positive_value) {
  train_df <- df %>% filter(profile_id %in% train_ids)
  test_df <- df %>% filter(!profile_id %in% train_ids)

  list(
    train_df = train_df,
    test_df = test_df,
    y_train = ifelse(train_df[[outcome]] == positive_value, 1, 0),
    y_test = ifelse(test_df[[outcome]] == positive_value, 1, 0),
    x_train = train_df %>% select(-profile_id, -gender, -ethnicity) %>% as.matrix(),
    x_test = test_df %>% select(-profile_id, -gender, -ethnicity) %>% as.matrix(),
    foldid = as.numeric(fold_map[as.character(train_df$profile_id)])
  )
}

run_count_lasso <- function(df, train_ids, outcome, positive_value) {
  dat <- make_xy(df, train_ids, outcome, positive_value)

  cv_fit <- cv.glmnet(
    x = dat$x_train,
    y = dat$y_train,
    family = "binomial",
    alpha = 1,
    foldid = dat$foldid
  )

  lambda_min <- cv_fit$lambda.min
  lambda_1se <- cv_fit$lambda.1se

  lasso_fit <- glmnet(
    x = dat$x_train,
    y = dat$y_train,
    family = "binomial",
    alpha = 1,
    lambda = lambda_1se
  )

  coef_fit <- coef(lasso_fit)
  active <- coef_fit[coef_fit[, 1] != 0, , drop = FALSE]

  pred_probs <- predict(lasso_fit, newx = dat$x_test, type = "response")
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)
  acc <- mean(pred_class == dat$y_test)
  roc_fit <- roc(response = dat$y_test, predictor = as.numeric(pred_probs), quiet = TRUE)
  auc_fit <- auc(roc_fit)

  list(
    train_df = dat$train_df,
    test_df = dat$test_df,
    x_train = dat$x_train,
    x_test = dat$x_test,
    y_train = dat$y_train,
    y_test = dat$y_test,
    foldid = dat$foldid,
    cv_fit = cv_fit,
    lambda_min = lambda_min,
    lambda_1se = lambda_1se,
    lasso = lasso_fit,
    coef = coef_fit,
    active = active,
    pred_probs = pred_probs,
    pred_class = pred_class,
    acc = acc,
    roc = roc_fit,
    auc = auc_fit
  )
}

save_cv_plot <- function(cv_fit, lambda_min, lambda_1se, filename) {
  png(file.path(plot_dir, filename), width = 1600, height = 1100, res = 180)
  plot(cv_fit$glmnet.fit, xvar = "lambda")
  abline(v = log(lambda_1se), col = "red", lty = 2)
  abline(v = log(lambda_min), col = "black", lty = 2)
  dev.off()
}

lasso_results <- list()

for (i in seq_len(nrow(models))) {
  model_key <- models$model_key[i]
  model_label <- models$model_label[i]
  df_model <- count_data[[model_key]]

  gender_result <- run_count_lasso(
    df_model,
    train_ids = train_ids,
    outcome = "gender",
    positive_value = "male"
  )

  ethnicity_result <- run_count_lasso(
    df_model,
    train_ids = train_ids,
    outcome = "ethnicity",
    positive_value = "german"
  )

  lasso_results[[model_key]] <- list(
    model_label = model_label,
    gender = gender_result,
    ethnicity = ethnicity_result
  )

  save_cv_plot(
    gender_result$cv_fit,
    gender_result$lambda_min,
    gender_result$lambda_1se,
    paste0("count_lasso_gender_", model_key, ".png")
  )
  save_cv_plot(
    ethnicity_result$cv_fit,
    ethnicity_result$lambda_min,
    ethnicity_result$lambda_1se,
    paste0("count_lasso_ethnicity_", model_key, ".png")
  )
}

lasso_summary <- bind_rows(lapply(names(lasso_results), function(model_key) {
  model_result <- lasso_results[[model_key]]
  bind_rows(
    data.frame(
      model_key = model_key,
      model_label = model_result$model_label,
      outcome = "gender_male",
      lambda_min = model_result$gender$lambda_min,
      lambda_1se = model_result$gender$lambda_1se,
      accuracy = model_result$gender$acc,
      auc = as.numeric(model_result$gender$auc),
      n_active_features = nrow(model_result$gender$active) - 1
    ),
    data.frame(
      model_key = model_key,
      model_label = model_result$model_label,
      outcome = "ethnicity_german",
      lambda_min = model_result$ethnicity$lambda_min,
      lambda_1se = model_result$ethnicity$lambda_1se,
      accuracy = model_result$ethnicity$acc,
      auc = as.numeric(model_result$ethnicity$auc),
      n_active_features = nrow(model_result$ethnicity$active) - 1
    )
  )
}))

write.csv(lasso_summary, file.path(out_dir, "count_lasso_summary_by_model.csv"), row.names = FALSE)

write_active_features <- function(model_key, outcome, active_mat) {
  active_df <- data.frame(
    feature = rownames(active_mat),
    coefficient = as.numeric(active_mat[, 1]),
    row.names = NULL
  ) %>%
    filter(feature != "(Intercept)")

  write.csv(
    active_df,
    file.path(out_dir, paste0("count_lasso_active_", outcome, "_", model_key, ".csv")),
    row.names = FALSE
  )
}

for (model_key in names(lasso_results)) {
  write_active_features(model_key, "gender", lasso_results[[model_key]]$gender$active)
  write_active_features(model_key, "ethnicity", lasso_results[[model_key]]$ethnicity$active)
}

## Backward-compatible object names for the interactive workflow.
train_df_chat <- lasso_results$chat$gender$train_df
test_df_chat <- lasso_results$chat$gender$test_df
x_train_chat <- lasso_results$chat$gender$x_train
x_test_chat <- lasso_results$chat$gender$x_test
y_train_g_chat <- lasso_results$chat$gender$y_train
y_test_g_chat <- lasso_results$chat$gender$y_test
y_train_e_chat <- lasso_results$chat$ethnicity$y_train
y_test_e_chat <- lasso_results$chat$ethnicity$y_test
cv_fit_g_chat <- lasso_results$chat$gender$cv_fit
cv_fit_e_chat <- lasso_results$chat$ethnicity$cv_fit
lasso_g_chat <- lasso_results$chat$gender$lasso
lasso_e_chat <- lasso_results$chat$ethnicity$lasso
active_g_chat <- lasso_results$chat$gender$active
active_e_chat <- lasso_results$chat$ethnicity$active
acc_g_chat <- lasso_results$chat$gender$acc
acc_e_chat <- lasso_results$chat$ethnicity$acc
auc_g_chat <- lasso_results$chat$gender$auc
auc_e_chat <- lasso_results$chat$ethnicity$auc

train_df_gem <- lasso_results$gem$gender$train_df
test_df_gem <- lasso_results$gem$gender$test_df
x_train_gem <- lasso_results$gem$gender$x_train
x_test_gem <- lasso_results$gem$gender$x_test
y_train_g_gem <- lasso_results$gem$gender$y_train
y_test_g_gem <- lasso_results$gem$gender$y_test
y_train_e_gem <- lasso_results$gem$ethnicity$y_train
y_test_e_gem <- lasso_results$gem$ethnicity$y_test
cv_fit_g_gem <- lasso_results$gem$gender$cv_fit
cv_fit_e_gem <- lasso_results$gem$ethnicity$cv_fit
lasso_g_gem <- lasso_results$gem$gender$lasso
lasso_e_gem <- lasso_results$gem$ethnicity$lasso
active_g_gem <- lasso_results$gem$gender$active
active_e_gem <- lasso_results$gem$ethnicity$active
acc_g_gem <- lasso_results$gem$gender$acc
acc_e_gem <- lasso_results$gem$ethnicity$acc
auc_g_gem <- lasso_results$gem$gender$auc
auc_e_gem <- lasso_results$gem$ethnicity$auc

train_df_qwen4B <- lasso_results$qwen4B$gender$train_df
test_df_qwen4B <- lasso_results$qwen4B$gender$test_df
x_train_qwen4B <- lasso_results$qwen4B$gender$x_train
x_test_qwen4B <- lasso_results$qwen4B$gender$x_test
y_train_g_qwen4B <- lasso_results$qwen4B$gender$y_train
y_test_g_qwen4B <- lasso_results$qwen4B$gender$y_test
y_train_e_qwen4B <- lasso_results$qwen4B$ethnicity$y_train
y_test_e_qwen4B <- lasso_results$qwen4B$ethnicity$y_test
cv_fit_g_qwen4B <- lasso_results$qwen4B$gender$cv_fit
cv_fit_e_qwen4B <- lasso_results$qwen4B$ethnicity$cv_fit
lasso_g_qwen4B <- lasso_results$qwen4B$gender$lasso
lasso_e_qwen4B <- lasso_results$qwen4B$ethnicity$lasso
active_g_qwen4B <- lasso_results$qwen4B$gender$active
active_e_qwen4B <- lasso_results$qwen4B$ethnicity$active
acc_g_qwen4B <- lasso_results$qwen4B$gender$acc
acc_e_qwen4B <- lasso_results$qwen4B$ethnicity$acc
auc_g_qwen4B <- lasso_results$qwen4B$gender$auc
auc_e_qwen4B <- lasso_results$qwen4B$ethnicity$auc

train_df_qwen8B <- lasso_results$qwen8B$gender$train_df
test_df_qwen8B <- lasso_results$qwen8B$gender$test_df
x_train_qwen8B <- lasso_results$qwen8B$gender$x_train
x_test_qwen8B <- lasso_results$qwen8B$gender$x_test
y_train_g_qwen8B <- lasso_results$qwen8B$gender$y_train
y_test_g_qwen8B <- lasso_results$qwen8B$gender$y_test
y_train_e_qwen8B <- lasso_results$qwen8B$ethnicity$y_train
y_test_e_qwen8B <- lasso_results$qwen8B$ethnicity$y_test
cv_fit_g_qwen8B <- lasso_results$qwen8B$gender$cv_fit
cv_fit_e_qwen8B <- lasso_results$qwen8B$ethnicity$cv_fit
lasso_g_qwen8B <- lasso_results$qwen8B$gender$lasso
lasso_e_qwen8B <- lasso_results$qwen8B$ethnicity$lasso
active_g_qwen8B <- lasso_results$qwen8B$gender$active
active_e_qwen8B <- lasso_results$qwen8B$ethnicity$active
acc_g_qwen8B <- lasso_results$qwen8B$gender$acc
acc_e_qwen8B <- lasso_results$qwen8B$ethnicity$acc
auc_g_qwen8B <- lasso_results$qwen8B$gender$auc
auc_e_qwen8B <- lasso_results$qwen8B$ethnicity$auc

print(lasso_summary)
cat("Saved count LASSO summary to:", file.path(out_dir, "count_lasso_summary_by_model.csv"), "\n")
cat("Saved count LASSO coefficient plots to:", plot_dir, "\n")
