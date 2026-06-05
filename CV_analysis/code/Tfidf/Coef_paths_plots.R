plot(
  cv_fit_g_tf_chat$glmnet.fit,
  xvar  = "lambda",
  label = FALSE,
  xlab  = ""
)

mtext(
  expression(log(lambda)),
  side = 1,
  line = 3
)

mtext(
  "Number of features",
  side = 3,
  line = 2.5
)


abline(
  v = log(lambda_1se_g_tf_chat),
  lty = 2
)

usr <- par("usr")

par(xpd = NA)
text(log(lambda_1se_g_tf_chat), usr[4], labels = expression(lambda * "-1se"), pos = 3, offset = 0.6, cex = 0.95)


# coefficients along the full path
beta_mat <- as.matrix(cv_fit_g_tf_chat$glmnet.fit$beta)
lambda_seq <- cv_fit_g_tf_chat$glmnet.fit$lambda

# column closest to lambda_1se
lambda_idx <- which.min(abs(lambda_seq - lambda_1se_g_tf_chat))

# coefficients at lambda_1se
beta_1se <- beta_mat[, lambda_idx]

# active (non-zero) features
active_names <- rownames(active_g_tf_chat)
active_names <- setdiff(active_names, "(Intercept)")


for (feat in active_names) {
  
  y_val <- beta_1se[feat]
  if (y_val == 0) next
  
  text(
    x      = log(lambda_1se_g_gem_tf),
    y      = y_val,
    labels = feat,
    pos    = 4,
    offset = 0.6,
    cex    = 0.95
  )
}


ggsave(filename = "plots/coef_path_tfidf_chat.png")




plot(
  cv_fit_g_gem_tf$glmnet.fit,
  xvar  = "lambda",
  label = FALSE,
  xlab  = ""
)

mtext(
  expression(log(lambda)),
  side = 1,
  line = 3
)

mtext(
  "Number of features",
  side = 3,
  line = 2.5
  )



abline(
  v   = log(lambda_1se_g_gem_tf),
  lty = 2
)

usr <- par("usr")

par(xpd = NA)
text(log(lambda_1se_g_gem_tf), usr[4], labels = expression(lambda * "-1se"), pos = 3, offset = 0.6, cex = 0.95)

# coefficients along the full path
beta_mat   <- as.matrix(cv_fit_g_gem_tf$glmnet.fit$beta)
lambda_seq <- cv_fit_g_gem_tf$glmnet.fit$lambda

# column closest to lambda_1se
lambda_idx <- which.min(abs(lambda_seq - lambda_1se_g_gem_tf))

# coefficients at lambda_1se
beta_1se <- beta_mat[, lambda_idx]

# active (non-zero) features
active_names <- rownames(active_g_gem_tf)
active_names <- setdiff(active_names, "(Intercept)")

for (feat in active_names) {

  y_val <- beta_1se[feat]
  if (y_val == 0) next

  text(
    x      = log(lambda_1se_g_gem_tf),
    y      = y_val,
    labels = feat,
    pos    = 4,
    offset = 0.6,
    cex    = 0.95
  )
}

ggsave(filename = "plots/coef_path_tfidf_gem.png")

