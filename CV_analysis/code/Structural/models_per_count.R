library(dplyr)
library(lme4)

counts_per_p_group <- counts_per_p_group %>%
  select(
    profile_id,
    name_type,   # nur, wenn vorhanden / gewünscht
    gender,
    ethnicity,
    num_nouns,
    num_verbs,
    num_adjectives,
    num_numerals,
    cv_total_words,
    tokens,
    tentative_count,
    certainty_count,
    agentic_count,
    communal_count
  ) %>%
  mutate(
    gender    = factor(gender),
    ethnicity = factor(ethnicity),
    profile_id = factor(profile_id),
    name_type  = factor(name_type)
  )


resp_vars <- colnames(counts_per_p_group)[
  !colnames(counts_per_p_group) %in% c("profile_id", "name_type", "gender", "ethnicity")
]



models <- lapply(resp_vars, function(y) {
  f <- as.formula(
    paste0(y, " ~ gender + ethnicity + (1 | profile_id) + (1 | name_type)")
  )
  glmer(f, data = counts_per_p_group, family = poisson)
})


summary(models$num_nouns)
summary(models$num_verbs)
summary(models$num_adjectives)
summary(models$num_numerals)
summary(models$cv_total_words)
summary(models$tokens)
summary(models$tentative_count)
summary(models$certainty_count)
summary(models$agentic_count)
summary(models$communal_count)




### random slope: 

models_re <- lapply(resp_vars, function(y) {
  f <- as.formula(
    paste0(
      y,
      " ~ gender + ethnicity + (gender | profile_id) + (1 | name_type:gender) + (ethnicity | profile_id) + (1 | name_type:ethnicity)"
    )
  )
  glmer(f, data = counts_per_p_group, family = poisson(link = "log"))
})

names(models_re) <- resp_vars




summary(models_re$num_nouns)
summary(models_re$num_verbs)
summary(models_re$num_adjectives)
summary(models_re$num_numerals)
summary(models_re$cv_total_words)
summary(models_re$tokens)
summary(models_re$tentative_count)
summary(models_re$certainty_count)
summary(models_re$agentic_count)
summary(models_re$communal_count)




models_re_g <- lapply(resp_vars, function(y) {
  f <- as.formula(
    paste0(
      y,
      " ~ gender + (gender | profile_id) + (1 | name_type:gender)"
    )
  )
  glmer(f, data = counts_per_p_group, family = poisson(link = "log"))
})

names(models_re_g) <- resp_vars




summary(models_re_g$num_nouns)
summary(models_re_g$num_verbs)
summary(models_re_g$num_adjectives)
summary(models_re_g$num_numerals)
summary(models_re_g$cv_total_words)
summary(models_re_g$tokens)
summary(models_re_g$tentative_count)
summary(models_re_g$certainty_count)
summary(models_re_g$agentic_count)
summary(models_re_g$communal_count)




models_re_e <- lapply(resp_vars, function(y) {
  f <- as.formula(
    paste0(
      y,
      " ~ ethnicity + (ethnicity | profile_id) + (1 | name_type:ethnicity)"
    )
  )
  glmer(f, data = counts_per_p_group, family = poisson(link = "log"))
})

names(models_re_e) <- resp_vars




summary(models_re_e$num_nouns)
summary(models_re_e$num_verbs)
summary(models_re_e$num_adjectives)
summary(models_re_e$num_numerals)
summary(models_re_e$cv_total_words)
summary(models_re_e$tokens)
summary(models_re_e$tentative_count)
summary(models_re_e$certainty_count)
summary(models_re_e$agentic_count)
summary(models_re_e$communal_count)



