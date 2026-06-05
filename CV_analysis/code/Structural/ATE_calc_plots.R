library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

############################################
## ATE Visualization
############################################
count_dat_chat <- read.csv("data/full_count_data/count_dat_openai.csv")
count_dat_gem <- read.csv("data/full_count_data/count_dat_google.csv")

############################################
## Prep data:
############################################

## ChatGPT data:

counts_group_chat <- count_dat_chat %>% 
  select(-c(name_type, name_ID, first_name, last_name, provider, model)) %>% 
  mutate(ethnicity = as.factor(ethnicity)) %>% 
  mutate(gender = as.factor(gender))


feat_cols <- setdiff(names(counts_group_chat),
                     c("profile_id", "gender", "ethnicity", "total_tokens", "num_numerals", "num_pronons", "num_top_keys", 
                       "k01_persoenliche_daten_num_items", "k01_persoenliche_daten_num_subkeys", 
                       "k03_faehigkeiten_num_items", "k03_faehigkeiten_num_subkeys", "k04_berufserfahrung_num_items", 
                       "k05_ausbildung_num_items", "k05_ausbildung_num_items", "k06_skills_num_items", "k06_skills_num_subkeys", 
                       "k07_sprachen_num_items", "k07_sprachen_num_subkeys", "k08_interessen_num_items", "k09_angestrebte_position_num_items", 
                       "k09_angestrebte_position_num_subkeys", "k10_cover_letter_snippet_num_items"))


## Gemini data:

counts_group_gem <- count_dat_gem %>% 
  select(-c(name_type, name_ID, first_name, last_name, provider, model)) %>% 
  mutate(ethnicity = as.factor(ethnicity)) %>% 
  mutate(gender = as.factor(gender))



## Average per group:

## Chat gender:
avg_gender_chat <- counts_group_chat %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      list(
        avg_male   = ~ mean(.x[gender == "male"], na.rm = TRUE),
        avg_female = ~ mean(.x[gender == "female"], na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

avg_gender_long_chat <- avg_gender_chat %>%
  pivot_longer(
    cols = -profile_id,
    names_to = c("feature", ".value"),
    names_sep = "__"
  )


## Gemini gender:
avg_gender_gem <- counts_group_gem %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      list(
        avg_male   = ~ mean(.x[gender == "male"], na.rm = TRUE),
        avg_female = ~ mean(.x[gender == "female"], na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

avg_gender_long_gem <- avg_gender_gem %>%
  pivot_longer(
    cols = -profile_id,
    names_to = c("feature", ".value"),
    names_sep = "__"
  )



## Chat ethnicity:
avg_eth_chat <- counts_group_chat %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      list(
        avg_german   = ~ mean(.x[ethnicity == "german"], na.rm = TRUE),
        avg_turkish = ~ mean(.x[ethnicity == "turkish"], na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

avg_eth_long_chat <- avg_eth_chat %>%
  pivot_longer(
    cols = -profile_id,
    names_to = c("feature", ".value"),
    names_sep = "__"
  )


## Gemini ethnicity:
avg_eth_gem <- counts_group_gem %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      list(
        avg_german   = ~ mean(.x[ethnicity == "german"], na.rm = TRUE),
        avg_turkish = ~ mean(.x[ethnicity == "turkish"], na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

avg_eth_long_gem <- avg_eth_gem %>%
  pivot_longer(
    cols = -profile_id,
    names_to = c("feature", ".value"),
    names_sep = "__"
  )






## ate per group:

ate_gender_chat <- counts_group_chat %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      ~ mean(.x[gender == "male"], na.rm = TRUE) -
        mean(.x[gender == "female"], na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(effect = "gender_male_minus_female") %>% 
  select(profile_id, effect, everything())

ate_long_gender_chat <- ate_gender_chat %>%
  select(profile_id, all_of(feat_cols)) %>%
  pivot_longer(
    cols = all_of(feat_cols),
    names_to = "feature",
    values_to = "ATE"
  )

ate_long_gender_chat <- ate_long_gender_chat %>%
  left_join(
    avg_gender_long_chat,
    by = c("profile_id", "feature")
  )

feat_avgs_g_chat <- ate_long_gender_chat %>%
  group_by(feature) %>%
  summarise(
    avg_male_profile   = mean(avg_male, na.rm = TRUE),
    avg_female_profile = mean(avg_female, na.rm = TRUE),
    avg_ATE            = avg_male_profile - avg_female_profile,
    .groups = "drop"
  )



##Gemini:
ate_gender_gem <- counts_group_gem %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      ~ mean(.x[gender == "male"], na.rm = TRUE) -
        mean(.x[gender == "female"], na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(effect = "gender_male_minus_female") %>% 
  select(profile_id, effect, everything())


ate_long_gender_gem <- ate_gender_gem %>%
  select(profile_id, all_of(feat_cols)) %>%
  pivot_longer(
    cols = all_of(feat_cols),
    names_to = "feature",
    values_to = "ATE"
  )

ate_long_gender_gem <- ate_long_gender_gem %>%
  left_join(
    avg_gender_long_gem,
    by = c("profile_id", "feature")
  )

feat_avgs_g_gem <- ate_long_gender_gem %>%
  group_by(feature) %>%
  summarise(
    avg_male_profile   = mean(avg_male, na.rm = TRUE),
    avg_female_profile = mean(avg_female, na.rm = TRUE),
    avg_ATE            = avg_male_profile - avg_female_profile,
    .groups = "drop"
  )




## Ethnicity chat:

ate_ethnicity_chat <- counts_group_chat %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      ~ mean(.x[ethnicity == "german"], na.rm = TRUE) -
        mean(.x[ethnicity == "turkish"], na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(effect = "ethnicity_german_minus_turkish")


ate_long_eth_chat <- ate_ethnicity_chat %>%
  select(profile_id, all_of(feat_cols)) %>%
  pivot_longer(
    cols = all_of(feat_cols),
    names_to = "feature",
    values_to = "ATE"
  )

ate_long_eth_chat <- ate_long_eth_chat %>%
  left_join(
    avg_eth_long_chat,
    by = c("profile_id", "feature")
  )

feat_avgs_e_chat <- ate_long_eth_chat %>%
  group_by(feature) %>%
  summarise(
    avg_german_profile   = mean(avg_german, na.rm = TRUE),
    avg_turkish_profile = mean(avg_turkish, na.rm = TRUE),
    avg_ATE            = avg_german_profile - avg_turkish_profile,
    .groups = "drop"
  )




ate_ethnicity_gem <- counts_group_gem %>%
  group_by(profile_id) %>%
  summarise(
    across(
      all_of(feat_cols),
      ~ mean(.x[ethnicity == "german"], na.rm = TRUE) -
        mean(.x[ethnicity == "turkish"], na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(effect = "ethnicity_german_minus_turkish")



ate_long_eth_gem <- ate_ethnicity_gem %>%
  select(profile_id, all_of(feat_cols)) %>%
  pivot_longer(
    cols = all_of(feat_cols),
    names_to = "feature",
    values_to = "ATE"
  )


ate_long_eth_gem <- ate_long_eth_gem %>%
  left_join(
    avg_eth_long_gem,
    by = c("profile_id", "feature")
  )

feat_avgs_e_gem <- ate_long_eth_gem %>%
  group_by(feature) %>%
  summarise(
    avg_german_profile   = mean(avg_german, na.rm = TRUE),
    avg_turkish_profile = mean(avg_turkish, na.rm = TRUE),
    avg_ATE            = avg_german_profile - avg_turkish_profile,
    .groups = "drop"
  )


############################################
## Def. Feature Names: 
############################################


lexica_feat <- c(
  "agentic_count",
  "communal_count",
  "certainty_count",
  "tentative_count"
)

lexica_feat_names <- c(
  "Agentic",
  "Communal",
  "Certainty",
  "Tentative"
)

feature_names_lex <- stats::setNames(lexica_feat_names, lexica_feat)

pos_feat <- c("num_verbs", "num_adjectives", "num_numerals", "num_nouns")
pos_feat_names <- c(
  n_verbs = "Verbs",
  n_adj   = "Adjectives",
  n_num   = "Numerals", 
  num_nouns = "Nouns"
)
feature_names_pos <- stats::setNames(pos_feat_names, pos_feat)

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


struc_feat_names <- c(
  "Total words",
  "Personal",
  "Profile",
  "Competences",
  "Work experience",
  "Education",
  "Skills",
  "Languages",
  "Interests",
  "Position",
  "Cover Letter"
)

feature_names_struc <- stats::setNames(struc_feat_names, struc_feat)

# struc_feat_min <- c("cv_total_words",
#                     "k01_persoenliche_daten_num_words",
#                     "k02_profil_num_words",
#                     "k04_berufserfahrung_num_words", 
#                     "k05_ausbildung_num_words", 
#                     "k06_skills_num_words",
#                     "k08_interessen_num_words", 
#                     "k10_cover_letter_snippet_num_words")
# 
# struc_feat_min_names <- c("Total words",
#                             "Personal",
#                             "Profile",
#                             "Work experience",
#                             "Education",
#                             "Skills",
#                             "Interests",
#                             "Cover Letter")
# 
# feature_names_struc_min <- stats::setNames(struc_feat_min_names, struc_feat_min)



struc_feat_min <- c("cv_total_words",
                    "k01_persoenliche_daten_num_words",
                    "k02_profil_num_words",
                    "k05_ausbildung_num_words", 
                    "k08_interessen_num_words", 
                    "k10_cover_letter_snippet_num_words")

struc_feat_min_names <- c("Total words",
                          "Personal",
                          "Profile",
                          "Education",
                          "Interests",
                          "Cover Letter")

feature_names_struc_min <- stats::setNames(struc_feat_min_names, struc_feat_min)





############################################
## Plot functions single plots:
############################################


plot_ate_grid <- function(ate_df, ate_col, x_label = "ATE") {

  plots <- vector("list", length = length(unique(ate_df$feature)))
  f_list <- unique(ate_df$feature)

  for (i in seq_along(f_list)) {

    f <- f_list[i]

    df_f <- ate_df %>% 
      filter(feature == f)

    p <- ggplot(df_f,
                aes(x = .data[[ate_col]], y = reorder(profile_id, .data[[ate_col]]))) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point() +
      labs(
        title = f,
        x = x_label,
        y = "profile_id"
      ) +
      theme_bw()

    plots[[i]] <- p
  }

  (wrap_plots(plots, ncol = 2))
}


boxplot_ate <- function(ate_df, ate_col, x_label = "ATE") {
  
  for (f in unique(ate_df$feature)) {
    
    df_f <- ate_df %>% 
      filter(feature == f)
    
    p <- ggplot(df_f,
                aes(x = .data[[ate_col]], y = 1)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_boxplot() +
      labs(
        title = f,
        x = x_label,
        y = NULL
      ) +
      theme_bw() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    print(p)
  }
}

boxplot_ate_grid <- function(ate_df, ate_col, x_label = "ATE",
                             features = NULL, nrow = NULL, ncol = NULL) {
  
  if (is.null(features)) {
    features <- unique(ate_df$feature)
  }
  
  plots <- vector("list", length = length(features))
  
  for (i in seq_along(features)) {
    
    f <- features[i]
    
    df_f <- ate_df %>%
      filter(feature == f)
    
    p <- ggplot(df_f,
                aes(x = .data[[ate_col]], y = 1)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_boxplot() +
      labs(
        title = f,
        x = x_label,
        y = NULL
      ) +
      theme_bw() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    plots[[i]] <- p
  }
  
  wrap_plots(plots, nrow = nrow, ncol = ncol)
}


############################################
## Boxplots per Group per Model
############################################


### Chat Gender
boxplot_ate_grid(ate_long_gender_chat,
            ate_col = "ATE",
            x_label = "ATE (male - female)", 
            lexica_feat, 2, 2)

boxplot_ate_grid(ate_long_gender_chat,
                 ate_col = "ATE",
                 x_label = "ATE (male - female)", 
                 pos_feat, 2, 2)

boxplot_ate_grid(ate_long_gender_chat,
                 ate_col = "ATE",
                 x_label = "ATE (male - female)", 
                 struc_feat, 4, 3)



### Chat Ethnicity

boxplot_ate_grid(ate_long_eth_chat,
                 ate_col = "ATE",
                 x_label = "ATE (german - turkish)", 
                 lexica_feat, 2, 2)

boxplot_ate_grid(ate_long_eth_chat,
                 ate_col = "ATE",
                 x_label = "ATE (german - turkish)", 
                 pos_feat, 2, 2)

boxplot_ate_grid(ate_long_eth_chat,
                 ate_col = "ATE",
                 x_label = "ATE (german - turkish)", 
                 struc_feat, 4, 3)



### Gemini Gender
boxplot_ate_grid(ate_long_gender_gem,
                 ate_col = "ATE",
                 x_label = "ATE (male - female)", 
                 lexica_feat, 2, 2)

boxplot_ate_grid(ate_long_gender_gem,
                 ate_col = "ATE",
                 x_label = "ATE (male - female)", 
                 pos_feat, 2, 2)

boxplot_ate_grid(ate_long_gender_gem,
                 ate_col = "ATE",
                 x_label = "ATE (male - female)", 
                 struc_feat, 4, 3)



### Gemini Ethnicity

boxplot_ate_grid(ate_long_eth_gem,
                 ate_col = "ATE",
                 x_label = "ATE (german - turkish)", 
                 lexica_feat, 2, 2)

boxplot_ate_grid(ate_long_eth_gem,
                 ate_col = "ATE",
                 x_label = "ATE (german - turkish)", 
                 pos_feat, 2, 2)

boxplot_ate_grid(ate_long_eth_gem,
                 ate_col = "ATE",
                 x_label = "ATE (german - turkish)", 
                 struc_feat, 4, 3)





############################################
## Boxplots Comparison Models:
############################################

boxplot_ate_grid_two_models <- function(
    ate_df_chat,
    ate_df_gem,
    ate_col,
    x_label = "ATE",
    features = NULL,
    feature_names = NULL,
    llm_colors = c("#a6611a", "#018571"),
    limits = NULL,
    feat_avgs_chat = NULL,
    feat_avgs_gem  = NULL
) {
  if (is.null(names(llm_colors))) {
    llm_colors <- c(Gemini = llm_colors[1], ChatGPT = llm_colors[2])
  }
  
  if (is.null(features)) {
    features <- intersect(unique(ate_df_chat$feature), unique(ate_df_gem$feature))
  }
  
  df_all <- bind_rows(
    ate_df_chat %>% filter(feature %in% features) %>% mutate(llm = "ChatGPT"),
    ate_df_gem  %>% filter(feature %in% features) %>% mutate(llm = "Gemini")
  )
  
  if (is.null(limits)) {
    rng <- range(df_all[[ate_col]], na.rm = TRUE)
    max_abs <- max(abs(rng))
    limits <- c(-max_abs, max_abs)
  }
  
  ann_df <- NULL
  if (!is.null(feat_avgs_chat) && !is.null(feat_avgs_gem)) {
    ann_chat <- feat_avgs_chat %>%
      select(feature = 1, avg_g1 = 2, avg_g2 = 3, avg_ATE = 4) %>%
      mutate(llm = "ChatGPT")
    
    ann_gem <- feat_avgs_gem %>%
      select(feature = 1, avg_g1 = 2, avg_g2 = 3, avg_ATE = 4) %>%
      mutate(llm = "Gemini")
    
    ann_df <- bind_rows(ann_chat, ann_gem) %>%
      filter(feature %in% features)
    
    if (!is.null(feature_names)) {
      ann_df$feature <- recode(ann_df$feature, !!!feature_names)
    }
    
    ann_df$llm <- factor(ann_df$llm, levels = c("ChatGPT", "Gemini"))
    
    is_gender <- grepl("male\\s*-\\s*female", x_label)
    
    ann_df <- ann_df %>%
      mutate(
        label = if (is_gender) {
          sprintf(
            "avg. female=%.2f\navg. male=%.2f\navg. ATE=%.2f",
            avg_g2, avg_g1, avg_ATE
          )
        } else {
          sprintf(
            "avg. german=%.2f\navg. turkish=%.2f",
            avg_g1, avg_g2
            # "\nATE=%.2f%%", avg_ATE_pct
          )
          
        }
      )
  }
  
  if (!is.null(feature_names)) {
    df_all$feature <- recode(df_all$feature, !!!feature_names)
  }
  
  df_all$llm <- factor(df_all$llm, levels = c("ChatGPT", "Gemini"))
  
  p <- ggplot(df_all, aes(x = .data[[ate_col]], y = "")) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_boxplot(alpha = 0.5, outlier.alpha = 0.6, aes(fill = llm)) +
    facet_grid(feature ~ llm, switch = "y") +
    scale_fill_manual(values = llm_colors, name = "Model") +
    scale_x_continuous(
      limits = limits,
      breaks = seq(limits[1], limits[2], by = 1)
    ) +
    labs(
      x = x_label,
      y = "Num. words"
    ) +
    theme_bw() +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 90),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(angle = 90, vjust = 0.5),
      panel.spacing.y = unit(0.2, "lines"),
      panel.spacing.x = unit(0.8, "lines"),
      legend.position = "none"
    )
  
  if (!is.null(ann_df)) {
    p <- p +
      geom_text(
        data = ann_df,
        aes(x = Inf, y = Inf, label = label),
        inherit.aes = FALSE,
        hjust = 1.05,
        vjust = 1.15,
        size = 2
      ) +
      coord_cartesian(clip = "off")
  }
  
  p
}



boxplot_ate_grid_two_models_sd <- function(
    ate_df_chat,
    ate_df_gem,
    ate_col,
    x_label = "ATE",
    features = NULL,
    feature_names = NULL,
    llm_colors = c("#a6611a", "#018571"),
    limits = NULL,
    feat_avgs_chat = NULL,
    feat_avgs_gem  = NULL
) {
  
  if (is.null(names(llm_colors))) {
    llm_colors <- c(Gemini = llm_colors[1], ChatGPT = llm_colors[2])
  }
  
  if (is.null(features)) {
    features <- intersect(unique(ate_df_chat$feature),
                          unique(ate_df_gem$feature))
  }
  
  df_all <- bind_rows(
    ate_df_chat %>% filter(feature %in% features) %>% mutate(llm = "ChatGPT"),
    ate_df_gem  %>% filter(feature %in% features) %>% mutate(llm = "Gemini")
  )
  
  ann_df <- NULL
  if (!is.null(feat_avgs_chat) && !is.null(feat_avgs_gem)) {
    
    ann_chat <- feat_avgs_chat %>%
      select(feature = 1, avg_g1 = 2, avg_g2 = 3, avg_ATE = 4) %>%
      mutate(llm = "ChatGPT")
    
    ann_gem <- feat_avgs_gem %>%
      select(feature = 1, avg_g1 = 2, avg_g2 = 3, avg_ATE = 4) %>%
      mutate(llm = "Gemini")
    
    ann_df <- bind_rows(ann_chat, ann_gem) %>%
      filter(feature %in% features)
    
    if (!is.null(feature_names)) {
      ann_df$feature <- recode(ann_df$feature, !!!feature_names)
    }
    
    ann_df$llm <- factor(ann_df$llm, levels = c("ChatGPT", "Gemini"))
    
    ann_df <- ann_df %>%
      mutate(
        denom = 0.5 * (avg_g1 + avg_g2),
        denom = ifelse(is.na(denom) | denom == 0, NA_real_, denom),
        avg_ATE_pct = 100 * (avg_ATE / denom)
      )
    
    is_gender <- grepl("male\\s*-\\s*female", x_label)
    
    # plotmath string rendered with parse=TRUE; use atop() instead of newline
    ann_df <- ann_df %>%
      mutate(
        label = if (is_gender) {
          paste0(
            "atop(",
            "bar(female)==", sprintf("%.2f", avg_g2), ",",
            "bar(male)==",   sprintf("%.2f", avg_g1),
            ")"
          )
        } else {
          paste0(
            "atop(",
            "bar(german)==",  sprintf("%.2f", avg_g1), ",",
            "bar(turkish)==", sprintf("%.2f", avg_g2),
            ")"
          )
        }
      )
  }
  
  if (!is.null(feature_names)) {
    df_all$feature <- recode(df_all$feature, !!!feature_names)
  }
  
  df_all$llm <- factor(df_all$llm, levels = c("ChatGPT", "Gemini"))
  
  if (!is.null(ann_df)) {
    denom_map <- ann_df %>% select(feature, llm, denom)
    
    df_all <- df_all %>%
      left_join(denom_map, by = c("feature", "llm")) %>%
      mutate(ate_pct = 100 * (.data[[ate_col]] / denom))
    
    plot_x <- "ate_pct"
  } else {
    plot_x <- ate_col
  }
  
  if (is.null(limits)) {
    rng <- range(df_all[[plot_x]], na.rm = TRUE)
    max_abs <- max(abs(rng))
    limits <- c(-max_abs, max_abs)
  }
  
  p <- ggplot(df_all, aes(x = .data[[plot_x]], y = "")) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_boxplot(alpha = 0.5, outlier.alpha = 0.6, aes(fill = llm)) +
    facet_grid(feature ~ llm, switch = "y") +
    scale_fill_manual(values = llm_colors, name = "Model") +
    scale_x_continuous(
      limits = limits,
      breaks = pretty(limits, n = 5)
    ) +
    labs(
      x = paste0(x_label, " (%)"),
      y = "Num. words"
    ) +
    theme_bw() +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 90),
      axis.text.x  = element_text(size = 12),
      axis.title.x = element_text(size = 13),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(angle = 90, vjust = 0.5, size = 13),
      panel.spacing.y = unit(0.2, "lines"),
      panel.spacing.x = unit(0.8, "lines"),
      legend.position = "none"
    )
  
  if (!is.null(ann_df)) {
    p <- p +
      geom_text(
        data = ann_df,
        aes(x = Inf, y = Inf, label = label),
        inherit.aes = FALSE,
        hjust = 1.05,
        vjust = 1.15,
        size = 4,
        parse = TRUE
      ) +
      coord_cartesian(clip = "off")
  }
  
  p
}



### 1. Gender ATEs:

## Gender POS: 
boxplot_ate_grid_two_models(
  ate_df_chat = ate_long_gender_chat,
  ate_df_gem  = ate_long_gender_gem,
  ate_col     = "ATE",
  x_label     = "Avg. difference (male - female)",
  features    = pos_feat,
  feature_names = feature_names_pos, 
  limits = c(-4, 4), 
  feat_avgs_chat = feat_avgs_g_chat, 
  feat_avgs_gem = feat_avgs_g_gem

)


## Gender POS: 
boxplot_ate_grid_two_models_sd(
  ate_df_chat = ate_long_gender_chat,
  ate_df_gem  = ate_long_gender_gem,
  ate_col     = "ATE",
  x_label     = "Avg. difference (male - female)",
  features    = pos_feat,
  feature_names = feature_names_pos, 
  limits = c(-20, 20),
  feat_avgs_chat = feat_avgs_g_chat, 
  feat_avgs_gem = feat_avgs_g_gem
  
)

# ggsave(
#   filename = "plots/boxplot_ate_pos_gender.png",
#   width    = 10,
#   height   = 6)

## Gender lexica: 

boxplot_ate_grid_two_models(
  ate_df_chat = ate_long_gender_chat,
  ate_df_gem  = ate_long_gender_gem,
  ate_col     = "ATE",
  x_label     = "ATE (male - female)",
  features    = lexica_feat,
  feature_names = feature_names_lex, 
  limits = c(-3, 3), 
  feat_avgs_chat = feat_avgs_g_chat, 
  feat_avgs_gem = feat_avgs_g_gem
  
  
)



boxplot_ate_grid_two_models_sd(
  ate_df_chat = ate_long_gender_chat,
  ate_df_gem  = ate_long_gender_gem,
  ate_col     = "ATE",
  x_label     = "Avg. difference (male - female)",
  features    = lexica_feat,
  feature_names = feature_names_lex, 
  limits = c(-20, 20),
  feat_avgs_chat = feat_avgs_g_chat, 
  feat_avgs_gem = feat_avgs_g_gem
  
  
)

# ggsave(
#   filename = "plots/boxplot_ate_lexical_gender.png",
#   width    = 10,
#   height   = 6)



## Gender struc: 
boxplot_ate_grid_two_models(
  ate_df_chat = ate_long_gender_chat,
  ate_df_gem  = ate_long_gender_gem,
  ate_col     = "ATE",
  x_label     = "ATE (male - female)",
  features    = struc_feat_min,
  feature_names = feature_names_struc_min, 
  limits = c(-10, 10), 
  feat_avgs_chat = feat_avgs_g_chat, 
  feat_avgs_gem = feat_avgs_g_gem
)


boxplot_ate_grid_two_models_sd(
  ate_df_chat = ate_long_gender_chat,
  ate_df_gem  = ate_long_gender_gem,
  ate_col     = "ATE",
  x_label     = "Avg. Difference (male - female)",
  features    = struc_feat_min,
  feature_names = feature_names_struc_min, 
  limits = c(-20, 20),
  feat_avgs_chat = feat_avgs_g_chat, 
  feat_avgs_gem = feat_avgs_g_gem
  
  
)

# ggsave(
#   filename = "plots/boxplot_ate_struc_gender.png",
#   width    = 10,
#   height   = 6)
# 




### 2. Ethnicity ATEs:

## Ethnicity POS: 
boxplot_ate_grid_two_models(
  ate_df_chat = ate_long_eth_chat,
  ate_df_gem  = ate_long_eth_gem,
  ate_col     = "ATE",
  x_label     = "ATE (german - turkish)",
  features    = pos_feat,
  feature_names = feature_names_pos, 
  limits = c(-4, 4), 
  feat_avgs_chat = feat_avgs_e_chat, 
  feat_avgs_gem = feat_avgs_e_gem
  
  
  
)

## Ethnicity lexica: 

boxplot_ate_grid_two_models(
  ate_df_chat = ate_long_eth_chat,
  ate_df_gem  = ate_long_eth_gem,
  ate_col     = "ATE",
  x_label     = "ATE (german - turkish)",
  features    = lexica_feat,
  feature_names = feature_names_lex, 
  limits = c(-3, 3), 
  feat_avgs_chat = feat_avgs_e_chat, 
  feat_avgs_gem = feat_avgs_e_gem
  
  
)

## Ethnicity struc: 
boxplot_ate_grid_two_models(
  ate_df_chat = ate_long_eth_chat,
  ate_df_gem  = ate_long_eth_gem,
  ate_col     = "ATE",
  x_label     = "ATE (german - turkish)",
  features    = struc_feat_min,
  feature_names = feature_names_struc_min, 
  limits = c(-10, 10), 
  feat_avgs_chat = feat_avgs_e_chat, 
  feat_avgs_gem = feat_avgs_e_gem
  
)



