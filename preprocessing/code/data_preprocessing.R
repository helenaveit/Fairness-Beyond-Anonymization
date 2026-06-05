# data_preprocessing.R
# ------------------------------------------------------------------------------
# End-to-end pipeline:
#   RAW (no ID) → CLEAN (strings) → CLEAN_SPLIT (list-cols)
#   → explode WEITERE languages → LONG → WIDE
# Saves LONG and WIDE to /output.
# ------------------------------------------------------------------------------

# Load required packages
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)
library(stats)
library(ggplot2)


# Load helper functions
source("preprocessing/code/helpers.R")
source("preprocessing/code/transform_fns.R")

# --- 0) Paths -----------------------------------------------------------------
path_raw <- "preprocessing/data/jobmatch.csv"  # original RAW file

if (!file.exists(path_raw)) {
  stop("Expected RAW CSV at ",
  path_raw,
  ". Provide one column per variable; NO ID needed.")
}

# --- 1) Load RAW --------------------------------------------------------------
data_raw <- read_csv(path_raw, show_col_types = FALSE)
message("Loaded RAW:")
print(overview_tbl(data_raw))

# --- 2a) RAW → CLEAN (normalize strings; keep as plain columns) ---------------
data_clean <- raw_to_clean(
  data_raw,
  max_missing = 0L,          # set Inf to keep all rows
  treat_empty_as_na = TRUE
)
message("Built CLEAN (strings).")
print(overview_tbl(data_clean))

# --- 2b) CLEAN → CLEAN_SPLIT (split cells into list-cols) ---------------------
data_clean_split <- clean_to_clean_split(
  data_clean,
  seps = c(";")
)
message("Built CLEAN_SPLIT (list-cols). Columns: ", ncol(data_clean_split))

# --- 2c) Explode per-language columns from 'WEITERE Sprachkompetenzen' --------
data_clean_split <- explode_weitere_sprachkompetenzen(
  data_clean_split,
  fill_missing = "keine Angabe",
  drop_original = TRUE
)
message("Exploded per-language columns. Columns: ", ncol(data_clean_split))

# --- 3) CLEAN_SPLIT → LONG ----------------------------------------------------
data_long <- clean_to_long(data_clean_split)
message("Built LONG:")
print(overview_tbl(data_long))

# --- 4) LONG → WIDE -----------------------------------------------------------
data_wide <- long_to_wide(data_long, fill = 0L)
message("Built WIDE with ", ncol(data_wide), " columns.")

# --- 5) WIDE → ONE-HOT ENCODED ------------------------------------------------

data_one_hot <- wide_to_one_hot(data_wide, sc_prefix = "")
message("Built ONE-HOT ENCODED WIDE with ", ncol(data_one_hot), " columns.")

# --- 6) Sample 30 IDs for the experiment --------------------------------------

# Set random seed for reproducibility
set.seed(798632)
sample_size <- 30

sample_ids <- sample(data_clean$ID, size = sample_size)

sample_data_clean <- data_clean %>%
  filter(ID %in% sample_ids)

# get response counts for the sampled IDs
response_counts <- data_one_hot %>%
  rowwise() %>%
  mutate(response_counts = sum(c_across(-ID))) %>%
  select(ID, response_counts)
sample_response_counts <- data_one_hot %>%
  filter(ID %in% sample_ids) %>%
  rowwise() %>%
  mutate(response_counts = sum(c_across(-ID))) %>%
  select(ID, response_counts)

combined_response_counts <- bind_rows(
  response_counts %>% mutate(group = "Overall"),
  sample_response_counts %>% mutate(group = "Sampled")
)

overall_average_responses <- mean(response_counts$response_counts)
sample_average_responses <- mean(sample_response_counts$response_counts)

message("Sampled ", sample_size, " IDs for the experiment. Response Counts:")
print(sample_response_counts)
message("Overall average responses per ID: ", round(overall_average_responses, 2))
message("Sample average responses per ID: ", round(sample_average_responses, 2))

# Histogram of response counts
hist_response_counts <- ggplot(combined_response_counts, aes(x = response_counts, fill = group)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 10,
                 alpha = 0.4,
                 position = "identity") +
  labs(
    x = "Number of Responses",
    y = "Relative Frequency",
    fill = "Dataset",
    color = "Dataset"
  ) +
  geom_vline(aes(xintercept = overall_average_responses, color = "Overall Average"),
             linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = sample_average_responses, color = "Sample Average"),
             linetype = "dashed", size = 0.5) +
  scale_fill_manual(values = c("Overall" = "blue", "Sampled" = "#666666")) +
  scale_color_manual(name = "Averages",
                     values = c("Overall Average" = "blue", "Sample Average" = "#666666")) +
  xlim(0, NA) +
  theme_bw() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top")
  )


# --- Reconstruct Questionnaire from answers -----------------------------------
data_clean_qa <- raw_to_clean(
  data_raw,
  max_missing = Inf,          # set Inf to keep all rows
  treat_empty_as_na = TRUE
)

data_clean_split_qa <- clean_to_clean_split(
  data_clean_qa,
  seps = c(";")
)

data_clean_split_qa <- explode_weitere_sprachkompetenzen(
  data_clean_split_qa,
  drop_original = TRUE
)

question_types <- get_question_types(data_clean_split_qa)

qa_df <- get_answers(data_clean_split_qa, question_types)
qa_df <- explode_ffkp_answers(qa_df)
qa_df <- order_qa(qa_df)

# Transform into wide format: one column question, one per answer 
qa_wide <- qa_df %>%
  group_by(question) %>%
  mutate(answer_id = paste0(seq_len(n()))) %>%
  ungroup() %>%
  select(-question_type) %>%
  pivot_wider(names_from = answer_id, values_from = answer) %>%
  order_wide_qa() %>%
  mutate(question_id = paste0("Q", ifelse(row_number() < 10, paste0("0", row_number()), as.character(row_number())))) %>%
  left_join(select(qa_df, question, question_type) %>% distinct(), by = "question") %>%
  mutate(question_type = ifelse(grepl("^Fachliches & funktionales Kompetenz-Profil", question), "SINGLE", question_type)) %>%
  select(question_id, question_type, question, everything())

# --- 5) Save artifacts --------------------------------------------------------
if (!dir.exists("output")) dir.create("output", recursive = TRUE)
write_csv(qa_wide, "preprocessing/output/questionnaire.csv")
write_csv(data_clean, "preprocessing/output/data_clean.csv")
write_csv(data_clean_split, "preprocessing/output/data_clean_split.csv")
write_csv(data_long, "preprocessing/output/data_long.csv")
write_csv(data_wide, "preprocessing/output/data_wide.csv")
write_csv(data_one_hot, "preprocessing/output/data_one_hot.csv")
write_csv(sample_data_clean, "preprocessing/output/sample_data_clean.csv")
write_csv(sample_response_counts, "preprocessing/output/sample_response_counts.csv")
ggsave("preprocessing/output/plots/hist_response_counts.jpeg", plot = hist_response_counts, width = 6, height = 3.5)

message("\nDone.")
