# helpers.R
# ------------------------------------------------------------------------------
# String normalization, gendering fixes, safe multiselect parsing, and
# a stable column-ordering helper for WIDE outputs.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

# --- Generic string cleaners -------------------------------------

# Collapse whitespace to single spaces and trim (Unicode-safe)
str_squish_unicode <- function(x) {
  if (is.null(x)) return(x)
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

# Normalize common punctuation variants in pasted text
normalize_punctuation <- function(x) {
  if (is.null(x)) return(x)
  x <- str_replace_all(x, fixed("–"), "-")
  x <- str_replace_all(x, fixed("—"), "-")
  x <- str_replace_all(x, fixed("’"), "'")
  x <- str_replace_all(x, fixed("`"), "'")
  x
}

# Remove trailing item codes like "- 06e" per token (safe w.r.t. semicolons).
strip_trailing_item_codes <- function(x, sep = ";") {
  if (is.null(x)) return(x)
  code_re <- "\\s*[\\-–—]\\s*\\d+[A-Za-z]{0,2}\\)?\\s*$"
  sep_re  <- paste0("\\s*\\Q", sep, "\\E\\s*")
  ifelse(
    is.na(x) | !nzchar(x),
    x,
    vapply(
      strsplit(x, sep_re, perl = TRUE),
      function(parts) {
        parts <- sub(code_re, "", parts, perl = TRUE)
        parts <- trimws(parts)
        paste(parts[nzchar(parts)], collapse = paste0(" ", sep, " "))
      },
      character(1)
    )
  )
}

# --- Project-specific normalization -------------------------------------------

# Gendering normalization (colon → star) *in gender context only*,
# and phrase tweak: "Großunternehmen ab 1000 Mitarbeiter:innen"
#                     -> "Großunternehmen: ab 1000 Mitarbeiter:innen"
normalize_gendering_delimiters <- function(x, to = "*") {
  if (is.null(x)) return(x)
  x <- str_replace_all(
    x,
    fixed("Großunternehmen ab 1000 Mitarbeiter:innen"),
    "Großunternehmen: ab 1000 Mitarbeiter:innen"
  )
  x <- str_replace_all(
    x,
    regex("(\\p{L})\\s*:\\s*(?=in(nen)?\\b)", ignore_case = TRUE),
    paste0("\\1", to)
  )
  x
}

# Replace Unicode ellipsis (…) with ASCII "..."
normalize_ellipsis_ascii <- function(x) {
  if (is.null(x)) return(x)
  str_replace_all(x, fixed("\u2026"), "...")
}

# Stable variable tokens (strip trailing ":"
# normalize whitespace/ellipses/punct)
normalize_variable_token <- function(x) {
  x %>%
    normalize_ellipsis_ascii() %>%
    str_squish_unicode() %>%
    normalize_punctuation() %>%
    str_remove(":\\s*$")
}

# Stable category/subcategory tokens (gendering + ellipses + whitespace)
normalize_tokens <- function(x) {
  x %>%
    normalize_gendering_delimiters() %>%
    normalize_ellipsis_ascii() %>%
    str_squish_unicode()
}

# Single entry point for raw cell cleaning during ingestion
clean_value <- function(x) {
  x %>%
    normalize_punctuation() %>%
    str_squish_unicode() %>%
    normalize_gendering_delimiters()
}

# --- Multiselect parsing (RAW → CLEAN_SPLIT) ----------------------------------
split_multiselect_to_lists <- function(x, seps = c(";")) {
  if (is.null(x)) return(list(character(0)))
  sep_re <- paste0("\\s*(?:", paste(seps, collapse = "|"), ")\\s*")
  purrr::map(x, function(cell) {
    if (is.na(cell) || !nzchar(cell)) return(character(0))
    parts <- unlist(str_split(as.character(cell), sep_re), use.names = FALSE)
    parts <- str_trim(parts)
    parts <- parts[nzchar(parts)]
    parts
  })
}

# --- Helpers for LONG → WIDE --------------------------------------------------

# Assert that for the FFkP variable there is at most one subcategory per (ID, category).
# action = "error" (default) stops on violations; "warn" just warns.
assert_ffkp_unique <- function(long_tbl,
                               var = "Fachliches & funktionales Kompetenz-Profil",
                               action = c("error","warn"),
                               max_show = 10) {
  action <- match.arg(action)
  bad <- long_tbl %>%
    filter(variable == var) %>%
    group_by(ID, category) %>%
    summarise(n_sub = n_distinct(na.omit(subcategory)),
                     subs  = paste(sort(unique(na.omit(subcategory))), collapse = " | "),
                     .groups = "drop") %>%
    filter(n_sub > 1)
  
  if (nrow(bad) > 0) {
    msg <- paste0(
      "[FFkP] Found multiple distinct subcategories for the same (ID, category).\n",
      "Examples:\n",
      paste(capture.output(print(utils::head(bad, max_show), row.names = FALSE)), collapse = "\n")
    )
    if (action == "error") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  invisible(long_tbl)
}

# Detect which variables are single-choice (at most one non-NA category per ID).
# Returns a tibble with columns 'variable' and 'is_single_choice' (logical).
detect_single_choice_vars <- function(long_tbl, key_vars = "ID") {
  
  # Ensure all key columns are present
  stopifnot(all(key_vars %in% names(long_tbl)))
  
  long_tbl %>%
    mutate(
      variable    = normalize_variable_token(variable),
      category    = normalize_tokens(category),
      subcategory = normalize_tokens(subcategory)
    ) %>%
    # Step 1: Group by 'variable' AND the composite key (key_vars)
    group_by(variable, across(all_of(key_vars))) %>%
    
    # Step 2: Calculate the number of distinct categories chosen per unique profile
    summarize(
      n_choices = n_distinct(category[!is.na(category)]), 
      .groups = "drop_last"
    ) %>%
    
    # Step 3: Check the maximum number of choices across all profiles for that variable
    summarize(
      max_per_id = max(n_choices, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    
    # Step 4: Determine if it's a single-choice variable
    transmute(variable, is_single_choice = (max_per_id <= 1))
}

# Final, hard-coded variable ordering for WIDE frames.
# Blocks appear in this order; within blocks:
#   1) the single-choice column (exact variable name),
#   2) non-indicator text cols,
#   3) indicator cols.
order_wide <- function(wide_tbl) {
  stopifnot("ID" %in% names(wide_tbl))
  
  var_order <- c(
    "Wunschbranche",
    "Branchen-/Bereichserfahrung",
    "Favorisierte Unternehmensgröße",
    "Unternehmensumfeld",
    "Einsatzort (Großraum)",
    "Remote-Tätigkeit",
    "Umfang der Stelle",
    "Reisetätigkeit",
    "Sprachkompetenz DEUTSCH",
    "Sprachkompetenz ENGLISCH",
    "WEITERE Sprachkompetenzen",  # bucket for per-language columns (except DE/EN)
    "Fachliches & funktionales Kompetenz-Profil",
    "Spezifische/r Themen- und Fachbereich/e",
    "Tätigkeitsfeld",
    "Beschreibung der Aufgabe",
    "Persönliche Kompetenzen",
    "Sozial-kommunikative Kompetenzen",
    "Aktivitäts- und umsetzungsorientierte Kompetenzen",
    "Denke an Deine bisherigen Teamerfahrungen zurück. Welche Rolle(n) nimmst Du in Teams am liebsten ein?",
    "Welche Werte sind Dir in dem Team der zu besetzenden Stelle besonders wichtig?",
    "Mit welchen Aussagen können Unternehmen bei Dir punkten?",
    "Meine Bildungsabschlüsse",
    "Bruttojahresgehalt"
  )
  
  other <- setdiff(names(wide_tbl), "ID")
  
  is_indicator_vec <- vapply(
    wide_tbl[other],
    function(col) is.numeric(col) && all(col %in% c(0, 1, NA), na.rm = TRUE),
    logical(1)
  )
  
  parts <- tibble(col = other) %>%
    mutate(
      sp          = str_split_fixed(col, ":", n = 3),
      variable    = str_trim(sp[, 1]),
      category    = str_trim(na_if(sp[, 2], "")),
      subcategory = str_trim(na_if(sp[, 3], "")),
      variable_key = case_when(
        variable %in% c("Sprachkompetenz DEUTSCH", "Sprachkompetenz ENGLISCH") ~ variable,
        grepl("^Sprachkompetenz\\s+", variable) ~ "WEITERE Sprachkompetenzen",
        TRUE ~ variable
      ),
      var_rank    = match(variable_key, var_order),
      var_rank    = if_else(is.na(var_rank), length(var_order) + 1L, var_rank),
      is_sc_col   = (col == variable),
      is_indicator= is_indicator_vec[match(col, other)]
    ) %>%
    select(-sp)
  
  ordered_cols <- c(
    "ID",
    parts %>%
      arrange(var_rank, desc(is_sc_col), is_indicator, category, subcategory, col) %>%
      pull(col)
  )
  
  wide_tbl %>% select(all_of(ordered_cols))
}

# --- Helper for questionnaire reconstruction ----------------------------------
# function to get question types
get_question_types <- function(df) {
  # if even one entry in a column is longer than 1, it's a multi-choice question,
  # otherwise single-choice. Exclude ID column. 
  # Output: named character vector with question types
  question_types <- sapply(df %>% select(-matches("^ID$")), function(col) {
    if (any(sapply(col, function(x) length(x) > 1))) {
      return("MULTI")
    } else {
      return("SINGLE")
    }
  })
  return(question_types)
}

# now get all unique answers per question (excluding ID column)
# Output: aq data.frame with columns: question, question_type, answer
get_answers <- function(df, question_types) {
  aq_list <- lapply(names(question_types), function(q) {
    answers <- df %>%
      pull(!!sym(q)) %>%
      unlist() %>%
      unique() %>%
      na.omit() %>%
      sort()
    tibble(
      question = q,
      question_type = question_types[[q]],
      answer = answers
    )
  })
  aq_df <- bind_rows(aq_list)
  return(aq_df)
}

# Explode Fachliches & funktionales Kompetenz-Profil and Spezifische/r Themen- und Fachbereich/e 
# answers at ":" and add the part before to the question and keep only the part after as answer
explode_ffkp_answers <- function(df) {
df <- df %>%
    mutate(
      split_answer = ifelse(question %in% c("Fachliches & funktionales Kompetenz-Profil", "Spezifische/r Themen- und Fachbereich/e"),
                            strsplit(answer, ": "), NA)
    ) %>%
    mutate(
      question = ifelse(!is.na(split_answer),
                        paste0(question, ": ", sapply(split_answer, `[`, 1)),
                        question),
      answer = ifelse(!is.na(split_answer),
                      sapply(split_answer, `[`, 2),
                      answer)
    ) %>%
    select(-split_answer) %>%
    distinct()

df <- df  %>% rbind(data.frame(
  question = "Fachliches & funktionales Kompetenz-Profil: Naturwissenschaften (sonstige)",
  question_type = "MULTI",
  answer = "Management (mit Führungserfahrung)"))

return(df)
}

# function to order answers per question in a sensible way
order_qa <- function(df){
  df <- df %>%
    distinct() %>%
    arrange(question, answer) %>%
    mutate(answer = case_when(
      question == "Bruttojahresgehalt" & answer == "unbezahltes Pflichtpraktikum" ~ "01_unbezahltes Pflichtpraktikum",
      question == "Bruttojahresgehalt" & answer == "bis 6.240 EUR" ~ "02_bis 6.240 EUR",
      question == "Bruttojahresgehalt" & answer == "bis 8.000 EUR" ~ "03_bis 8.000 EUR",
      question == "Bruttojahresgehalt" & answer == "bis 12.000 EUR" ~ "04_bis 12.000 EUR",
      question == "Bruttojahresgehalt" & answer == "bis zu 20.000 EUR" ~ "05_bis zu 20.000 EUR",
      question == "Bruttojahresgehalt" & answer == "20.000 - 30.000 EUR" ~ "06_20.000 - 30.000 EUR",
      question == "Bruttojahresgehalt" & answer == "30.000 - 40.000 EUR" ~ "07_30.000 - 40.000 EUR",
      question == "Bruttojahresgehalt" & answer == "40.000 - 50.000 EUR" ~ "08_40.000 - 50.000 EUR",
      question == "Bruttojahresgehalt" & answer == "50.000 - 60.000 EUR" ~ "09_50.000 - 60.000 EUR",
      question == "Bruttojahresgehalt" & answer == "60.000 - 70.000 EUR" ~ "10_60.000 - 70.000 EUR",
      question == "Bruttojahresgehalt" & answer == "70.000 - 80.000 EUR" ~ "11_70.000 - 80.000 EUR",
      question == "Bruttojahresgehalt" & answer == "80.000 - 90.000 EUR" ~ "12_80.000 - 90.000 EUR",
      question == "Bruttojahresgehalt" & answer == "90.000 - 100.000 EUR" ~ "13_90.000 - 100.000 EUR",
      question == "Bruttojahresgehalt" & answer == "100.000 - 110.000 EUR" ~ "14_100.000 - 110.000 EUR",
      question == "Bruttojahresgehalt" & answer == "110.000 - 120.000 EUR" ~ "15_110.000 - 120.000 EUR",
      question == "Bruttojahresgehalt" & answer == "120.000 - 130.000 EUR" ~ "16_120.000 - 130.000 EUR",
      question == "Bruttojahresgehalt" & answer == "130.000 - 140.000 EUR" ~ "17_130.000 - 140.000 EUR",
      question == "Bruttojahresgehalt" & answer == "140.000 - 150.000 EUR" ~ "18_140.000 - 150.000 EUR",
      question == "Bruttojahresgehalt" & answer == "ab 150.000 EUR" ~ "19_ab 150.000 EUR",
      question == "Bruttojahresgehalt" & answer == "Ich möchte dazu keine Angabe machen." ~ "20_Ich möchte dazu keine Angabe machen.",
      TRUE ~ answer
    )) %>%
    mutate(answer = case_when(
      question == "Einsatzort (Großraum)" & answer == "Ausschließlich remote" ~ "00_Ausschließlich remote",
      question == "Einsatzort (Großraum)" & answer == "Bundesweit" ~ "01_Bundesweit",
      question == "Einsatzort (Großraum)" & grepl("^Europa außer Deutschland", answer) ~ paste0("03_", answer),
      question == "Einsatzort (Großraum)" & grepl("^USA", answer) ~ paste0("04_", answer),
      question == "Einsatzort (Großraum)" & grepl("^Übrige Welt", answer) ~ paste0("05_", answer),
      question == "Einsatzort (Großraum)" ~ paste0("02_", answer),   # only for this question
      TRUE ~ answer
    )) %>%
    mutate(answer = case_when(
      question == "Meine Bildungsabschlüsse" & answer == "Fachhochschulreife" ~ "01_Fachhochschulreife",
      question == "Meine Bildungsabschlüsse" & answer == "Hochschulreife, Abitur" ~ "02_Hochschulreife, Abitur",
      question == "Meine Bildungsabschlüsse" & answer == "Berufsausbildung" ~ "03_Berufsausbildung",
      question == "Meine Bildungsabschlüsse" & answer == "Meister*in" ~ "04_Meister*in",
      question == "Meine Bildungsabschlüsse" & answer == "Bachelor" ~ "05_Bachelor",
      question == "Meine Bildungsabschlüsse" & answer == "Diplom" ~ "06_Diplom",
      question == "Meine Bildungsabschlüsse" & answer == "Master" ~ "07_Master",
      question == "Meine Bildungsabschlüsse" & answer == "Magister" ~ "08_Magister",
      question == "Meine Bildungsabschlüsse" & answer == "MBA" ~ "09_MBA",
      question == "Meine Bildungsabschlüsse" & answer == "1. Staatsexamen" ~ "10_1. Staatsexamen",
      question == "Meine Bildungsabschlüsse" & answer == "2. Staatsexamen" ~ "11_2. Staatsexamen",
      question == "Meine Bildungsabschlüsse" & answer == "3. Staatsexamen" ~ "12_3. Staatsexamen",
      question == "Meine Bildungsabschlüsse" & answer == "PhD" ~ "13_PhD",
      question == "Meine Bildungsabschlüsse" & answer == "Promotion" ~ "14_Promotion",
      question == "Meine Bildungsabschlüsse" & answer == "Sonstiges" ~ "15_Sonstiges",
      TRUE ~ answer
    )) %>%
    mutate(answer = case_when(
      question == "Favorisierte Unternehmensgröße" & answer == "Die Unternehmensgröße spielt keine Rolle" ~ "01_Die Unternehmensgröße spielt keine Rolle",
      question == "Favorisierte Unternehmensgröße" & answer == "Kleinstunternehmen: bis 9 Mitarbeiter*innen" ~ "02_Kleinstunternehmen: bis 9 Mitarbeiter*innen",
      question == "Favorisierte Unternehmensgröße" & answer == "Kleinunternehmen: bis 49 Mitarbeiter*innen" ~ "03_Kleinunternehmen: bis 49 Mitarbeiter*innen",
      question == "Favorisierte Unternehmensgröße" & answer == "Mittleres Unternehmen: bis 249 Mitarbeiter*innen" ~ "04_Mittleres Unternehmen: bis 249 Mitarbeiter*innen",
      question == "Favorisierte Unternehmensgröße" & answer == "Großunternehmen: ab 250 Mitarbeiter*innen" ~ "05_Großunternehmen: ab 250 Mitarbeiter*innen",
      question == "Favorisierte Unternehmensgröße" & answer == "Großunternehmen: ab 1000 Mitarbeiter*innen" ~ "06_Großunternehmen: ab 1000 Mitarbeiter*innen",
      TRUE ~ answer
    )) %>%
    mutate(answer = case_when(
      grepl("^Sprachkompetenz ", question) & answer == "Elementare Sprachanwendung (Kann vertraute, alltägliche Ausdrücke und ganz einfache Sätze verstehen und verwenden - A1 / A2)" ~ "Elementare Sprachanwendung (A1 / A2)",
      TRUE ~ answer
    )) %>%
    mutate(answer = case_when(
      # DEUTSCH: "nicht vorhanden" first
      question == "Sprachkompetenz DEUTSCH" & answer == "Deutschkenntnisse sind nicht vorhanden" ~
        "01_Deutschkenntnisse sind nicht vorhanden",
      # ENGLISCH: "nicht vorhanden" first
      question == "Sprachkompetenz ENGLISCH" & answer == "Englischkenntnisse sind nicht vorhanden" ~
        "01_Englischkenntnisse sind nicht vorhanden",
      grepl("^Sprachkompetenz (ENGLISCH|DEUTSCH)$", question) & answer == "Elementare Sprachanwendung (A1 / A2)" ~
        "02_Elementare Sprachanwendung (A1 / A2)",
      grepl("^Sprachkompetenz (ENGLISCH|DEUTSCH)$", question)  & answer == "Fortgeschrittene Sprachverwendung (B1)" ~
        "03_Fortgeschrittene Sprachverwendung (B1)",
      grepl("^Sprachkompetenz (ENGLISCH|DEUTSCH)$", question)  & answer == "Selbstständige Sprachanwendung (B2)" ~
        "04_Selbstständige Sprachanwendung (B2)",
      grepl("^Sprachkompetenz (ENGLISCH|DEUTSCH)$", question)  & answer == "Fachkundige Sprachkenntnisse (C1)" ~
        "05_Fachkundige Sprachkenntnisse (C1)",+
        grepl("^Sprachkompetenz (ENGLISCH|DEUTSCH)$", question)  & answer == "(Annähernd) Muttersprachliche Kenntnisse (C2)" ~
        "06_(Annähernd) Muttersprachliche Kenntnisse (C2)",
      TRUE ~ answer
    )) %>%
    mutate(answer = case_when(
      grepl("^Sprachkompetenz ", question) & answer == "Elementare Sprachanwendung (A1 / A2)" ~ "01_Elementare Sprachanwendung (A1 / A2)",
      grepl("^Sprachkompetenz ", question) & answer == "Selbstständige Sprachanwendung (B1 / B2)" ~ "02_Selbstständige Sprachanwendung (B1 / B2)",
      grepl("^Sprachkompetenz ", question) & answer == "Kompetente Sprachverwendung (C1 / C2)" ~ "03_Kompetente Sprachverwendung (C1 / C2)",
      TRUE ~ answer
    )) %>%
    mutate(answer = case_when(
      grepl("Fachliches & funktionales Kompetenz-Profil:", question) & answer == "Einstieg (Quereinstieg)" ~ "01_Einstieg (Quereinstieg)",
      grepl("Fachliches & funktionales Kompetenz-Profil:", question) & answer == "Einstieg (Expertise autodidaktisch erworben)" ~ "02_Einstieg (Expertise autodidaktisch erworben)",
      grepl("Fachliches & funktionales Kompetenz-Profil:", question) & answer == "Einstieg (nach Weiterbildung)" ~ "03_Einstieg (nach Weiterbildung)",
      grepl("Fachliches & funktionales Kompetenz-Profil:", question) & answer == "Einstieg (nach Studium / Ausbildung mit < 3 Jahre Berufserfahrung)" ~ "04_Einstieg (nach Studium / Ausbildung mit < 3 Jahre Berufserfahrung)",
      grepl("Fachliches & funktionales Kompetenz-Profil:", question) & answer == "Professional (Erfahrung > 3 Jahre)" ~ "05_Professional (Erfahrung > 3 Jahre)",
      grepl("Fachliches & funktionales Kompetenz-Profil:", question) & answer == "Management (ohne Führungserfahrung)" ~ "06_Management (ohne Führungserfahrung)",
      grepl("Fachliches & funktionales Kompetenz-Profil:", question) & answer == "Management (mit Führungserfahrung)" ~ "07_Management (mit Führungserfahrung)",
      TRUE ~ answer
    )) %>%
    arrange(question, answer) %>%
    mutate(answer = sub("^[0-9]+_", "", answer))
}

# order wide QA dataframe according to question order in the survey
order_wide_qa <- function(df) {
  var_order <- c(
    "Wunschbranche",
    "Branchen-/Bereichserfahrung",
    "Favorisierte Unternehmensgröße",
    "Unternehmensumfeld",
    "Einsatzort (Großraum)",
    "Remote-Tätigkeit",
    "Umfang der Stelle",
    "Reisetätigkeit",
    "Sprachkompetenz DEUTSCH",
    "Sprachkompetenz ENGLISCH",
    "WEITERE Sprachkompetenzen",  # bucket for per-language columns (except DE/EN)
    "Fachliches & funktionales Kompetenz-Profil",
    "Spezifische/r Themen- und Fachbereich/e",
    "Tätigkeitsfeld",
    "Beschreibung der Aufgabe",
    "Persönliche Kompetenzen",
    "Sozial-kommunikative Kompetenzen",
    "Aktivitäts- und umsetzungsorientierte Kompetenzen",
    "Denke an Deine bisherigen Teamerfahrungen zurück. Welche Rolle(n) nimmst Du in Teams am liebsten ein?",
    "Welche Werte sind Dir in dem Team der zu besetzenden Stelle besonders wichtig?",
    "Mit welchen Aussagen können Unternehmen bei Dir punkten?",
    "Meine Bildungsabschlüsse",
    "Bruttojahresgehalt"
  )
  
  weitere_pos <- match("WEITERE Sprachkompetenzen", var_order)
  
  df_ordered <- df %>%
    mutate(
      is_other_lang = str_starts(question, "Sprachkompetenz ") &
        !question %in% c("Sprachkompetenz DEUTSCH", "Sprachkompetenz ENGLISCH"),
      
      # base position from var_order
      order_main = match(question, var_order),
      
      # send other languages to the bucket position
      order_main = ifelse(is_other_lang, weitere_pos, order_main),
      
      # unlisted items go to the end
      order_main = ifelse(is.na(order_main), length(var_order) + 1L, order_main),
      
      # alphabetical within the “WEITERE …” bucket, empty otherwise
      order_sub = ifelse(is_other_lang, question, "")
    ) %>%
    arrange(order_main, order_sub) %>%
    select(-is_other_lang, -order_main, -order_sub)
  
  return(df_ordered)
}
# --- Quick overview for any data.frame/tibble ---------------------------------

overview_tbl <- function(df) {
  tibble(
    n_rows    = nrow(df),
    n_cols    = ncol(df),
    mem_mb    = as.numeric(object.size(df)) / (1024^2),
    col_names = list(names(df))
  )
}
