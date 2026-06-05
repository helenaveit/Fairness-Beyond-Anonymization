# transform_fns.R
# ------------------------------------------------------------------------------
# Core transforms:
#   RAW   -> CLEAN        (wide; character columns; add ID)
#   CLEAN -> CLEAN_SPLIT  (wide; list columns)
#   CLEAN_SPLIT -> LONG   (tidy: ID, variable, category, subcategory)
#   LONG  -> WIDE         (analysis-ready frame; indicators + special text cols)
#   WIDE -> ONE-HOT    (for stratified sampling and per feature accuracy)
#
# All normalization lives in helpers.R 
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

# --- RAW → CLEAN ---------------------------------------------------------------
# RAW:
# - No ID column
# - One column per question/variable (semicolon-delimited multiselects)
#
# Parameters:
#   max_missing        : max number of missing cells allowed per row (default 0).
#                        Set to Inf or NULL to keep all rows.
#   treat_empty_as_na  : if TRUE, trim and coerce "" / whitespace-only cells to NA
#                        before counting missings.
raw_to_clean <- function(raw_df,
                         max_missing = 0L,
                         treat_empty_as_na = TRUE) {
  
  # 0) Column names: drop numeric prefixes and trailing ":", e.g. "01 Frage:" -> "Frage"
  colnames(raw_df) <- raw_df %>%
    colnames() %>%
    str_remove("^[0-9]+\\s+") %>%
    str_remove(":\\s*$")
  
  # 1) Optionally treat empty strings as NA for fair missing counts
  if (isTRUE(treat_empty_as_na)) {
    raw_df <- mutate(
      raw_df,
      across(
        everything(),
        ~ {
          z <- as.character(.x)
          z <- str_squish(z)
          z[z == ""] <- NA_character_
          z
        }
      )
    )
  }
  
  # 2) Filter rows by missing-count budget (default 0 = strict)
  raw_keep <- if (!is.null(max_missing) && is.finite(max_missing)) {
    raw_df %>%
      mutate(.na_cnt = rowSums(is.na(across(everything())))) %>%
      filter(.na_cnt <= max_missing) %>%
      select(-.na_cnt)
  } else {
    raw_df
  }
  
  # 3) Add ID (since RAW has none)
  raw_keep <- raw_keep %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())
  
  # 4) Normalize strings (no list-splitting here)
  #    - clean_value(): punctuation/whitespace/gendering, ellipsis …→...
  #    - normalize fullwidth colon to ASCII ':'
  #    - strip trailing item codes per token ("- 06e", "– 5", …) safely w.r.t. semicolons
  clean <- raw_keep %>%
    mutate(across(-ID, clean_value)) %>%
    mutate(
      across(
        -ID,
        ~ .x %>%
          str_replace_all("\uFF1A", ":") %>%   # fullwidth colon → ASCII
          strip_trailing_item_codes(sep = ";") %>%      # per-token cleanup
          str_trim()
      )
    )
  
  # For WEITERE Sprachkompetenzen, convert 
  # "Sonstige: Auf der nachfolgenden Seite..." to "Sonstige" if it's part of the answer
  if ("WEITERE Sprachkompetenzen" %in% names(clean)) {
    clean <- clean %>%
      mutate(
        `WEITERE Sprachkompetenzen` = str_replace_all(
          `WEITERE Sprachkompetenzen`,
          "Sonstige\\s*(:\\s*Auf der nachfolgenden Seite.*)?$",
          "Sonstige"
        )
      )
  }
  
  clean
}

# --- CLEAN → CLEAN_SPLIT (turn semicolon strings into list-columns) -----------
clean_to_clean_split <- function(clean_tbl, seps = c(";")) {
  stopifnot("ID" %in% names(clean_tbl))
  clean_tbl %>%
    mutate(across(-ID, ~ split_multiselect_to_lists(.x, seps = seps)))
}

# --- Explode "WEITERE Sprachkompetenzen" into per-language single-choice cols --
# Run AFTER clean_to_clean_split() and BEFORE clean_to_long()
# Produces columns like "Sprachkompetenz Französisch", each a list-col of length-1
# character vectors (so they behave as single-choice in clean_to_long())
explode_weitere_sprachkompetenzen <- function(clean_split_tbl,
                                              src_col = "WEITERE Sprachkompetenzen",
                                              prefix  = "Sprachkompetenz ",
                                              fill_missing = "keine Angabe",
                                              drop_original = TRUE) {
  stopifnot("ID" %in% names(clean_split_tbl))
  if (!(src_col %in% names(clean_split_tbl))) return(clean_split_tbl)
  
  # Map verbose levels to 3 normalized labels (A < B < C)
  map_level <- function(txt) {
    if (is.null(txt) || is.na(txt) || !nzchar(txt)) return(NA_character_)
    t <- tolower(txt)
    if (grepl("\\b(c1|c2)\\b", t)) {
      "Kompetente Sprachverwendung (C1 / C2)"
    } else if (grepl("\\b(b1|b2)\\b", t)) {
      "Selbstständige Sprachanwendung (B1 / B2)"
    } else if (grepl("\\b(a1|a2)\\b", t)) {
      "Elementare Sprachanwendung (Kann vertraute, alltägliche Ausdrücke und ganz einfache Sätze verstehen und verwenden - A1 / A2)"
    } else {
      NA_character_
    }
  }
  level_rank <- c(
    "Elementare Sprachanwendung (Kann vertraute, alltägliche Ausdrücke und ganz einfache Sätze verstehen und verwenden - A1 / A2)"     = 1L,
    "Selbstständige Sprachanwendung (B1 / B2)" = 2L,
    "Kompetente Sprachverwendung (C1 / C2)"    = 3L
  )
  
  # Parse one row's tokens (a character vector) into a named list: lang -> level
  parse_cell <- function(cell_tokens) {
    if (length(cell_tokens) == 0) return(list())
    toks <- trimws(as.character(cell_tokens))
    toks <- toks[nzchar(toks)]
    out <- list()
    for (p in toks) {
      if (grepl("^Ich verfüge über keine weiteren Fremdsprachen", p)) next
      if (grepl("^Sonstige\\b", p)) next
      sp   <- str_split_fixed(p, "\\s*:\\s*", n = 2)
      lang <- trimws(sp[, 1])
      lvl  <- ifelse(sp[, 2] == "", NA_character_, sp[, 2])
      lvl  <- map_level(lvl)
      if (!is.na(lvl) && nzchar(lang)) {
        if (!is.null(out[[lang]])) {
          if (level_rank[[lvl]] > level_rank[[out[[lang]]]]) out[[lang]] <- lvl
        } else {
          out[[lang]] <- lvl
        }
      }
    }
    out
  }
  
  # Collect language set across the dataset
  src_list <- clean_split_tbl[[src_col]]
  all_langs <- unique(sort(unlist(lapply(src_list, function(tokens) {
    if (length(tokens) == 0) return(character(0))
    toks <- trimws(as.character(tokens))
    toks <- toks[nzchar(toks)]
    toks <- toks[grepl(":", toks) &
                   !grepl("^Sonstige\\b", toks) &
                   !grepl("^Ich verfüge über keine weiteren Fremdsprachen", toks)]
    sub(":.*$", "", toks) %>% trimws()
  }), use.names = FALSE)))
  
  if (length(all_langs) == 0) {
    return(if (isTRUE(drop_original))
      select(clean_split_tbl, -all_of(src_col))
      else clean_split_tbl)
  }
  
  # Per-row map: lang -> highest level
  lang_maps <- lapply(src_list, parse_cell)
  
  # Create per-language single-choice list-columns (fill missing with fill_missing)
  for (L in all_langs) {
    colname <- paste0(prefix, L)
    vals_chr <- vapply(lang_maps, function(m) {
      v <- m[[L]]
      if (is.null(v) || is.na(v) || !nzchar(v)) fill_missing else as.character(v)
    }, character(1))
    clean_split_tbl[[colname]] <- lapply(vals_chr, function(z) z)  # length-1 list-elt
  }
  
  if (isTRUE(drop_original)) {
    clean_split_tbl <- select(clean_split_tbl, -all_of(src_col))
  }
  
  clean_split_tbl
}

# --- CLEAN_SPLIT → LONG --------------------------------------------------------
# LONG: one row per selected token; split on first colon unless whitelisted
clean_to_long <- function(clean_tbl, exclude_vars = NULL, key_vars = "ID") {
  
  # 1. Validation and Setup
  
  # Ensure all key_vars and exclude_vars exist
  all_vars_to_check <- unique(c(key_vars, exclude_vars))
  stopifnot(all(all_vars_to_check %in% names(clean_tbl)))
  
  # The variables to pivot are those *not* in the key or the exclusion list
  pivot_vars <- setdiff(names(clean_tbl), c(key_vars, exclude_vars))
  
  # The true unique identifier is the combination of variables in key_vars
  # We will use key_vars for all joining operations.
  
  # Ensure we received list-columns (run CLEAN_SPLIT first)
  non_list <- pivot_vars[!vapply(clean_tbl[pivot_vars], is.list, logical(1))]
  if (length(non_list)) {
    stop(
      "clean_to_long(): Non-list columns found: ",
      paste(non_list, collapse = ", "),
      ". Run clean_to_clean_split() (and explode_weitere_sprachkompetenzen(), if used) first."
    )
  }
  
  # Single-choice variables: every element has length 0/1 (after explosion)
  is_len1_or_empty <- function(x) all(lengths(x) %in% c(0L, 1L))
  single_choice_vars <- names(Filter(is_len1_or_empty, clean_tbl[pivot_vars]))
  
  # Never split these on ":"
  team_q   <- "Denke an Deine bisherigen Teamerfahrungen zurück. Welche Rolle(n) nimmst Du in Teams am liebsten ein?"
  punkte_q <- "Mit welchen Aussagen können Unternehmen bei Dir punkten?"
  
  # Unnest list-cols to tidy (key_vars, variable, value)
  as_tokvec <- function(x) {
    if (is.null(x)) return(character(0))
    x <- unlist(x, use.names = FALSE)
    x <- x[!is.na(x)]
    x <- str_trim(x)
    x[nchar(x) > 0]
  }
  
  # The map_dfr call now uses key_vars instead of ID
  long_vals <- map_dfr(pivot_vars, function(v) {
    vals <- lapply(clean_tbl[[v]], as_tokvec)
    # The key_vars columns must be repeated based on the lengths of vals
    key_data <- clean_tbl[key_vars]
    
    tibble(
      # Repeat the key columns for each unnested value
      !!!purrr::map(key_data, ~rep.int(.x, lengths(vals))),
      variable = v,
      value    = unlist(vals, use.names = FALSE)
    )
  })
  
  # Handle empty result set
  if (nrow(long_vals) == 0) {
    # If no data, return key_vars + exclude_vars + the new pivoted columns
    return(select(clean_tbl, all_of(c(key_vars, exclude_vars))) %>%
             distinct() %>%
             mutate(variable = character(), category = character(), subcategory = character()))
  }
  
  # 2. Processing (largely unchanged, but grouping uses key_vars)
  long_vals <- long_vals %>%
    mutate(
      variable = normalize_variable_token(variable),
      value    = as.character(value)
    ) %>%
    # Group by key_vars and variable
    group_by(across(all_of(key_vars)), variable) %>%
    distinct(value, .keep_all = TRUE) %>%
    ungroup()
  
  # Split once at the first colon into cat/subcat (unchanged logic)
  with_default_split <- long_vals %>%
    mutate(
      has_colon   = str_detect(value, ":"),
      cat_default = if_else(has_colon,
                                   str_trim(str_remove(value, ":.*$")),
                                   value),
      sub_default = if_else(has_colon,
                                   str_trim(str_remove(value, "^.*?:")),
                                   NA_character_)
    )
  
  # Apply “never split” overrides and single-choice behavior
  data_long_pivot <- with_default_split %>%
    mutate(
      category = case_when(
        variable == team_q   ~ value,
        variable == punkte_q ~ value,
        TRUE                 ~ cat_default
      ),
      subcategory = case_when(
        variable == team_q   ~ NA_character_,
        variable == punkte_q ~ NA_character_,
        TRUE                 ~ sub_default
      )
    ) %>%
    mutate(
      category    = if_else(variable %in% single_choice_vars, value, category),
      subcategory = if_else(variable %in% single_choice_vars, NA_character_, subcategory)
    ) %>%
    # Select key_vars and core pivot columns
    select(all_of(key_vars), variable, category, subcategory) %>%
    mutate(
      variable    = normalize_variable_token(variable),
      category    = normalize_tokens(category),
      subcategory = normalize_tokens(subcategory)
    ) %>%
    distinct(!!!syms(key_vars), variable, category, subcategory) %>%
    arrange(!!!syms(key_vars), variable, category, subcategory)
  
  # 3. Merging Excluded Variables
  
  # Select the key_vars and the exclude_vars from the original table
  exclude_data <- clean_tbl %>%
    select(all_of(key_vars), all_of(exclude_vars)) %>%
    distinct()

  # Join the excluded variables back to the long pivot data by the key_vars
  data_long <- left_join(data_long_pivot, exclude_data, by = key_vars)

  # Arrange columns: key_vars first, then pivot columns, then exclude_vars
  data_long <- data_long %>%
    select(all_of(key_vars), variable, category, subcategory, all_of(exclude_vars), everything())
  
  data_long
}

# --- LONG → WIDE ---------------------------------------------------------------
# Design:
# - Multi-select variables → 0/1 indicators:
#     "Variable: Category" or "Variable: Category: Subcategory"
# - Single-choice variables → one column per variable (cell = chosen category)
long_to_wide <- function(long_tbl, key_vars = "ID", fill = 0L) {
  
  # Ensure necessary columns are present
  required_cols <- c(key_vars, "variable", "category", "subcategory")
  stopifnot(all(required_cols %in% names(long_tbl)))
  
  # Use a symbol list for dynamic tidy evaluation
  key_syms <- rlang::syms(key_vars)
  
  # 1) Normalize & de-duplicate
  long_norm <- long_tbl %>%
    mutate(
      variable    = normalize_variable_token(variable),
      category    = normalize_tokens(category),
      subcategory = normalize_tokens(subcategory)
    ) %>%
    # Use key_vars for distinct check
    distinct(!!!key_syms, variable, category, subcategory)
  
  # 2) Detect single-choice variables (per variable, max one category per ID set)
  sc_map  <- detect_single_choice_vars(long_norm, key_vars = key_vars) 
  # Aggregate sc_map to ensure a variable is only deemed single-choice 
  # if it was TRUE for all key sets.
  sc_map_aggregated <- sc_map %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      is_single_choice_final = all(is_single_choice),
      .groups = "drop"
    )
  
  # Use the final, consolidated single-choice status
  sc_vars <- sc_map_aggregated %>% 
    dplyr::filter(is_single_choice_final) %>% 
    dplyr::pull(variable)
  
  # 3) MULTI-SELECT → indicator columns
  wide_ind <- long_norm %>%
    filter(!(variable %in% sc_vars)) %>%
    filter(!(is.na(category) & is.na(subcategory))) %>%
    mutate(
      var_clean = str_remove(variable, ":\\s*$"),
      colname   = if_else(
        is.na(subcategory) | subcategory == "",
        paste(var_clean, category, sep = ": "),
        paste(var_clean, category, subcategory, sep = ": ")
      ),
      val = 1L
    ) %>%
    # Use key_vars for the distinct guard
    distinct(!!!key_syms, colname, .keep_all = TRUE) %>%
    tidyr::pivot_wider(
      # Use key_vars for id_cols
      id_cols     = all_of(key_vars),
      names_from  = colname,
      values_from = val,
      values_fill = fill,
      values_fn   = list(val = max)
    )
  
  # 4) SINGLE-CHOICE → one text column per variable
  single_levels <- long_norm %>%
    filter(variable %in% sc_vars) %>%
    group_by(variable) %>%
    summarise(levels = list(unique(na.omit(category))), .groups = "drop") %>%
    tibble::deframe()
  
  wide_sc <- long_norm %>%
    filter(variable %in% sc_vars) %>%
    # Group by key_vars and variable
    group_by(!!!key_syms, variable) %>%
    summarise(value = first(category), .groups = "drop") %>%
    # Use key_vars for id_cols
    tidyr::pivot_wider(id_cols = all_of(key_vars), names_from = variable, values_from = value) %>%
    mutate(
      across(
        # EXCLUDE all columns in key_vars from factor conversion
        -all_of(key_vars), 
        ~ {
          levs <- single_levels[[dplyr::cur_column()]]
          if (is.null(levs)) levs <- sort(unique(na.omit(as.character(.x))))
          factor(.x, levels = levs)
        }
      )
    )
  
  # 5) Combine pieces and apply final column ordering
  pieces <- list(wide_ind, wide_sc) %>%
    purrr::keep(~ is.data.frame(.x) && nrow(.x) > 0)
  
  # Use key_vars for the join
  wide <- Reduce(function(a, b) full_join(a, b, by = key_vars), pieces) %>%
    # Use key_vars for arranging
    arrange(!!!key_syms)
  
  order_wide(wide)
}

# --- WIDE → ONE-HOT ENCODED -----------------------------------------------------
# Also one-hot encode single-choice factor columns. All character columns are treated
# as single-choice. The value 'keine Angabe' in the Sprachkompetenz columns is
# ignored (no indicator column created)
wide_to_one_hot <- function(wide_tbl,
                            key_vars = "ID",
                            sc_prefix = "SC_",
                            ignore_levels = c("keine Angabe")) {
  
  # 1. Validation and Key Identification
  stopifnot(all(key_vars %in% names(wide_tbl)))
  
  # Identify all potential single-choice columns (factors and characters)
  potential_sc_cols <- names(wide_tbl)[vapply(wide_tbl, function(col) {
    is.factor(col) || is.character(col)
  }, logical(1))]
  
  # 2. Exclude key_vars from the columns to be encoded
  sc_cols <- setdiff(potential_sc_cols, key_vars)
  
  # 3. One-Hot Encoding Logic (UNCHANGED for SC columns)
  one_hot_list <- lapply(sc_cols, function(colname) {
    col_vals <- wide_tbl[[colname]]
    if (is.character(col_vals)) {
      col_vals <- factor(col_vals)
    }
    levels_to_use <- setdiff(levels(col_vals), ignore_levels)
    indicators <- lapply(levels_to_use, function(lvl) {
      as.integer(col_vals == lvl)
    })
    names(indicators) <- paste0(sc_prefix, colname, ": ", levels_to_use)
    indicators
  })
  
  one_hot_df <- if (length(one_hot_list) > 0) {
    # Ensure this is always a tibble for reliable binding
    tibble::as_tibble(do.call(cbind, unlist(one_hot_list, recursive = FALSE)))
  } else {
    tibble::tibble()
  }
  
  # 4. Final Combination and Ordering
  
  # Columns to KEEP: all multi-select indicators (numeric) AND the key_vars
  cols_to_keep <- setdiff(names(wide_tbl), sc_cols)
  
  wide_final <- bind_cols(
    # Select all columns *except* the one-hot encoded columns
    dplyr::select(wide_tbl, all_of(cols_to_keep)),
    one_hot_df
  )
  
  # Arrange columns to place key_vars first
  wide_final <- wide_final %>%
    dplyr::select(all_of(key_vars), everything())
  
  # Assume order_wide handles the final cleanup/ordering
  order_wide(wide_final)
}

