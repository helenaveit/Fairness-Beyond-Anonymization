#  CV Analysis (STAGE 2: RÉSUMÉ SCREENING AUDIT)

This folder contains code and derived data used to analyse generated CVs across protected groups (`gender`, `ethnicity`). The analysis focuses on structural résumé features, agentic/communal lexical counts (only part of appendix), and TF-IDF lasso seed-sensitivity models.

## Project Structure

- `code/` 
  - `01_structural_features.py` — extracts section counts and word-count features from CV JSON
  - `02_lexical_counts.py` — counts German agentic and communal word stems
  - `03_tfidf_lasso_seed_sensitivity.R` — repeats TF-IDF lasso models across random seeds
  - `04_plot_structural.R` — creates structural summary plots from derived features
  - `05_plot_lexical.R` — creates agentic/communal lexical summary plots

- `data/`
  - `structural_features.csv` — combined structural feature table
  - `lexical_counts.csv` — combined agentic/communal lexical count table
  - `structural/` — per-model structural feature tables
  - `word_counts/` — per-model agentic/communal count tables
  - `lasso_results/seed_sensitivity/` — TF-IDF lasso seed-sensitivity outputs
  - `raw_tfidf_traintest_*.csv` — TF-IDF matrices used by the seed-sensitivity script

- `lexica/` — German agentic/communal word lists and stems

- `figures/` — paper figures and tables 

## Reproducing the Analysis

Install Python dependencies:

```bash
pip install pandas
```

Install R dependencies:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "glmnet", "pROC", "tibble"))
```
