# CV Analysis: Résumé Screening Audit

This folder contains the minimal code and derived data used to analyse generated CVs across protected groups (`gender`, `ethnicity`). The analysis focuses on structural résumé features, agentic/communal lexical counts, and TF-IDF lasso seed-sensitivity models.

Raw generated CV JSON/text files are intentionally excluded from this submission. Derived result tables are included so the main findings can be inspected without rerunning the full pipeline.

## Project Structure

- `code/` — minimal analysis scripts
  - `00_fix_cv_json.py` — normalizes generated CV JSON if raw run exports are available locally
  - `01_structural_features.py` — extracts section counts and word-count features from CV JSON
  - `02_lexical_counts.py` — counts German agentic and communal word stems
  - `03_tfidf_lasso_seed_sensitivity.R` — repeats TF-IDF lasso models across random seeds
  - `04_plot_structural.R` — creates structural summary plots from derived features
  - `05_plot_lexical.R` — creates agentic/communal lexical summary plots

- `data/` — derived data used for review and reproduction
  - `structural_features.csv` — combined structural feature table
  - `lexical_counts.csv` — combined agentic/communal lexical count table
  - `structural/` — per-model structural feature tables
  - `word_counts/` — per-model agentic/communal count tables
  - `lasso_results/seed_sensitivity/` — TF-IDF lasso seed-sensitivity outputs
  - `raw_tfidf_traintest_*.csv` — TF-IDF matrices used by the seed-sensitivity script

- `lexica/` — German agentic/communal word lists and stems

- `../figures/` — paper figures and tables copied from the original analysis outputs

## Reproducing the Analysis

Install Python dependencies:

```bash
pip install pandas
```

Install R dependencies:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "glmnet", "pROC", "tibble"))
```

If raw CV run exports are available locally, rerun the feature extraction:

```bash
python code/00_fix_cv_json.py --input data/cv_runs.csv --output data/cv_runs_fixed.csv
python code/01_structural_features.py --input data/cv_runs_fixed.csv --output data/structural_features.csv
python code/02_lexical_counts.py --input data/cv_runs_fixed.csv --output data/lexical_counts.csv
```

Rerun the TF-IDF seed-sensitivity models and plots:

```bash
Rscript code/03_tfidf_lasso_seed_sensitivity.R
Rscript code/04_plot_structural.R
Rscript code/05_plot_lexical.R
```

## Notes

- The lexical analysis only uses agentic and communal categories.
- Count-based lasso models and single-run TF-IDF lasso models are not included in this minimal submission.
- The submitted paper figures are stored in `../figures/` with the original colors and filenames.
- Raw generated CV JSON/text exports should not be committed to the anonymous repository.
