# CV ANALYSIS (STAGE 2: RÉSUMÉ SCREENING AUDIT)

This folder contains code, notebooks and derived data for analysing the resumes (CV-texts) across protected groups (gender, ethnicity). It contains the derivation/extraction of the observed features (structural, POS, lexical and TF-IDF) in Python and the training of corresponing bias classifiers in R.


## Project Structure

- `code/` — analysis scripts and notebooks
  - `Strucutral/` — Extraction of sections, length and POS counts
  - `Lexical_based/` Agentic/communal and certainty/tentativeness word counts
  - `Tfidf/` Analysis of keywords 

- `data/` — input data and derived outputs
  - `tfidf/` — TF‑IDF matrices
  - `full_count_data/` — combined data of extracted count features (structural and lexical)

- `lexica/`- Agentic/communal and certainty/tentativeness lexica


- `CV_json_to_text/` — Convert JSON CVs to TeX 


Before running the Python code, install required packages from the project `requirements.txt`:

```bash
pip install -r CV_analysis/requirements.txt
```
