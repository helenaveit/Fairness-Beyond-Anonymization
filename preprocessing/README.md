# Preprocessing

This folder manages the preprocessing R pipeline for transforming raw profile data into analysis-ready profiles and preparing random samples for subsequent pipeline stages.


## Folder Contents

| Folder/File | Purpose |
| :--- | :--- |
| **`data/`** | Stores the raw source file (excluded here for confidentiality reasons).|
| **`code/`** | Contains the R scripts: |
| &nbsp;&nbsp;&nbsp; `data_preprocessing.R` | The main script that runs all transformation, **sampling** of profiles, and saving stages. |
| &nbsp;&nbsp;&nbsp; `transform_fns.R` | Core functions for data transformation (cleaning, pivoting, one-hot encoding). |
| &nbsp;&nbsp;&nbsp; `helpers.R` | Helper functions for string normalization and column ordering etc. |
| **`output/`** | Stores all generated data artifacts (excluded here for confidentiality reasons).|
