# name_sampler_1985.py

import random
from pathlib import Path
import pandas as pd

# ---------------------------------------------------
# Paths
# ---------------------------------------------------

BASE_DIR = Path(__file__).resolve().parent
DATA_DIR = BASE_DIR.parent / "data" / "name_lists"

# ---------------------------------------------------
# Helpers
# ---------------------------------------------------

# First names form https://www.behindthename.com/top/lists/turkey/1985 and 
# https://www.behindthename.com/top/lists/germany/1985
def load_firstnames(path: Path):
    df = pd.read_csv(path, encoding="utf-8-sig")
    return df["first_name"].dropna().astype(str).str.strip().tolist()

# Last names from https://nachnamen.net/turkei and https://nachnamen.net/deutschland
def load_surnames(path: Path):
    df = pd.read_csv(path, encoding="utf-8-sig")
    return df["last_name"].dropna().astype(str).str.strip().tolist()

# ---------------------------------------------------
# Sampling logic
# ---------------------------------------------------

def sample_group(rng, n, female_list, male_list, surname_list, ethnicity):
    max_n = min(len(female_list), len(male_list), len(surname_list))

    if n > max_n:
        print(f"Warnung: n={n} > max_n={max_n} für '{ethnicity}'. Verwende n={max_n}.")
        n = max_n

    # Shuffle Kopien — garantiert seed-abhängigen Output
    f = female_list[:]
    m = male_list[:]
    s = surname_list[:]

    rng.shuffle(f)
    rng.shuffle(m)
    rng.shuffle(s)

    rows = []
    for i in range(n):
        rows.append({
            "first_name": f[i],
            "last_name": s[i],
            "gender": "female",
            "ethnicity": ethnicity
        })
        rows.append({
            "first_name": m[i],
            "last_name": s[i],
            "gender": "male",
            "ethnicity": ethnicity
        })
    return rows

# ---------------------------------------------------
# Main function
# ---------------------------------------------------

def sample_names(seed=678, n=10):
    # Load CSV lists
    de_female = load_firstnames(DATA_DIR / "german_female_first_1985.csv")
    de_male   = load_firstnames(DATA_DIR / "german_male_first_1985.csv")
    tr_female = load_firstnames(DATA_DIR / "turkish_female_first_1985.csv")
    tr_male   = load_firstnames(DATA_DIR / "turkish_male_first_1985.csv")

    de_last   = load_surnames(DATA_DIR / "german_last_name.csv")
    tr_last   = load_surnames(DATA_DIR / "turkish_last_name.csv")

    rng = random.Random(seed)

    rows = []
    rows += sample_group(rng, n, de_female, de_male, de_last, ethnicity="german")
    rows += sample_group(rng, n, tr_female, tr_male, tr_last, ethnicity="turkish")

    df = pd.DataFrame(rows)
    df.insert(0, "name_ID", range(1, len(df) + 1))
    return df

# ---------------------------------------------------
# Run directly
# ---------------------------------------------------

if __name__ == "__main__":
    df = sample_names(seed=42, n=10)
    print(df)
df.to_csv("CV_analysis/data/CV_names_1985.csv", index=False)