import argparse
import json
import re
from pathlib import Path

import pandas as pd


SECTION_KEYS = [
    "01_persoenliche_daten",
    "02_profil",
    "03_faehigkeiten",
    "04_berufserfahrung",
    "05_ausbildung",
    "06_skills",
    "07_sprachen",
    "08_interessen",
    "09_angestrebte_position",
    "10_cover_letter_snippet",
]

SECTION_PREFIXES = [
    "k01_persoenliche_daten",
    "k02_profil",
    "k03_faehigkeiten",
    "k04_berufserfahrung",
    "k05_ausbildung",
    "k06_skills",
    "k07_sprachen",
    "k08_interessen",
    "k09_angestrebte_position",
    "k10_cover_letter_snippet",
]

SECTION_PATTERNS = [
    r"persoenliche|persönliche",
    r"profil",
    r"faehigkeiten|fähigkeiten",
    r"berufserfahrung",
    r"ausbildung",
    r"skills",
    r"sprachen",
    r"interessen",
    r"angestrebte[_ ]?position",
    r"cover[_ ]?letter",
]


def count_words(value):
    if value is None:
        return 0
    if isinstance(value, str):
        return len(re.findall(r"\S+", value))
    if isinstance(value, dict):
        return sum(count_words(child) for child in value.values())
    if isinstance(value, list):
        return sum(count_words(child) for child in value)
    return 0


def section_match(key):
    key = re.sub(r"^\d+_?", "", str(key).lower())
    for section_key, pattern in zip(SECTION_KEYS, SECTION_PATTERNS):
        if re.search(pattern, key, flags=re.IGNORECASE):
            return section_key
    return None


def extract_structural_features(payload):
    sections = {key: None for key in SECTION_KEYS}
    if isinstance(payload, dict):
        for key, value in payload.items():
            matched = section_match(key)
            if matched:
                sections[matched] = value

    features = {
        "num_top_keys": len(payload) if isinstance(payload, dict) else 0,
        "cv_total_words": count_words(payload),
    }

    for section_key, prefix in zip(SECTION_KEYS, SECTION_PREFIXES):
        value = sections[section_key]
        if value is None:
            num_items = 0
            num_subkeys = 0
        elif isinstance(value, dict):
            num_items = len(value)
            num_subkeys = sum(1 for key in value.keys() if str(key).strip())
        elif isinstance(value, list):
            num_items = len(value)
            num_subkeys = 0
        elif isinstance(value, str):
            num_items = int(bool(value.strip()))
            num_subkeys = 0
        else:
            num_items = 0
            num_subkeys = 0

        features[f"{prefix}_num_items"] = num_items
        features[f"{prefix}_num_subkeys"] = num_subkeys
        features[f"{prefix}_num_words"] = count_words(value)

    return features


def main():
    parser = argparse.ArgumentParser(description="Extract structural CV features from normalized JSON.")
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--json-column", default="response_json")
    args = parser.parse_args()

    df = pd.read_csv(args.input)
    rows = []
    for _, row in df.iterrows():
        payload = json.loads(row[args.json_column])
        meta = {col: row[col] for col in ["profile_id", "gender", "ethnicity", "name_ID", "provider", "model"] if col in df.columns}
        rows.append({**meta, **extract_structural_features(payload)})

    out = pd.DataFrame(rows)
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(args.output, index=False)
    print(f"Wrote {len(out)} rows to {args.output}")


if __name__ == "__main__":
    main()

