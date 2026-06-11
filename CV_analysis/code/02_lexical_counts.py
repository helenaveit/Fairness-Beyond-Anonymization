import argparse
import json
import re
from pathlib import Path

import pandas as pd


TOKEN_RE = re.compile(r"[a-zäöüß\-]+", re.IGNORECASE)


def tokenize(text):
    return TOKEN_RE.findall(str(text).lower())


def load_tum_stems(path):
    stems = pd.read_csv(path)
    stems["stem"] = stems["stem"].astype(str).str.strip().str.lower()
    return {
        category: stems.loc[stems["category"] == category, "stem"].tolist()
        for category in ["agentic", "communal"]
    }


def build_patterns(stems):
    return [re.compile(rf"^{re.escape(stem)}") for stem in stems if len(stem) > 1]


def count_stems(tokens, patterns):
    return sum(any(pattern.match(token) for pattern in patterns) for token in tokens)


def extract_text_without_personal(payload):
    if isinstance(payload, dict):
        payload = {
            key: value
            for key, value in payload.items()
            if "persoenliche" not in key.lower() and "persönliche" not in key.lower()
        }

    texts = []

    def collect(value):
        if isinstance(value, dict):
            for child in value.values():
                collect(child)
        elif isinstance(value, list):
            for child in value:
                collect(child)
        elif isinstance(value, str) and value.strip():
            texts.append(value.strip())

    collect(payload)
    return "\n".join(texts)


def main():
    parser = argparse.ArgumentParser(description="Count lexical categories in generated CV JSON.")
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--json-column", default="response_json")
    parser.add_argument("--lexica-dir", default="lexica")
    args = parser.parse_args()

    lexica_dir = Path(args.lexica_dir)
    tum_stems = load_tum_stems(lexica_dir / "tum_gender_stems.csv")
    patterns = {
        "agentic": build_patterns(tum_stems["agentic"]),
        "communal": build_patterns(tum_stems["communal"]),
    }

    df = pd.read_csv(args.input)
    rows = []
    for _, row in df.iterrows():
        payload = json.loads(row[args.json_column])
        tokens = tokenize(extract_text_without_personal(payload))
        meta = {col: row[col] for col in ["profile_id", "gender", "ethnicity", "name_ID", "provider", "model"] if col in df.columns}
        rows.append({
            **meta,
            "tokens": len(tokens),
            "agentic_count": count_stems(tokens, patterns["agentic"]),
            "communal_count": count_stems(tokens, patterns["communal"]),
        })

    out = pd.DataFrame(rows)
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(args.output, index=False)
    print(f"Wrote {len(out)} rows to {args.output}")


if __name__ == "__main__":
    main()
