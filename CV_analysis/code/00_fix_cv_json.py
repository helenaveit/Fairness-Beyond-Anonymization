import argparse
import ast
import json
import re
from pathlib import Path

import pandas as pd


JSON_FENCE_RE = re.compile(r"```(?:json)?\s*(.*?)\s*```", re.DOTALL | re.IGNORECASE)
JSON_OBJECT_RE = re.compile(r"(\{.*\})", re.DOTALL)


def extract_json_text(value):
    if pd.isna(value):
        return None

    text = str(value).strip()
    if not text:
        return None

    fenced = JSON_FENCE_RE.search(text)
    if fenced:
        text = fenced.group(1).strip()

    if not text.startswith("{"):
        match = JSON_OBJECT_RE.search(text)
        if match:
            text = match.group(1).strip()

    return text


def parse_cv_json(value):
    text = extract_json_text(value)
    if text is None:
        raise ValueError("empty response_json")

    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass

    try:
        parsed = ast.literal_eval(text)
    except Exception as exc:
        raise ValueError(f"could not parse JSON/Python literal: {exc}") from exc

    if not isinstance(parsed, dict):
        raise ValueError("parsed response is not a JSON object")
    return parsed


def normalize_response_json(value):
    payload = parse_cv_json(value)
    return json.dumps(payload, ensure_ascii=False, sort_keys=True)


def main():
    parser = argparse.ArgumentParser(description="Normalize generated CV JSON in a CSV export.")
    parser.add_argument("--input", required=True, help="Input CSV with response_json column.")
    parser.add_argument("--output", required=True, help="Output CSV with normalized response_json column.")
    parser.add_argument("--json-column", default="response_json")
    parser.add_argument("--error-output", default=None, help="Optional CSV for rows that failed parsing.")
    args = parser.parse_args()

    df = pd.read_csv(args.input)
    if args.json_column not in df.columns:
        raise ValueError(f"Missing column: {args.json_column}")

    fixed_values = []
    errors = []

    for idx, value in df[args.json_column].items():
        try:
            fixed_values.append(normalize_response_json(value))
        except Exception as exc:
            fixed_values.append(value)
            row = df.loc[idx].to_dict()
            row["row_index"] = idx
            row["parse_error"] = str(exc)
            errors.append(row)

    out = df.copy()
    out[args.json_column] = fixed_values
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(args.output, index=False)

    if args.error_output:
        Path(args.error_output).parent.mkdir(parents=True, exist_ok=True)
        pd.DataFrame(errors).to_csv(args.error_output, index=False)

    print(f"Wrote {len(out)} rows to {args.output}")
    print(f"Rows with parse errors: {len(errors)}")


if __name__ == "__main__":
    main()

