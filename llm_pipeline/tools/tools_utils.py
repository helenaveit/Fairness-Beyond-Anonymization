from __future__ import annotations

import argparse
import json
import re
import pandas as pd
from pathlib import Path
from typing import List, Dict, Any, Optional

from ..pipeline.firestore_utils import get_firestore_client


def load_collection_as_df(collection: str) -> pd.DataFrame:
    db = get_firestore_client()
    docs = db.collection(collection).stream()

    rows = []
    for doc in docs:
        d = doc.to_dict()
        d["_id"] = doc.id
        rows.append(d)
    return pd.DataFrame(rows)


def filter_fields(df: pd.DataFrame, exclude: List[str]) -> pd.DataFrame:
    """
    Exclude columns that match any of the given names.
    Match is exact but case-insensitive.
    """
    if df.empty or not exclude:
        return df

    exclude_lower = {c.lower() for c in exclude}
    keep_cols = [c for c in df.columns if c.lower() not in exclude_lower]
    return df[keep_cols]


def normalize_column_names(df: pd.DataFrame) -> pd.DataFrame:
    """
    Strip numeric prefixes like '01_' from column names.
    Example: '01_name' -> 'name', '12_usage' -> 'usage'.
    """
    def _norm(col: str) -> str:
        return re.sub(r"^\d+_", "", str(col))

    df = df.copy()
    df.columns = [_norm(c) for c in df.columns]
    return df


def _parse_maybe_json(value: Any) -> Any:
    """
    Try to parse a JSON-encoded string; otherwise return as-is.
    """
    if isinstance(value, (dict, list)) or pd.isna(value):
        return value
    s = str(value).strip()
    if not s:
        return None
    try:
        return json.loads(s)
    except Exception:
        return value


def flatten_name_and_usage(df: pd.DataFrame) -> pd.DataFrame:
    """
    - 'name' column: dict or JSON string -> split into 'first_name' and 'last_name'.
    - 'usage' column: dict or JSON string with prompt/completion/total tokens.
    """
    df = df.copy()

    # name -> first_name, last_name
    if "name" in df.columns:
        first_vals = []
        last_vals = []

        for v in df["name"]:
            parsed = _parse_maybe_json(v)
            if isinstance(parsed, dict):
                first_vals.append(parsed.get("first"))
                last_vals.append(parsed.get("last"))
            else:
                first_vals.append(None)
                last_vals.append(None)

        df["first_name"] = first_vals
        df["last_name"] = last_vals
        df = df.drop(columns=["name"])

    # usage -> prompt_tokens, completion_tokens, total_tokens
    if "usage" in df.columns:
        pt_vals = []
        ct_vals = []
        tt_vals = []

        for v in df["usage"]:
            parsed = _parse_maybe_json(v)
            if isinstance(parsed, dict):
                # usage dict itself has numbered keys inside, e.g. '01_prompt_tokens'
                # normalize inner keys
                inner = {re.sub(r"^\d+_", "", str(k)): val for k, val in parsed.items()}
                pt_vals.append(inner.get("prompt_tokens"))
                ct_vals.append(inner.get("completion_tokens"))
                tt_vals.append(inner.get("total_tokens"))
            else:
                pt_vals.append(None)
                ct_vals.append(None)
                tt_vals.append(None)

        df["prompt_tokens"] = pt_vals
        df["completion_tokens"] = ct_vals
        df["total_tokens"] = tt_vals
        df = df.drop(columns=["usage"])

    return df


def transform_for_collection(collection: str, df: pd.DataFrame) -> pd.DataFrame:
    """
    Apply collection-specific transformations:
      - normalize column names
      - flatten name and usage
    """
    if df.empty:
        return df

    df = normalize_column_names(df)
    df = flatten_name_and_usage(df)

    return df


def export_collections(collections: List[str], out_dir: Path, exclude: List[str]) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)

    for coll in collections:
        df = load_collection_as_df(coll)
        df = filter_fields(df, exclude)
        df = transform_for_collection(coll, df)

        out_path = out_dir / f"{coll}.csv"
        df.to_csv(out_path, index=False)
        print(f"Exported {coll} -> {out_path} ({len(df)} rows, {len(df.columns)} columns)")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Export Firestore collections to CSV.")
    parser.add_argument(
        "--collections", "-c",
        nargs="+",
        default=["cv_runs"],
        help="Collections to export (default: cv_runs)",
    )
    parser.add_argument(
        "--out-dir", "-o",
        default="llm_pipeline/outputs_export",
        help="Output directory for CSV files (default: llm_pipeline/outputs_export)",
    )
    parser.add_argument(
        "--exclude", "-x",
        nargs="+",
        default=[],
        help="Additional field names to exclude from export (case-insensitive)",
    )
    return parser.parse_args()
