from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import List, Tuple, Dict, Any

import pandas as pd

# Exact headers expected in data_csv
REQUIRED_HEADERS = [
    "ID",
    "Wunschbranche",
    "Branchen-/Bereichserfahrung",
    "Favorisierte Unternehmensgröße",
    "Unternehmensumfeld",
    "Einsatzort (Großraum)",
    "Remote-Tätigkeit",
    "Umfang der Stelle",
    "Reisetätigkeit",
    "Sprachkompetenz DEUTSCH",
    "Sprachkompetenz ENGLISCH",
    "WEITERE Sprachkompetenzen",
    "Fachliches & funktionales Kompetenz-Profil",
    "Spezifische/r Themen- und Fachbereich/e",
    "Tätigkeitsfeld",
    "Beschreibung der Aufgabe",
    "Persönliche Kompetenzen",
    "Sozial-kommunikative Kompetenzen",
    "Aktivitäts- und umsetzungsorientierte Kompetenzen",
    "Denke an Deine bisherigen Teamerfahrungen zurück. Welche Rolle(n) nimmst Du in Teams am liebsten ein?",
    "Welche Werte sind Dir in dem Team der zu besetzenden Stelle besonders wichtig?",
    "Mit welchen Aussagen können Unternehmen bei Dir punkten?",
    "Meine Bildungsabschlüsse",
    "Bruttojahresgehalt",
]

def read_profiles(path: str | Path, *, nrows: int | None = None) -> pd.DataFrame:
    """
    Read and validate the profiles CSV (data_csv). Fails if headers don't match exactly.

    - No extra columns allowed.
    - Case-sensitive header match.
    - Delimiter auto-detected; falls back to comma.
    - Uses utf-8-sig encoding to tolerate BOM.
    """
    path = Path(path)

    # Detect delimiter
    with path.open("rb") as fh:
        sample = fh.read(4096)
    try:
        sep = csv.Sniffer().sniff(sample.decode(errors="ignore")).delimiter
    except Exception:
        sep = ","

    # Header check
    try:
        hdr_df = pd.read_csv(path, sep=sep, nrows=0, encoding="utf-8-sig", engine="python")
    except pd.errors.EmptyDataError as e:
        raise ValueError("profiles file is empty") from e

    raw_cols = list(hdr_df.columns)
    missing = sorted(set(REQUIRED_HEADERS) - set(raw_cols))
    extras = sorted(set(raw_cols) - set(REQUIRED_HEADERS))
    if missing or extras:
        parts = []
        if missing:
            parts.append(f"missing: {missing}")
        if extras:
            parts.append(f"unexpected: {extras}")
        raise ValueError("profiles header check failed; " + "; ".join(parts))

    # Load full or partial
    return pd.read_csv(
        path,
        sep=sep,
        nrows=nrows,
        encoding="utf-8-sig",
        engine="python",
        dtype="string",  # keeps values as strings
        keep_default_na=True,
    )


def load_names(names_file: str | Path) -> List[Tuple[str, str]]:
    """
    Load names from a file with one 'first last' per line.
    If there are more than 2 tokens, last token is last name, the rest is first name.
    """
    names: List[Tuple[str, str]] = []
    for line in Path(names_file).read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line:
            continue
        parts = line.split()
        if len(parts) == 1:
            first, last = parts[0], ""
        else:
            first = " ".join(parts[:-1])
            last = parts[-1]
        names.append((first, last))
    return names
