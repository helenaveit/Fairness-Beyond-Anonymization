## download_tum_gender_lists.py
# -----------------------------
# Fetches TUM Gender Decoder word lists (agentic & communal) and saves:
#  - lexica/tum_gender_words.csv  (example words)
#  - lexica/tum_gender_stems.csv  (clean stems, no '*')

import os
import pandas as pd
import requests
from bs4 import BeautifulSoup

URLS = {
    "agentic": "https://www.msl.mgt.tum.de/rm/third-party-funded-projects/projekt-fuehrmint/gender-decoder/wortlisten/agentische-woerter/",
    "communal": "https://www.msl.mgt.tum.de/rm/third-party-funded-projects/projekt-fuehrmint/gender-decoder/wortlisten/kommunale-woerter/",
}

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                  "AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.5 Safari/605.1.15"
}

def parse_page(url: str, category: str):
    """Return two DataFrames: (words_df, stems_df)."""
    r = requests.get(url, headers=HEADERS, timeout=30)
    r.raise_for_status()
    soup = BeautifulSoup(r.text, "html.parser")

    table = soup.find("table")
    if table is None:
        ul = soup.find("ul")
        if not ul:
            print(f"Could not find a table or list on {url}")
            empty = pd.DataFrame(columns=["word","category"])
            return empty, empty
        words = []
        for li in ul.find_all("li"):
            w = li.get_text(strip=True).lower()
            if w:
                words.append({"word": w, "category": category})
        dfw = pd.DataFrame(words).drop_duplicates()
        return dfw, pd.DataFrame(columns=["stem","category"])

    words_rows, stems_rows = [], []
    words_rows, stems_rows = [], []

    # Expect: Wortstamm | Beispielwörter | Beispielausdruck
    for tr in table.find_all("tr"):
        cells = [td.get_text(strip=True) for td in tr.find_all(["td","th"])]
        if not cells:
            continue
        head = " ".join(cells).lower()
        if "wort" in head and "beispiel" in head:
            continue

        wortstamm = cells[0] if len(cells) > 0 else ""
        beispiele = cells[1] if len(cells) > 1 else ""

        # example words
        for w in (beispiele or "").split(","):
            w = w.strip().lower()
            if w:
                words_rows.append({"word": w, "category": category})

        # clean stem (no '*')
        stem = (wortstamm or "").replace("*", "").strip().lower()
        if stem and stem != "-":
            stems_rows.append({"stem": stem, "category": category})

    dfw = pd.DataFrame(words_rows)
    dfs = pd.DataFrame(stems_rows)

    if not dfw.empty:
        dfw["word"] = dfw["word"].astype(str).str.strip().str.lower()
        dfw = dfw[dfw["word"].str.len() >= 2].drop_duplicates(subset=["word","category"])

    if not dfs.empty:
        dfs["stem"] = dfs["stem"].astype(str).str.strip().str.lower()
        dfs = dfs[dfs["stem"].str.len() >= 2].drop_duplicates(subset=["stem","category"])

    return dfw, dfs

def main():
    os.makedirs("lexica", exist_ok=True)
    words_all, stems_all = [], []

    for cat, url in URLS.items():
        print(f"Fetching {cat} words …")
        dfw, dfs = parse_page(url, cat)
        print(f"→ words: {len(dfw)} | stems: {len(dfs)}")
        words_all.append(dfw)
        stems_all.append(dfs)

    words = pd.concat([d for d in words_all if not d.empty], ignore_index=True) if words_all else pd.DataFrame(columns=["word","category"])
    stems = pd.concat([d for d in stems_all if not d.empty], ignore_index=True) if stems_all else pd.DataFrame(columns=["stem","category"])

    words = words.drop_duplicates(subset=["word","category"])
    stems = stems.drop_duplicates(subset=["stem","category"])

    words_out = "lexica/tum_gender_words.csv"
    stems_out = "lexica/tum_gender_stems.csv"
    words.to_csv(words_out, index=False)
    stems.to_csv(stems_out, index=False)

    print(f"Saved words → {words_out}  ({len(words)} rows)")
    print(f"Saved stems → {stems_out}  ({len(stems)} rows)")
    if not words.empty:
        print(words.head())
    if not stems.empty:
        print(stems.head())

if __name__ == "__main__":
    main()
