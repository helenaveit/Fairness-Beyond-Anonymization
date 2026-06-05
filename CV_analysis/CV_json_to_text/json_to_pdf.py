import argparse
import ast
import os
import re
import shutil
import subprocess
from pathlib import Path

import pandas as pd


SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parents[1]
DEFAULT_INPUT_CSV = REPO_ROOT / "llm_pipeline/outputs_export/cv_runs_fixed.csv"
DEFAULT_OUTPUT_ROOT = SCRIPT_DIR

MODEL_PRESETS = {
    "openai": {
        "provider": "openai",
        "output_dir": "openai_cvs_tex_harvard_ordered",
    },
    "gemini": {
        "provider": "google-gemini",
        "output_dir": "gemini_cvs_tex_harvard_ordered",
    },
    "qwen4B": {
        "provider": "qwen3-4b",
        "output_dir": "qwen4B_cvs_tex_harvard_ordered",
    },
    "qwen8B": {
        "provider": "qwen3-8b",
        "output_dir": "qwen8B_cvs_tex_harvard_ordered",
    },
}


def latex_escape(text):
    if text is None:
        return ""
    text = str(text)
    replacements = {
        "\\": r"\textbackslash{}",
        "&": r"\&",
        "%": r"\%",
        "$": r"\$",
        "#": r"\#",
        "_": r"\_",
        "{": r"\{",
        "}": r"\}",
        "~": r"\textasciitilde{}",
        "^": r"\textasciicircum{}",
    }
    for old, new in replacements.items():
        text = text.replace(old, new)
    return text


def slugify(value):
    value = str(value or "").lower()
    value = (
        value.replace("ä", "ae")
        .replace("ö", "oe")
        .replace("ü", "ue")
        .replace("ß", "ss")
        .replace("ş", "s")
        .replace("ğ", "g")
        .replace("ı", "i")
        .replace("ç", "c")
    )
    value = re.sub(r"\s+", "_", value)
    value = re.sub(r"[^a-z0-9_]+", "", value)
    return value.strip("_") or "unknown"


def parse_response_json(raw):
    if pd.isna(raw):
        raise ValueError("response_json is missing")
    return ast.literal_eval(str(raw))


def section_title(num, title):
    return rf"\section{{{num}. {latex_escape(title)}}}"


def stringify_value(value):
    if value is None:
        return ""
    if isinstance(value, list):
        return "; ".join(stringify_value(v) for v in value if stringify_value(v))
    if isinstance(value, dict):
        parts = []
        for key, val in value.items():
            val_str = stringify_value(val)
            if val_str:
                parts.append(f"{key}: {val_str}")
        return "; ".join(parts)
    return str(value)


def format_personal_data_section(num, personal):
    if not personal or not isinstance(personal, dict):
        return ""

    lines = [section_title(num, "Persönliche Daten")]
    lines.append(r"\begin{itemize}[leftmargin=1.2em,itemsep=0.2em]")
    for key, value in personal.items():
        value_str = stringify_value(value)
        if value_str:
            lines.append(r"\item " + rf"\textbf{{{latex_escape(key)}}}: {latex_escape(value_str)}")
    lines.append(r"\end{itemize}")
    return "\n".join(lines)


def format_text_section(num, title, content):
    content = stringify_value(content)
    if not content:
        return ""
    return "\n".join([section_title(num, title), latex_escape(content)])


def format_list_section(num, title, content):
    if not content:
        return ""

    lines = [section_title(num, title)]
    if isinstance(content, str):
        lines.append(latex_escape(content))
        return "\n".join(lines)

    if isinstance(content, dict):
        iterable = [f"{key}: {stringify_value(value)}" for key, value in content.items()]
    elif isinstance(content, list):
        iterable = [stringify_value(item) for item in content]
    else:
        lines.append(latex_escape(str(content)))
        return "\n".join(lines)

    items = [item for item in iterable if item]
    if not items:
        return ""
    lines.append(r"\begin{itemize}[leftmargin=1.2em,itemsep=0.2em]")
    for item in items:
        lines.append(r"\item " + latex_escape(item))
    lines.append(r"\end{itemize}")
    return "\n".join(lines)


def format_experience_section(num, experience):
    if not experience:
        return ""

    lines = [section_title(num, "Berufserfahrung")]
    if isinstance(experience, str):
        lines.append(latex_escape(experience))
        return "\n".join(lines)

    if isinstance(experience, dict):
        experience = [experience]

    if not isinstance(experience, list):
        lines.append(latex_escape(stringify_value(experience)))
        return "\n".join(lines)

    for job in experience:
        if not isinstance(job, dict):
            item = stringify_value(job)
            if item:
                lines.append(latex_escape(item) + r"\\")
            continue

        company = latex_escape(job.get("Unternehmen") or job.get("unternehmen") or job.get("company") or "")
        position = latex_escape(
            job.get("Position")
            or job.get("position")
            or job.get("job_title")
            or job.get("stelle")
            or ""
        )
        duration = latex_escape(
            job.get("Zeitraum")
            or job.get("zeitraum")
            or job.get("Dauer")
            or job.get("dauer")
            or job.get("duration")
            or ""
        )
        location = latex_escape(job.get("Ort") or job.get("ort") or "")

        header = position
        if header:
            header = r"\textbf{" + header + "}"
        if company:
            header = (header + r" \hfill " if header else "") + company
        if duration:
            header = (header + r" \hfill " if header else "") + duration
        if header:
            lines.append(header + r"\\")
        if location:
            lines.append(location + r"\\")

        description = (
            job.get("Beschreibung")
            or job.get("beschreibung")
            or job.get("Aufgabenbeschreibung")
            or job.get("aufgabenbeschreibung")
        )
        if description:
            lines.append(latex_escape(stringify_value(description)) + r"\\")

        tasks = job.get("Aufgaben") or job.get("aufgaben")
        if isinstance(tasks, list) and tasks:
            lines.append(r"\begin{itemize}[leftmargin=1.2em,itemsep=0.2em]")
            for task in tasks:
                lines.append(r"\item " + latex_escape(stringify_value(task)))
            lines.append(r"\end{itemize}")
        lines.append(r"\vspace{0.4em}")

    return "\n".join(lines)


def format_education_section(num, education):
    if not education:
        return ""

    lines = [section_title(num, "Ausbildung")]
    if isinstance(education, str):
        lines.append(latex_escape(education))
        return "\n".join(lines)

    if isinstance(education, dict):
        education = [education]

    if not isinstance(education, list):
        lines.append(latex_escape(stringify_value(education)))
        return "\n".join(lines)

    for entry in education:
        if not isinstance(entry, dict):
            item = stringify_value(entry)
            if item:
                lines.append(latex_escape(item) + r"\\")
            continue

        degree = latex_escape(entry.get("Abschluss") or entry.get("abschluss") or entry.get("degree") or "")
        field = latex_escape(
            entry.get("Studienrichtung")
            or entry.get("Fachrichtung")
            or entry.get("fachrichtung")
            or entry.get("field")
            or entry.get("studiengang")
            or ""
        )
        institution = latex_escape(
            entry.get("Institution")
            or entry.get("institution")
            or entry.get("Universität")
            or entry.get("Universitaet")
            or entry.get("unternehmen")
            or ""
        )
        year = latex_escape(
            entry.get("Jahr")
            or entry.get("year")
            or entry.get("Abschlussjahr")
            or entry.get("Zeitraum")
            or entry.get("zeitraum")
            or entry.get("dauer")
            or ""
        )

        line = degree
        if field:
            line = f"{line}, {field}" if line else field
        if institution:
            line = f"{line}, {institution}" if line else institution
        if year:
            line = f"{line} \\hfill {year}" if line else year
        if line:
            lines.append(line + r"\\")

    return "\n".join(lines)


def extract_display_name(meta, data):
    personal = data.get("01_persoenliche_daten", {}) or {}
    if isinstance(personal, dict):
        for key in ["Name", "name", "voller_name", "vollname", "full_name"]:
            value = personal.get(key)
            if isinstance(value, str) and value.strip():
                return value.strip()
        first = personal.get("Vorname") or personal.get("vorname")
        last = personal.get("Nachname") or personal.get("nachname")
        if first or last:
            return f"{first or ''} {last or ''}".strip()
    return f"{meta['first_name']} {meta['last_name']}".strip()


def build_cv_latex(meta, data):
    personal = data.get("01_persoenliche_daten", {}) or {}
    name = extract_display_name(meta, data)

    header = r"""\documentclass[9pt,a4paper]{article}
\usepackage[margin=1.8cm]{geometry}
\usepackage[ngerman]{babel}
\usepackage{fontspec}
\usepackage{microtype}
\usepackage{enumitem}
\usepackage[hidelinks]{hyperref}
\usepackage{titlesec}

\setlength{\parskip}{3pt}
\setlength{\parindent}{0pt}

\titleformat{\section}{\large\bfseries\scshape}{}{0pt}{}[\titlerule]

\begin{document}
\small
"""
    name_block = r"{\huge\bfseries " + latex_escape(name) + r"}" + "\n\n"
    sections = [
        format_personal_data_section(1, personal),
        format_text_section(2, "Profil", data.get("02_profil", "")),
        format_list_section(3, "Fähigkeiten", data.get("03_faehigkeiten", "")),
        format_experience_section(4, data.get("04_berufserfahrung", "")),
        format_education_section(5, data.get("05_ausbildung", "")),
        format_list_section(6, "Skills", data.get("06_skills", "")),
        format_list_section(7, "Sprachen", data.get("07_sprachen", "")),
        format_list_section(8, "Interessen", data.get("08_interessen", "")),
        format_text_section(9, "Angestrebte Position", data.get("09_angestrebte_position", "")),
        format_text_section(10, "Cover Letter Snippet", data.get("10_cover_letter_snippet", "")),
    ]
    body = "\n\n".join(section for section in sections if section)
    return header + name_block + body + "\n\\end{document}\n"


def compile_tex(tex_path, pdf_dir, engine="xelatex"):
    tex_path = Path(tex_path).resolve()
    pdf_dir = Path(pdf_dir).resolve()
    pdf_dir.mkdir(parents=True, exist_ok=True)
    if shutil.which(engine) is None:
        raise RuntimeError(f"LaTeX engine not found: {engine}")

    cmd = [
        engine,
        "-interaction=nonstopmode",
        "-halt-on-error",
        f"-output-directory={pdf_dir}",
        tex_path.name,
    ]
    result = subprocess.run(
        cmd,
        cwd=tex_path.parent,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    return result.returncode == 0, result.stdout


def filter_runs(df, provider=None, model_contains=None, sample_per_profile=None, limit=None):
    out = df.copy()
    if provider:
        out = out[out["provider"].astype(str).str.contains(provider, case=False, na=False)].copy()
    if model_contains:
        out = out[out["model"].astype(str).str.contains(model_contains, case=False, na=False)].copy()
    out = out.sort_values(["profile_id", "first_name", "last_name"]).reset_index(drop=True)
    if sample_per_profile:
        out = out.groupby("profile_id", group_keys=False).head(sample_per_profile).reset_index(drop=True)
    if limit:
        out = out.head(limit).reset_index(drop=True)
    return out


def write_cvs(
    input_csv,
    output_dir,
    provider=None,
    model_contains=None,
    compile_pdfs=False,
    engine="xelatex",
    sample_per_profile=None,
    limit=None,
):
    input_csv = Path(input_csv).resolve()
    output_dir = Path(output_dir).resolve()
    tex_dir = output_dir / "tex"
    pdf_dir = output_dir / "pdfs"
    tex_dir.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(input_csv)
    df = filter_runs(df, provider=provider, model_contains=model_contains, sample_per_profile=sample_per_profile, limit=limit)

    records = []
    for _, row in df.iterrows():
        try:
            data = parse_response_json(row["response_json"])
        except Exception as exc:
            records.append({
                "profile_id": row.get("profile_id"),
                "first_name": row.get("first_name", ""),
                "last_name": row.get("last_name", ""),
                "provider": row.get("provider", ""),
                "model": row.get("model", ""),
                "status": "parse_failed",
                "error": str(exc),
            })
            continue

        meta = {
            "profile_id": row.get("profile_id"),
            "first_name": row.get("first_name", ""),
            "last_name": row.get("last_name", ""),
        }
        filename = f"cv_{meta['profile_id']}_{slugify(meta['first_name'])}_{slugify(meta['last_name'])}.tex"
        tex_path = (tex_dir / filename).resolve()
        tex_path.write_text(build_cv_latex(meta, data), encoding="utf-8")

        record = {
            "profile_id": meta["profile_id"],
            "first_name": meta["first_name"],
            "last_name": meta["last_name"],
            "provider": row.get("provider", ""),
            "model": row.get("model", ""),
            "tex_path": str(tex_path),
            "pdf_path": str((pdf_dir / filename.replace(".tex", ".pdf")).resolve()),
            "status": "tex_written",
            "error": "",
        }

        if compile_pdfs:
            ok, log = compile_tex(tex_path, pdf_dir=pdf_dir, engine=engine)
            record["status"] = "pdf_compiled" if ok else "compile_failed"
            if not ok:
                log_path = tex_path.with_suffix(".compile.log")
                log_path.write_text(log, encoding="utf-8", errors="replace")
                record["error"] = str(log_path)
        records.append(record)

    index = pd.DataFrame(records)
    index_path = output_dir / "index.csv"
    index.to_csv(index_path, index=False)
    print(f"Rows selected: {len(df)}")
    print(f"TeX output: {tex_dir}")
    if compile_pdfs:
        print(f"PDF output: {pdf_dir}")
    print(f"Index: {index_path}")
    print(index["status"].value_counts(dropna=False).to_string())
    return index


def parse_args():
    parser = argparse.ArgumentParser(description="Convert generated CV JSON rows to LaTeX and optional PDFs.")
    parser.add_argument("--input-csv", default=str(DEFAULT_INPUT_CSV))
    parser.add_argument("--output-root", default=str(DEFAULT_OUTPUT_ROOT))
    parser.add_argument("--preset", choices=sorted(MODEL_PRESETS), help="Convenience preset for provider/output folder.")
    parser.add_argument("--provider", help="Provider filter, e.g. qwen3-8b.")
    parser.add_argument("--model-contains", help="Optional model substring filter.")
    parser.add_argument("--output-dir", help="Output directory. Overrides preset output_dir.")
    parser.add_argument("--compile", action="store_true", help="Compile TeX files to PDFs.")
    parser.add_argument("--engine", default="xelatex", help="LaTeX engine for --compile.")
    parser.add_argument("--sample-per-profile", type=int, help="Keep first N rows per profile_id after sorting.")
    parser.add_argument("--limit", type=int, help="Keep only the first N rows after filtering.")
    return parser.parse_args()


def main():
    args = parse_args()
    preset = MODEL_PRESETS.get(args.preset or "", {})
    provider = args.provider or preset.get("provider")
    output_name = args.output_dir or preset.get("output_dir") or "cvs_tex_harvard_ordered"
    output_dir = Path(args.output_root) / output_name
    write_cvs(
        input_csv=args.input_csv,
        output_dir=output_dir,
        provider=provider,
        model_contains=args.model_contains,
        compile_pdfs=args.compile,
        engine=args.engine,
        sample_per_profile=args.sample_per_profile,
        limit=args.limit,
    )


if __name__ == "__main__":
    main()
