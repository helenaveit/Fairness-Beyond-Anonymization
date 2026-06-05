from __future__ import annotations

import asyncio
import json
import os
import time
from datetime import timedelta
from pathlib import Path
from typing import Optional, Tuple, List, Dict, Any
import re

import pandas as pd
import yaml
from openai import AsyncOpenAI
from google.cloud.firestore import Client as FirestoreClient

from .data_utils import read_profiles, load_names
from .firestore_utils import get_firestore_client, build_doc_id, _clean_firestore_payload, _save_error_as_txt
from .prompts_utils import build_cv_prompt, _split_system_user, _chat_completion_with_retry
from .pricing import estimate_cost
from .unified_client import get_client

async def generate_cv_for_run(
    *,
    fs: FirestoreClient,
    openai_client: Any,
    provider_name: str,
    model: str,
    temperature: float,
    max_output_tokens: int,
    config_path: str | Path,
    profiles_df: pd.DataFrame,
    profile_index: int,
    first_name: str,
    last_name: str,
    age: int,
    prefer_json: bool = True,
    collection: str = "cv_runs",
    max_attempts: int,
    initial_backoff_seconds: float,
) -> Tuple[Optional[int], Optional[int], Optional[int], float, bool, str]:
    """
    Single CV run for one (row, name, age) combination.

    Returns:
        (prompt_tokens, completion_tokens, total_tokens, cost_usd, was_json, doc_path)
    """
    # Initialize variables for failure path (if API call fails)
    prompt_tokens, completion_tokens, total_tokens, cost_usd = 0, 0, 0, 0.0
    doc_id = None
    raw = "N/A"
    system_text = "N/A"
    user_text = "N/A"

    try:
        row = profiles_df.iloc[profile_index]
        profile_id = row.get("ID", profile_index)

        cfg = yaml.safe_load(Path(config_path).read_text(encoding="utf-8")) or {}
        inputs_cfg = cfg.get("inputs") or {}
        top_k = cfg.get("top_k")

        doc_id = build_doc_id(
            provider_name=provider_name,
            first_name=first_name,
            last_name=last_name,
            age=age,
            profile_id=profile_id,
            task_type="cv",
            top_k=top_k,
        )

        cv_prompt_path = inputs_cfg.get("cv_prompt_template")
        if not cv_prompt_path:
            raise ValueError("No CV prompt template path found in config (inputs.cv_prompt_template).")

        filled = build_cv_prompt(
            config_path=config_path,
            cv_prompt_path=cv_prompt_path,
            profiles_df=profiles_df,
            profile_index=profile_index,
            first_name=first_name,
            last_name=last_name,
            age=age,
        )

        system_text, user_text = _split_system_user(filled)

        routing_provider = cfg.get("provider", "").lower()
        top_p = cfg.get("top_p")

        kwargs = {}
        if prefer_json and routing_provider != "local":
            kwargs["response_format"] = {"type": "json_object"}
        if top_p is not None:
            kwargs["top_p"] = top_p
        extra_body = dict(cfg.get("extra_body") or {})
        if top_k is not None:
            extra_body["top_k"] = top_k
        if extra_body:
            kwargs["extra_body"] = extra_body

        resp = await _chat_completion_with_retry(
            openai_client=openai_client,
            model=model,
            temperature=temperature,
            max_tokens=max_output_tokens,
            system_text=system_text,
            user_text=user_text,
            max_attempts=max_attempts,
            initial_backoff_seconds=initial_backoff_seconds,
            **kwargs,
        )

        msg = resp.choices[0].message
        raw = msg.content or ""

        # --- Token and Cost Calculation ---
        if routing_provider == "local":
            prompt_tokens = None
            completion_tokens = None
            total_tokens = None
            cost_usd = 0.0
        else:
            usage = getattr(resp, "usage", None) or {}
            prompt_tokens = int(getattr(usage, "prompt_tokens", 0) or usage.get("prompt_tokens", 0) or 0)
            completion_tokens = int(getattr(usage, "completion_tokens", 0) or usage.get("completion_tokens", 0) or 0)
            total_tokens = int(getattr(usage, "total_tokens", prompt_tokens + completion_tokens))
            cost_usd = estimate_cost(provider_name, model, prompt_tokens, completion_tokens)

        was_json = False
        parsed: Optional[dict] = None

        if isinstance(raw, dict):
            parsed = raw
            was_json = True
        else:
            try:
                parsed = json.loads(raw)
                was_json = True
            except Exception:
                # Attempt robust JSON extraction with regex (for malformed text/wrappers)
                json_match = re.search(r"```json\s*(\{.*\})\s*```|(\{.*\})", raw, re.DOTALL)

                if json_match:
                    # Use the captured group (1 for wrapped, 2 for plain)
                    json_string = json_match.group(1) or json_match.group(2)
                    try:
                        parsed = json.loads(json_string)
                        was_json = True
                    except Exception as e_regex:
                        parsed = None
                        was_json = False
                        print(f"DEBUG: CV Regex extraction failed to parse JSON: {e_regex}")

                if not was_json:
                    parsed = None
                    was_json = False

        payload = {
            "01_name": {"first": first_name, "last": last_name},
            "02_age": age,
            "03_profile_id": profile_id,
            "04_task_type": "cv",
            "05_provider": provider_name,
            "06_model": model,
            "07_temperature": temperature,
            "08_prefer_json": prefer_json,
            "09_was_json": was_json,
            "10_cost_usd": cost_usd,
            "11_max_output_tokens": max_output_tokens,
            "12_usage": {
                "01_prompt_tokens": prompt_tokens,
                "02_completion_tokens": completion_tokens,
                "03_total_tokens": total_tokens,
            },
            "13_system_prompt": system_text,
            "14_user_prompt": user_text,
            "15_response_json": parsed if was_json else None,
            "16_response_text": None if was_json else raw,
            "17_top_k": top_k,
            "18_top_p": top_p,
        }

        doc_ref = fs.collection(collection).document(doc_id)
        cleaned_payload = _clean_firestore_payload(payload)
        doc_ref.set(cleaned_payload, merge=False)
        doc_path = f"{collection}/{doc_ref.id}"

        return prompt_tokens, completion_tokens, total_tokens, cost_usd, was_json, doc_path

    except Exception as e:
        print(f"FATAL CV ERROR: An unhandled exception occurred for job {doc_id or 'N/A'}: {e}")

        # Save to TXT file
        doc_id_prefix = doc_id if doc_id else f"row{profile_index}-age{age}-{first_name}{last_name}"
        error_doc_path = _save_error_as_txt(
            file_prefix=f"CV-{doc_id_prefix}",
            system_text=system_text,
            user_text=user_text,
            raw_response=raw,
            error_message=str(e),
            config_path=config_path,
        )

        # Return failure values, using the calculated tokens/cost (0 if API call failed)
        return prompt_tokens, completion_tokens, total_tokens, cost_usd, False, error_doc_path



async def run_one(
    *,
    job_index: int,
    total_runs: int,
    row_idx: int,
    age: int,
    first_name: str,
    last_name: str,
    fs: FirestoreClient,
    openai_client: Any,
    provider: str,
    model: str,
    temperature: float,
    max_output_tokens: int,
    config_path: Path,
    profiles_df: pd.DataFrame,
    prefer_json_cv: bool,
    sem: asyncio.Semaphore,
    max_attempts: int,
    initial_backoff_seconds: float,
) -> Dict[str, Any]:
    """
    One CV run for a single (row_idx, age, first_name, last_name) combination.
    Wrapped with a semaphore to respect the concurrency limit.
    Returns token + cost stats and the Firestore doc path.
    """
    async with sem:
        job_start = time.perf_counter()
        print(
            f"[{job_index}/{total_runs}] "
            f"row={row_idx}, name={first_name} {last_name}, age={age}"
        )

        (
            cv_pt,
            cv_ct,
            cv_tt,
            cv_cost,
            cv_was_json,
            cv_doc_path,
        ) = await generate_cv_for_run(
            fs=fs,
            openai_client=openai_client,
            provider_name=provider,
            model=model,
            temperature=temperature,
            max_output_tokens=max_output_tokens,
            config_path=config_path,
            profiles_df=profiles_df,
            profile_index=row_idx,
            first_name=first_name,
            last_name=last_name,
            age=age,
            prefer_json=prefer_json_cv,
            max_attempts=max_attempts,
            initial_backoff_seconds=initial_backoff_seconds,
        )

        job_end = time.perf_counter()
        job_elapsed = job_end - job_start

        print(
            f"  -> finished [{job_index}/{total_runs}] "
            f"row={row_idx}, name={first_name} {last_name}, age={age}; "
            f"job_time={job_elapsed:.2f}s"
        )

        return {
            "cv_doc_path": cv_doc_path,
            "cv_prompt_tokens": cv_pt,
            "cv_completion_tokens": cv_ct,
            "cv_cost_usd": cv_cost,
        }

def format_duration(seconds: float) -> str:
    td = timedelta(seconds=int(seconds))
    return str(td)


async def run_all_from_config(config_path: str | Path):
    """
    Read config, then for each (row, age, name) combination generate a résumé
    via generate_cv_for_run (stored in Firestore).

    Supports concurrency > 1 via config['concurrency'].
    Returns a list of dicts: {"cv": <cv_doc_path>}.
    """
    start_time = time.perf_counter()
    config_path = Path(config_path)
    cfg = yaml.safe_load(config_path.read_text(encoding="utf-8"))

    provider = cfg["provider"]
    model = cfg["model"]
    # provider_name is used for Firestore doc IDs and logging.
    # Defaults to provider, but can be overridden in the config to distinguish
    # multiple local models that share the same provider value.
    provider_name = cfg.get("provider_name", provider)
    temperature = cfg["temperature"]
    max_output_tokens = cfg["max_output_tokens"]
    run_scope = cfg["run_scope"]
    inputs = cfg["inputs"]
    alerts = cfg.get("alerts", {})
    cv_output_pref = str(cfg.get("cv_output_preference", "json")).lower()
    concurrency = int(cfg.get("concurrency", 1))

    retry_cfg = cfg.get("retry", {})
    max_attempts = int(retry_cfg.get("max_attempts", 3))
    initial_backoff_seconds = float(retry_cfg.get("initial_backoff_seconds", 5.0))

    prefer_json_cv = cv_output_pref == "json"

    data_csv = inputs["data_csv"]
    names_file = inputs["names_file"]

    profiles_df = read_profiles(data_csv)
    names = load_names(names_file)

    max_rows_conf = run_scope["rows"]
    max_rows = min(max_rows_conf, len(profiles_df))
    row_indices = list(range(max_rows))

    ages = run_scope["ages"]
    names_mode = run_scope.get("names", "all")
    mode = names_mode.strip().lower()

    if mode == "all":
        names_to_use = names

    elif "-" in mode:
        # e.g. "1-50" means: use names[0:50] (1-based inclusive range)
        start_str, end_str = mode.split("-", 1)
        start = int(start_str)
        end = int(end_str)

        # convert 1-based to 0-based, clamp to list length
        start_idx = max(start - 1, 0)
        end_idx = min(end, len(names))

        if start_idx >= end_idx:
            raise ValueError(f"names range {names_mode!r} is empty for {len(names)} names")

        names_to_use = names[start_idx:end_idx]
    else:
        raise ValueError(f"Unsupported names mode: {names_mode!r}")

    fs = get_firestore_client()

    # Set up the LLM client
    client = get_client(provider, model)

    # Build job list
    jobs: List[tuple[int, int, str, str]] = []
    for row_idx in row_indices:
        for age in ages:
            for first_name, last_name in names_to_use:
                jobs.append((row_idx, age, first_name, last_name))

    total_runs = len(jobs)
    if total_runs == 0:
        print("Nothing to run (no rows/ages/names).")
        return []

    print(f"Starting CV runs with concurrency={concurrency}: {total_runs} combinations")

    sem = asyncio.Semaphore(concurrency)

    # Create tasks
    tasks = [
        run_one(
            job_index=i + 1,
            total_runs=total_runs,
            row_idx=row_idx,
            age=age,
            first_name=first_name,
            last_name=last_name,
            fs=fs,
            openai_client=client,
            provider=provider_name,
            model=model,
            temperature=temperature,
            max_output_tokens=max_output_tokens,
            config_path=config_path,
            profiles_df=profiles_df,
            prefer_json_cv=prefer_json_cv,
            sem=sem,
            max_attempts=max_attempts,
            initial_backoff_seconds=initial_backoff_seconds,
        )
        for i, (row_idx, age, first_name, last_name) in enumerate(jobs)
    ]

    results = await asyncio.gather(*tasks)

    doc_paths: List[Dict[str, str]] = []

    total_prompt_tokens = 0
    total_completion_tokens = 0
    total_cost_usd = 0.0

    for res in results:
        doc_paths.append({"cv": res["cv_doc_path"]})

        total_prompt_tokens += res["cv_prompt_tokens"] or 0
        total_completion_tokens += res["cv_completion_tokens"] or 0
        total_cost_usd += res["cv_cost_usd"]

    total_tokens = total_prompt_tokens + total_completion_tokens

    warn_tokens = alerts.get("token_total_warn")
    hard_cap_tokens = alerts.get("token_total_hard_cap")

    if hard_cap_tokens is not None and total_tokens > hard_cap_tokens:
        print(f"HARD CAP exceeded: total tokens {total_tokens} > {hard_cap_tokens}")
    elif warn_tokens is not None and total_tokens > warn_tokens:
        print(f"Warning: total tokens {total_tokens} > {warn_tokens}")

    end_time = time.perf_counter()
    elapsed = end_time - start_time

    print("All CV runs finished.")
    print(
        f"Summary: runs={total_runs}, "
        f"tokens_total={total_tokens} (prompt={total_prompt_tokens}, completion={total_completion_tokens}), "
        f"approx_cost={total_cost_usd:.6f} USD"
    )
    print(f"Total wall-clock time: {format_duration(elapsed)}")
