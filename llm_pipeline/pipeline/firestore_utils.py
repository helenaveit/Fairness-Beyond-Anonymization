from __future__ import annotations

import json
import os
import re
import time
from typing import Any
from pathlib import Path

import firebase_admin
from firebase_admin import credentials, firestore
from google.cloud.firestore import Client as FirestoreClient



def get_firestore_client():
    """
    Initialize Firebase app (if not already) and return a Firestore client.
    Uses GOOGLE_APPLICATION_CREDENTIALS env var for the service account JSON.
    """
    if not firebase_admin._apps:
        cred_path = os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
        if not cred_path:
            raise RuntimeError("GOOGLE_APPLICATION_CREDENTIALS env var is not set")
        cred = credentials.Certificate(cred_path)
        firebase_admin.initialize_app(cred)
    return firestore.client()



def _clean_segment(s: str) -> str:
    """
    Make a string segment safe for use in a Firestore document ID:
    - strip spaces
    - replace spaces with '-'
    - remove characters that are likely to be annoying in IDs
    """
    s = str(s).strip()
    s = s.replace(" ", "-")
    # Remove slashes and other problematic chars
    s = re.sub(r"[\/\?#\[\]{}]+", "", s)
    return s



def build_doc_id(
    provider_name: str,
    first_name: str,
    last_name: str,
    age: int,
    profile_id: str | int,
    task_type: str = "cv",  # either "cv" or "qa"
    top_k: int | None = None,
) -> str:
    """
    Build a readable Firestore document ID like:
      provider_task_first_last_age_ID
    Examples:
      openai_cv_Anna_Becker_45_1
      openai_qa_Anna_Becker_45_1
      qwen3-8b_cv_Anna_Becker_45_1_topk20
    """
    if task_type not in {"cv", "qa"}:
        raise ValueError(f"Invalid task_type: {task_type!r} (expected 'cv' or 'qa')")

    seg_provider = _clean_segment(provider_name)
    seg_task = _clean_segment(task_type)
    seg_first = _clean_segment(first_name)
    seg_last = _clean_segment(last_name)
    seg_age = age
    seg_id = profile_id

    doc_id = f"{seg_provider}_{seg_task}_{seg_first}_{seg_last}_{seg_age}_{seg_id}"
    if top_k is not None:
        doc_id += f"_topk{top_k}"
    return doc_id

def _clean_firestore_payload(payload: dict) -> dict:
    """
    Ensures a dictionary is safe for Firestore by converting all nested elements
    to standard Python types (like converting numpy.int64 to int) via JSON serialization.
    """
    try:
        json_string = json.dumps(payload)
        return json.loads(json_string)
    except Exception as e:
        print(f"WARNING: Failed to clean payload for Firestore: {e}")
        return payload.copy()


def _save_error_as_txt(
    file_prefix: str,
    system_text: str,
    user_text: str,
    raw_response: str | Any,
    error_message: str,
    config_path: str | Path,
) -> str:
    """Saves error details and response to a .txt file."""
    config_name = Path(config_path).stem
    filename = f"{config_name}-{file_prefix}-ERROR.txt"

    logs_dir = Path("llm_pipeline/logs")
    logs_dir.mkdir(parents=True, exist_ok=True)
    filepath = logs_dir / filename
    
    content = (
        "--- RUN FAILED ---\n"
        f"Error: {error_message}\n"
        f"Time: {time.ctime()}\n"
        f"File: {filepath.resolve()}\n\n"
        "--- SYSTEM PROMPT ---\n"
        f"{system_text}\n\n"
        "--- USER PROMPT ---\n"
        f"{user_text}\n\n"
        "--- RAW LLM RESPONSE ---\n"
        f"{raw_response}\n\n"
    )
    
    filepath.write_text(content, encoding="utf-8")
    print(f"FATAL ERROR: Job failed and saved as text file: {filename}")
    return str(filepath.resolve())
