from __future__ import annotations

import asyncio
import json
from pathlib import Path
from typing import Dict, Any
from openai import AsyncOpenAI, RateLimitError

import pandas as pd
import yaml

from .unified_client import chat_completion

def build_cv_prompt(
    config_path: str | Path,
    cv_prompt_path: str | Path,
    profiles_df: pd.DataFrame,
    profile_index: int,
    first_name: str,
    last_name: str,
    age: int,
) -> str:
    """
    Build a ready-to-send CV prompt string.

    - Loads the template file.
    - Takes one profile row, removes the 'ID' column, converts it to JSON.
    - Inserts {first_name}, {last_name}, {age}, and {profile_qa_json} into the template.
    - Returns the final prompt text.
    """
    config = yaml.safe_load(Path(config_path).read_text(encoding="utf-8"))  # kept for future use
    template = Path(cv_prompt_path).read_text(encoding="utf-8")

    # One profile → dict, drop "ID", make JSON-safe (None instead of NaN)
    row_dict = profiles_df.iloc[profile_index].to_dict()
    row_dict.pop("ID", None)
    row_dict = {k: (None if pd.isna(v) else v) for k, v in row_dict.items()}

    profile_json = json.dumps(row_dict, ensure_ascii=False, indent=2)

    return template.format(
        first_name=first_name,
        last_name=last_name,
        age=age,
        profile_qa_json=profile_json,
    )



def _split_system_user(filled_template: str) -> tuple[str, str]:
    """
    Split a [SYSTEM]/[USER] markdown template into system and user prompts.
    """
    sys_tag, usr_tag = "[SYSTEM]", "[USER]"
    if sys_tag not in filled_template or usr_tag not in filled_template:
        raise ValueError("Template must contain [SYSTEM] and [USER] sections.")
    sys_part = filled_template.split(sys_tag, 1)[1]
    if usr_tag not in sys_part:
        raise ValueError("Template missing [USER] section.")
    system_text, user_text = sys_part.split(usr_tag, 1)
    return system_text.strip(), user_text.strip()




async def _chat_completion_with_retry(
    openai_client: Any,
    model: str,
    temperature: float,
    max_tokens: int,
    system_text: str,
    user_text: str,
    max_attempts: int,
    initial_backoff_seconds: float,
    **kwargs,
) -> Any:
    """A wrapper for chat.completions.create with exponential backoff and retry."""
    
    current_backoff = initial_backoff_seconds
    for attempt in range(max_attempts):
        try:
            resp = await chat_completion(
                openai_client,
                model=model,
                temperature=temperature,
                max_tokens=max_tokens,
                messages=[
                    {"role": "system", "content": system_text},
                    {"role": "user", "content": user_text},
                ],
                **kwargs,
            )
            return resp
        except RateLimitError as e:
            if attempt < max_attempts - 1:
                print(f"Rate limit hit. Retrying in {current_backoff:.2f}s (Attempt {attempt + 1}/{max_attempts}).")
                await asyncio.sleep(current_backoff)
                current_backoff *= 2  # Exponential backoff
            else:
                print(f"Rate limit hit. Max attempts ({max_attempts}) reached. Failing.")
                raise e
        except Exception as e:
            # Re-raise any non-rate-limit exceptions immediately
            raise e
    
    raise RuntimeError("Retry loop finished without returning or re-raising.")
