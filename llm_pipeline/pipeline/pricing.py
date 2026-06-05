# llm_pipeline/pricing.py
from __future__ import annotations

PRICING = {
    "openai:gpt-4o-mini": {"input": 0.00015, "output": 0.00060},
    "google-gemini:gemini-2.5-flash-lite": {"input": 0.00010, "output": 0.00040},
}


def estimate_cost(provider_name: str, model: str, prompt_tokens: int, completion_tokens: int) -> float:
    key = f"{provider_name}:{model}"
    p = PRICING.get(key)
    if not p:
        return 0.0
    return (prompt_tokens / 1000.0) * p["input"] + (completion_tokens / 1000.0) * p["output"]
