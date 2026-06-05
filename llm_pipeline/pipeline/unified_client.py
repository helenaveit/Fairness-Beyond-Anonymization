from __future__ import annotations

import os
from typing import Any
from openai import AsyncOpenAI

LOCAL_LLM_BASE_URL = os.environ.get("LOCAL_LLM_BASE_URL", "http://localhost:8000/v1")


def get_client(provider: str, model: str) -> Any:
    """
    Returns the appropriate client based on provider and model.
    """
    provider_key = provider.strip().lower()
    model_name = model.strip().lower()

    if provider_key in {"google-gemini", "gemini"}:
        api_key = os.environ.get("GEMINI_API_KEY")
        base_url = "https://generativelanguage.googleapis.com/v1beta/openai/"
        if not api_key:
            raise ValueError("GEMINI_API_KEY environment variable not set for Gemini model.")
        return AsyncOpenAI(api_key=api_key, base_url=base_url)

    elif provider_key == "openai":
        api_key = os.environ.get("OPENAI_API_KEY")
        if not api_key:
            raise ValueError("OPENAI_API_KEY environment variable not set for OpenAI model.")
        return AsyncOpenAI(api_key=api_key)

    elif provider_key == "local":
        # Both vLLM and mlx-lm expose an OpenAI-compatible API; api_key is required but ignored.
        # Timeout is set high because local models can take several minutes on large prompts.
        return AsyncOpenAI(api_key="local", base_url=LOCAL_LLM_BASE_URL, timeout=600.0)

    else:
        # Fallback for backward compatibility
        if model_name.startswith("gemini"):
            api_key = os.environ.get("GEMINI_API_KEY")
            base_url = "https://generativelanguage.googleapis.com/v1beta/openai/"
            if not api_key:
                raise ValueError("GEMINI_API_KEY environment variable not set for Gemini model.")
            return AsyncOpenAI(api_key=api_key, base_url=base_url)
        elif model_name.startswith("gpt"):
            api_key = os.environ.get("OPENAI_API_KEY")
            if not api_key:
                raise ValueError("OPENAI_API_KEY environment variable not set for OpenAI model.")
            return AsyncOpenAI(api_key=api_key)
        else:
            raise ValueError(f"Unsupported provider/model configuration: provider={provider!r}, model={model!r}")


async def chat_completion(client: Any, model: str, **kwargs) -> Any:
    """
    Unified chat completion function that dispatches to the appropriate provider.
    """
    if isinstance(client, AsyncOpenAI):
        return await client.chat.completions.create(model=model, **kwargs)
    else:
        raise ValueError(f"Unsupported client type: {type(client)}")