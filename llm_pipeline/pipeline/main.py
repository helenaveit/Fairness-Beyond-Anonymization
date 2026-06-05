from __future__ import annotations

import argparse
import asyncio
from pathlib import Path
from dotenv import load_dotenv

from .pipeline_funs import run_all_from_config

# Load environment variables from .env file
load_dotenv()

CONFIG_DIR = Path("llm_pipeline/configs")
DEFAULT_CONFIG = "config_test.yaml"

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run the LLM pipeline with the given config.")
    parser.add_argument(
        "--config",
        default=DEFAULT_CONFIG,
        help=f"Config filename inside {CONFIG_DIR}/ (default: {DEFAULT_CONFIG})",
    )
    args = parser.parse_args()

    config_file = CONFIG_DIR / args.config
    asyncio.run(run_all_from_config(config_file))
