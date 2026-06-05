
from pathlib import Path
from dotenv import load_dotenv

from .tools_utils import export_collections, parse_args

load_dotenv()

# Fields excluded by default from export
DEFAULT_EXCLUDE = [
    "07_temperature",
    "08_prefer_json",
    "09_was_json",
    "10_cost_usd",
    "13_system_prompt",
    "14_user_prompt",
    "04_task_type",
    "11_max_output_tokens",
    "15_cv_json",
    "16_cv_text",
    "18_response_text",
    "_id"
]

def main():
    args = parse_args()

    # Merge defaults with user-provided excludes (case-insensitive, no duplicates)
    combined_exclude = list({
        name.lower(): name
        for name in (DEFAULT_EXCLUDE + args.exclude)
    }.values())

    out_dir = Path(args.out_dir)
    export_collections(args.collections, out_dir, combined_exclude)


if __name__ == "__main__":
    main()