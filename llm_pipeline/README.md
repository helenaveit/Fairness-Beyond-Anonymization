# LLM PIPELINE (STAGE 1: RÉSUMÉ GENERATION)

This folder contains all code for generating résumés from profile data using LLM providers (OpenAI, Gemini, and local models via vLLM).
Results are automatically stored and tracked in Firebase Firestore.


## Key Modules


/pipeline

| Module | Purpose |
| ------- | -------- |
| main.py | Loads environment and config, runs pipeline |
| pipeline_funs.py | Core orchestration of résumé generation |
| data_utils.py | Read and validate profile data |
| prompts_utils.py | Build prompt templates and split SYSTEM/USER sections |
| firestore_utils.py | Initialize Firebase and manage document IDs |
| unified_client.py | Unified LLM client routing (OpenAI, Gemini, local/vLLM) |
| pricing.py | Compute approximate cost per call |


/tools

| Module | Purpose |
| ------- | -------- |
| export_results.py | Export results from Firebase into local CSV files |
| tools_utils.py | Utils for export_results.py |



## Setup

Uses **Python 3.12.2**.

1. Create and activate a virtual environment:
   macOS / Linux:
```bash
python -m venv .venv
source .venv/bin/activate
```

   Windows:
```bash
 python -m venv .venv
.venv\Scripts\activate
```

2. Install dependencies:
```bash
pip install -r llm_pipeline/requirements.txt
```

3. Configure your environment variables in a `.env` file in the project root:

   Example:

       # LLM Providers (required based on provider in config)
       OPENAI_API_KEY=sk-...your-key...
       GEMINI_API_KEY=...your-key...

       # Firebase
       GOOGLE_APPLICATION_CREDENTIALS=/path/to/your-firebase-key.json

       # Local models via vLLM (optional — defaults to http://localhost:8000/v1)
       LOCAL_LLM_BASE_URL=http://localhost:8000/v1

   The pipeline automatically loads this file via python-dotenv.

4. (Local models only) Install vLLM and start the model server before running the pipeline.
   The pipeline will error if nothing is listening on `LOCAL_LLM_BASE_URL`.

```bash
pip install vllm

python -m vllm.entrypoints.openai.api_server \
    --model Qwen/Qwen3-8B \
    --port 8000
```

   vLLM uses HuggingFace model IDs and will download the model automatically on first run.

   If running the pipeline from a different machine than the vLLM server, either set
   `LOCAL_LLM_BASE_URL` to the server's address, or use an SSH tunnel:

```bash
ssh -L 8000:localhost:8000 user@server
```


# Running

The pipeline runs based on a YAML config file located in `llm_pipeline/configs/`.

Pass the config filename via `--config`:
```bash
python -m llm_pipeline.pipeline.main --config config_test.yaml
```

If `--config` is omitted, it defaults to `config_test.yaml`.

**Available configs:**

| Config | Provider | Model |
|--------|----------|-------|
| `config_test.yaml` | OpenAI | gpt-4o-mini |
| `config_gpt.yaml` | OpenAI | gpt-4o-mini |
| `config_gemini.yaml` | Google Gemini | gemini-2.5-flash-lite |
| `config_qwen3_4b.yaml` | Local (vLLM) | Qwen/Qwen3-4B |
| `config_qwen3_8b.yaml` | Local (vLLM) | Qwen/Qwen3-8B |
| `config_qwen3_14b.yaml` | Local (vLLM) | Qwen/Qwen3-14B |


Each run will:
- Read configuration (inputs, prompts, run scope, concurrency, retry settings).
- Use a unified client system (unified_client.py) to handle OpenAI, Gemini, and local models.
- Generate a résumé for each profile from the input data and (name, age) combination, stored in the `cv_runs` collection in Firestore.
- All LLM calls are executed with concurrency control and retry logic.

## Firebase Output

Each run produces one Firestore document per résumé:

    cv_runs/provider_cv_first_last_age_row

Each document includes:
- Candidate info (name, age, profile ID)
- Prompts used (system and user)
- Model output (JSON)
- Token usage and cost
- Whether the model output was valid JSON

**Error Handling:** If an LLM call fails completely, the detailed request (prompts, config)
and error message are saved locally to a `.txt` file for debugging.

## Exporting Results from Firebase

To automatically export results from Firestore and convert them into
CSV files, run:
```bash
python -m llm_pipeline.tools.export_results
```


## Notes

- Run commands from the project root so paths in the config resolve correctly.
- The CV prompt template must contain `[SYSTEM]` and `[USER]` tags for prompt separation.
- The model provider is selected based on the `provider` field in the config.
- Cost estimation in `pricing.py` is approximate and model-dependent; local model runs are recorded as $0.
- For local configs, an optional `extra_body` field can be passed to forward additional parameters to the model server.
