# Iowa Speech Sample to Cognition

> Initial analysis pipeline mapping Iowa Speech Sample (ISS) speech and language features to cognitive performance. This repository covers audio preprocessing, transcription, feature extraction, and language-to-cognition correlation analyses across two cohort versions of the ISS task.

---

## Overview

This repository contains the full processing and analysis pipeline for the **Iowa Speech Sample (ISS)** — a battery of speech-based cognitive tasks including semantic fluency (WAT), phonemic fluency (COWAT), Rapid Automatized Naming (RAN), and face-matching. The pipeline begins with raw audio recordings, extracts a rich set of linguistic and psycholinguistic features, and correlates those features with cognitive outcomes (IQ composites, NIH Toolbox scores).

> **For full details on ISS task design, feature definitions, and the containerized processing pipeline, see the ISS documentation and Docker repository: `[https://github.com/melsadany/iss-pipeline]`**

This repo represents the **initial analysis version** of the ISS-to-cognition work and is organized into two sub-cohorts corresponding to early task iterations: `ISS_PSVC_100` (n ≈ 105, PSVC protocol) and `ISS_110` (n ≈ 47).

---

## Repository Structure

```
iowa_speech_sample_to_cognition/
├── src/
│   ├── ISS_110/                          # Pipeline for second ISS cohort
│   │   ├── 00_audio-alignment-to-task.R
│   │   ├── 01_preparing-for-tx.R
│   │   ├── 02_transcribe.sh
│   │   ├── 03_tx-cleanup.R
│   │   ├── 04_ISS-metrics-extraction.R
│   │   ├── 04_features_01-general.R
│   │   ├── 04_features_02-embeddings.R
│   │   ├── 04_features_03-combining.R
│   │   ├── 05_ISS-features-to-IQ.R
│   │   ├── 05_ISS-metrics-to-IQ.R
│   │   └── 99_transcription-revision-app.R
│   ├── ISS_PSVC_100/                     # Pipeline for PSVC cohort (100 participants)
│   │   ├── 01_audio-crop.R
│   │   ├── 02_01_whisper-transcription.R
│   │   ├── 02_02-whisperx-run.sh
│   │   ├── 03_tx-cleanup.R
│   │   ├── 04_features_01-general.R
│   │   ├── 04_features_02-time-and-trajectory-related.R
│   │   ├── 04_features_03-semantics.R
│   │   ├── 04_features_04-semantics-to-spatial-words.R
│   │   ├── 04_features_05-phonetics-pwe-run.py
│   │   ├── 04_features_05-phonetics.R
│   │   ├── 04_features_06-combining.R
│   │   ├── 05_word-count-vs-feat-to-cog.R
│   │   ├── 06_semantic-fluency-vs-phonemic-fluency.R
│   │   ├── 07_detailed-lang-to-cog.R
│   │   └── 99_example-figs.R
│   ├── utils/                            # Shared helper functions
│   ├── 02_language-cognition-sPLS-CCA-FA.R
│   └── 03_PSVC-vs-ISS-to-cog.R
├── figs/                                 # Output figures
├── logs/                                 # HPC job logs
└── .gitignore
```

---

## ISS Task Background

The Iowa Speech Sample is a speech-based cognitive screening battery. Participants complete a series of verbal tasks while being recorded. The tasks include:

| Task | Type | Description |
|---|---|---|
| WAT (Word Association Task) | Semantic fluency | Generate words within semantic categories across 20 prompts |
| COWAT (Controlled Oral Word Association Test) | Phonemic fluency | Generate words starting with specific letters across 5 prompts |
| RAN (Rapid Automatized Naming) | Processing speed | Name stimuli rapidly |
| PS Face Matching | Processing speed | Match faces, duration recorded |

Each task yields audio recordings that are transcribed and processed into quantitative features spanning lexical, semantic, psycholinguistic, temporal, phonetic, and embedding-based dimensions.

> Full feature documentation and the containerized pipeline are available in the ISS repository: `[https://github.com/melsadany/iss-pipeline]`

---

## Pipeline: ISS_PSVC_100 Cohort

The `ISS_PSVC_100/` pipeline covers the PSVC cohort (~100 participants) with an expanded feature extraction scheme and WhisperX-based transcription.

### `01_audio-crop.R`
Crops raw audio recordings to task-relevant windows based on timing markers, removing pre/post-task silence.

### `02_01_whisper-transcription.R` / `02_02-whisperx-run.sh`
Transcription pipeline using **WhisperX** (OpenAI Whisper with forced alignment). The `.sh` script submits batch jobs per participant on the HPC; the `.R` script handles output parsing and job management.

### `03_tx-cleanup.R`
Cleans transcription output for the PSVC cohort: standardizes formatting, handles WhisperX-specific output quirks, and produces a clean transcript RDS.

### Feature Extraction (Step 04)

| Script | Feature Class | Details |
|---|---|---|
| `04_features_01-general.R` | Lexical & psycholinguistic | Lingmatch metrics, sentiment (NRC), AoA, GPT familiarity, MRC (imageability, phonemes), concreteness, unique word count, productivity slope |
| `04_features_02-time-and-trajectory-related.R` | Temporal & trajectory | Response timing, inter-response intervals, production trajectory over trial duration |
| `04_features_03-semantics.R` | Semantic | Semantic similarity to prompt, community structure (graph-based), semantic spread, Gini coefficient of semantic distribution |
| `04_features_04-semantics-to-spatial-words.R` | Spatial language | Mapping semantic content to spatial/directional vocabulary |
| `04_features_05-phonetics.R` / `.py` | Phonetics | Phoneme-level features using the `pwe` (phonetic word embedding) tool; run via Python, parsed in R |
| `04_features_06-combining.R` | Integration | Merges all feature modules; produces the final combined feature matrix used for downstream analyses |

### `05_word-count-vs-feat-to-cog.R`
Benchmarks whether richer linguistic features provide additional predictive value over simple word count. Fits separate linear models (word count alone vs. each feature) and compares effect sizes across cognitive outcomes using forest plots.

### `06_semantic-fluency-vs-phonemic-fluency.R`
Directly compares the two fluency tasks (WAT vs. COWAT) in terms of their relationship to one another and their differential associations with specific cognitive domains.

### `07_detailed-lang-to-cog.R`
Full language-to-cognition correlation analysis. Computes pairwise Pearson correlations between all ISS features (per task) and all cognitive outcomes (IQ + NIH Toolbox), with FDR correction. Generates:
- Full correlation heatmaps (faceted by feature category and cognitive domain)
- Forest plots for selected features vs. word count baseline
- Summary bar plots of significant correlation counts per feature class

### `99_example-figs.R`
Generates polished example figures for presentations, manuscripts, and supplementary materials.

### `02_language-cognition-sPLS-CCA-FA.R`
Applies sparse Partial Least Squares (sPLS) across the combined language-cognition feature space to extract latent dimensions of covariance.

---

## Feature Reference

> For comprehensive definitions of all extracted features, their psycholinguistic basis, units, and interpretation, refer to the ISS documentation repository: `[https://github.com/melsadany/iss-pipeline]`

Feature categories covered in this pipeline:

- **Lexical**: word count, unique word count, productivity slope, characters/syllables per word, reading grade
- **Psycholinguistic**: age of acquisition (AoA), word familiarity (GPT-4 estimates), imageability, concreteness, phoneme count (MRC database)
- **Sentiment & pragmatics**: NRC emotion categories, profanity count
- **Semantic**: semantic similarity to prompt, semantic community structure, Gini coefficient, spatial word usage
- **Temporal**: response timing, production trajectory, inter-response intervals
- **Phonetic**: phoneme-level embeddings (pwe)
- **Embeddings**: distributional semantic representations

---

## Author

**Muhammad Elsadany**  
Interdisciplinary Graduate Program in Genetics (Computational Genetics Track)  
Department of Psychiatry, University of Iowa  
[GitHub: melsadany](https://github.com/melsadany)

---

## License

This project is intended for academic use. Please contact the author before reusing or adapting code for other projects.
