# Joint Hyperuricemia-Periodontitis Phenotype and Mortality in NHANES

Reproducibility package for the manuscript:

> **Joint hyperuricemia-periodontitis phenotype and all-cause mortality: a population-based cohort analysis of NHANES 2009-2014 linked mortality data**

## Study summary

- **Design**: Population-based cohort study using NHANES 2009-2014 linked to 2019 NCHS public-use Linked Mortality Files
- **Population**: U.S. adults aged 30+ with complete periodontal examination and serum uric acid data
- **Exposure**: Joint phenotype of hyperuricemia (serum urate > 7.0 mg/dL men, > 6.0 mg/dL women) and moderate/severe periodontitis
- **Outcome**: All-cause mortality
- **Final analytic N**: 9,018 (743 deaths)
- **Primary result**: Survey-weighted Cox HR = 1.85 (95% CI 1.33-2.58), P < 0.001

## Repository structure

```
.
├── scripts/                    # R analysis scripts (run in order)
│   ├── 00_v20_setup_safety_check.R
│   ├── 01_v20_public_raw_import_and_merge.R
│   ├── 02_v20_variable_construction_public_raw.R
│   ├── 03_v20_public_raw_cohort_definition.R
│   ├── 04_v20_descriptive_tables.R
│   ├── 05_v20_primary_cox_models.R
│   ├── 06_v20_ph_diagnostics.R
│   ├── 07_v20_figures.R
│   ├── 08_v20_sensitivity_and_supplementary.R
│   ├── 09_v20_source_data_and_hashes.R
│   ├── 10_v20_manuscript_numbers_patch.R
│   ├── 11_v20_repository_candidate.R
│   └── run_all_v20_public_raw_primary.R   # Entry point
├── download_scripts/           # NHANES data download helpers
├── config/                     # File registries and variable definitions
├── data_dictionary/            # Variable dictionary
├── source_data/                # Aggregate source data for tables and figures (CSV)
├── tables/                     # Generated table outputs (CSV)
├── validation/                 # Reproducibility checks and hash validation
├── reports/                    # Analysis reports
├── renv.lock                   # R package versions (use renv::restore())
├── CITATION.cff                # Citation metadata
└── LICENSE.md                  # MIT License
```

## How to reproduce

### Prerequisites

- R >= 4.3.0
- Install `renv` package: `install.packages("renv")`

### Step 1: Restore R packages

```r
renv::restore()
```

### Step 2: Download NHANES raw data

Run the download script to fetch NHANES XPT files and Linked Mortality Files from CDC:

```r
source("download_scripts/01_download_public_nhanes_lmf_raw_files.R")
```

Or download manually from:
- NHANES: https://wwwn.cdc.gov/nchs/nhanes/
- Linked Mortality Files: https://www.cdc.gov/nchs/data-linkage/mortality-public.htm

### Step 3: Run the full analysis pipeline

```r
source("scripts/run_all_v20_public_raw_primary.R")
```

This will:
1. Import and merge raw NHANES XPT files
2. Construct derived variables (hyperuricemia, periodontitis, CKD, etc.)
3. Define the analytic cohort with exclusion cascade
4. Generate descriptive tables (Table 1)
5. Fit primary survey-weighted Cox models (Table 2)
6. Run proportional hazards diagnostics
7. Generate figures (flow diagram, KM curves, forest plots)
8. Run sensitivity analyses (Table 3, supplementary tables)
9. Export source data and hash manifests

### Expected primary output

```
N: 9,018
Deaths: 743
Model 4 HR (hyperuricemia + moderate/severe vs normouricemia + none/mild):
  HR = 1.85 (95% CI 1.33-2.58), P < 0.001
```

## Data sources

All input data are publicly available from U.S. federal sources:

| Data | Source | URL |
|------|--------|-----|
| NHANES 2009-2014 exam/lab/questionnaire | CDC/NCHS | https://wwwn.cdc.gov/nchs/nhanes/ |
| 2019 Public-use Linked Mortality Files | CDC/NCHS | https://www.cdc.gov/nchs/data-linkage/mortality-public.htm |

No private or restricted data are included in this repository.

## Citation

If you use this code, please cite:

```
[Manuscript citation to be added upon publication]
```

See `CITATION.cff` for machine-readable citation metadata.

## License

This code is released under the MIT License. See [LICENSE.md](LICENSE.md).
