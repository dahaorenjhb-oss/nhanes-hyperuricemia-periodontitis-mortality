source(file.path("scripts", "00_v20_setup_safety_check.R"))

repo <- v20_path("public_repository_candidate")
for (d in c("scripts", "download_scripts", "config", "docs", "data_dictionary", "manifest", "validation",
            "source_data", "reproduced_tables", "reproduced_figures")) ensure_dir(file.path(repo, d))

script_files <- list.files(v20_path("scripts"), pattern = "[.]R$", full.names = TRUE)
for (f in script_files) safe_copy(f, file.path(repo, "scripts", basename(f)))

reg <- utils::read.csv(v20_path("config", "raw_file_registry_v20.csv"), stringsAsFactors = FALSE, check.names = FALSE)
public_reg <- reg |>
  dplyr::transmute(file_label, data_domain, cycle_or_year, cycle_suffix, module, file_type, source_url, required_for,
                   expected_cache_subdir = ifelse(file_type == "DAT", "linked_mortality", cycle_or_year),
                   participant_level_raw_file_included = "no")
safe_write_csv(public_reg, file.path(repo, "config", "public_raw_file_registry_v20.csv"))

safe_write_lines(c(
  "options(timeout = max(900, getOption('timeout')))",
  "reg <- read.csv(file.path('config', 'public_raw_file_registry_v20.csv'), stringsAsFactors = FALSE)",
  "cache_root <- file.path('data_private_DO_NOT_UPLOAD', 'public_raw_cache')",
  "dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)",
  "for (i in seq_len(nrow(reg))) {",
  "  dest <- file.path(cache_root, reg$expected_cache_subdir[i], reg$file_label[i])",
  "  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)",
  "  if (!file.exists(dest) || file.info(dest)$size == 0) download.file(reg$source_url[i], dest, mode = 'wb')",
  "  message(reg$file_label[i], ' -> ', dest)",
  "}"
), file.path(repo, "download_scripts", "01_download_public_nhanes_lmf_raw_files.R"))

safe_write_lines(c(
  "# Joint Hyperuricemia-Periodontitis Phenotype and Mortality in NHANES",
  "",
  "This public reproducibility repository supports reproduction of the NHANES 2009-2014 linked mortality analysis.",
  "",
  "Included:",
  "- R scripts for public raw download, reconstruction, analysis, tables, figures, and validation.",
  "- Public raw file registry with CDC/NCHS URLs.",
  "- Aggregate source data for reproduced tables and figures.",
  "- Validation reports and manifests.",
  "",
  "Excluded:",
  "- Complete NHANES raw files and Linked Mortality File copies.",
  "- Participant-level reconstructed analytic datasets.",
  "- Private historical comparison data.",
  "- `fallback_nested_audit_dataset.rds`.",
  "",
  "Repository URL: https://github.com/dahaorenjhb-oss/nhanes-hyperuricemia-periodontitis-mortality"
), file.path(repo, "README.md"))

safe_write_lines(c(
  "MIT License",
  "",
  "Copyright (c) 2026 The Authors",
  "",
  "Permission is hereby granted, free of charge, to any person obtaining a copy",
  "of this software and associated documentation files (the \"Software\"), to deal",
  "in the Software without restriction, including without limitation the rights",
  "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
  "copies of the Software, and to permit persons to whom the Software is",
  "furnished to do so, subject to the following conditions:",
  "",
  "The above copyright notice and this permission notice shall be included in all",
  "copies or substantial portions of the Software.",
  "",
  "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR",
  "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
  "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
  "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
  "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
  "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE",
  "SOFTWARE.",
  "",
  "Data note: NHANES and NCHS Linked Mortality public-use files remain governed by CDC/NCHS terms. This repository does not redistribute those raw files."
), file.path(repo, "LICENSE.md"))

safe_write_lines(c(
  "cff-version: 1.2.0",
  "title: Joint hyperuricemia-periodontitis phenotype and all-cause mortality: NHANES linked mortality reproducibility repository",
  "message: Please cite this repository and the underlying NHANES/NCHS data sources.",
  "type: software",
  "authors:",
  "  - family-names: Jin",
  "    given-names: Haibin",
  "  - family-names: Zou",
  "    given-names: Siyu",
  "  - family-names: Chu",
  "    given-names: Yiting",
  "  - family-names: Gong",
  "    given-names: Aixiu",
  "version: v1.0.0",
  "date-released: 2026-06-04",
  "url: https://github.com/dahaorenjhb-oss/nhanes-hyperuricemia-periodontitis-mortality",
  "license: MIT"
), file.path(repo, "CITATION.cff"))

safe_write_lines(c(
  "# Repository exclusion notice",
  "",
  "The public repository candidate intentionally excludes all participant-level rows, raw XPT/DAT files, private derived cohorts, private validation cohorts, and `fallback_nested_audit_dataset.rds`.",
  "",
  "The old private-derived N=9018 result is historical/private validation context only and is not the primary v20 result."
), file.path(repo, "repository_exclusion_notice.md"))

safe_write_lines(c(
  "# Analysis workflow",
  "",
  "Run `scripts/run_all_v20_public_raw_primary.R` after downloading public raw files or allowing the script to populate `data_private_DO_NOT_UPLOAD/public_raw_cache/`.",
  "",
  "The analysis reconstructs variables from public raw files, defines complete cases, fits survey-weighted Cox models, checks proportional hazards, regenerates tables/figures, and writes validation manifests.",
  "",
  "Participant-level outputs stay under `data_private_DO_NOT_UPLOAD/` and are not for upload."
), file.path(repo, "docs", "analysis_workflow.md"))

safe_write_lines(c(
  "# Data dictionary",
  "",
  "- `joint_urate_periodontitis`: four-category cross-classification of hyperuricemia and periodontitis.",
  "- `hyperuricemia`: serum uric acid >7.0 mg/dL in men and >6.0 mg/dL in women.",
  "- `periodontitis_binary`: CDC/AAP none/mild versus moderate/severe periodontitis.",
  "- `death_allcause`: public LMF MORTSTAT == 1.",
  "- `followup_years`: PERMTH_EXM / 12.",
  "- `WTMEC6YR`: WTMEC2YR / 3 for three NHANES cycles.",
  "- `SDMVPSU`, `SDMVSTRA`: NHANES design variables."
), file.path(repo, "data_dictionary", "variable_dictionary_v20.md"))

for (f in list.files(v20_path("validation"), full.names = TRUE)) safe_copy(f, file.path(repo, "validation", basename(f)))
for (f in list.files(v20_path("source_data", "tables"), full.names = TRUE)) safe_copy(f, file.path(repo, "source_data", basename(f)))
for (f in list.files(v20_path("source_data", "figures"), full.names = TRUE)) safe_copy(f, file.path(repo, "source_data", basename(f)))
for (f in list.files(v20_path("source_data", "supplementary"), full.names = TRUE)) safe_copy(f, file.path(repo, "source_data", basename(f)))
for (f in list.files(v20_path("tables", "main"), full.names = TRUE)) safe_copy(f, file.path(repo, "reproduced_tables", basename(f)))
for (f in list.files(v20_path("tables", "supplementary"), full.names = TRUE)) safe_copy(f, file.path(repo, "reproduced_tables", basename(f)))
for (f in list.files(v20_path("figures", "main"), full.names = TRUE)) safe_copy(f, file.path(repo, "reproduced_figures", basename(f)))

repo_manifest <- manifest_for_dir(repo)
safe_write_csv(repo_manifest, file.path(repo, "repository_file_manifest_sha256.csv"))
safe_write_csv(repo_manifest, file.path(repo, "manifest", "repository_file_manifest_sha256.csv"))
write_all_manifests()

safe_write_lines(c(
  "# v20 repository candidate report",
  "",
  "Repository URL: https://github.com/dahaorenjhb-oss/nhanes-hyperuricemia-periodontitis-mortality",
  "Participant-level rows: excluded.",
  "Raw XPT/DAT files: excluded.",
  "Private historical comparison data: excluded.",
  "fallback_nested_audit_dataset.rds: excluded."
), v20_path("reports", "v20_reproducibility_status_report.md"))

log_action("11 public repository candidate prepared.")
