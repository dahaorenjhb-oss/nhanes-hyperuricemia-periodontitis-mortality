args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(root, "scripts", "00_v20_setup_safety_check.R"))) setwd(root)
}
if (!file.exists(file.path(getwd(), "scripts", "00_v20_setup_safety_check.R"))) {
  stop("Run from the repository root, or call with Rscript scripts/run_all_v20_public_raw_primary.R.")
}

run_step <- function(script) {
  message("Running ", script)
  source(file.path("scripts", script), local = new.env(parent = globalenv()))
}

scripts <- c(
  "00_v20_setup_safety_check.R",
  "01_v20_public_raw_import_and_merge.R",
  "02_v20_variable_construction_public_raw.R",
  "03_v20_public_raw_cohort_definition.R",
  "04_v20_descriptive_tables.R",
  "05_v20_primary_cox_models.R",
  "06_v20_ph_diagnostics.R",
  "08_v20_sensitivity_and_supplementary.R",
  "07_v20_figures.R",
  "10_v20_manuscript_numbers_patch.R",
  "11_v20_repository_candidate.R",
  "09_v20_source_data_and_hashes.R"
)

for (s in scripts) run_step(s)
message("v20 public raw-to-final primary reanalysis completed.")
