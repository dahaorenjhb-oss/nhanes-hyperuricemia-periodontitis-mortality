cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
run_file <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else file.path(getwd(), "scripts", "run_all_v20_public_raw_primary.R")
root <- normalizePath(file.path(dirname(run_file), ".."), winslash = "/", mustWork = FALSE)
if (basename(root) == "v20_public_raw_to_final_primary_reanalysis") {
  setwd(root)
} else args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_dir <- dirname(sub("^--file=", "", file_arg[[1]]))
  setwd(normalizePath(file.path(script_dir, "..")))
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
