source(file.path("scripts", "00_v20_setup_safety_check.R"))

dat <- readRDS(file.path(RECON_DIR, "final_model4_public_raw_analytic_cohort_DO_NOT_UPLOAD.rds"))
primary <- utils::read.csv(v20_path("validation", "primary_model_summary_v20.csv"), stringsAsFactors = FALSE)
pv <- setNames(primary$value, primary$metric)
N <- as.integer(pv[["N"]]); deaths <- as.integer(pv[["deaths"]])
hr <- as.numeric(pv[["HR"]]); lo <- as.numeric(pv[["CI_low"]]); hi <- as.numeric(pv[["CI_high"]]); pval <- as.numeric(pv[["P_value"]])
ph <- utils::read.csv(v20_path("validation", "ph_diagnostics_v20.csv"), stringsAsFactors = FALSE)
ph_pass <- all(ph$p_value >= 0.05, na.rm = TRUE)

table_files <- list.files(v20_path("tables"), recursive = TRUE, full.names = TRUE)
figure_files <- list.files(v20_path("figures"), recursive = TRUE, full.names = TRUE)
display_files <- c(table_files, figure_files)
hash_validation <- data.frame(
  file = sub(paste0("^", V20_ROOT, "/"), "", normalizePath(display_files, winslash = "/", mustWork = FALSE)),
  exists = file.exists(display_files),
  size_bytes = as.numeric(file.info(display_files)$size),
  non_empty = file.exists(display_files) & file.info(display_files)$size > 0,
  sha256 = vapply(display_files, sha256_file, character(1)),
  stringsAsFactors = FALSE
)
safe_write_csv(hash_validation, v20_path("validation", "table_figure_hash_validation_v20.csv"))

table2 <- utils::read.csv(v20_path("source_data", "tables", "table2_source_data_v20.csv"), stringsAsFactors = FALSE)
primary_t2 <- table2 |> dplyr::filter(model == "Model 4", exposure_group == joint_labels[["hyperuricemia + moderate/severe"]]) |> dplyr::slice(1)
flow <- utils::read.csv(v20_path("source_data", "supplementary", "public_raw_reconstruction_flow_v20.csv"), stringsAsFactors = FALSE)
repo_files <- list.files(v20_path("public_repository_candidate"), recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
repo_private_extensions <- repo_files[grepl("[.](rds|RData|xpt|XPT|dat|DAT)$", repo_files)]
repo_forbidden_name <- repo_files[grepl("fallback_nested_audit_dataset|private_historical|DO_NOT_UPLOAD|analytic_cohort.*rds", repo_files, ignore.case = TRUE)]
marker_time <- file.info(v20_path("logs", "v20_start_marker.txt"))$mtime
readonly_newer <- unlist(lapply(READ_ONLY_ROOTS, function(root) {
  if (!dir.exists(root)) return(character())
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files[file.exists(files) & !dir.exists(files) & file.info(files)$mtime > marker_time]
}), use.names = FALSE)

checks <- data.frame(
  check = c("final_N_matches_private_cohort_file", "final_deaths_matches_private_cohort_file",
            "primary_HR_matches_table2_source", "primary_CI_low_matches_table2_source", "primary_CI_high_matches_table2_source",
            "all_display_files_non_empty", "public_repo_no_raw_or_rds_files", "public_repo_no_private_named_files",
            "read_only_v15_to_v18e_no_files_modified_after_v20_start"),
  observed = c(N == nrow(dat), deaths == sum(dat$death_allcause, na.rm = TRUE),
               abs(hr - primary_t2$HR) < 1e-10, abs(lo - primary_t2$conf_low) < 1e-10, abs(hi - primary_t2$conf_high) < 1e-10,
               all(hash_validation$non_empty), length(repo_private_extensions) == 0, length(repo_forbidden_name) == 0,
               length(readonly_newer) == 0),
  detail = c(
    paste(N, nrow(dat), sep = " vs "),
    paste(deaths, sum(dat$death_allcause, na.rm = TRUE), sep = " vs "),
    paste(hr, primary_t2$HR, sep = " vs "),
    paste(lo, primary_t2$conf_low, sep = " vs "),
    paste(hi, primary_t2$conf_high, sep = " vs "),
    paste(sum(hash_validation$non_empty), nrow(hash_validation), sep = "/"),
    ifelse(length(repo_private_extensions), paste(basename(repo_private_extensions), collapse = "; "), "none"),
    ifelse(length(repo_forbidden_name), paste(basename(repo_forbidden_name), collapse = "; "), "none"),
    ifelse(length(readonly_newer), paste(scrub_path(readonly_newer), collapse = "; "), "none")
  ),
  stringsAsFactors = FALSE
)
checks$status <- ifelse(checks$observed, "PASS", "FAIL")
safe_write_csv(checks, v20_path("validation", "numeric_consistency_check_v20.csv"))

repro <- data.frame(
  item = c("public raw files available", "final analytic N", "final deaths", "primary HR", "PH diagnostics pass",
           "Table 1 regenerated", "Table 2 regenerated", "Table 3 regenerated", "Figure 1 regenerated", "Figure 2 regenerated", "Figure 3 regenerated",
           "public repository candidate prepared"),
  value = c(
    all(read_registry()$download_status %in% c("AVAILABLE", "DOWNLOADED", "COPIED_FROM_READONLY_PUBLIC_RAW_CACHE")),
    N, deaths, fmt_num(hr, 4), ph_pass,
    file.exists(v20_path("tables", "main", "Table_1_baseline_characteristics_by_joint_exposure_v20.csv")),
    file.exists(v20_path("tables", "main", "Table_2_joint_exposure_cox_models_v20.csv")),
    file.exists(v20_path("tables", "main", "Table_3_sensitivity_analyses_v20.csv")),
    file.exists(v20_path("figures", "main", "Figure_1_strobe_flowchart_v20.png")),
    file.exists(v20_path("figures", "main", "Figure_2_joint_exposure_forest_plot_v20.png")),
    file.exists(v20_path("figures", "main", "Figure_3_sensitivity_forest_plot_v20.png")),
    file.exists(v20_path("public_repository_candidate", "README.md"))
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(repro, v20_path("validation", "public_raw_to_final_reproducibility_check.csv"))

failed <- checks$status == "FAIL"
status <- if (any(failed)) {
  "NUMERIC_INCONSISTENCY_REQUIRES_AUTHOR_REVIEW"
} else if (is.na(hr) || is.na(lo) || lo <= 1 || pval >= 0.05 || abs(hr - 1.95) >= 0.20) {
  "PUBLIC_RAW_REANALYSIS_COMPLETED_WITH_WEAKER_OR_NULL_RESULTS"
} else {
  "FULL_PUBLIC_RAW_TO_FINAL_REANALYSIS_COMPLETED_FOR_AUTHOR_REVIEW"
}
data_availability_category <- if (status == "NUMERIC_INCONSISTENCY_REQUIRES_AUTHOR_REVIEW") {
  "PUBLIC_RAW_REANALYSIS_REQUIRES_AUTHOR_REVIEW_BEFORE_REPOSITORY_DEPOSITION"
} else {
  "PUBLIC_RAW_TO_FINAL_REPRODUCTION_SUPPORTED_PUBLIC_REPOSITORY"
}

safe_write_lines(c(
  "# v20 final QA report",
  "",
  paste("Final status:", status),
  paste("Final public raw-only N:", N),
  paste("Final public raw-only deaths:", deaths),
  paste0("Primary HR: ", fmt_num(hr, 2), " (95% CI ", fmt_num(lo, 2), "-", fmt_num(hi, 2), ")"),
  paste("PH diagnostics pass:", ph_pass),
  paste("Data Availability category:", data_availability_category),
  "",
  "QA boundaries:",
  "- v15-v18e treated as read-only.",
  "- No participant-level data are included in the public repository candidate.",
  "- Old N=9018/HR=1.95 is historical/private validation only, not the primary result.",
  "- No unsupported causal claim, Fine-Gray model, or GEO claim was added.",
  "",
  "Ready for code availability review. A DOI can be added after GitHub archival through Zenodo or a similar repository."
), v20_path("reports", "v20_final_QA_report.md"))

safe_write_lines(c(
  "# STATUS v20",
  "",
  paste("Final status:", status),
  "",
  "The previous N=9018 private-derived-cohort result is no longer the primary analysis.",
  "The new primary analysis uses the public raw-only reconstructed cohort.",
  "No old frozen result may be carried forward unless regenerated under v20.",
  "",
  paste("Final public raw-only N:", N),
  paste("Final public raw-only deaths:", deaths),
  paste0("Primary Model 4 HR: ", fmt_num(hr, 2), " (95% CI ", fmt_num(lo, 2), "-", fmt_num(hi, 2), ")"),
  paste("Primary Model 4 P value:", fmt_p(pval)),
  paste("PH diagnostics pass:", ph_pass),
  paste("Data Availability category:", data_availability_category),
  "",
  "Ready for code availability review. A DOI can be added after GitHub archival through Zenodo or a similar repository."
), v20_path("STATUS_v20.md"))

repo <- v20_path("public_repository_candidate")
if (dir.exists(repo)) {
  for (f in list.files(v20_path("validation"), full.names = TRUE)) safe_copy(f, file.path(repo, "validation", basename(f)))
  repo_manifest <- manifest_for_dir(repo)
  safe_write_csv(repo_manifest, file.path(repo, "repository_file_manifest_sha256.csv"))
  safe_write_csv(repo_manifest, file.path(repo, "manifest", "repository_file_manifest_sha256.csv"))
}
write_all_manifests()
log_action(paste("09 validation and hashes completed with final status:", status))
