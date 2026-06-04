source(file.path("scripts", "00_v20_setup_safety_check.R"))

dat <- readRDS(file.path(RECON_DIR, "derived_variables_public_raw.rds"))
flow <- data.frame()
add_step <- function(step_number, rule, before, excluded, after, expected = NA_integer_, notes = "") {
  flow <<- dplyr::bind_rows(flow, data.frame(
    step_number = step_number,
    exclusion_rule = rule,
    n_before = before,
    n_excluded = excluded,
    n_after = after,
    expected_n_after_from_v18e_if_known = expected,
    match_status = ifelse(is.na(expected), "NO_V18E_TARGET", ifelse(after == expected, "MATCH", "MISMATCH")),
    notes = notes,
    stringsAsFactors = FALSE
  ))
}
track <- function(x, step, rule, expr, expected = NA_integer_, notes = "") {
  before <- nrow(x)
  out <- x |> dplyr::filter({{ expr }})
  add_step(step, rule, before, before - nrow(out), nrow(out), expected, notes)
  out
}

current <- dat
add_step(1, "Downloaded and merged NHANES 2009-2014 public-use files", nrow(current), 0L, nrow(current), 30468L)
current <- track(current, 2, "Age >=30 years", age >= 30, 14556L)
current <- track(current, 3, "Mortality linkage eligible", mortality_eligible, 14521L)
current <- track(current, 4, "Dentate adults", dentate, 11410L)
current <- track(current, 5, "Complete periodontal examination data", periodontal_exam_complete, 10715L)
current <- track(current, 6, "CDC/AAP periodontitis computable", periodontitis_computable, 10715L)
current <- track(current, 7, "Serum uric acid available", !is.na(uric_acid_mg_dl) & !is.na(hyperuricemia), 10188L)
current <- track(current, 8, "MORTSTAT and PERMTH_EXM available", !is.na(MORTSTAT) & !is.na(PERMTH_EXM) & !is.na(death_allcause) & !is.na(followup_years), 10188L)

candidate <- current
core_covariates <- c("age", "sex", "race_ethnicity", "education", "PIR", "BMI", "smoking_status",
                     "log_cotinine", "diabetes", "hypertension", "CVD")
current <- track(current, 9, "Core covariates complete", stats::complete.cases(dplyr::pick(dplyr::all_of(core_covariates))), 9069L,
                 "Current CDC public raw-only core complete N.")
core_complete <- current
model4_vars <- c(core_covariates, "egfr", "UACR", "CKD", "followup_years", "death_allcause",
                 "joint_urate_periodontitis", "WTMEC6YR", "SDMVSTRA", "SDMVPSU")
current <- track(current, 10, "Final Model 4 complete cases", stats::complete.cases(dplyr::pick(dplyr::all_of(model4_vars))), 9018L,
                 "Current CDC public raw-only final analytic N.")
final <- current

safe_save_rds(candidate, file.path(RECON_DIR, "candidate_public_raw_after_periodontal_sua_mortality.rds"))
safe_save_rds(core_complete, file.path(RECON_DIR, "core_complete_public_raw.rds"))
safe_save_rds(final, file.path(RECON_DIR, "final_model4_public_raw_analytic_cohort_DO_NOT_UPLOAD.rds"))
safe_write_csv(flow, v20_path("source_data", "supplementary", "public_raw_reconstruction_flow_v20.csv"))

candidate_vars <- c(core_covariates, "egfr", "UACR", "CKD", "followup_years", "death_allcause",
                    "joint_urate_periodontitis", "WTMEC6YR", "SDMVSTRA", "SDMVPSU",
                    "gout", "urate_lowering_therapy", "diuretic_use")
var_excl <- data.frame(
  variable = candidate_vars,
  denominator = nrow(candidate),
  missing_n = vapply(candidate_vars, function(v) sum(is.na(candidate[[v]])), integer(1)),
  missing_percent = vapply(candidate_vars, function(v) 100 * mean(is.na(candidate[[v]])), numeric(1)),
  stringsAsFactors = FALSE
)
safe_write_csv(var_excl, v20_path("source_data", "supplementary", "variable_specific_exclusions_v20.csv"))

cycle_var <- candidate |>
  dplyr::select(NHANES_CYCLE, dplyr::all_of(candidate_vars)) |>
  dplyr::mutate(dplyr::across(dplyr::all_of(candidate_vars), is.na)) |>
  tidyr::pivot_longer(-NHANES_CYCLE, names_to = "variable", values_to = "is_missing") |>
  dplyr::group_by(cycle = NHANES_CYCLE, variable) |>
  dplyr::summarise(denominator = dplyr::n(), missing_n = sum(is_missing), missing_percent = 100 * mean(is_missing), .groups = "drop")
safe_write_csv(cycle_var, v20_path("source_data", "supplementary", "cycle_specific_exclusions_v20.csv"))

smq <- candidate |>
  dplyr::group_by(cycle = NHANES_CYCLE) |>
  dplyr::summarise(
    candidate_n = dplyr::n(),
    smoking_status_missing = sum(is.na(smoking_status)),
    smoking_status_nonmissing = sum(!is.na(smoking_status)),
    .groups = "drop"
  )
safe_write_csv(smq, v20_path("source_data", "supplementary", "smoking_status_reconstruction_summary_v20.csv"))

cohort_summary <- data.frame(
  metric = c("initial_public_raw_denominator", "mortality_eligible_denominator", "candidate_denominator",
             "core_complete_N", "final_analytic_N", "final_analytic_deaths", "selected_weight", "psu", "strata", "cycles"),
  value = c(nrow(dat), flow$n_after[flow$step_number == 3], nrow(candidate), nrow(core_complete), nrow(final),
            sum(final$death_allcause, na.rm = TRUE), "WTMEC6YR = WTMEC2YR / 3", "SDMVPSU", "SDMVSTRA",
            paste(sort(unique(final$NHANES_CYCLE)), collapse = "; ")),
  stringsAsFactors = FALSE
)
safe_write_csv(cohort_summary, v20_path("source_data", "supplementary", "public_raw_cohort_summary_v20.csv"))

safe_write_lines(c(
  "# v20 public raw cohort report",
  "",
  "Primary input boundary: public-use NHANES raw files and public-use NCHS/CDC Linked Mortality Files only.",
  "The private recovered derived cohort and `fallback_nested_audit_dataset.rds` were not used as v20 primary inputs.",
  "",
  paste("Initial public raw denominator:", nrow(dat)),
  paste("Mortality-eligible denominator:", flow$n_after[flow$step_number == 3]),
  paste("Candidate denominator after periodontal/SUA/mortality gates:", nrow(candidate)),
  paste("Core complete N:", nrow(core_complete)),
  paste("Final public raw-only analytic N:", nrow(final)),
  paste("Final public raw-only deaths:", sum(final$death_allcause, na.rm = TRUE)),
  "",
  "Survey design:",
  "- Weight: WTMEC6YR = WTMEC2YR / 3 for three 2-year NHANES MEC cycles.",
  "- PSU: SDMVPSU.",
  "- Strata: SDMVSTRA.",
  "- Cycles: NHANES 2009-2010, 2011-2012, and 2013-2014.",
  "",
  "Known public raw-only difference from the old private-derived cohort:",
  "- 2013-2014 public SMQ_H lacks 931 candidate respondent records reported in v18e/v18c.",
  "- Public-rule smoking_status missingness is concentrated in 2013-2014.",
  "- The v20 analysis does not impute or reverse-engineer those records."
), v20_path("reports", "v20_public_raw_cohort_report.md"))

log_action(paste("03 cohort defined: final N", nrow(final), "deaths", sum(final$death_allcause, na.rm = TRUE)))
