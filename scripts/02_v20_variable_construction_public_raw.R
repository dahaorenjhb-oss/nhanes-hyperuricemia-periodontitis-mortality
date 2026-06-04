source(file.path("scripts", "00_v20_setup_safety_check.R"))

merged <- readRDS(file.path(RECON_DIR, "merged_nhanes_lmf_2009_2014_public_raw.rds"))
rx_files <- list.files(IMPORTED_RAW_DIR, pattern = "^RXQ_RX_.*[.]rds$", full.names = TRUE)
rx <- if (length(rx_files)) dplyr::bind_rows(lapply(rx_files, readRDS)) else data.frame()
rx_flags <- rx_flag_table(rx)

derived <- merged |>
  dplyr::mutate(WTMEC6YR = WTMEC2YR / 3) |>
  derive_common_covariates() |>
  derive_mortality_outcomes() |>
  derive_periodontitis() |>
  derive_kidney_and_urate() |>
  dplyr::left_join(rx_flags, by = "SEQN") |>
  dplyr::mutate(
    urate_lowering_therapy = dplyr::coalesce(urate_lowering_therapy, 0L),
    diuretic_use = dplyr::coalesce(diuretic_use, 0L)
  ) |>
  add_joint_exposure()

safe_save_rds(derived, file.path(RECON_DIR, "derived_variables_public_raw.rds"))

vars <- c("age", "sex", "race_ethnicity", "education", "PIR", "BMI", "smoking_status",
          "cotinine", "log_cotinine", "HbA1c", "diabetes", "hypertension", "CVD",
          "gout", "urate_lowering_therapy", "diuretic_use", "egfr", "UACR", "CKD",
          "uric_acid_mg_dl", "hyperuricemia", "periodontitis_4cat", "periodontitis_binary",
          "joint_urate_periodontitis", "death_allcause", "followup_years", "WTMEC6YR",
          "SDMVSTRA", "SDMVPSU", "NHANES_CYCLE")
missingness <- data.frame(
  variable = vars,
  n_total = nrow(derived),
  n_missing = vapply(vars, function(v) sum(is.na(derived[[v]])), integer(1)),
  n_nonmissing = vapply(vars, function(v) sum(!is.na(derived[[v]])), integer(1)),
  stringsAsFactors = FALSE
)
safe_write_csv(missingness, v20_path("source_data", "supplementary", "variable_missingness_after_derivation_v20.csv"))

safe_write_csv(data.frame(
  medication_flag = c("urate_lowering_therapy", "diuretic_use"),
  public_raw_source = "RXQ_RX_F/G/H.XPT",
  derivation = c("drug-name match to allopurinol/febuxostat/probenecid/related urate-lowering agents",
                 "drug-name match to thiazide/loop/potassium-sparing/related diuretic agents"),
  participant_level_exported_publicly = "no",
  stringsAsFactors = FALSE
), v20_path("source_data", "supplementary", "medication_flag_derivation_summary_v20.csv"))

log_action("02 public raw variables constructed.")
