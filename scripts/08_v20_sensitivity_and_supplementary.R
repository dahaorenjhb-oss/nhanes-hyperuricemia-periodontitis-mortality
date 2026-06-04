source(file.path("scripts", "00_v20_setup_safety_check.R"))

dat <- readRDS(file.path(RECON_DIR, "final_model4_public_raw_analytic_cohort_DO_NOT_UPLOAD.rds"))
dat$joint_urate_periodontitis <- stats::relevel(factor(dat$joint_urate_periodontitis, levels = joint_levels), ref = "normouricemia + none/mild")

drop_constant_terms <- function(data, terms) {
  keep <- c()
  dropped <- c()
  for (tm in terms) {
    if (tm == "joint_urate_periodontitis") {
      keep <- c(keep, tm)
      next
    }
    vals <- data[[tm]]
    nlev <- length(unique(vals[!is.na(vals)]))
    if (nlev >= 2) keep <- c(keep, tm) else dropped <- c(dropped, tm)
  }
  attr(keep, "dropped_constant_terms") <- dropped
  keep
}

fit_sensitivity <- function(label, data, terms) {
  terms2 <- drop_constant_terms(data, terms)
  fit_obj <- tryCatch(fit_svy_model(data, terms2, label), error = function(e) e)
  if (inherits(fit_obj, "error")) {
    return(tibble::tibble(sensitivity = label, N = nrow(data), deaths = sum(data$death_allcause, na.rm = TRUE),
                          HR = NA_real_, conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
                          direction_consistent = "not estimable", interpretation_flag = paste("model error:", conditionMessage(fit_obj)),
                          dropped_terms = paste(attr(terms2, "dropped_constant_terms") %||% character(), collapse = ";")))
  }
  out <- extract_core_comparison(fit_obj, label)
  out$dropped_terms <- paste(attr(terms2, "dropped_constant_terms") %||% character(), collapse = ";")
  out
}

sens <- dplyr::bind_rows(
  fit_sensitivity("Main fully adjusted model", dat, model_terms[["Model 4"]]),
  fit_sensitivity("Additionally adjusted for gout, urate-lowering therapy, and diuretic use", dat, model_terms[["Model 5"]]),
  fit_sensitivity("Excluding deaths within first 2 years", dat[!(dat$death_allcause == 1 & dat$followup_years < 2), , drop = FALSE], model_terms[["Model 4"]]),
  fit_sensitivity("Excluding participants with baseline CVD", dat[dat$CVD == 0, , drop = FALSE], model_terms[["Model 4"]]),
  fit_sensitivity("Excluding participants with CKD", dat[dat$CKD == 0, , drop = FALSE], model_terms[["Model 4"]]),
  fit_sensitivity("Men", dat[as.character(dat$sex) == "Male", , drop = FALSE], model_terms[["Model 4"]]),
  fit_sensitivity("Women", dat[as.character(dat$sex) == "Female", , drop = FALSE], model_terms[["Model 4"]]),
  fit_sensitivity("Never smokers", dat[as.character(dat$smoking_status) == "Never", , drop = FALSE], model_terms[["Model 4"]]),
  fit_sensitivity("Ever smokers", dat[as.character(dat$smoking_status) %in% c("Former", "Current"), , drop = FALSE], model_terms[["Model 4"]])
) |>
  dplyr::mutate(HR_95CI = ifelse(is.na(HR), "Not estimable", fmt_hr_ci(HR, conf_low, conf_high, dash = "-")))

safe_write_csv(sens, v20_path("source_data", "tables", "table3_source_data_v20.csv"))

table3 <- sens |>
  dplyr::transmute(
    Analysis = sensitivity,
    N,
    Deaths = deaths,
    HR = fmt_num(HR, 2),
    `95% CI` = ifelse(is.na(HR), "Not estimable", fmt_ci(conf_low, conf_high)),
    `P value` = fmt_p(p_value),
    `Direction consistent` = direction_consistent,
    `Interpretation flag` = interpretation_flag
  )
safe_write_csv(table3, v20_path("tables", "main", "Table_3_sensitivity_analyses_v20.csv"))

fig3_source <- sens |>
  dplyr::mutate(analysis = sensitivity, display = ifelse(is.na(HR), "Not estimable", fmt_hr_ci(HR, conf_low, conf_high, dash = "-")))
safe_write_csv(fig3_source, v20_path("source_data", "figures", "figure3_sensitivity_source_v20.csv"))

candidate <- readRDS(file.path(RECON_DIR, "candidate_public_raw_after_periodontal_sua_mortality.rds"))
flow <- utils::read.csv(v20_path("source_data", "supplementary", "public_raw_reconstruction_flow_v20.csv"), stringsAsFactors = FALSE)
varmiss <- utils::read.csv(v20_path("source_data", "supplementary", "variable_specific_exclusions_v20.csv"), stringsAsFactors = FALSE)

included <- candidate$SEQN %in% dat$SEQN
included_excluded_rows <- list()
add_cont <- function(v, label) {
  included_excluded_rows[[length(included_excluded_rows) + 1]] <<- data.frame(
    section = "included_vs_excluded", item = paste0(label, ": mean"), denominator = nrow(candidate),
    missing_n = NA, missing_percent = NA,
    included_n = sum(included & !is.na(candidate[[v]])), excluded_n = sum(!included & !is.na(candidate[[v]])),
    included_value = fmt_num(mean(candidate[[v]][included], na.rm = TRUE), 2),
    excluded_value = fmt_num(mean(candidate[[v]][!included], na.rm = TRUE), 2),
    source = "v20 public raw candidate denominator versus final public raw-only cohort"
  )
}
add_cat <- function(v, label) {
  lev <- if (is.factor(candidate[[v]])) levels(candidate[[v]]) else sort(unique(candidate[[v]][!is.na(candidate[[v]])]))
  for (lv in lev) {
    included_excluded_rows[[length(included_excluded_rows) + 1]] <<- data.frame(
      section = "included_vs_excluded", item = paste0(label, ": ", lv), denominator = nrow(candidate),
      missing_n = NA, missing_percent = NA,
      included_n = sum(included & candidate[[v]] == lv, na.rm = TRUE),
      excluded_n = sum(!included & candidate[[v]] == lv, na.rm = TRUE),
      included_value = NA, excluded_value = NA,
      source = "v20 public raw candidate denominator versus final public raw-only cohort"
    )
  }
}
add_cont("age", "age"); add_cont("PIR", "PIR"); add_cont("BMI", "BMI")
add_cat("sex", "sex"); add_cat("race_ethnicity", "race_ethnicity"); add_cat("education", "education"); add_cat("smoking_status", "smoking_status")

s1 <- dplyr::bind_rows(
  flow |> dplyr::transmute(section = "denominator", item = exclusion_rule, denominator = n_after,
                           missing_n = NA_real_, missing_percent = NA_real_, included_n = NA_real_, excluded_n = n_excluded,
                           included_value = NA_character_, excluded_value = NA_character_, source = "v20 public raw exclusion flow"),
  varmiss |> dplyr::transmute(section = "missingness", item = variable, denominator = denominator,
                              missing_n = missing_n, missing_percent = missing_percent, included_n = NA_real_, excluded_n = NA_real_,
                              included_value = NA_character_, excluded_value = NA_character_, source = "v20 public raw candidate denominator"),
  dplyr::bind_rows(included_excluded_rows)
)
safe_write_csv(s1, v20_path("tables", "supplementary", "Supplementary_Table_S1_missingness_included_excluded_v20.csv"))
safe_write_csv(s1, v20_path("source_data", "supplementary", "supplementary_table_s1_source_v20.csv"))

pois_ci <- function(events, py) {
  if (is.na(py) || py <= 0) return(c(NA_real_, NA_real_))
  lo <- if (events == 0) 0 else stats::qchisq(0.025, 2 * events) / 2
  hi <- stats::qchisq(0.975, 2 * (events + 1)) / 2
  c(1000 * lo / py, 1000 * hi / py)
}
s6 <- dat |>
  dplyr::group_by(joint_phenotype = joint_urate_periodontitis) |>
  dplyr::summarise(unweighted_N = dplyr::n(), deaths = sum(death_allcause, na.rm = TRUE),
                   person_years = sum(followup_years, na.rm = TRUE),
                   mortality_rate_per_1000_py = 1000 * deaths / person_years,
                   .groups = "drop") |>
  dplyr::rowwise() |>
  dplyr::mutate(rate_95ci_lower = pois_ci(deaths, person_years)[1],
                rate_95ci_upper = pois_ci(deaths, person_years)[2],
                weighted_rate_per_1000_py = NA_real_,
                weighted_rate_status = "not_estimated_v20",
                interpretation = "descriptive_unweighted_supplement_only",
                source_dataset = "v20 public raw-only final analytic cohort; participant-level rows not public",
                v20_use_boundary_note = "Supplementary descriptive person-time context only; not causal evidence and not a replacement for survey-weighted Cox models.") |>
  dplyr::ungroup()
safe_write_csv(s6, v20_path("tables", "supplementary", "Supplementary_Table_S6_person_time_mortality_rates_v20.csv"))
safe_write_csv(s6, v20_path("source_data", "supplementary", "supplementary_table_s6_source_v20.csv"))

safe_write_lines(c(
  "# v20 sensitivity and supplementary report",
  "",
  "Table 3 was regenerated from v20 public raw-only data.",
  "Supplementary Table S1 was regenerated from v20 public raw-only flow and missingness.",
  "Supplementary Table S6 was regenerated as descriptive, unweighted, supplementary person-time context.",
  "No Fine-Gray model was run, and no new model outside the previous manuscript structure was added."
), v20_path("reports", "v20_sensitivity_and_supplementary_report.md"))

log_action("08 sensitivity analyses and S1/S6 regenerated.")
