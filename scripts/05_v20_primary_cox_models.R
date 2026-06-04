source(file.path("scripts", "00_v20_setup_safety_check.R"))

dat <- readRDS(file.path(RECON_DIR, "final_model4_public_raw_analytic_cohort_DO_NOT_UPLOAD.rds"))
dat$joint_urate_periodontitis <- stats::relevel(factor(dat$joint_urate_periodontitis, levels = joint_levels), ref = "normouricemia + none/mild")

main_models <- model_terms[c("Model 1", "Model 2", "Model 3", "Model 4")]
model_results <- list()
model_counts <- data.frame()
for (m in names(main_models)) {
  fit_obj <- fit_svy_model(dat, main_models[[m]], m)
  safe_save_rds(fit_obj$fit, file.path(MODEL_DIR, paste0(gsub(" ", "_", tolower(m)), "_svycoxph_v20.rds")))
  model_results[[m]] <- extract_joint_terms(fit_obj$fit, m, fit_obj$data)
  model_counts <- dplyr::bind_rows(model_counts, data.frame(
    model = m,
    complete_case_N = nrow(fit_obj$data),
    deaths = sum(fit_obj$data$death_allcause, na.rm = TRUE),
    terms = paste(main_models[[m]], collapse = " + "),
    stringsAsFactors = FALSE
  ))
}
long <- dplyr::bind_rows(model_results) |>
  dplyr::mutate(
    exposure_group = factor(exposure_group, levels = joint_labels),
    model = factor(model, levels = names(main_models)),
    events_in_exposure_group = vapply(as.character(exposure_group), function(label) {
      raw <- names(joint_labels)[match(label, joint_labels)]
      sum(dat$death_allcause[dat$joint_urate_periodontitis == raw], na.rm = TRUE)
    }, integer(1)),
    HR_95CI = fmt_hr_ci(HR, conf_low, conf_high, dash = "-"),
    P_value = fmt_p(p_value)
  ) |>
  dplyr::arrange(model, exposure_group)

safe_write_csv(long, v20_path("source_data", "tables", "table2_source_data_v20.csv"))
safe_write_csv(model_counts, v20_path("source_data", "tables", "model_specific_complete_case_counts_v20.csv"))

wide <- long |>
  dplyr::mutate(model = as.character(model), HR_CI = fmt_hr_ci(HR, conf_low, conf_high, dash = "-")) |>
  dplyr::select(exposure_group, model, HR_CI) |>
  tidyr::pivot_wider(names_from = model, values_from = HR_CI) |>
  dplyr::left_join(
    long |> dplyr::filter(as.character(model) == "Model 4") |>
      dplyr::transmute(exposure_group, `Group N` = vapply(as.character(exposure_group), function(label) {
        raw <- names(joint_labels)[match(label, joint_labels)]
        sum(dat$joint_urate_periodontitis == raw, na.rm = TRUE)
      }, integer(1)), `Deaths in group` = events_in_exposure_group),
    by = "exposure_group"
  ) |>
  dplyr::arrange(factor(exposure_group, levels = joint_labels)) |>
  dplyr::select(`Exposure group` = exposure_group, `Group N`, `Deaths in group`, `Model 1`, `Model 2`, `Model 3`, `Model 4`)

safe_write_csv(wide, v20_path("tables", "main", "Table_2_joint_exposure_cox_models_v20.csv"))

primary <- long |>
  dplyr::filter(as.character(model) == "Model 4", exposure_group == joint_labels[["hyperuricemia + moderate/severe"]]) |>
  dplyr::slice(1)
primary_summary <- data.frame(
  metric = c("primary_model", "N", "deaths", "comparison", "HR", "CI_low", "CI_high", "P_value"),
  value = c("Model 4 public raw-only", primary$N, primary$deaths, as.character(primary$exposure_group),
            sprintf("%.12f", primary$HR), sprintf("%.12f", primary$conf_low), sprintf("%.12f", primary$conf_high),
            sprintf("%.12g", primary$p_value)),
  stringsAsFactors = FALSE
)
safe_write_csv(primary_summary, v20_path("validation", "primary_model_summary_v20.csv"))

figure2_source <- long |>
  dplyr::filter(as.character(model) == "Model 4") |>
  dplyr::mutate(label = as.character(exposure_group),
                display = fmt_hr_ci(HR, conf_low, conf_high, dash = "-"),
                side_label = ifelse(is.na(conf_low), "Reference", paste0("Deaths=", events_in_exposure_group, "; ", display)))
safe_write_csv(figure2_source, v20_path("source_data", "figures", "figure2_forest_source_v20.csv"))

safe_write_lines(c(
  "# v20 primary model report",
  "",
  paste("Primary public raw-only final analytic N:", primary$N),
  paste("Deaths:", primary$deaths),
  paste0("Primary Model 4 HR: ", fmt_num(primary$HR, 2), " (95% CI ", fmt_ci(primary$conf_low, primary$conf_high), "), P=", fmt_p(primary$p_value), "."),
  "",
  "Models 1-4 used the v20 public raw-only final complete-case cohort and NHANES MEC 6-year weights.",
  "No old HR=1.95 target was used, and no model result was tuned to match the private-derived cohort."
), v20_path("reports", "v20_primary_model_report.md"))

log_action(paste("05 primary Cox models completed; Model 4 HR", fmt_num(primary$HR, 3), "CI", fmt_ci(primary$conf_low, primary$conf_high)))
