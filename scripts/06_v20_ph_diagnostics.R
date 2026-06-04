source(file.path("scripts", "00_v20_setup_safety_check.R"))

dat <- readRDS(file.path(RECON_DIR, "final_model4_public_raw_analytic_cohort_DO_NOT_UPLOAD.rds"))
terms <- model_terms[["Model 4"]]
use_vars <- unique(c("followup_years", "death_allcause", "WTMEC6YR", "SDMVSTRA", "SDMVPSU", terms))
cc <- dat[stats::complete.cases(dat[, use_vars, drop = FALSE]), , drop = FALSE]
cc <- droplevels(cc)
cc$joint_urate_periodontitis <- stats::relevel(factor(cc$joint_urate_periodontitis, levels = joint_levels), ref = "normouricemia + none/mild")
form <- stats::as.formula(paste("survival::Surv(followup_years, death_allcause) ~", paste(terms, collapse = " + ")))

ph_status <- "NOT_RUN"
ph_pass <- NA
ph_table <- data.frame()
err <- NA_character_
ph_result <- tryCatch({
  cfit <- survival::coxph(form, data = cc, ties = "efron", x = TRUE)
  z <- survival::cox.zph(cfit)
  raw <- as.data.frame(z$table)
  tab <- data.frame(
    model = "Model 4 public raw-only",
    analytic_n = nrow(cc),
    deaths = sum(cc$death_allcause, na.rm = TRUE),
    diagnostic = "Schoenfeld residual test",
    term = rownames(raw),
    chisq = raw$chisq,
    df = raw$df,
    p_value = raw$p,
    interpretation = ifelse(raw$p >= 0.05, "no_evidence_p_ge_0_05", "possible_PH_violation_p_lt_0_05"),
    note = "PH diagnostics only; cox.zph applied to an unweighted coxph approximation using the same v20 Model 4 complete-case data and formula because survey::svycoxph does not provide a stable cox.zph path.",
    stringsAsFactors = FALSE
  )
  list(table = tab, pass = all(tab$p_value >= 0.05, na.rm = TRUE),
       status = ifelse(all(tab$p_value >= 0.05, na.rm = TRUE), "PASS", "PH_SIGNAL_REQUIRES_AUTHOR_REVIEW"))
}, error = function(e) {
  e
})

if (inherits(ph_result, "error")) {
  err <- conditionMessage(ph_result)
  ph_status <- "PH_DIAGNOSTICS_FAILED"
} else {
  ph_table <- ph_result$table
  ph_pass <- ph_result$pass
  ph_status <- ph_result$status
}

if (!nrow(ph_table)) {
  ph_table <- data.frame(model = "Model 4 public raw-only", analytic_n = nrow(cc),
                         deaths = sum(cc$death_allcause, na.rm = TRUE), diagnostic = "Schoenfeld residual test",
                         term = NA_character_, chisq = NA_real_, df = NA_real_, p_value = NA_real_,
                         interpretation = ph_status, note = err, stringsAsFactors = FALSE)
}
safe_write_csv(ph_table, v20_path("validation", "ph_diagnostics_v20.csv"))
if (isTRUE(ph_pass)) {
  safe_write_csv(ph_table, v20_path("tables", "supplementary", "Supplementary_Table_S2_PH_diagnostics_v20.csv"))
}

safe_write_lines(c(
  "# v20 PH diagnostics report",
  "",
  paste("Status:", ph_status),
  paste("Analytic N:", nrow(cc)),
  paste("Deaths:", sum(cc$death_allcause, na.rm = TRUE)),
  if (isTRUE(ph_pass)) "Supplementary Table S2 was regenerated from v20 PH diagnostics." else "Supplementary Table S2 was not finalized as a pass-confirmed table.",
  if (!is.na(err)) paste("Diagnostic error:", err) else "",
  "",
  "Diagnostics were not used to alter the exposure, covariate set, censoring, or primary model."
), v20_path("reports", "v20_ph_diagnostics_report.md"))

log_action(paste("06 PH diagnostics status:", ph_status))
