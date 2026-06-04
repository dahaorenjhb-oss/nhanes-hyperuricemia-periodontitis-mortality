source(file.path("scripts", "00_v20_setup_safety_check.R"))

dat <- readRDS(file.path(RECON_DIR, "final_model4_public_raw_analytic_cohort_DO_NOT_UPLOAD.rds"))
dat$joint_urate_periodontitis <- factor(dat$joint_urate_periodontitis, levels = joint_levels)
des <- make_design(dat)

weighted_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- as.numeric(x[ok]); w <- as.numeric(w[ok])
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

wtd_mean_se <- function(var, group_raw) {
  sub <- subset(des, joint_urate_periodontitis == group_raw)
  est <- tryCatch(survey::svymean(stats::as.formula(paste0("~", var)), sub, na.rm = TRUE), error = function(e) NA)
  if (all(is.na(est))) return(NA_character_)
  paste0(fmt_num(coef(est)[1], 1), " +/- ", fmt_num(SE(est)[1], 1))
}

manual_weighted_pct <- function(x, w, level) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  100 * sum(w[ok & x == level], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
}

continuous_summary <- function(var, label, method = c("mean_se", "median_iqr")) {
  method <- match.arg(method)
  rows <- lapply(joint_levels, function(g) {
    gd <- dat[dat$joint_urate_periodontitis == g, , drop = FALSE]
    value <- if (method == "mean_se") {
      wtd_mean_se(var, g)
    } else {
      q <- weighted_quantile(gd[[var]], gd$WTMEC6YR, c(0.25, 0.5, 0.75))
      paste0(fmt_num(q[2], 1), " [", fmt_num(q[1], 1), "-", fmt_num(q[3], 1), "]")
    }
    tibble::tibble(variable = label, level = ifelse(method == "mean_se", "Weighted mean +/- SE", "Weighted median [IQR]"),
                   group = unname(joint_labels[g]), value = value)
  })
  dplyr::bind_rows(rows)
}

categorical_summary <- function(var, label, levels_keep = NULL, level_labels = NULL) {
  x_all <- dat[[var]]
  levels <- levels_keep %||% {
    if (is.factor(x_all)) levels(x_all) else sort(unique(x_all[!is.na(x_all)]))
  }
  rows <- list()
  for (lvl in levels) {
    for (g in joint_levels) {
      gd <- dat[dat$joint_urate_periodontitis == g, , drop = FALSE]
      x <- gd[[var]]
      count <- sum(x == lvl, na.rm = TRUE)
      pct <- manual_weighted_pct(x, gd$WTMEC6YR, lvl)
      shown_level <- if (!is.null(level_labels) && as.character(lvl) %in% names(level_labels)) level_labels[[as.character(lvl)]] else as.character(lvl)
      rows[[length(rows) + 1]] <- tibble::tibble(
        variable = label, level = shown_level, group = unname(joint_labels[g]),
        value = paste0(count, " (", fmt_num(pct, 1), ")")
      )
    }
  }
  dplyr::bind_rows(rows)
}

n_by_group <- dat |>
  dplyr::count(joint_urate_periodontitis, name = "N") |>
  dplyr::mutate(group = unname(joint_labels[as.character(joint_urate_periodontitis)]), value = as.character(N)) |>
  dplyr::select(variable = N, group, value) |>
  dplyr::mutate(variable = "Unweighted N", level = "")

weight_total <- sum(dat$WTMEC6YR, na.rm = TRUE)
wp_by_group <- dat |>
  dplyr::group_by(joint_urate_periodontitis) |>
  dplyr::summarise(weighted_percent = 100 * sum(WTMEC6YR, na.rm = TRUE) / weight_total, .groups = "drop") |>
  dplyr::mutate(group = unname(joint_labels[as.character(joint_urate_periodontitis)]), value = fmt_pct(weighted_percent)) |>
  dplyr::transmute(variable = "Weighted %", level = "", group, value)

death_by_group <- dat |>
  dplyr::group_by(joint_urate_periodontitis) |>
  dplyr::summarise(deaths = sum(death_allcause, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(group = unname(joint_labels[as.character(joint_urate_periodontitis)]), value = as.character(deaths)) |>
  dplyr::transmute(variable = "All-cause deaths", level = "", group, value)

yes <- c(`1` = "Yes")
table1_long <- dplyr::bind_rows(
  n_by_group,
  wp_by_group,
  continuous_summary("age", "Age, years"),
  categorical_summary("sex", "Sex"),
  categorical_summary("race_ethnicity", "Race/ethnicity"),
  categorical_summary("education", "Education"),
  continuous_summary("PIR", "Poverty-income ratio"),
  continuous_summary("BMI", "Body mass index, kg/m2"),
  categorical_summary("smoking_status", "Smoking status"),
  continuous_summary("cotinine", "Serum cotinine, ng/mL", "median_iqr"),
  continuous_summary("uric_acid_mg_dl", "Serum uric acid, mg/dL"),
  categorical_summary("diabetes", "Diabetes", levels_keep = 1, level_labels = yes),
  categorical_summary("hypertension", "Hypertension", levels_keep = 1, level_labels = yes),
  categorical_summary("CVD", "Cardiovascular disease", levels_keep = 1, level_labels = yes),
  continuous_summary("egfr", "eGFR, mL/min/1.73m2"),
  continuous_summary("UACR", "UACR, mg/g", "median_iqr"),
  categorical_summary("CKD", "CKD", levels_keep = 1, level_labels = yes),
  categorical_summary("gout", "Gout", levels_keep = 1, level_labels = yes),
  categorical_summary("urate_lowering_therapy", "Urate-lowering therapy", levels_keep = 1, level_labels = yes),
  categorical_summary("diuretic_use", "Diuretic use", levels_keep = 1, level_labels = yes),
  continuous_summary("followup_years", "Follow-up time, years", "median_iqr"),
  death_by_group
)

table1 <- table1_long |>
  tidyr::pivot_wider(names_from = group, values_from = value) |>
  dplyr::arrange(match(variable, unique(table1_long$variable)))

safe_write_csv(table1, v20_path("tables", "main", "Table_1_baseline_characteristics_by_joint_exposure_v20.csv"))
safe_write_csv(table1_long, v20_path("source_data", "tables", "table1_source_data_v20.csv"))
safe_write_csv(tibble::tribble(
  ~item, ~definition,
  "Hyperuricemia", "Serum uric acid >7.0 mg/dL in men and >6.0 mg/dL in women.",
  "Periodontitis binary definition", "CDC/AAP none/mild versus moderate/severe periodontitis.",
  "Joint exposure reference group", "Normouricemia plus none/mild periodontitis.",
  "Primary comparison", "Hyperuricemia plus moderate/severe periodontitis versus normouricemia plus none/mild periodontitis."
), v20_path("tables", "supplementary", "Supplementary_Table_exposure_definitions_v20.csv"))

safe_write_lines(c(
  "# v20 Table 1 report",
  "",
  paste("Table 1 regenerated from v20 public raw-only final cohort, N =", nrow(dat), "deaths =", sum(dat$death_allcause, na.rm = TRUE)),
  "Values are unweighted counts with survey-weighted percentages or survey-weighted means unless noted.",
  "No old frozen Table 1 values were copied."
), v20_path("reports", "v20_table_figure_generation_report.md"))

log_action("04 descriptive Table 1 regenerated.")
