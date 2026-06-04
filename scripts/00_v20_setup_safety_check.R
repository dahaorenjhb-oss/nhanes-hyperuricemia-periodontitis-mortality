options(stringsAsFactors = FALSE)
options(survey.lonely.psu = "adjust")
options(timeout = max(900, getOption("timeout")))

`%||%` <- function(a, b) if (!is.null(a)) a else b

script_path <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", cmd, value = TRUE)
  if (length(hit)) return(normalizePath(sub("^--file=", "", hit[[1]]), winslash = "/", mustWork = FALSE))
  of <- sys.frames()[[1]]$ofile %||% ""
  normalizePath(of, winslash = "/", mustWork = FALSE)
}

V20_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
valid_root_names <- c("v20_public_raw_to_final_primary_reanalysis", "public_repository_candidate")
if (!basename(V20_ROOT) %in% valid_root_names) {
  candidate <- normalizePath(file.path(dirname(script_path()), ".."), winslash = "/", mustWork = FALSE)
  if (basename(candidate) %in% valid_root_names) V20_ROOT <- candidate
}
if (!basename(V20_ROOT) %in% valid_root_names) {
  stop("Run from v20 root, public repository candidate root, or a script path inside one of those roots.")
}

PROJECT_ROOT <- normalizePath(file.path(V20_ROOT, ".."), winslash = "/", mustWork = TRUE)
V15_ROOT <- file.path(PROJECT_ROOT, "v15_scirep_polishing_with_v14_s1_GOAL")
V16_ROOT <- file.path(PROJECT_ROOT, "v16_author_confirmation_final_lock_with_minimal_PH_QA")
V17_ROOT <- file.path(PROJECT_ROOT, "v17_reproducibility_bundle_verified_rerun")
V18_ROOT <- file.path(PROJECT_ROOT, "v18_public_raw_rebuild_attempt_and_repository_boundary_lock")
V18B_ROOT <- file.path(PROJECT_ROOT, "v18b_raw_to_cohort_reconstruction_engineering")
V18C_ROOT <- file.path(PROJECT_ROOT, "v18c_smoking_status_mapping_discrepancy_resolution")
V18D_ROOT <- file.path(PROJECT_ROOT, "v18d_original_derived_data_recovery_and_private_validation")
V18E_ROOT <- file.path(PROJECT_ROOT, "v18e_strict_public_raw_to_final_variant_A_feasibility_test")
READ_ONLY_ROOTS <- c(V15_ROOT, V16_ROOT, V17_ROOT, V18_ROOT, V18B_ROOT, V18C_ROOT, V18D_ROOT, V18E_ROOT)

v20_path <- function(...) file.path(V20_ROOT, ...)

assert_inside_v20 <- function(path) {
  np <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(V20_ROOT, winslash = "/", mustWork = TRUE)
  if (!startsWith(np, paste0(root, "/")) && np != root) stop("Refusing to write outside v20: ", path)
  invisible(TRUE)
}

ensure_dir <- function(path) {
  assert_inside_v20(path)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

safe_write_csv <- function(x, path) {
  assert_inside_v20(path)
  ensure_dir(dirname(path))
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

safe_write_lines <- function(x, path) {
  assert_inside_v20(path)
  ensure_dir(dirname(path))
  writeLines(x, path, useBytes = TRUE)
  invisible(path)
}

safe_save_rds <- function(x, path) {
  assert_inside_v20(path)
  ensure_dir(dirname(path))
  saveRDS(x, path)
  invisible(path)
}

safe_copy <- function(from, to, overwrite = TRUE) {
  assert_inside_v20(to)
  ensure_dir(dirname(to))
  ok <- file.copy(from, to, overwrite = overwrite)
  if (!ok) stop("Failed to copy ", from, " to ", to)
  invisible(to)
}

log_action <- function(text) {
  path <- v20_path("logs", "v20_run_log.txt")
  assert_inside_v20(path)
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), text), file = path, append = TRUE)
  invisible(TRUE)
}

sha256_file <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)
  unname(tools::sha256sum(path))
}

scrub_path <- function(path) {
  x <- normalizePath(path, winslash = "/", mustWork = FALSE)
  x <- gsub(PROJECT_ROOT, "<PROJECT_ROOT>", x, fixed = TRUE)
  x <- gsub("LOCAL_PATH_REMOVED", "<USER_HOME>", x, fixed = TRUE)
  x
}

required_packages <- c("haven", "dplyr", "tidyr", "stringr", "survey", "survival",
                       "broom", "ggplot2", "readr", "purrr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing required R packages: ", paste(missing_packages, collapse = ", "))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(survey)
  library(survival)
  library(ggplot2)
  library(tibble)
})

PRIVATE_DIR <- v20_path("data_private_DO_NOT_UPLOAD")
RAW_CACHE_DIR <- file.path(PRIVATE_DIR, "public_raw_cache")
IMPORTED_RAW_DIR <- file.path(RAW_CACHE_DIR, "imported_raw_rds")
RECON_DIR <- file.path(PRIVATE_DIR, "reconstructed_public_raw_cohort")
PRIVATE_COMPARE_DIR <- file.path(PRIVATE_DIR, "private_historical_comparison")
MODEL_DIR <- file.path(RECON_DIR, "model_outputs")

for (d in c("scripts", "source_data/tables", "source_data/figures", "source_data/supplementary",
            "tables/main", "tables/supplementary", "figures/main", "figures/supplementary",
            "validation", "reports", "manuscript_text", "public_repository_candidate",
            "manifest", "logs", RAW_CACHE_DIR, IMPORTED_RAW_DIR, RECON_DIR, PRIVATE_COMPARE_DIR, MODEL_DIR)) {
  ensure_dir(if (grepl("^/", d)) d else v20_path(d))
}

start_marker <- v20_path("logs", "v20_start_marker.txt")
if (!file.exists(start_marker)) {
  safe_write_lines(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), start_marker)
}

fmt_num <- function(x, digits = 2) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = digits))
fmt_pct <- function(x, digits = 1) ifelse(is.na(x), "", paste0(formatC(as.numeric(x), format = "f", digits = digits), "%"))
fmt_p <- function(x) dplyr::case_when(is.na(x) ~ "", x < 0.001 ~ "<0.001", TRUE ~ formatC(x, format = "f", digits = 3))
fmt_ci <- function(lo, hi, dash = "-") paste0(fmt_num(lo, 2), dash, fmt_num(hi, 2))
fmt_hr_ci <- function(hr, lo, hi, dash = "-") ifelse(is.na(lo) | is.na(hi), "Reference", paste0(fmt_num(hr, 2), " (", fmt_ci(lo, hi, dash), ")"))

cycle_suffix_from_file <- function(file_name) {
  if (grepl("_F[.]XPT$", file_name)) return("F")
  if (grepl("_G[.]XPT$", file_name)) return("G")
  if (grepl("_H[.]XPT$", file_name)) return("H")
  if (grepl("2009_2010", file_name)) return("F")
  if (grepl("2011_2012", file_name)) return("G")
  if (grepl("2013_2014", file_name)) return("H")
  NA_character_
}

cycle_label_from_suffix <- function(suffix) {
  c(F = "2009-2010", G = "2011-2012", H = "2013-2014")[[suffix]]
}

module_from_file <- function(file_name) sub("_[FGH][.]XPT$", "", file_name)

read_registry <- function() {
  utils::read.csv(v20_path("config", "raw_file_registry_v20.csv"), stringsAsFactors = FALSE, check.names = FALSE)
}

available_registry <- function(file_type = NULL) {
  x <- read_registry()
  x <- x[x$download_status %in% c("AVAILABLE", "DOWNLOADED", "COPIED_FROM_READONLY_PUBLIC_RAW_CACHE"), , drop = FALSE]
  if (!is.null(file_type)) x <- x[x$file_type == file_type, , drop = FALSE]
  x
}

read_xpt_upper <- function(path) {
  dat <- haven::read_xpt(path)
  names(dat) <- toupper(names(dat))
  as.data.frame(dat)
}

full_join_unique <- function(x, y) {
  if (is.null(x)) return(y)
  keep <- c("SEQN", setdiff(names(y), names(x)))
  keep <- intersect(keep, names(y))
  dplyr::full_join(x, y[, keep, drop = FALSE], by = "SEQN")
}

pick_first_existing <- function(data, candidates) {
  found <- intersect(candidates, names(data))
  if (!length(found)) return(rep(NA_real_, nrow(data)))
  data[[found[[1]]]]
}

clean_yes_no <- function(x) {
  dplyr::case_when(x == 1 ~ 1L, x == 2 ~ 0L, TRUE ~ NA_integer_)
}

natural_tooth_count <- function(data) {
  dent_cols <- names(data)[stringr::str_detect(names(data), "^OHX\\d{2}TC$")]
  if (!length(dent_cols)) return(rep(NA_integer_, nrow(data)))
  dent <- data |>
    dplyr::select(SEQN, dplyr::all_of(dent_cols)) |>
    tidyr::pivot_longer(-SEQN, names_to = "tooth_var", values_to = "tooth_code") |>
    dplyr::mutate(
      tooth_number = as.integer(stringr::str_extract(tooth_var, "\\d{2}")),
      natural_tooth = tooth_code %in% c(1, 2, 5)
    ) |>
    dplyr::filter(!tooth_number %in% c(1, 16, 17, 32)) |>
    dplyr::group_by(SEQN) |>
    dplyr::summarise(n_natural_teeth = sum(natural_tooth, na.rm = TRUE), .groups = "drop")
  dplyr::left_join(data |> dplyr::select(SEQN), dent, by = "SEQN")$n_natural_teeth
}

derive_periodontitis <- function(data) {
  data_with_teeth <- data |> dplyr::mutate(n_natural_teeth = natural_tooth_count(data))
  ppd_cols <- names(data_with_teeth)[stringr::str_detect(names(data_with_teeth), "^OHX\\d{2}PC[DSPA]$")]
  cal_cols <- names(data_with_teeth)[stringr::str_detect(names(data_with_teeth), "^OHX\\d{2}LA[DSPA]$")]
  if (!length(ppd_cols) || !length(cal_cols)) stop("Periodontal PPD/CAL columns missing.")
  ppd <- data_with_teeth |>
    dplyr::select(SEQN, dplyr::all_of(ppd_cols)) |>
    tidyr::pivot_longer(-SEQN, names_to = "var", values_to = "PPD") |>
    dplyr::mutate(
      tooth_number = as.integer(stringr::str_match(var, "^OHX(\\d{2})PC([DSPA])$")[, 2]),
      site = stringr::str_match(var, "^OHX(\\d{2})PC([DSPA])$")[, 3],
      PPD = dplyr::na_if(as.numeric(PPD), 99)
    ) |>
    dplyr::select(SEQN, tooth_number, site, PPD)
  cal <- data_with_teeth |>
    dplyr::select(SEQN, dplyr::all_of(cal_cols)) |>
    tidyr::pivot_longer(-SEQN, names_to = "var", values_to = "CAL") |>
    dplyr::mutate(
      tooth_number = as.integer(stringr::str_match(var, "^OHX(\\d{2})LA([DSPA])$")[, 2]),
      site = stringr::str_match(var, "^OHX(\\d{2})LA([DSPA])$")[, 3],
      CAL = dplyr::na_if(as.numeric(CAL), 99)
    ) |>
    dplyr::select(SEQN, tooth_number, site, CAL)
  site_long <- dplyr::full_join(ppd, cal, by = c("SEQN", "tooth_number", "site")) |>
    dplyr::filter(site %in% c("D", "M"))
  count_sites <- function(x) sum(x, na.rm = TRUE)
  count_teeth <- function(flag, tooth_number) dplyr::n_distinct(tooth_number[which(flag %in% TRUE)], na.rm = TRUE)
  perio <- site_long |>
    dplyr::group_by(SEQN) |>
    dplyr::summarise(
      perio_sites_observed = sum(!is.na(PPD) | !is.na(CAL)),
      cal6_teeth = count_teeth(CAL >= 6, tooth_number),
      cal4_teeth = count_teeth(CAL >= 4, tooth_number),
      cal3_sites = count_sites(CAL >= 3),
      ppd5_sites = count_sites(PPD >= 5),
      ppd5_teeth = count_teeth(PPD >= 5, tooth_number),
      ppd4_teeth = count_teeth(PPD >= 4, tooth_number),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      severe_periodontitis = cal6_teeth >= 2 & ppd5_sites >= 1,
      moderate_periodontitis = !severe_periodontitis & (cal4_teeth >= 2 | ppd5_teeth >= 2),
      mild_periodontitis = !severe_periodontitis & !moderate_periodontitis &
        ((cal3_sites >= 2 & ppd4_teeth >= 2) | ppd5_sites >= 1),
      periodontitis_4cat = dplyr::case_when(
        severe_periodontitis ~ "severe",
        moderate_periodontitis ~ "moderate",
        mild_periodontitis ~ "mild",
        TRUE ~ "none"
      ),
      periodontitis_binary = dplyr::case_when(
        periodontitis_4cat %in% c("moderate", "severe") ~ 1L,
        periodontitis_4cat %in% c("none", "mild") ~ 0L,
        TRUE ~ NA_integer_
      )
    )
  data_with_teeth |>
    dplyr::left_join(perio, by = "SEQN") |>
    dplyr::mutate(
      dentate = !is.na(n_natural_teeth) & n_natural_teeth >= 1,
      edentulous = !is.na(n_natural_teeth) & n_natural_teeth == 0,
      periodontal_exam_complete = OHDPDSTS == 1 & OHDEXCLU == 2,
      periodontitis_computable = dentate & periodontal_exam_complete & !is.na(periodontitis_4cat),
      periodontitis_4cat = factor(periodontitis_4cat, levels = c("none", "mild", "moderate", "severe"))
    )
}

derive_common_covariates <- function(data) {
  cot <- pick_first_existing(data, c("LBXCOT", "URXCOT", "LBXSCOT"))
  hba1c <- pick_first_existing(data, c("LBXGH", "LBXGHNA"))
  serum_creatinine <- pick_first_existing(data, c("LBXSCR"))
  sbp_vars <- intersect(c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4"), names(data))
  dbp_vars <- intersect(c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4"), names(data))
  cvd_vars <- intersect(c("MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", "MCQ160F"), names(data))
  data |>
    dplyr::mutate(
      age = as.numeric(RIDAGEYR),
      sex = factor(dplyr::case_when(RIAGENDR == 1 ~ "Male", RIAGENDR == 2 ~ "Female", TRUE ~ NA_character_)),
      race_ethnicity = factor(dplyr::case_when(
        RIDRETH1 == 1 ~ "Mexican American",
        RIDRETH1 == 2 ~ "Other Hispanic",
        RIDRETH1 == 3 ~ "Non-Hispanic White",
        RIDRETH1 == 4 ~ "Non-Hispanic Black",
        RIDRETH1 == 5 ~ "Other race/multiracial",
        TRUE ~ NA_character_
      )),
      education = factor(dplyr::case_when(
        DMDEDUC2 %in% c(1, 2) ~ "<High school",
        DMDEDUC2 == 3 ~ "High school/GED",
        DMDEDUC2 == 4 ~ "Some college",
        DMDEDUC2 == 5 ~ "College graduate",
        TRUE ~ NA_character_
      ), levels = c("<High school", "High school/GED", "Some college", "College graduate")),
      PIR = dplyr::na_if(as.numeric(INDFMPIR), 999),
      BMI = as.numeric(BMXBMI),
      cotinine = as.numeric(cot),
      log_cotinine = log1p(cotinine),
      smoking_status = factor(dplyr::case_when(
        SMQ020 == 2 ~ "Never",
        SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ "Current",
        SMQ020 == 1 & SMQ040 == 3 ~ "Former",
        SMQ020 == 1 & is.na(SMQ040) ~ "Former",
        TRUE ~ NA_character_
      ), levels = c("Never", "Former", "Current")),
      HbA1c = as.numeric(hba1c),
      diabetes = dplyr::case_when(
        DIQ010 == 1 ~ 1L,
        !is.na(HbA1c) & HbA1c >= 6.5 ~ 1L,
        DIQ010 == 2 & (is.na(HbA1c) | HbA1c < 6.5) ~ 0L,
        TRUE ~ NA_integer_
      ),
      serum_creatinine = as.numeric(serum_creatinine),
      mean_sbp = if (length(sbp_vars)) rowMeans(dplyr::pick(dplyr::all_of(sbp_vars)), na.rm = TRUE) else NA_real_,
      mean_dbp = if (length(dbp_vars)) rowMeans(dplyr::pick(dplyr::all_of(dbp_vars)), na.rm = TRUE) else NA_real_,
      mean_sbp = dplyr::na_if(mean_sbp, NaN),
      mean_dbp = dplyr::na_if(mean_dbp, NaN),
      hypertension = dplyr::case_when(
        BPQ020 == 1 ~ 1L,
        !is.na(mean_sbp) & mean_sbp >= 130 ~ 1L,
        !is.na(mean_dbp) & mean_dbp >= 80 ~ 1L,
        BPQ020 == 2 & (is.na(mean_sbp) | mean_sbp < 130) & (is.na(mean_dbp) | mean_dbp < 80) ~ 0L,
        TRUE ~ NA_integer_
      ),
      CVD = dplyr::if_else(
        rowSums(dplyr::pick(dplyr::all_of(cvd_vars)) == 1, na.rm = TRUE) > 0,
        1L,
        dplyr::if_else(rowSums(!is.na(dplyr::pick(dplyr::all_of(cvd_vars)))) > 0, 0L, NA_integer_)
      ),
      gout = clean_yes_no(pick_first_existing(data, c("MCQ160N", "MCQ195")))
    )
}

ckd_epi_2021_egfr <- function(scr, age, sex) {
  female <- as.character(sex) == "Female"
  kappa <- ifelse(female, 0.7, 0.9)
  alpha <- ifelse(female, -0.241, -0.302)
  142 * pmin(scr / kappa, 1)^alpha * pmax(scr / kappa, 1)^(-1.200) * (0.9938^age) * ifelse(female, 1.012, 1)
}

derive_kidney_and_urate <- function(data) {
  uacr <- pick_first_existing(data, c("URDACT", "URXUACR"))
  urine_albumin <- pick_first_existing(data, c("URXUMA"))
  urine_creatinine <- pick_first_existing(data, c("URXUCR"))
  uacr_calc <- dplyr::if_else(!is.na(uacr), as.numeric(uacr), as.numeric(urine_albumin) / as.numeric(urine_creatinine) * 100)
  data |>
    dplyr::mutate(
      uric_acid_mg_dl = as.numeric(LBXSUA),
      hyperuricemia = dplyr::case_when(
        sex == "Male" & uric_acid_mg_dl > 7.0 ~ 1L,
        sex == "Female" & uric_acid_mg_dl > 6.0 ~ 1L,
        sex %in% c("Male", "Female") & !is.na(uric_acid_mg_dl) ~ 0L,
        TRUE ~ NA_integer_
      ),
      egfr = ckd_epi_2021_egfr(serum_creatinine, age, sex),
      UACR = uacr_calc,
      albuminuria = dplyr::case_when(
        !is.na(UACR) & UACR >= 30 ~ 1L,
        !is.na(UACR) & UACR < 30 ~ 0L,
        TRUE ~ NA_integer_
      ),
      CKD = dplyr::case_when(
        !is.na(egfr) & egfr < 60 ~ 1L,
        !is.na(albuminuria) & albuminuria == 1 ~ 1L,
        !is.na(egfr) & egfr >= 60 & !is.na(albuminuria) & albuminuria == 0 ~ 0L,
        !is.na(egfr) & egfr >= 60 & is.na(albuminuria) ~ 0L,
        TRUE ~ NA_integer_
      )
    )
}

derive_mortality_outcomes <- function(data) {
  data |>
    dplyr::mutate(
      mortality_eligible = ELIGSTAT == 1,
      death_allcause = MORTSTAT == 1,
      death_event = death_allcause,
      followup_years = PERMTH_EXM / 12,
      time_years = followup_years
    )
}

add_joint_exposure <- function(data) {
  data |>
    dplyr::mutate(
      periodontitis_binary_factor = factor(periodontitis_binary, levels = c(0, 1), labels = c("none/mild", "moderate/severe")),
      hyperuricemia_factor = factor(hyperuricemia, levels = c(0, 1), labels = c("normouricemia", "hyperuricemia")),
      joint_urate_periodontitis = factor(
        paste(hyperuricemia_factor, periodontitis_binary_factor, sep = " + "),
        levels = c("normouricemia + none/mild", "normouricemia + moderate/severe",
                   "hyperuricemia + none/mild", "hyperuricemia + moderate/severe")
      )
    )
}

rx_flag_table <- function(rx) {
  if (!nrow(rx)) return(tibble::tibble(SEQN = integer(), urate_lowering_therapy = integer(), diuretic_use = integer()))
  drug <- toupper(as.character(rx$RXDDRUG %||% ""))
  ult_regex <- paste(c("ALLOPURINOL", "FEBUXOSTAT", "PROBENECID", "PEGLOTICASE",
                       "LESINURAD", "RASBURICASE", "SULFINPYRAZONE"), collapse = "|")
  diuretic_regex <- paste(c("HYDROCHLOROTHIAZIDE", "CHLORTHALIDONE", "CHLOROTHIAZIDE",
                            "METHYCLOTHIAZIDE", "INDAPAMIDE", "METOLAZONE", "FUROSEMIDE",
                            "BUMETANIDE", "TORSEMIDE", "ETHACRYNIC", "SPIRONOLACTONE",
                            "EPLERENONE", "TRIAMTERENE", "AMILORIDE", "ACETAZOLAMIDE"), collapse = "|")
  rx |>
    dplyr::mutate(
      drug_upper = drug,
      urate_lowering_therapy = as.integer(stringr::str_detect(drug_upper, ult_regex)),
      diuretic_use = as.integer(stringr::str_detect(drug_upper, diuretic_regex))
    ) |>
    dplyr::group_by(SEQN) |>
    dplyr::summarise(
      urate_lowering_therapy = as.integer(any(urate_lowering_therapy == 1, na.rm = TRUE)),
      diuretic_use = as.integer(any(diuretic_use == 1, na.rm = TRUE)),
      .groups = "drop"
    )
}

model_terms <- list(
  `Model 1` = c("joint_urate_periodontitis", "age", "sex", "race_ethnicity"),
  `Model 2` = c("joint_urate_periodontitis", "age", "sex", "race_ethnicity", "education", "PIR", "BMI", "smoking_status", "log_cotinine"),
  `Model 3` = c("joint_urate_periodontitis", "age", "sex", "race_ethnicity", "education", "PIR", "BMI", "smoking_status", "log_cotinine", "diabetes", "hypertension", "CVD"),
  `Model 4` = c("joint_urate_periodontitis", "age", "sex", "race_ethnicity", "education", "PIR", "BMI", "smoking_status", "log_cotinine", "diabetes", "hypertension", "CVD", "egfr", "UACR", "CKD"),
  `Model 5` = c("joint_urate_periodontitis", "age", "sex", "race_ethnicity", "education", "PIR", "BMI", "smoking_status", "log_cotinine", "diabetes", "hypertension", "CVD", "egfr", "UACR", "CKD", "gout", "urate_lowering_therapy", "diuretic_use")
)

joint_levels <- c("normouricemia + none/mild", "normouricemia + moderate/severe",
                  "hyperuricemia + none/mild", "hyperuricemia + moderate/severe")
joint_labels <- c("Normouricemia + none/mild periodontitis",
                  "Normouricemia + moderate/severe periodontitis",
                  "Hyperuricemia + none/mild periodontitis",
                  "Hyperuricemia + moderate/severe periodontitis")
names(joint_labels) <- joint_levels

make_design <- function(dat) {
  dat$joint_urate_periodontitis <- stats::relevel(factor(dat$joint_urate_periodontitis, levels = joint_levels), ref = "normouricemia + none/mild")
  survey::svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC6YR, nest = TRUE, data = dat)
}

extract_joint_terms <- function(fit, model, dat) {
  tidy <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  observed <- tidy |>
    dplyr::filter(stringr::str_detect(term, "^joint_urate_periodontitis")) |>
    dplyr::mutate(
      exposure_raw = stringr::str_remove(term, "^joint_urate_periodontitis"),
      exposure_group = unname(joint_labels[exposure_raw]),
      model = model,
      N = nrow(dat),
      deaths = sum(dat$death_allcause, na.rm = TRUE),
      HR = estimate,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value
    ) |>
    dplyr::select(model, exposure_group, N, deaths, HR, conf_low, conf_high, p_value)
  ref <- tibble::tibble(
    model = model,
    exposure_group = joint_labels[["normouricemia + none/mild"]],
    N = nrow(dat),
    deaths = sum(dat$death_allcause, na.rm = TRUE),
    HR = 1,
    conf_low = NA_real_,
    conf_high = NA_real_,
    p_value = NA_real_
  )
  dplyr::bind_rows(ref, observed)
}

fit_svy_model <- function(dat, terms, model_label = "model") {
  use_vars <- unique(c("followup_years", "death_allcause", "WTMEC6YR", "SDMVSTRA", "SDMVPSU", terms))
  cc <- dat[stats::complete.cases(dat[, use_vars, drop = FALSE]), , drop = FALSE]
  cc <- droplevels(cc)
  des <- make_design(cc)
  form <- stats::as.formula(paste("survival::Surv(followup_years, death_allcause) ~", paste(terms, collapse = " + ")))
  fit <- survey::svycoxph(form, design = des)
  list(fit = fit, data = cc, terms = terms, formula = form, model_label = model_label)
}

extract_core_comparison <- function(fit_obj, analysis, direction_ref = 1) {
  out <- extract_joint_terms(fit_obj$fit, analysis, fit_obj$data)
  term <- out[out$exposure_group == joint_labels[["hyperuricemia + moderate/severe"]], , drop = FALSE]
  if (!nrow(term)) {
    return(tibble::tibble(sensitivity = analysis, N = nrow(fit_obj$data), deaths = sum(fit_obj$data$death_allcause, na.rm = TRUE),
                          HR = NA_real_, conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
                          direction_consistent = "not estimable", interpretation_flag = "not estimable"))
  }
  term |>
    dplyr::transmute(sensitivity = analysis, N, deaths, HR, conf_low, conf_high, p_value,
                     direction_consistent = ifelse(!is.na(HR) & HR > direction_ref, "yes", "no"),
                     interpretation_flag = ifelse(is.na(p_value), "not estimable", ifelse(p_value < 0.05, "nominally significant", "not statistically significant")))
}

write_standard_readme_status <- function(status = "IN_PROGRESS", detail = "v20 public raw-only primary reanalysis is running.") {
  safe_write_lines(c(
    "# v20 public raw-to-final primary reanalysis",
    "",
    "This directory is the new public raw-only primary reanalysis workspace.",
    "",
    "Primary rule:",
    "- The previous N=9018 private-derived-cohort result is no longer the primary analysis.",
    "- The new primary analysis uses a public raw-only reconstructed NHANES 2009-2014 cohort linked to public-use NCHS/CDC Linked Mortality Files.",
    "- Old frozen tables, figures, source data, and manuscript text must not be carried forward unless regenerated under v20.",
    "- `fallback_nested_audit_dataset.rds` and any participant-level private derived analytic cohort are not valid v20 primary inputs.",
    "- Participant-level rows are kept only under `data_private_DO_NOT_UPLOAD/` and are excluded from the public repository candidate."
  ), v20_path("README_v20.md"))
  safe_write_lines(c(
    "# STATUS v20",
    "",
    paste("Final status:", status),
    "",
    "The previous N=9018 private-derived-cohort result is no longer the primary analysis.",
    "The new primary analysis uses the public raw-only reconstructed cohort.",
    "No old frozen result may be carried forward unless regenerated under v20.",
    "",
    paste("Current detail:", detail)
  ), v20_path("STATUS_v20.md"))
}

manifest_for_dir <- function(root, include_dirs = FALSE) {
  files <- list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE, include.dirs = include_dirs)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) return(data.frame(relative_path = character(), size_bytes = numeric(), sha256 = character(), stringsAsFactors = FALSE))
  data.frame(
    relative_path = sub(paste0("^", gsub("([\\W])", "\\\\\\1", normalizePath(root, winslash = "/", mustWork = TRUE)), "/?"), "", normalizePath(files, winslash = "/", mustWork = FALSE)),
    size_bytes = as.numeric(file.info(files)$size),
    sha256 = vapply(files, sha256_file, character(1)),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(relative_path)
}

write_all_manifests <- function() {
  all_manifest <- manifest_for_dir(V20_ROOT)
  safe_write_csv(all_manifest, v20_path("manifest", "v20_all_files_manifest_sha256.csv"))
  repo_root <- v20_path("public_repository_candidate")
  repo_manifest <- manifest_for_dir(repo_root)
  safe_write_csv(repo_manifest, v20_path("manifest", "v20_public_repository_manifest_sha256.csv"))
  private_manifest <- manifest_for_dir(PRIVATE_DIR)
  safe_write_csv(private_manifest, v20_path("manifest", "v20_private_DO_NOT_UPLOAD_manifest_sha256.csv"))
  invisible(list(all = all_manifest, repo = repo_manifest, private = private_manifest))
}

write_standard_readme_status()
log_action("00 setup and safety check loaded; writes are guarded inside v20.")
