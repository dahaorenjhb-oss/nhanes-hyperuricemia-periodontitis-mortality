source(file.path("scripts", "00_v20_setup_safety_check.R"))

ensure_dir(v20_path("config"))
v17_manifest_path <- file.path(V17_ROOT, "raw_data", "public_data_manifest.csv")
repo_registry_path <- v20_path("config", "public_raw_file_registry_v20.csv")
if (file.exists(v17_manifest_path)) {
  manifest <- utils::read.csv(v17_manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- manifest |>
    dplyr::filter(public_use_or_restricted == "public_use",
                  required_for_reproduction == "yes",
                  file_type %in% c("XPT", "DAT")) |>
    dplyr::mutate(
      file_label = file_name,
      cycle_suffix = vapply(file_name, cycle_suffix_from_file, character(1)),
      source_url = expected_public_url_or_source,
      local_path = file.path(RAW_CACHE_DIR, ifelse(file_type == "DAT", "linked_mortality", cycle), file_name),
      download_status = "NOT_CHECKED",
      size_bytes = NA_real_,
      sha256 = NA_character_,
      error_message = NA_character_,
      copied_from_readonly = NA_character_
    )
} else if (file.exists(repo_registry_path)) {
  public_reg <- utils::read.csv(repo_registry_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- public_reg |>
    dplyr::mutate(
      data_source_name = data_domain,
      cycle = cycle_or_year,
      file_name = file_label,
      local_path_if_found = NA_character_,
      role_in_analysis = required_for,
      local_path = file.path(RAW_CACHE_DIR, expected_cache_subdir, file_label),
      download_status = "NOT_CHECKED",
      size_bytes = NA_real_,
      sha256 = NA_character_,
      error_message = NA_character_,
      copied_from_readonly = NA_character_
    )
} else {
  stop("No v17 public data manifest or repository public raw registry found.")
}

v18b_registry_path <- file.path(V18B_ROOT, "config", "raw_file_registry_v18b.csv")
v18b_registry <- if (file.exists(v18b_registry_path)) {
  utils::read.csv(v18b_registry_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  data.frame()
}

readonly_candidates <- function(file_label, manifest_path = NA_character_) {
  cand <- character()
  if (nrow(v18b_registry)) cand <- c(cand, v18b_registry$local_path[v18b_registry$file_label == file_label])
  if (!is.na(manifest_path) && nzchar(manifest_path)) {
    cand <- c(cand, gsub("<PROJECT_ROOT>", PROJECT_ROOT, manifest_path, fixed = TRUE))
  }
  cand[file.exists(cand)]
}

for (i in seq_len(nrow(required))) {
  dest <- required$local_path[i]
  assert_inside_v20(dest)
  ensure_dir(dirname(dest))
  if (!file.exists(dest) || file.info(dest)$size == 0) {
    cand <- readonly_candidates(required$file_label[i], required$local_path_if_found[i])
    if (length(cand)) {
      file.copy(cand[[1]], dest, overwrite = TRUE)
      required$download_status[i] <- "COPIED_FROM_READONLY_PUBLIC_RAW_CACHE"
      required$copied_from_readonly[i] <- scrub_path(cand[[1]])
    }
  }
  if (!file.exists(dest) || file.info(dest)$size == 0) {
    msg <- NA_character_
    status <- tryCatch({
      utils::download.file(required$source_url[i], dest, mode = "wb", quiet = TRUE)
      "DOWNLOADED"
    }, error = function(e) {
      msg <<- conditionMessage(e)
      if (file.exists(dest)) unlink(dest)
      "RAW_DOWNLOAD_BLOCKED"
    })
    required$download_status[i] <- status
    required$error_message[i] <- msg
  }
  if (file.exists(dest)) {
    required$size_bytes[i] <- file.info(dest)$size
    required$sha256[i] <- sha256_file(dest)
    if (!required$download_status[i] %in% c("COPIED_FROM_READONLY_PUBLIC_RAW_CACHE", "DOWNLOADED")) required$download_status[i] <- "AVAILABLE"
  }
  log_action(paste("Public raw file checked:", required$file_label[i], required$download_status[i]))
}

registry_out <- required |>
  dplyr::select(file_label, data_domain = data_source_name, cycle_or_year = cycle, cycle_suffix, module,
                file_type, source_url, local_path, download_status, size_bytes, sha256, error_message,
                copied_from_readonly, required_for = role_in_analysis)
safe_write_csv(registry_out, v20_path("config", "raw_file_registry_v20.csv"))

blocked <- registry_out |> dplyr::filter(!download_status %in% c("COPIED_FROM_READONLY_PUBLIC_RAW_CACHE", "DOWNLOADED", "AVAILABLE"))
safe_write_csv(registry_out |>
  dplyr::mutate(local_available = file.exists(local_path),
                special_check = ifelse(file_label %in% c("GHB_F.XPT", "DEMO_H.XPT"), "SPECIFICALLY_VERIFIED", ""),
                notes = ifelse(local_available, "public raw file available in v20 private cache", "missing")),
  v20_path("source_data", "supplementary", "public_raw_file_completeness_check_v20.csv"))

if (nrow(blocked)) {
  safe_write_lines(c(
    "# v20 public raw import report",
    "",
    "Status: BLOCKED",
    "",
    paste("Unavailable files:", nrow(blocked)),
    paste0("- ", blocked$file_label, ": ", blocked$error_message)
  ), v20_path("reports", "v20_public_raw_cohort_report.md"))
  stop("Public raw file download/cache incomplete.")
}

parse_lmf <- function(path, suffix) {
  dat <- utils::read.fwf(
    path,
    widths = c(6, 8, 1, 1, 3, 1, 1, 21, 3, 3),
    col.names = c("SEQN", "skip1", "ELIGSTAT", "MORTSTAT", "UCOD_LEADING", "DIABETES_MORT",
                  "HYPERTENSION_MORT", "skip2", "PERMTH_INT", "PERMTH_EXM"),
    stringsAsFactors = FALSE
  )
  dat <- dat[, c("SEQN", "ELIGSTAT", "MORTSTAT", "UCOD_LEADING", "DIABETES_MORT", "HYPERTENSION_MORT", "PERMTH_INT", "PERMTH_EXM")]
  for (nm in names(dat)) dat[[nm]] <- suppressWarnings(as.integer(dat[[nm]]))
  dat$NHANES_CYCLE_SUFFIX <- suffix
  dat$NHANES_CYCLE <- cycle_label_from_suffix(suffix)
  dat
}

xpt_reg <- available_registry("XPT")
availability <- data.frame()
for (i in seq_len(nrow(xpt_reg))) {
  dat <- read_xpt_upper(xpt_reg$local_path[i])
  suffix <- xpt_reg$cycle_suffix[i]
  dat$NHANES_CYCLE_SUFFIX <- suffix
  dat$NHANES_CYCLE <- cycle_label_from_suffix(suffix)
  safe_save_rds(dat, file.path(IMPORTED_RAW_DIR, paste0(xpt_reg$file_label[i], ".rds")))
  availability <- dplyr::bind_rows(
    availability,
    data.frame(cycle_or_year = xpt_reg$cycle_or_year[i], file_label = xpt_reg$file_label[i],
               module = xpt_reg$module[i], variable_name = setdiff(names(dat), "SEQN"),
               available = TRUE, stringsAsFactors = FALSE)
  )
}
safe_write_csv(availability, v20_path("source_data", "supplementary", "variable_availability_matrix_v20.csv"))

dat_reg <- available_registry("DAT")
lmf_summary <- data.frame()
for (i in seq_len(nrow(dat_reg))) {
  dat <- parse_lmf(dat_reg$local_path[i], dat_reg$cycle_suffix[i])
  safe_save_rds(dat, file.path(IMPORTED_RAW_DIR, paste0(dat_reg$file_label[i], ".rds")))
  lmf_summary <- dplyr::bind_rows(lmf_summary, data.frame(
    cycle_or_year = dat_reg$cycle_or_year[i],
    records = nrow(dat),
    linkage_eligible = sum(dat$ELIGSTAT == 1, na.rm = TRUE),
    deaths = sum(dat$MORTSTAT == 1, na.rm = TRUE),
    followup_nonmissing = sum(!is.na(dat$PERMTH_EXM)),
    stringsAsFactors = FALSE
  ))
}
safe_write_csv(lmf_summary, v20_path("source_data", "supplementary", "mortality_linkage_summary_aggregate_v20.csv"))

one_to_many <- xpt_reg[grepl("^RXQ_RX_", xpt_reg$file_label), , drop = FALSE]
participant_xpt <- xpt_reg[!grepl("^RXQ_RX_", xpt_reg$file_label), , drop = FALSE]
merged_cycles <- list()
cycle_counts <- data.frame()
for (suffix in c("F", "G", "H")) {
  rows <- participant_xpt[participant_xpt$cycle_suffix == suffix, , drop = FALSE]
  parts <- list()
  for (i in seq_len(nrow(rows))) {
    parts[[rows$file_label[i]]] <- readRDS(file.path(IMPORTED_RAW_DIR, paste0(rows$file_label[i], ".rds")))
  }
  merged <- Reduce(full_join_unique, parts)
  lmf_row <- dat_reg[dat_reg$cycle_suffix == suffix, , drop = FALSE]
  if (nrow(lmf_row)) {
    lmf <- readRDS(file.path(IMPORTED_RAW_DIR, paste0(lmf_row$file_label[1], ".rds")))
    merged <- dplyr::left_join(merged, lmf[, setdiff(names(lmf), c("NHANES_CYCLE", "NHANES_CYCLE_SUFFIX")), drop = FALSE], by = "SEQN")
  }
  safe_save_rds(merged, file.path(RECON_DIR, paste0("merged_cycle_", suffix, ".rds")))
  merged_cycles[[suffix]] <- merged
  cycle_counts <- dplyr::bind_rows(cycle_counts, data.frame(
    cycle_or_year = cycle_label_from_suffix(suffix),
    merged_records = nrow(merged),
    lmf_linked_records = sum(!is.na(merged$ELIGSTAT)),
    stringsAsFactors = FALSE
  ))
}
all_merged <- dplyr::bind_rows(merged_cycles)
safe_save_rds(all_merged, file.path(RECON_DIR, "merged_nhanes_lmf_2009_2014_public_raw.rds"))
safe_write_csv(cycle_counts, v20_path("source_data", "supplementary", "cycle_level_counts_v20.csv"))
safe_write_csv(one_to_many |>
  dplyr::select(file_label, cycle_or_year, module, file_type, download_status) |>
  dplyr::mutate(merge_decision = "aggregated_to_participant_medication_flags_before_analysis",
                reason = "RXQ_RX is one-to-many and is not directly joined to the participant base"),
  v20_path("source_data", "supplementary", "one_to_many_module_merge_decisions_v20.csv"))

log_action("01 public raw files cached/imported and participant-cycle files merged.")
