options(timeout = max(900, getOption('timeout')))
reg <- read.csv(file.path('config', 'public_raw_file_registry_v20.csv'), stringsAsFactors = FALSE)
cache_root <- file.path('data_private_DO_NOT_UPLOAD', 'public_raw_cache')
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
for (i in seq_len(nrow(reg))) {
  dest <- file.path(cache_root, reg$expected_cache_subdir[i], reg$file_label[i])
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(dest) || file.info(dest)$size == 0) download.file(reg$source_url[i], dest, mode = 'wb')
  message(reg$file_label[i], ' -> ', dest)
}
