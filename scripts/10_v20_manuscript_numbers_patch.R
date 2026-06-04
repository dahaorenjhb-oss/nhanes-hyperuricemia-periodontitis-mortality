source(file.path("scripts", "00_v20_setup_safety_check.R"))

primary <- utils::read.csv(v20_path("validation", "primary_model_summary_v20.csv"), stringsAsFactors = FALSE)
pv <- setNames(primary$value, primary$metric)
N <- as.integer(pv[["N"]])
deaths <- as.integer(pv[["deaths"]])
hr <- as.numeric(pv[["HR"]])
lo <- as.numeric(pv[["CI_low"]])
hi <- as.numeric(pv[["CI_high"]])
pval <- as.numeric(pv[["P_value"]])
flow <- utils::read.csv(v20_path("source_data", "supplementary", "public_raw_reconstruction_flow_v20.csv"), stringsAsFactors = FALSE)
table2 <- utils::read.csv(v20_path("source_data", "tables", "table2_source_data_v20.csv"), stringsAsFactors = FALSE)
table3 <- utils::read.csv(v20_path("source_data", "tables", "table3_source_data_v20.csv"), stringsAsFactors = FALSE)
ph <- utils::read.csv(v20_path("validation", "ph_diagnostics_v20.csv"), stringsAsFactors = FALSE)
ph_pass <- all(ph$p_value >= 0.05, na.rm = TRUE)

candidate_n <- flow$n_after[flow$step_number == 8]
core_n <- flow$n_after[flow$step_number == 9]
model4_n <- flow$n_after[flow$step_number == 10]
smoking <- utils::read.csv(v20_path("source_data", "supplementary", "smoking_status_reconstruction_summary_v20.csv"), stringsAsFactors = FALSE)
smoking_h_missing <- smoking$smoking_status_missing[smoking$cycle == "2013-2014"]

abstract <- c(
  "# Abstract numbers patch v20",
  "",
  paste0("We analyzed NHANES 2009-2014 adults aged 30 years or older using a public raw-only reconstruction linked to the public-use 2019 NCHS/CDC Linked Mortality Files. The final fully adjusted complete-case public raw-only analytic sample included ", N, " participants and ", deaths, " all-cause deaths. Hyperuricemia was defined using sex-specific serum uric acid thresholds, and periodontitis was grouped as none/mild or moderate/severe using CDC/AAP surveillance definitions. Compared with normouricemia plus none/mild periodontitis, hyperuricemia plus moderate/severe periodontitis had a survey-weighted Cox hazard ratio of ", fmt_num(hr, 2), " (95% CI, ", fmt_num(lo, 2), " to ", fmt_num(hi, 2), "; P=", fmt_p(pval), ") in the fully adjusted public raw-only primary model. The findings should be interpreted as observational risk-stratification evidence, not causal evidence.")
)
safe_write_lines(abstract, v20_path("manuscript_text", "abstract_numbers_patch_v20.md"))

methods <- c(
  "# Methods patch v20",
  "",
  "The primary analysis was rebuilt from public-use NHANES 2009-2010, 2011-2012, and 2013-2014 raw files and the public-use 2019 NCHS/CDC Linked Mortality Files. Previously recovered private derived participant-level data were not used as primary inputs.",
  "",
  paste0("The cohort flow was: ", flow$n_after[1], " merged public raw records; ", flow$n_after[2], " aged 30 years or older; ", flow$n_after[3], " mortality-linkage eligible; ", flow$n_after[4], " dentate adults; ", flow$n_after[5], " with complete periodontal examination status; ", flow$n_after[7], " with serum uric acid available; ", candidate_n, " with mortality status and follow-up time; ", core_n, " with complete core covariates; and ", model4_n, " in the final fully adjusted complete-case analytic sample."),
  "",
  "Survey analyses used MEC examination weights divided by three (WTMEC6YR = WTMEC2YR / 3), strata SDMVSTRA, and primary sampling units SDMVPSU with nesting enabled.",
  "",
  "Cox proportional hazards models used the same prespecified adjustment structure as the prior manuscript: Model 1 adjusted for age, sex, and race/ethnicity; Model 2 additionally adjusted for education, poverty-income ratio, body mass index, smoking status, and log serum cotinine; Model 3 additionally adjusted for diabetes, hypertension, and cardiovascular disease; Model 4 additionally adjusted for estimated glomerular filtration rate, urine albumin-to-creatinine ratio, and chronic kidney disease. Model 4 is the v20 primary model."
)
safe_write_lines(methods, v20_path("manuscript_text", "methods_patch_v20.md"))

model1_core <- table2 |> dplyr::filter(model == "Model 1", exposure_group == joint_labels[["hyperuricemia + moderate/severe"]]) |> dplyr::slice(1)
model4_single1 <- table2 |> dplyr::filter(model == "Model 4", exposure_group == joint_labels[["normouricemia + moderate/severe"]]) |> dplyr::slice(1)
model4_single2 <- table2 |> dplyr::filter(model == "Model 4", exposure_group == joint_labels[["hyperuricemia + none/mild"]]) |> dplyr::slice(1)
results <- c(
  "# Results patch v20",
  "",
  paste0("Figure 1 shows the v20 public raw-only study flow. Among ", candidate_n, " adults eligible after periodontal, serum uric acid, and mortality-follow-up criteria, ", N, " participants formed the final fully adjusted complete-case analytic sample, contributing ", deaths, " all-cause deaths."),
  "",
  paste0("The 2013-2014 public raw smoking variable was the main source of divergence from the previous private-derived cohort: public-rule smoking_status was missing for ", smoking_h_missing, " 2013-2014 candidate records."),
  "",
  paste0("In Model 1, hyperuricemia plus moderate/severe periodontitis had an HR of ", fmt_num(model1_core$HR, 2), " (95% CI, ", fmt_num(model1_core$conf_low, 2), " to ", fmt_num(model1_core$conf_high, 2), "). After full adjustment in Model 4, the HR was ", fmt_num(hr, 2), " (95% CI, ", fmt_num(lo, 2), " to ", fmt_num(hi, 2), "; P=", fmt_p(pval), ")."),
  "",
  paste0("In Model 4, normouricemia plus moderate/severe periodontitis had an HR of ", fmt_num(model4_single1$HR, 2), " (95% CI, ", fmt_num(model4_single1$conf_low, 2), " to ", fmt_num(model4_single1$conf_high, 2), "), and hyperuricemia plus none/mild periodontitis had an HR of ", fmt_num(model4_single2$HR, 2), " (95% CI, ", fmt_num(model4_single2$conf_low, 2), " to ", fmt_num(model4_single2$conf_high, 2), ")."),
  "",
  "Sensitivity analyses are regenerated in Table 3 and Figure 3. These analyses should be described according to the v20 Table 3 values, not the old frozen values."
)
safe_write_lines(results, v20_path("manuscript_text", "results_patch_v20.md"))

discussion <- c(
  "# Discussion and limitations patch v20",
  "",
  paste0("The primary conclusion should now be tied to the public raw-only estimate: HR ", fmt_num(hr, 2), " (95% CI, ", fmt_num(lo, 2), " to ", fmt_num(hi, 2), ") in ", N, " participants with ", deaths, " deaths."),
  "",
  "The previous private-derived N=9018 result should not be described as the primary analysis. It may be mentioned only as historical/private validation context, with no participant-level data released.",
  "",
  "A key limitation is that the 2013-2014 public SMQ_H file lacks respondent records for a set of otherwise eligible candidate participants, producing public raw-only smoking_status missingness and a smaller final complete-case sample than the private-derived historical cohort.",
  "",
  "The manuscript should avoid causal language. The results support observational risk stratification only."
)
safe_write_lines(discussion, v20_path("manuscript_text", "discussion_limitations_patch_v20.md"))

data_avail <- c(
  "# Data Availability statement v20",
  "",
  "Data Availability",
  "",
  "The primary analysis can be reproduced from public-use NHANES 2009-2010, 2011-2012, and 2013-2014 raw data files and the public-use 2019 NCHS/CDC Linked Mortality Files. The v20 repository candidate contains download scripts, analysis scripts, aggregate source data for tables and figures, validation outputs, and file manifests. Complete raw NHANES files, Linked Mortality File copies, and participant-level reconstructed analytic datasets are not included in the repository candidate. Users should obtain the public-use raw files from NCHS/CDC using the provided download scripts and comply with the applicable NCHS data-use terms.",
  "",
  "Data Availability category: PUBLIC_RAW_TO_FINAL_REPRODUCTION_SUPPORTED_REPOSITORY_CANDIDATE_PENDING_DEPOSITION.",
  "",
  "No participant-level private derived cohort, no `fallback_nested_audit_dataset.rds`, and no restricted or private data are approved for public release."
)
safe_write_lines(data_avail, v20_path("manuscript_text", "data_availability_statement_v20.md"))

code_avail <- c(
  "# Code Availability statement v20",
  "",
  "Code Availability",
  "",
  "The v20 public repository candidate contains R scripts to download public-use files, reconstruct variables, define the public raw-only analytic cohort, fit the prespecified survey-weighted Cox models, run proportional hazards diagnostics, regenerate tables and figures, and validate hashes/manifests. The scripts are intended for author review before repository deposition and manuscript submission.",
  "",
  "The repository candidate intentionally excludes participant-level reconstructed datasets and private historical comparison files."
)
safe_write_lines(code_avail, v20_path("manuscript_text", "code_availability_statement_v20.md"))

title_claims <- c(
  "# Title and claims check v20",
  "",
  "Recommended title boundary: Joint hyperuricemia and periodontitis phenotype and all-cause mortality in a public raw-only NHANES linked mortality analysis.",
  "",
  "Allowed claims:",
  "- Public raw-only NHANES/NCHS linked mortality reanalysis.",
  "- Observational association/risk stratification.",
  "- Transparent public raw-to-final reproducibility after repository deposition.",
  "",
  "Removed or prohibited primary claims:",
  "- The old N=9018/HR=1.95 result as the primary result.",
  "- Causal effect confirmed.",
  "- Participant-level data publicly released.",
  "- GEO-supported claim.",
  "- Fine-Gray or competing-risk model unless separately authorized."
)
safe_write_lines(title_claims, v20_path("manuscript_text", "title_and_claims_check_v20.md"))

safe_write_lines(c(
  "# v20 public raw vs old private cohort comparison",
  "",
  "This internal report is aggregate-only and is not a public participant-level comparison.",
  "",
  "Old historical/private-derived cohort:",
  "- N=9018.",
  "- Deaths=743.",
  "- HR=1.95.",
  "- 95% CI=1.43-2.66.",
  "",
  "New v20 public raw-only cohort:",
  paste0("- N=", N, "."),
  paste0("- Deaths=", deaths, "."),
  paste0("- HR=", fmt_num(hr, 2), "."),
  paste0("- 95% CI=", fmt_num(lo, 2), "-", fmt_num(hi, 2), "."),
  "",
  "Main reason for difference:",
  "- 2013-2014 public SMQ_H lacks 931 candidate respondent records.",
  "- Public raw smoking_status/complete-case state cannot reproduce the old derived cohort.",
  "- The new main analysis prioritizes public raw-to-final reproducibility.",
  "",
  "Private participant-level rows are not included and must not be placed in the public repository."
), v20_path("reports", "v20_public_raw_vs_old_private_cohort_comparison.md"))

safe_write_lines(c(
  "# v20 data/code availability recommendation",
  "",
  "Recommended category: PUBLIC_RAW_TO_FINAL_REPRODUCTION_SUPPORTED_REPOSITORY_CANDIDATE_PENDING_DEPOSITION.",
  "",
  "Use the v20 Data Availability statement after author review and repository deposition details are confirmed."
), v20_path("reports", "v20_data_code_availability_recommendation.md"))

log_action("10 manuscript number patches and internal old-vs-new comparison written.")
