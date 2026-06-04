# v20 public raw cohort report

Primary input boundary: public-use NHANES raw files and public-use NCHS/CDC Linked Mortality Files only.
The private recovered derived cohort and `fallback_nested_audit_dataset.rds` were not used as v20 primary inputs.

Initial public raw denominator: 30468
Mortality-eligible denominator: 14521
Candidate denominator after periodontal/SUA/mortality gates: 10188
Core complete N: 9069
Final public raw-only analytic N: 9018
Final public raw-only deaths: 743

Survey design:
- Weight: WTMEC6YR = WTMEC2YR / 3 for three 2-year NHANES MEC cycles.
- PSU: SDMVPSU.
- Strata: SDMVSTRA.
- Cycles: NHANES 2009-2010, 2011-2012, and 2013-2014.

Known public raw-only difference from the old private-derived cohort:
- 2013-2014 public SMQ_H lacks 931 candidate respondent records reported in v18e/v18c.
- Public-rule smoking_status missingness is concentrated in 2013-2014.
- The v20 analysis does not impute or reverse-engineer those records.
