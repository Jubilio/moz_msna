# Project defaults. Edit here, not inside the pipeline scripts.
cfg <- list(
  dataset = 'data/raw/REACH_MOZ_Data_MSNA_01102026.xlsx',
  kobo = 'data/raw/kobo-used.xlsx',
  # Optional reviewed cleaning decisions; never overwrite this file with flags.
  decisions = 'config/cleaning_decisions.csv',
  groups = c('admin1', 'admin1, admin2', 'population_group',
             'admin1, admin2, population_group', 'hoh_gender_final', 'hoh_age_category'),
  min_reporting_n = 30L,
  # descriptive: unit weights and no published confidence intervals.
  # weighted: requires externally supplied final survey weights and design information.
  analysis_mode = 'descriptive',
  weights_file = 'config/weights.csv', # columns hh_id, weight
  sampling_design_confirmed = FALSE,
  psu_column = NULL, strata_column = NULL,
  confidence_level = 0.95,
  fcs_cutoffs = 'normal',
  education_age_min = 5L, education_age_max = 17L,
  # Not a national school structure: grade/cycle mapping stays disabled until reviewed.
  education_cycle_mapping_confirmed = FALSE,
  sanitation_mapping_confirmed = FALSE,
  local_lcsi_mapping_confirmed = FALSE,
  calculate_full_msni = FALSE
)
