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


# Local WASH adaptation. Add compound/neighbour ONLY after confirming the local instrument.
cfg$water_on_premises_sources <- c('piped_dwelling')
cfg$water_max_collection_minutes <- 600
cfg$require_blocker_review <- TRUE
cfg$presentation_language <- 'en' # 'en' or 'pt'
cfg$presentation_indicators <- c('fsl_fcs_cat','fsl_hhs_cat','water_jmp_service',
                                  'education_oos','health_unmet_among_need')

# Optional age correction: set a mapped main date column only after confirming the school year.
# NULL keeps age-at-interview analysis; missing interview dates are never replaced by an old date.
cfg$education_interview_date_column <- NULL
cfg$education_school_year_start_month <- NULL
cfg$education_school_year_confirmed <- FALSE
