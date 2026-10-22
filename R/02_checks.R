check_data <- function(cfg) {
  tables <- readRDS('data/processed/prepared.rds')
  flags <- read_csv_text('outputs/qa/import_flags.csv')
  dictionary <- read_csv_text('config/survey_dictionary.csv')
  # Missingness profile is descriptive: structural skips must not be flagged as errors.
  missingness <- dplyr::bind_rows(lapply(names(tables),function(tab) {
    d <- tables[[tab]]
    tibble::tibble(table=tab,variable=names(d),n_rows=nrow(d),
      n_blank=vapply(d,function(z)sum(is.na(z)),integer(1)))
  }))
  write_csv(missingness,'outputs/qa/missingness_profile.csv')
  roster_count <- tables$roster |> dplyr::count(hh_id,name='roster_n')
  main_count <- dplyr::left_join(tables$main,roster_count,by='hh_id',relationship='many-to-one')
  flags <- dplyr::bind_rows(flags,flags_from(main_count,'main','hh_size',
    is.na(main_count$roster_n) | num(main_count$hh_size)!=main_count$roster_n,
    'Reported household size differs from number of roster records','roster_size','blocker'))
  # Use native IMPACT checks, with immutable row keys for individual-level logs.
  for (tab in names(tables)) {
    d <- tables[[tab]]
    out <- cleaningtools::check_duplicate(d,uuid_column='record_id',
      columns_to_check=if(tab=='main') 'hh_id' else c('hh_id','_index'))$duplicate_log
    if (nrow(out)) flags <- dplyr::bind_rows(flags,dplyr::mutate(out,
      table=tab,check_id='duplicate',severity='blocker'))
    qs <- dictionary[dictionary$table==tab & dictionary$type %in% c('integer','decimal'),]
    for (v in intersect(qs$name,names(d))) {
      y <- num(d[[v]])
      bad <- !is.na(d[[v]]) & (!is.finite(y) | y<0)
      flags <- dplyr::bind_rows(flags,flags_from(d,tab,v,bad,
        'Non-numeric or negative response in a non-negative numeric field',paste0('numeric_',v),'blocker'))
    }
    # Validate integer fields and binary option columns before analysis masks invalid inputs.
    for(v in intersect(qs$name[qs$type=='integer'],names(d))) {
      y <- num(d[[v]])
      flags <- dplyr::bind_rows(flags,flags_from(d,tab,v,
        is.finite(y) & y!=floor(y),'Integer response contains a fraction',paste0('integer_',v),'blocker'))
    }
    mapping <- read_csv_text('config/column_map.csv')
    binary <- mapping$name[mapping$table==tab & mapping$type=='binary']
    for(v in intersect(binary,names(d))) flags <- dplyr::bind_rows(flags,flags_from(d,tab,v,
      !is.na(d[[v]]) & !d[[v]] %in% c('0','1'),
      'Binary option must be 0 or 1',paste0('binary_',v),'blocker'))
    if (tab=='main') {
      days <- c(paste0('m',3269:3277,'_hh_fcs_',
        c('staples','pulses','dairy','protein','veg','fruits','fat','sugar','condiments')),
        'fsl_rcsi_rely_on_less_expensive_food','fsl_rcsi_borrow','fsl_rcsi_mealsize',
        'fsl_rcsi_mealadult','fsl_rcsi_mealnb')
      for (v in intersect(days,names(d))) {
        y <- num(d[[v]])
        flags <- dplyr::bind_rows(flags,flags_from(d,tab,v,
          !is.na(y) & (y>7 | y!=floor(y)),'Recall must be an integer from 0 to 7',paste0('recall_',v),'blocker'))
      }
      if ('hh_size' %in% names(d)) {
        dd <- d; dd$hh_size <- num(dd$hh_size)
        out <- cleaningtools::check_logical(dd,uuid_column='record_id',check_id='hh_size_invalid',
          check_to_perform='!is.na(hh_size) & (hh_size < 1 | hh_size != floor(hh_size))',
          columns_to_clean='hh_size',description='Household size must be a positive integer')$hh_size_invalid
        if (nrow(out)) flags <- dplyr::bind_rows(flags,dplyr::mutate(out,table=tab,severity='blocker'))
      }
      for (pair in list(c('m2754_hh_hhs_no_food','m2755_hh_hhs_no_food_frequency'),
                       c('m2756_hh_hhs_sleep_hungry','m2757_hh_hhs_sleep_hungry_frequency'),
                       c('m2758_hh_hhs_no_day_night','m2759_hh_hhs_no_day_night_frequency'))) {
        assert_cols(d,pair)
        bad <- (d[[pair[1]]]=='yes' & is.na(d[[pair[2]]])) |
               (d[[pair[1]]]=='no' & !is.na(d[[pair[2]]]))
        flags <- dplyr::bind_rows(flags,flags_from(d,tab,pair[1],bad,
          'HHS occurrence and frequency inconsistent; review both responses',paste0('hhs_',pair[1]),'blocker'))
      }
      fcs <- days[1:8]; dd <- d
      for (v in fcs) dd[[v]] <- num(dd[[v]])
      out <- cleaningtools::check_fcs(dd,uuid_column='record_id',
        cereals_column=fcs[1],pulses_column=fcs[2],dairy_column=fcs[3],meat_column=fcs[4],
        vegetables_column=fcs[5],fruits_column=fcs[6],oil_column=fcs[7],sugar_column=fcs[8])
      if (nrow(out)) {
        out <- out |> dplyr::rename(uuid=record_id) |>
          tidyr::pivot_longer(dplyr::all_of(fcs),names_to='question',values_to='old_value') |>
          dplyr::mutate(old_value=as.character(old_value),table=tab,check_id='fcs_same',severity='review')
        flags <- dplyr::bind_rows(flags,out)
      }
    }
  }
  flags <- flags |> dplyr::mutate(dplyr::across(dplyr::everything(),as.character)) |>
    dplyr::distinct(table,uuid,question,issue,.keep_all=TRUE)
  write_csv(flags,'outputs/qa/cleaning_flags.csv')
  # Writable decisions are a separate file and are never overwritten by reruns.
  if (!file.exists(cfg$decisions)) write_csv(tibble::tibble(table=character(),uuid=character(),
    question=character(),old_value=character(),change_type=character(),new_value=character(),
    reason=character(),reviewer=character(),approved=character()),cfg$decisions)
  write_csv(flags |> dplyr::count(table,check_id,severity,name='n_flags'),'outputs/qa/flag_summary.csv')
  message(nrow(flags),' flags for review; no responses automatically changed.')
}

