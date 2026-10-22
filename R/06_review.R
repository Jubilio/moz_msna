review_results <- function(cfg) {
  obj <- readRDS('outputs/analysis/results.rds')
  r <- obj$results; d <- obj$denominators
  issues <- list()
  add <- function(check,bad,detail) {
    issues[[length(issues)+1L]] <<- tibble::tibble(check=check,
      status=if(any(bad %in% TRUE)) 'FAIL' else 'PASS',n_failed=sum(bad %in% TRUE),detail=detail)
  }
  keys <- c('table','requested_variable','requested_group','analysis_type','analysis_var_value','group_var_value')
  add('unique_results',duplicated(r[,keys]),'One result per table, indicator, option and group')
  prop <- r$analysis_type %in% c('prop_select_one','prop_select_multiple')
  add('proportion_range',prop & (!is.finite(r$stat) | r$stat< -1e-8 | r$stat>1+1e-8),
      'Valid proportions must be between 0 and 1')
  add('denominator_accounting',d$n_eligible!=d$n_valid+d$n_missing+d$n_invalid,
      'Eligible = valid + missing + invalid')
  add('valid_household_counts',d$n_valid_households>d$n_valid,
      'Distinct valid households cannot exceed valid records')
  add('reported_sample',is.na(r$n_unweighted_valid) | r$n_unweighted_valid<=0,
      'Each published result needs a positive valid denominator')
  # Compare independent denominator reports with the samples attached to estimates.
  dd <- d[,c('table','variable','analysis_type','group_spec','group_var_value','n_valid')]
  names(dd)[names(dd)=='variable'] <- 'requested_variable'
  names(dd)[names(dd)=='group_spec'] <- 'requested_group'
  rr <- r
  rr$group_var_value[is.na(rr$requested_group)] <- '__overall__'
  joined <- dplyr::left_join(rr,dd,
    by=c('table','requested_variable','analysis_type','requested_group','group_var_value'),
    relationship='many-to-one',na_matches='na')
  add('estimate_denominator_agreement',is.na(joined$n_valid) |
      joined$n_valid!=joined$n_unweighted_valid,
      'Estimate denominator must equal the independently exported valid count')
  # All select-one options on the same valid base must form a complete distribution.
  sums <- r[r$analysis_type=='prop_select_one',,drop=FALSE] |>
    dplyr::group_by(table,requested_variable,requested_group,group_var_value) |>
    dplyr::summarise(total=sum(stat),.groups='drop')
  add('select_one_sum',abs(sums$total-1)>1e-6,'Select-one option proportions must sum to one')
  if(cfg$analysis_mode=='descriptive')
    add('descriptive_no_ci',!is.na(r$stat_low) | !is.na(r$stat_upp),
        'Descriptive outputs must not publish population confidence intervals')
  write_csv(dplyr::bind_rows(issues),'outputs/qa/result_review.csv')
  # Diagnose, rather than force equal denominators across different WASH universes.
  wash <- d[d$variable %in% c('wash_drinking_water_source','wash_drinking_water_source_cat',
                             'water_jmp_service','water_collection_below_15l'),,drop=FALSE]
  write_csv(wash,'outputs/qa/wash_denominator_comparison.csv')
  if(any(vapply(issues,function(x)x$status=='FAIL',logical(1))))
    stop('Result review failed. Inspect outputs/qa/result_review.csv before presentation.')
  message('Result review passed.')
}
