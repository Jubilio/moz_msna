make_design <- function(d,cfg) {
  if (cfg$analysis_mode=='descriptive') {
    d$weight <- 1
    return(srvyr::as_survey_design(d,ids=NULL,weights=weight))
  }
  if (cfg$analysis_mode!='weighted' || !isTRUE(cfg$sampling_design_confirmed))
    stop('Weighted analysis needs a confirmed sampling design.')
  assert_cols(d,'weight')
  if (anyNA(d$weight) || any(d$weight<=0)) stop('Missing/non-positive weights.')
  args <- list(.data=d,weights=rlang::sym('weight'),nest=TRUE)
  args$ids <- rlang::sym(cfg$psu_column %||% 'hh_id')
  if (!is.null(cfg$strata_column)) args$strata <- rlang::sym(cfg$strata_column)
  rlang::inject(srvyr::as_survey_design(!!!args))
}

run_one_analysis <- function(d,var,type,group,cfg,table) {
  grouping <- if(is.na(group)) character() else trimws(strsplit(group,',',fixed=TRUE)[[1]])
  assert_cols(d,c(var,grouping))
  # Exclude only missing indicator responses; structural skips are never denominator zeroes.
  eligible <- d[!is.na(d[[var]]),,drop=FALSE]
  if (!nrow(eligible)) return(tibble::tibble())
  # Preserve type and reporting denominator for raw binary/numeric fields.
  if(type %in% c('mean','median')) eligible[[var]] <- num(eligible[[var]])
  if(grepl('/',var,fixed=TRUE)) {
    eligible[[var]][!eligible[[var]] %in% c('0','1',0,1)] <- NA
    eligible <- eligible[!is.na(eligible[[var]]),,drop=FALSE]
    if(!nrow(eligible)) return(tibble::tibble())
  }
  design <- make_design(eligible,cfg)
  loa <- tibble::tibble(analysis_type=type,analysis_var=var,group_var=group,level=cfg$confidence_level)
  result <- analysistools::create_analysis(design=design,loa=loa,sm_separator='/')$results_table
  result$table <- table; result$requested_variable <- var; result$requested_group <- group
  result$analysis_mode <- cfg$analysis_mode
  if(grepl('^comp_.*_score$',var)) {
    result$reported_severity <- ifelse(as.character(result$analysis_var_value)=='5',
      '4+',as.character(result$analysis_var_value))
  }
  # Attach independently counted denominator; do not rename package n_sample fields.
  nn <- eligible |> dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
    dplyr::summarise(n_unweighted_valid=dplyr::n(),n_households=dplyr::n_distinct(hh_id),.groups='drop')
  if (length(grouping)) {
    nn$group_var_value <- apply(as.data.frame(nn[,grouping,drop=FALSE]),1,paste,collapse=' %/% ')
    result <- dplyr::left_join(result,nn,by='group_var_value',relationship='many-to-one')
  } else {
    result$n_unweighted_valid <- nrow(eligible);result$n_households <- dplyr::n_distinct(eligible$hh_id)
  }
  result$small_sample <- ifelse(is.na(result$n_unweighted_valid),NA,
                                result$n_unweighted_valid<cfg$min_reporting_n)
  if(cfg$analysis_mode=='descriptive') {
    ci <- grep('(^|_)(ci|se|deff|low|upp|lower|upper)($|_)',names(result),value=TRUE)
    for(v in ci) result[[v]] <- NA_real_
  }
  result
}

analyse_data <- function(cfg) {
  tables <- readRDS('data/processed/indicators.rds'); main <- tables$main
  if (cfg$analysis_mode=='weighted') {
    if (!file.exists(cfg$weights_file)) stop('Supply config/weights.csv with hh_id, weight.')
    weights <- read_csv_text(cfg$weights_file); assert_cols(weights,c('hh_id','weight'))
    if(anyDuplicated(weights$hh_id)) stop('Duplicate weight keys.')
    weights$weight <- num(weights$weight)
    main <- dplyr::left_join(main,weights,by='hh_id',relationship='one-to-one')
    if(anyNA(main$weight)) stop('Weights do not cover all cleaned households.')
  } else main$weight <- 1
  tables$main <- main
  for (tab in setdiff(names(tables),'main')) tables[[tab]] <- attach_household(tables[[tab]],main,
    extra=c(cfg$psu_column,cfg$strata_column))
  metrics <- read_csv_text('config/indicators.csv'); results <- list(); denominators <- list(); k <- 0L
  for(i in seq_len(nrow(metrics))) {
    tab <- metrics$table[i]; var <- metrics$variable[i]; d <- tables[[tab]]
    if (!var %in% names(d)) next
    if(metrics$population[i]=='school_age') d <- d[d$eligible_education %in% TRUE,,drop=FALSE]
    if(metrics$population[i]=='education_access') d <- d[d$education_access %in% 'yes',,drop=FALSE]
    if(metrics$population[i]=='education_oos') d <- d[d$education_access %in% 'no',,drop=FALSE]
    groups <- c(NA_character_,cfg$groups)
    if(tab=='edu_ind') groups <- c(groups,'gender','gender, admin1, admin2')
    for(g in unique(groups)) {
      k <- k+1L
      results[[k]] <- run_one_analysis(d,var,metrics$analysis_type[i],g,cfg,tab)
      gg <- if(is.na(g)) character() else trimws(strsplit(g,',',fixed=TRUE)[[1]])
      denominators[[k]] <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(gg))) |>
        dplyr::summarise(n_eligible=dplyr::n(),n_valid=sum(!is.na(.data[[var]])),
          n_missing=sum(is.na(.data[[var]])),n_households=dplyr::n_distinct(hh_id),.groups='drop') |>
        dplyr::mutate(table=tab,variable=var,group_spec=g)
    }
  }
  result <- dplyr::bind_rows(results); denom <- dplyr::bind_rows(denominators)
  coverage <- readRDS('data/processed/sector_coverage.rds')
  missing_tables <- list(); k_missing <- 0L
  sector_vars <- grep('^comp_.*_score$',names(coverage),value=TRUE)
  for(g in c(NA_character_,cfg$groups)) {
    gg <- if(is.na(g)) character() else trimws(strsplit(g,',',fixed=TRUE)[[1]])
    for(v in sector_vars) {
      k_missing <- k_missing+1L
      missing_tables[[k_missing]] <- coverage |>
        dplyr::group_by(dplyr::across(dplyr::all_of(gg))) |>
        dplyr::summarise(n_households=dplyr::n(),n_classified=sum(!is.na(.data[[v]])),
          n_missing=sum(is.na(.data[[v]])),pct_missing=n_missing/n_households,.groups='drop') |>
        dplyr::mutate(component=v,group_spec=g,count_basis='unweighted households')
    }
  }
  sector_missing <- dplyr::bind_rows(missing_tables)
  write_csv(sector_missing,'outputs/qa/sector_missingness_by_group.csv')
  write_csv(result,'outputs/analysis/results_long.csv')
  write_csv(denom,'outputs/analysis/denominators.csv')
  saveRDS(list(results=result,denominators=denom,config=cfg),'outputs/analysis/results.rds')
  openxlsx::write.xlsx(list(Results=result,Denominators=denom,
    Sector_missingness=sector_missing,
    Methodology_gaps=read_csv_text('config/methodology_gaps.csv')),
    'outputs/analysis/MOZ_MSNA_2026_results.xlsx',overwrite=TRUE)
  message('Results exported; inspect denominator and methodological gap tables before reporting.')
}
