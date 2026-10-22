pipeline_stages <- c('prepare','checks','clean','indicators','analyse','review','present')
stage_artifacts <- list(
  prepare=c('data/processed/prepared.rds','outputs/qa/import_flags.csv'),
  checks=c('outputs/qa/cleaning_flags.csv','outputs/qa/flag_summary.csv'),
  clean=c('data/processed/clean.rds','outputs/qa/applied_cleaning_log.csv'),
  indicators=c('data/processed/indicators.rds','data/processed/sector_coverage.rds'),
  analyse=c('outputs/analysis/results.rds','outputs/analysis/results_long.csv',
            'outputs/analysis/denominators.csv','outputs/analysis/MOZ_MSNA_2026_results.xlsx',
            'outputs/qa/sector_missingness_by_group.csv','outputs/qa/skipped_indicators.csv'),
  review=c('outputs/qa/result_review.csv','outputs/qa/wash_denominator_comparison.csv'),
  present=c('outputs/presentation/report.html','outputs/presentation/presentation_manifest.csv'))

# Changes to source, code or methodology invalidate downstream cached stages.
# Decisions are intentionally tracked only from clean onwards; weights from analyse onwards.
pipeline_signature <- function(stage,cfg) {
  cfg_signature <- cfg
  cfg_signature$decisions <- NULL; cfg_signature$weights_file <- NULL
  tmp <- tempfile(); on.exit(unlink(tmp)); dput(cfg_signature,tmp)
  paths <- c(cfg$dataset,cfg$kobo,list.files('R',full.names=TRUE,pattern='\\.R$'),
             list.files('config',full.names=TRUE,pattern='\\.(R|csv)$'))
  paths <- setdiff(paths,c(cfg$decisions,cfg$weights_file))
  if(match(stage,pipeline_stages)>=3) paths <- c(paths,cfg$decisions)
  if(match(stage,pipeline_stages)>=5 && cfg$analysis_mode=='weighted') paths <- c(paths,cfg$weights_file)
  paths <- sort(unique(paths))
  hash <- rep(NA_character_,length(paths)); names(hash) <- paths
  exists <- file.exists(paths); hash[exists] <- tools::md5sum(paths[exists])
  list(config=unname(tools::md5sum(tmp)),files=hash)
}

stage_record_path <- function(stage) paste0('outputs/metadata/stage_',stage,'.rds')
validate_stage <- function(stage,cfg) {
  i <- match(stage,pipeline_stages)
  if(i==1) return(invisible(TRUE))
  for(previous in pipeline_stages[seq_len(i-1)]) {
    p <- stage_record_path(previous)
    if(!file.exists(p)) stop('Run stage ',previous,' first; its successful run record is missing.')
    record <- readRDS(p)
    if(!identical(record$signature,pipeline_signature(previous,cfg)))
      stop('Inputs/configuration changed since ',previous,'. Rerun from that stage.')
    paths <- names(record$artifacts)
    if(!all(file.exists(paths)) || !identical(record$artifacts,tools::md5sum(paths)))
      stop('Cached outputs changed or disappeared after ',previous,'. Rerun from that stage.')
  }
}
invalidate_from <- function(stage) {
  affected <- pipeline_stages[seq.int(match(stage,pipeline_stages),length(pipeline_stages))]
  unlink(vapply(affected,stage_record_path,character(1)))
}
record_stage <- function(stage,cfg) {
  paths <- stage_artifacts[[stage]]
  if(stage=='present') {
    manifest <- read_csv_text('outputs/presentation/presentation_manifest.csv')
    paths <- unique(c(paths,manifest$path))
  }
  if(!all(file.exists(paths))) stop('Stage did not create all its required outputs: ',stage)
  saveRDS(list(stage=stage,finished_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
               signature=pipeline_signature(stage,cfg),artifacts=tools::md5sum(paths)),
          stage_record_path(stage))
  capture.output(sessionInfo(),file='outputs/metadata/sessionInfo.txt')
}

# One validity rule feeds both the estimates and denominator reports.
analysis_validity <- function(d,var,type) {
  raw <- d[[var]]
  valid <- !is.na(raw) & trimws(as.character(raw))!=''
  if(type %in% c('mean','median')) valid <- valid & is.finite(num(raw))
  if(grepl('/',var,fixed=TRUE)) valid <- valid & raw %in% c('0','1')
  valid
}
analysis_group_key <- function(d,grouping) {
  if(!length(grouping)) return(rep('__overall__',nrow(d)))
  if(!nrow(d)) return(character())
  apply(as.data.frame(d[,grouping,drop=FALSE]),1,paste,collapse=' %/% ')
}
analysis_denominators <- function(d,var,type,group) {
  gg <- if(is.na(group)) character() else trimws(strsplit(group,',',fixed=TRUE)[[1]])
  d$.valid <- analysis_validity(d,var,type)
  out <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(gg))) |>
    dplyr::summarise(n_eligible=dplyr::n(),n_valid=sum(.valid),
      n_missing=sum(is.na(.data[[var]])),
      n_invalid=sum(!.valid & !is.na(.data[[var]])),
      n_households=dplyr::n_distinct(hh_id),
      n_valid_households=dplyr::n_distinct(hh_id[.valid]),.groups='drop')
  out$group_var_value <- analysis_group_key(out,gg)
  out
}
