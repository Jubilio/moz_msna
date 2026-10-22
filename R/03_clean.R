clean_data <- function(cfg) {
  tables <- readRDS('data/processed/prepared.rds')
  unlink(list.files('outputs/qa',pattern='^cascade_removed_.*\\.csv$',full.names=TRUE))
  decisions <- read_csv_text(cfg$decisions)
  assert_cols(decisions,c('table','uuid','question','old_value','change_type','new_value','reason','reviewer','approved'))
  approved <- decisions[decisions$approved %in% 'yes',,drop=FALSE]
  allowed <- c('change_response','blank_response','remove_survey','no_action')
  if (any(!approved$change_type %in% allowed)) stop('Unknown cleaning action.')
  if (anyNA(approved$reason) || anyNA(approved$reviewer)) stop('Approved decisions require reason and reviewer.')
  if (any(!approved$table %in% names(tables))) stop('Unknown table in cleaning decisions.')
  if (anyDuplicated(paste(approved$table,approved$uuid,approved$question)))
    stop('Conflicting/duplicate decisions for one cell; consolidate the log.')
  mapping <- read_csv_text('config/column_map.csv')
  choices <- read_csv_text('config/choices_dictionary.csv')
  validate_decision_values(approved,mapping,choices)
  for(tab in names(tables)) {
    log <- approved[approved$table==tab,,drop=FALSE]
    removed <- log$uuid[log$change_type=='remove_survey']
    if(any(log$uuid[log$change_type %in% c('change_response','blank_response')] %in% removed))
      stop('Do not edit and delete the same record in one run.')
  }
  audit <- list(); parent_audit <- list()
  for (tab in names(tables)) {
    d <- tables[[tab]]; log <- approved[approved$table==tab,,drop=FALSE]
    if (!nrow(log)) next
    if (any(!log$uuid %in% d$record_id)) stop('Unknown record_id in ',tab,' cleaning log.')
    edits <- !log$change_type %in% c('remove_survey','no_action')
    check_old <- log$change_type!='remove_survey'
    if (any(!log$question[check_old] %in% names(d))) stop('Unknown correction column in ',tab)
    immutable <- c('uuid','hh_id','record_id','_index','_parent_index','Id')
    if (any(log$question[edits] %in% immutable)) stop('Do not alter link identifiers through a cleaning decision.')
    if (any(log$change_type=='change_response' & is.na(log$new_value)))
      stop('change_response needs new_value; use blank_response to set missing.')
    for (i in which(check_old)) {
      old <- as.character(d[[log$question[i]]][match(log$uuid[i],d$record_id)])
      if (!identical(old,as.character(log$old_value[i]))) stop('Stale old_value in decision ',tab,': ',log$uuid[i])
    }
    if (any(log$change_type=='change_response')) {
      review <- cleaningtools::review_cleaning_log(raw_dataset=d,raw_data_uuid_column='record_id',
        cleaning_log=log,cleaning_log_uuid_column='uuid',cleaning_log_question_column='question',
        cleaning_log_new_value_column='new_value',cleaning_log_change_type_column='change_type')
      if (is.data.frame(review) && nrow(review)) stop('IMPACT cleaning-log review failed for ',tab)
    }
    clean <- cleaningtools::create_clean_data(raw_dataset=d,raw_data_uuid_column='record_id',
      cleaning_log=log,cleaning_log_uuid_column='uuid',cleaning_log_question_column='question',
      cleaning_log_new_value_column='new_value',cleaning_log_change_type_column='change_type')
    # Restore character IDs and choice codes after package type conversion.
    for (v in names(d)) clean[[v]] <- as.character(clean[[v]])
    for (i in which(edits)) {
      expected <- if(log$change_type[i]=='blank_response') NA_character_ else log$new_value[i]
      actual <- clean[[log$question[i]]][match(log$uuid[i],clean$record_id)]
      # Compare the package's typed result, then restore canonical source strings below.
      comparable <- function(z) as.character(utils::type.convert(z,as.is=TRUE))
      if (length(actual)!=1 || !identical(comparable(actual),comparable(expected)))
        stop('Correction audit failed for ',log$uuid[i],' / ',log$question[i])
    }
    removed <- log$uuid[log$change_type=='remove_survey']
    if (any(removed %in% clean$record_id)) stop('Deletion audit failed.')
    # Restore source strings in untouched cells (type.convert can change leading zeroes).
    restored <- d[match(clean$record_id,d$record_id),,drop=FALSE]
    for(i in which(edits)) if(log$uuid[i] %in% restored$record_id)
      restored[[log$question[i]]][match(log$uuid[i],restored$record_id)] <-
        if(log$change_type[i]=='blank_response') NA_character_ else log$new_value[i]
    synced <- sync_multiple_choices(restored,log,tab,mapping)
    tables[[tab]] <- synced$data; parent_audit[[tab]] <- synced$audit; audit[[tab]] <- log
  }
  main <- tables$main
  if (anyNA(main$hh_id) || anyDuplicated(main$hh_id))
    stop('Missing/duplicate household UUIDs. Review cleaning_flags.csv and approve deletions by record_id.')
  for (tab in setdiff(names(tables),'main')) {
    d <- tables[[tab]]
    # Parent removal cascades explicitly and is included in the audit.
    original_main <- readRDS('data/processed/prepared.rds')$main
    deleted_parent_indices <- setdiff(original_main[['_index']],main[['_index']])
    dropped <- d[['_parent_index']] %in% deleted_parent_indices
    if (any(dropped)) write_csv(d[dropped,c('record_id','hh_id')],paste0('outputs/qa/cascade_removed_',tab,'.csv'))
    d <- d[!dropped,,drop=FALSE]
    parent <- match(d[['_parent_index']],main[['_index']])
    if (anyNA(parent) || any(d$hh_id != main$hh_id[parent])) stop('Invalid parent links in ',tab)
    tables[[tab]] <- d
  }
  flags <- read_csv_text('outputs/qa/cleaning_flags.csv')
  check_blocker_dispositions(flags,approved,tables,cfg)
  write_csv(dplyr::bind_rows(parent_audit),'outputs/qa/rebuilt_multiple_parents.csv')
  saveRDS(tables,'data/processed/clean.rds')
  write_csv(dplyr::bind_rows(audit),'outputs/qa/applied_cleaning_log.csv')
}

