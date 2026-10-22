validate_decision_values <- function(log,mapping,choices) {
  edits <- log[log$change_type %in% c('change_response','blank_response'),,drop=FALSE]
  for(i in seq_len(nrow(edits))) {
    m <- mapping[mapping$table==edits$table[i] & mapping$name==edits$question[i],,drop=FALSE]
    if(nrow(m)!=1) stop('Correction column is not uniquely mapped: ',edits$question[i])
    if(grepl('^select_multiple ',m$type))
      stop('Edit select-multiple binary options, not the parent: ',edits$question[i])
    if(edits$change_type[i]=='blank_response') next
    value <- edits$new_value[i]
    if(m$type=='binary' && !value %in% c('0','1')) stop('Binary correction must be 0 or 1.')
    if(grepl('^select_one ',m$type) &&
       !value %in% choices$code[choices$list_name==m$list_name])
      stop('Correction must use a canonical Kobo choice code: ',edits$question[i])
    if(m$type %in% c('integer','decimal') &&
       (!is.finite(num(value)) || num(value)<0 || (m$type=='integer' && num(value)!=floor(num(value)))))
      stop('Correction is not a valid ',m$type,': ',edits$question[i])
  }
}

sync_multiple_choices <- function(d,log,tab,mapping) {
  # Rebuild only touched parents; a wholly blank binary response stays missing.
  children <- mapping[mapping$table==tab & mapping$type=='binary',,drop=FALSE]
  touched <- intersect(log$question[log$change_type %in% c('change_response','blank_response')],children$name)
  parents <- unique(sub('/.*$','',touched))
  audit <- list()
  for(parent in parents) {
    ch <- children[sub('/.*$','',children$name)==parent,,drop=FALSE]
    if(!parent %in% names(d) || !all(ch$name %in% names(d))) stop('Incomplete select-multiple mapping: ',parent)
    ids <- unique(log$uuid[log$question %in% ch$name & log$change_type %in% c('change_response','blank_response')])
    for(id in intersect(ids,d$record_id)) {
      i <- match(id,d$record_id); values <- unlist(d[i,ch$name],use.names=FALSE)
      if(any(!is.na(values) & !values %in% c('0','1')))
        stop('Invalid binary sibling while rebuilding ',parent,': ',id)
      # Partial missingness cannot be resolved as a complete parent response.
      new <- if(anyNA(values)) NA_character_ else paste(ch$option[values=='1'],collapse=' ')
      if(identical(new,'')) new <- NA_character_
      audit[[length(audit)+1L]] <- tibble::tibble(table=tab,uuid=id,question=parent,
        old_value=d[[parent]][i],new_value=new,change_type='rebuild_multiple_parent',
        reason='Rebuilt from reviewed binary options')
      d[[parent]][i] <- new
    }
  }
  list(data=d,audit=dplyr::bind_rows(audit))
}

check_blocker_dispositions <- function(flags,approved,retained,cfg) {
  unresolved <- empty_flags()
  if(!isTRUE(cfg$require_blocker_review) || !nrow(flags)) return(invisible(TRUE))
  flags <- flags[flags$severity %in% 'blocker',,drop=FALSE]
  for(i in seq_len(nrow(flags))) {
    f <- flags[i,,drop=FALSE]
    if(!f$uuid %in% retained[[f$table]]$record_id) next
    log <- approved[approved$table==f$table & approved$uuid==f$uuid,,drop=FALSE]
    handled <- any(log$change_type %in% 'remove_survey') ||
      any(log$question==f$question & log$change_type %in% c('change_response','blank_response','no_action'))
    if(!handled) unresolved <- dplyr::bind_rows(unresolved,f)
  }
  write_csv(unresolved,'outputs/qa/unresolved_blockers.csv')
  if(nrow(unresolved)) stop(nrow(unresolved),
    ' unresolved blockers. Review outputs/qa/unresolved_blockers.csv and record decisions before clean.')
  invisible(TRUE)
}
