water_time_category <- function(x,cfg) {
  assert_cols(x,c('wash_drinking_water_source','wash_drinking_water_time_int'))
  source <- x$wash_drinking_water_source
  premises <- cfg$water_on_premises_sources %||% character()
  if(any(premises %in% c('dnk','pnta','other','surface_water')))
    stop('Unknown/other/surface-water sources cannot be configured as on premises.')
  out <- rep(NA_character_,nrow(x))
  # Explicit questionnaire branch, if it exists. Never manufacture it from a blank time.
  if('wash_drinking_water_time_yn' %in% names(x)) {
    yn <- x$wash_drinking_water_time_yn
    out[yn %in% c('water_in_dwelling','water_in_plot')] <- 'premises'
    if('wash_drinking_water_time_sl' %in% names(x)) {
      sl <- x$wash_drinking_water_time_sl
      fallback <- yn %in% c('dnk','pnta')
      out[fallback & sl %in% c('5min_or_less','5min_15min','15min_30min')] <- 'under_30min'
      out[fallback & sl %in% c('30min_1hr','more_than_1hr')] <- 'above_30min'
    }
  }
  minutes <- num(x$wash_drinking_water_time_int)
  valid <- is.finite(minutes) & minutes>0 & minutes==floor(minutes) &
    minutes<=(cfg$water_max_collection_minutes %||% 600)
  # For the integer-only local export, a valid reported duration is sufficient.
  scope <- if('wash_drinking_water_time_yn' %in% names(x))
    x$wash_drinking_water_time_yn %in% 'number_minutes' else rep(TRUE,nrow(x))
  out[scope & valid & minutes<=30] <- 'under_30min'
  out[scope & valid & minutes>30] <- 'above_30min'
  out[source %in% premises] <- 'premises'
  out[source %in% c('dnk','pnta')] <- NA_character_
  out
}

water_unknown_quantity <- function(x) {
  unknown <- rep(FALSE,nrow(x))
  for(v in intersect(c('wash_container_size/dnk','wash_container_size/pnta'),names(x)))
    unknown <- unknown | x[[v]] %in% '1'
  # Also protect exports with only a concatenated parent response.
  if('wash_container_size' %in% names(x)) {
    unknown <- unknown | is.na(x$wash_container_size) | trimws(x$wash_container_size)==''
    unknown <- unknown | grepl('(^|[ ,;])(dnk|pnta)($|[ ,;])',x$wash_container_size) %in% TRUE
  }
  unknown
}
