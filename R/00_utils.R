`%||%` <- function(x,y) if (is.null(x)) y else x
read_csv_text <- function(p) readr::read_csv(p, col_types=readr::cols(.default=readr::col_character()),
                                           show_col_types=FALSE, na=c('','NA'))
write_csv <- function(x,p) readr::write_csv(x,p,na='')
norm_label <- function(x) {
  x <- stringr::str_replace_all(as.character(x), c('’'="'", '‘'="'"))
  stringr::str_to_lower(stringr::str_squish(x))
}
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
assert_cols <- function(d, cols, label='data') {
  m <- setdiff(cols,names(d)); if (length(m)) stop(label, ': missing ', paste(m,collapse=', '))
}
empty_flags <- function() tibble::tibble(table=character(),uuid=character(),question=character(),
  old_value=character(),issue=character(),check_id=character(),severity=character())
flags_from <- function(d,table,variable,bad,issue,id,severity='review') {
  ii <- which(bad %in% TRUE)
  if (!length(ii)) return(empty_flags())
  tibble::tibble(table=table,uuid=d$record_id[ii],question=variable,
    old_value=as.character(d[[variable]][ii]),issue=issue,check_id=id,severity=severity)
}
strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
schema_comparison <- function(actual,expected,table) {
  n <- max(length(actual),length(expected))
  pad <- function(x) c(as.character(x),rep(NA_character_,n-length(x)))
  actual <- pad(actual); expected <- pad(expected)
  same <- !is.na(actual) & !is.na(expected) & norm_label(actual)==norm_label(expected)
  tibble::tibble(table=table,column=seq_len(n),expected_header=expected,
    actual_header=actual,equivalent_after_normalization=same)
}
mask_days <- function(x) {
  y <- num(x); y[!is.na(y) & (y<0 | y>7 | y!=floor(y))] <- NA_real_; y
}
append_humind <- function(df, fun, ...) {
  # Preserve every original input and append only newly computed columns.
  before <- names(df); out <- fun(df, ...)
  if (nrow(out)!=nrow(df)) stop('Humind changed the number of rows.')
  dplyr::bind_cols(df, out[,setdiff(names(out), before),drop=FALSE])
}
attach_household <- function(loop,main,extra=character()) {
  keys <- unique(c('hh_id','admin1','admin2','admin3','population_group','weight',
                   'hoh_gender_final','hoh_age_category',extra))
  keys <- intersect(keys,names(main))
  if (anyDuplicated(main$hh_id)) stop('Household UUIDs are not unique. Resolve duplicates in the cleaning log.')
  dplyr::left_join(loop[,setdiff(names(loop),setdiff(keys,'hh_id')),drop=FALSE],
                   main[,keys,drop=FALSE],by='hh_id',relationship='many-to-one')
}
