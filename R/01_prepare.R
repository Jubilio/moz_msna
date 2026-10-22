prepare_data <- function(cfg) {
  if (!file.exists(cfg$dataset)) stop('Copy the original dataset to ',cfg$dataset)
  mapping <- read_csv_text('config/column_map.csv')
  choices <- read_csv_text('config/choices_dictionary.csv')
  sheets <- read_csv_text('config/sheets.csv')
  tables <- list(); issues <- empty_flags()
  for (i in seq_len(nrow(sheets))) {
    tab <- sheets$table[i]; sh <- sheets$sheet[i]
    message('Reading ',sh)
    d <- readxl::read_excel(cfg$dataset,sheet=sh,col_types='text',.name_repair='minimal',trim_ws=FALSE)
    m <- mapping[mapping$table==tab,,drop=FALSE]; m <- m[order(as.integer(m$column)),]
    comparison <- schema_comparison(names(d),m$source_header,tab)
    diagnostic <- paste0('outputs/qa/schema_comparison_',tab,'.csv')
    write_csv(comparison,diagnostic)
    if (ncol(d)!=nrow(m) || !all(comparison$equivalent_after_normalization)) {
      differences <- comparison$column[!comparison$equivalent_after_normalization]
      stop('Schema differs for ',sh,': expected ',nrow(m),' columns, found ',ncol(d),
        '. First differing positions: ',paste(utils::head(differences,8),collapse=', '),
        '. Inspect ',diagnostic,'. No automatic reordering or positional override was applied.')
    }
    names(d) <- m$name
    if (anyDuplicated(names(d))) stop('Duplicate target names in ',tab)
    assert_cols(d,c('uuid','_index'),tab)
    idx <- d[['_index']]
    d$record_id <- if (tab=='main') paste(tab,idx,sep='::') else
      paste(tab,d[['_parent_index']],idx,sep='::')
    if (anyNA(idx) || anyDuplicated(d$record_id)) stop('Missing/duplicate immutable record keys in ',tab)
    d$hh_id <- as.character(d$uuid)
    # Select-one labels -> original Kobo XML codes. Unknown labels remain visible and flagged.
    for (j in which(grepl('^select_one ',m$type))) {
      v <- m$name[j]; ch <- choices[choices$list_name==m$list_name[j],]
      if (!nrow(ch)) next
      vals <- as.character(d[[v]]); original <- vals
      lookup <- dplyr::bind_rows(
        tibble::tibble(label=norm_label(ch$label_en),code=ch$code),
        tibble::tibble(label=norm_label(ch$label_pt),code=ch$code)) |>
        dplyr::filter(!is.na(label),label!='') |> dplyr::distinct()
      ambiguous <- lookup |> dplyr::count(label) |> dplyr::filter(n>1) |> dplyr::pull(label)
      lookup <- lookup[!lookup$label %in% ambiguous,]
      ii <- !is.na(vals) & !vals %in% ch$code
      converted <- lookup$code[match(norm_label(vals[ii]),lookup$label)]
      vals[ii] <- ifelse(is.na(converted),vals[ii],converted)
      d[[v]] <- vals
      issues <- dplyr::bind_rows(issues,flags_from(d,tab,v,
        !is.na(vals) & !vals %in% ch$code,'Response not uniquely mapped to a Kobo choice',
        paste0('unmapped_choice_',v),'blocker'))
    }
    tables[[tab]] <- d
  }
  # Verify both independent household identifiers before any parent-child join.
  main <- tables$main
  for (tab in setdiff(names(tables),'main')) {
    d <- tables[[tab]]; parent <- match(d[['_parent_index']],main[['_index']])
    bad <- is.na(parent) | is.na(d$hh_id) | d$hh_id != main$hh_id[parent]
    issues <- dplyr::bind_rows(issues,flags_from(d,tab,'_parent_index',bad,
      'Parent index and household UUID do not resolve to the same household','parent_link','blocker'))
  }
  saveRDS(tables,'data/processed/prepared.rds')
  write_csv(issues,'outputs/qa/import_flags.csv')
  write_csv(tibble::tibble(table=names(tables),records=vapply(tables,nrow,integer(1))),
            'outputs/metadata/input_inventory.csv')
  hashes <- tools::md5sum(c(cfg$dataset,cfg$kobo,'config/column_map.csv'))
  write_csv(tibble::tibble(path=names(hashes),md5=unname(hashes)), 'outputs/metadata/input_hashes.csv')
}
