html_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ''
  for(pair in list(c('&','&amp;'),c('<','&lt;'),c('>','&gt;'),c('"','&quot;'),c("'",'&#39;')))
    x <- gsub(pair[1],pair[2],x,fixed=TRUE)
  x
}
html_table <- function(d) {
  if(!nrow(d)) return('<p>No available results.</p>')
  header <- paste0('<th>',html_escape(names(d)),'</th>',collapse='')
  rows <- apply(as.data.frame(d),1,function(z)
    paste0('<tr>',paste0('<td>',html_escape(z),'</td>',collapse=''),'</tr>'))
  paste0('<div class="table-wrap"><table><thead><tr>',header,
         '</tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div>')
}

label_results <- function(r,cfg) {
  dictionary <- read_csv_text('config/survey_dictionary.csv')
  choices <- read_csv_text('config/choices_dictionary.csv')
  mapping <- read_csv_text('config/column_map.csv')
  derived <- read_csv_text('config/presentation_labels.csv')
  language <- cfg$presentation_language %||% 'en'
  if(!language %in% c('en','pt')) stop('presentation_language must be en or pt.')
  label_col <- paste0('label_',language)
  r$indicator_label <- r$requested_variable
  r$response_label <- gsub('_',' ',as.character(r$analysis_var_value),fixed=TRUE)
  for(i in seq_len(nrow(r))) {
    tab <- r$table[i]; var <- r$requested_variable[i]
    dd <- derived[derived$variable==var,,drop=FALSE]
    if(nrow(dd)) r$indicator_label[i] <- dd[[label_col]][1] else {
      parent <- sub('/.*$','',var)
      q <- dictionary[dictionary$table==tab & dictionary$name==parent,,drop=FALSE]
      if(nrow(q) && !is.na(q[[label_col]][1])) r$indicator_label[i] <- q[[label_col]][1]
      if(grepl('/',var,fixed=TRUE)) {
        child <- mapping[mapping$table==tab & mapping$name==var,,drop=FALSE]
        if(nrow(child)) {
          cc <- choices[choices$list_name==child$list_name[1] & choices$code==child$option[1],,drop=FALSE]
          if(nrow(cc)) r$indicator_label[i] <- paste(r$indicator_label[i],cc[[label_col]][1],sep=' / ')
        }
      } else if(nrow(q)) {
        cc <- choices[choices$list_name==q$list_name[1] & choices$code==r$analysis_var_value[i],,drop=FALSE]
        if(nrow(cc)) r$response_label[i] <- cc[[label_col]][1]
      }
    }
  }
  # Standard numeric dummies have clear display labels, while preserving codes.
  binary <- grepl('/',r$requested_variable,fixed=TRUE)
  r$response_label[binary & r$analysis_var_value %in% '1'] <- if(language=='pt') 'Sim' else 'Yes'
  r$response_label[binary & r$analysis_var_value %in% '0'] <- if(language=='pt') 'Não' else 'No'
  if('reported_severity' %in% names(r)) {
    ii <- !is.na(r$reported_severity)
    r$response_label[ii] <- r$reported_severity[ii]
  }
  r$estimate_display <- ifelse(r$analysis_type %in% c('prop_select_one','prop_select_multiple'),
    sprintf('%.1f%%',100*r$stat),sprintf('%.2f',r$stat))
  r$reporting_note <- ifelse(r$small_sample,'Small sample: interpret with caution','')
  r
}

style_results_workbook <- function(sheets,path) {
  wb <- openxlsx::createWorkbook()
  heading <- openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#163F4A',textDecoration='bold',wrapText=TRUE)
  for(name in names(sheets)) {
    d <- as.data.frame(sheets[[name]])
    if(!ncol(d)) d <- data.frame(Note='No records')
    openxlsx::addWorksheet(wb,name)
    openxlsx::writeData(wb,name,d,headerStyle=heading,withFilter=nrow(d)>0)
    openxlsx::freezePane(wb,name,firstRow=TRUE)
    openxlsx::setColWidths(wb,name,cols=seq_len(ncol(d)),widths=20)
    openxlsx::setColWidths(wb,name,cols=1,widths=42)
  }
  openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
}

present_results <- function(cfg) {
  review <- read_csv_text('outputs/qa/result_review.csv')
  if(!nrow(review) || any(review$status!='PASS')) stop('Presentation requires a passed result review.')
  obj <- readRDS('outputs/analysis/results.rds')
  r <- label_results(obj$results,cfg)
  dir.create('outputs/presentation/figures',recursive=TRUE,showWarnings=FALSE)
  paths <- character()
  save_path <- 'outputs/presentation/results_labelled.csv'
  write_csv(r,save_path); paths <- c(paths,save_path)
  # Partition tables before pivoting: identical variable names in different loops never collide.
  for(tab in unique(r$table)) {
    x <- r[r$table==tab,,drop=FALSE]
    detail <- x[,c('indicator_label','response_label','analysis_type','requested_group',
      'group_var_value','estimate_display','stat','n_unweighted_valid','n_households',
      'small_sample','reporting_note','analysis_mode','requested_variable','analysis_var_value')]
    sheets <- list(Results=detail,Denominators=obj$denominators[obj$denominators$table==tab,,drop=FALSE])
    # Native IMPACT wide output; samples remain in the adjacent Results/Denominators sheets.
    groups <- unique(x$requested_group)
    for(j in seq_along(groups)) {
      g <- groups[j]
      keep <- if(is.na(g)) is.na(x$requested_group) else !is.na(x$requested_group) & x$requested_group==g
      xx <- x[keep,,drop=FALSE]
      wide <- presentresults::create_table_variable_x_group(xx,value_columns='stat')
      sheets[[paste0('Wide_',j)]] <- wide
    }
    path <- paste0('outputs/presentation/Results_',tab,'.xlsx')
    style_results_workbook(sheets,path); paths <- c(paths,path)
  }
  # Data for a dashboard: estimates plus explicit universe and denominator columns.
  dashboard <- r
  dd <- obj$denominators[,c('table','variable','analysis_type','group_spec','group_var_value',
                           'n_eligible','n_valid','n_missing','n_invalid')]
  names(dd)[names(dd)=='variable'] <- 'requested_variable'
  names(dd)[names(dd)=='group_spec'] <- 'requested_group'
  dashboard$group_var_value[is.na(dashboard$requested_group)] <- '__overall__'
  dashboard <- dplyr::left_join(dashboard,dd,
    by=c('table','requested_variable','analysis_type','requested_group','group_var_value'),
    relationship='many-to-one',na_matches='na')
  path <- 'outputs/presentation/dashboard_results.csv'
  write_csv(dashboard,path); paths <- c(paths,path)

  overview <- r[is.na(r$requested_group) & r$requested_variable %in% cfg$presentation_indicators,,drop=FALSE]
  figures <- character()
  for(var in unique(overview$requested_variable)) {
    z <- overview[overview$requested_variable==var,,drop=FALSE]
    if(!all(z$analysis_type %in% c('prop_select_one','prop_select_multiple'))) next
    path <- paste0('outputs/presentation/figures/',gsub('[^a-zA-Z0-9_]','_',var),'.png')
    grDevices::png(path,width=1400,height=max(650,100*nrow(z)),res=150)
    tryCatch({
      graphics::par(mar=c(7,14,5,2))
      labels <- paste0(z$response_label,' (',z$estimate_display,')')
      graphics::barplot(100*z$stat,names.arg=labels,horiz=TRUE,las=1,col='#237D89',
        border=NA,xlim=c(0,100),xlab='Percent of valid responses',cex.names=.85,
        main=paste(strwrap(z$indicator_label[1],width=75),collapse='\n'))
      graphics::mtext(paste('Valid records:',unique(z$n_unweighted_valid)[1],
        '|',cfg$analysis_mode,'| household/individual basis: see table'),side=1,line=5.2,cex=.7)
    },finally=grDevices::dev.off())
    paths <- c(paths,path)
    figures <- c(figures,paste0('<figure><img src="figures/',basename(path),
      '" alt="',html_escape(z$indicator_label[1]),'"><figcaption>',
      html_escape(z$indicator_label[1]),'</figcaption></figure>'))
  }
  gaps <- read_csv_text('config/methodology_gaps.csv')
  missing <- read_csv_text('outputs/qa/sector_missingness_by_group.csv')
  missing <- missing[is.na(missing$group_spec),,drop=FALSE]
  report <- c('<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1"><title>Mozambique MSNA 2026</title>',
    '<style>body{font:16px/1.6 system-ui,sans-serif;color:#17343d;margin:0;background:#f2f6f7}main{max-width:1150px;margin:auto;padding:32px}header{background:#163f4a;color:white;padding:28px;border-radius:12px}section{background:white;padding:24px;margin-top:24px;border-radius:12px}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:9px;text-align:left;border-bottom:1px solid #ddd}th{background:#e7f1f2}.table-wrap{overflow:auto}img{max-width:100%;height:auto}figure{margin:24px 0}a{color:#12616f}</style></head><body><main>',
    '<header><h1>Mozambique MSNA 2026</h1><p>Cleaning, analysis and results review</p></header>',
    paste0('<section><h2>Interpretation</h2><p>Analysis mode: <strong>',html_escape(cfg$analysis_mode),
      '</strong>. Percentages use indicator-specific valid-response denominators. Individual estimates and household estimates have different units. Small-sample flags remain visible in the workbooks.</p>',
      '<p>This report does not certify a complete MSNI. Missing framework components and local adaptations are listed below. Water collected yesterday is not measured water consumption. JMP source/service categories do not establish tested water quality.</p></section>'),
    '<section><h2>Overall indicators</h2>',
    html_table(overview[,c('indicator_label','response_label','estimate_display','n_unweighted_valid','n_households','reporting_note')]),
    paste(figures,collapse=''),' </section>',
    '<section><h2>Result review</h2>',html_table(review),'</section>',
    '<section><h2>Sector coverage</h2>',html_table(missing),'</section>',
    '<section><h2>Methodology gaps</h2>',html_table(gaps),'</section>',
    '<section><h2>Downloads</h2><ul>',
    paste0('<li><a href="',html_escape(basename(paths[!grepl('/figures/',paths)])),'">',
      html_escape(basename(paths[!grepl('/figures/',paths)])),'</a></li>'),
    '</ul></section></main></body></html>')
  writeLines(report,'outputs/presentation/report.html',useBytes=TRUE)
  write_csv(tibble::tibble(path=paths,md5=unname(tools::md5sum(paths))),
            'outputs/presentation/presentation_manifest.csv')
  message('Presentation ready: outputs/presentation/report.html')
}
