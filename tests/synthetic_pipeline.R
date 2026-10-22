# End-to-end fixture: creates an isolated project copy; never opens user survey data.
project <- normalizePath('.',winslash='/')
keep_fixture <- identical(Sys.getenv('MSNA_KEEP_FIXTURE'),'yes')
fixture <- tempfile('msna-pipeline-',tmpdir=if(keep_fixture) dirname(project) else tempdir())
dir.create(fixture)
local({
  on.exit({setwd(project);if(!keep_fixture)unlink(fixture,recursive=TRUE)},add=TRUE)
  for(p in c('R','config','scripts','tests','MAIN.R')) file.copy(p,fixture,recursive=TRUE)
  setwd(fixture)
  cat("\ncfg$groups <- c('admin1, admin2','hoh_gender_final')\n",file='config/project.R',append=TRUE)
  source('config/project.R')
  for(p in sort(list.files('R',pattern='\\.R$',full.names=TRUE))) source(p)
  for(p in c('data/raw','data/processed','outputs/qa','outputs/analysis','outputs/metadata','outputs/presentation'))
    dir.create(p,recursive=TRUE,showWarnings=FALSE)
  mapping <- read_csv_text('config/column_map.csv'); sheets <- read_csv_text('config/sheets.csv')
  clean_tables <- list()
  for(tab in sheets$table) {
    m <- mapping[mapping$table==tab,,drop=FALSE]
    m <- m[order(as.integer(m$column)),,drop=FALSE]
    n <- if(tab=='main') 4L else if(tab %in% c('roster','health_ind')) 8L else if(tab=='edu_ind') 4L else 0L
    d <- as.data.frame(matrix(NA_character_,n,nrow(m)),stringsAsFactors=FALSE)
    names(d) <- m$name
    if(n) {
      parent <- if(tab=='main') seq_len(n) else if(n==8) rep(1:4,each=2) else 1:4
      d$uuid <- paste0('hh-',parent); d[['_index']] <- as.character(seq_len(n))
      if(tab!='main') d[['_parent_index']] <- as.character(parent)
    }
    clean_tables[[tab]] <- d
  }
  main <- clean_tables$main
  main$hh_size <- '2'; main$admin1 <- 'Cabo Delgado'; main$admin2 <- c('Mueda','Mueda','Macomia','Macomia')
  main$admin3 <- NA_character_; main$population_group <- 'idp'
  main$resp_hoh_yn <- 'yes'; main$resp_gender <- c('female','male','female','male'); main$resp_age <- '35'
  main$health_hh_any_member_required_healthcare <- 'yes'
  for(v in grep('^m32[0-9]+_hh_fcs_',names(main),value=TRUE)) main[[v]] <- '2'
  for(v in c('fsl_rcsi_rely_on_less_expensive_food','fsl_rcsi_borrow','fsl_rcsi_mealsize',
              'fsl_rcsi_mealadult','fsl_rcsi_mealnb')) main[[v]] <- '1'
  for(v in c('m2754_hh_hhs_no_food','m2756_hh_hhs_sleep_hungry','m2758_hh_hhs_no_day_night')) main[[v]] <- 'no'
  lc <- read_csv_text('config/lcsi_strategies.csv'); for(v in lc$variable) main[[v]] <- '10'
  main$wash_drinking_water_source <- c('piped_dwelling','borehole','surface_water','dnk')
  main$wash_drinking_water_time_int <- c(NA,'30','45',NA)
  main$wash_sanitation_facility <- 'open_defecation'
  main$wash_sanitation_facility_sharing_yn <- 'no'
  for(size in c(10,15,20,25)) {
    main[[paste0('wash_container_size/',size,'_litres')]] <- if(size==20) c('1','1','1','0') else '0'
    main[[paste0('wash_container_size',size,'_time_int')]] <- if(size==20)c('2','3','1',NA) else NA_character_
  }
  main[['wash_container_size/dnk']] <- c('0','0','0','1')
  main[['wash_container_size/pnta']] <- '0'
  main$wash_container_size <- c('20_litres','20_litres','20_litres','dnk')
  # Keep the fixture small while exercising household/individual, numeric/binary and WASH outputs.
  metrics <- read_csv_text('config/indicators.csv')
  chosen <- c('fsl_fcs_cat','fsl_fcs_score','fsl_hhs_cat','water_jmp_service',
    'wash_drinking_water_source','wash_drinking_water_source_cat','water_collection_below_15l',
    'water_litres_collected_per_person_yesterday','education_oos','health_unmet_among_need',
    'wash_container_size/20_litres')
  write_csv(metrics[metrics$variable %in% chosen,,drop=FALSE],'config/indicators.csv')
  clean_tables$main <- main
  e <- clean_tables$edu_ind
  e$edu_ind_age <- c('7','10','12','17'); e$edu_ind_gender <- c('female','male','female','male')
  e$edu_access <- c('yes','no','yes','no'); e$edu_level_grade <- c('grade_1',NA,'grade_6',NA)
  clean_tables$edu_ind <- e
  h <- clean_tables$health_ind; h$health_ind_healthcare_needed <- rep(c('yes','no'),4)
  h$health_ind_healthcare_received <- rep(c('no',NA),4); clean_tables$health_ind <- h
  wb <- openxlsx::createWorkbook()
  for(i in seq_len(nrow(sheets))) {
    tab <- sheets$table[i]; d <- clean_tables[[tab]]
    m <- mapping[mapping$table==tab,,drop=FALSE]; m <- m[order(as.integer(m$column)),,drop=FALSE]
    d <- d[,m$name,drop=FALSE]; names(d) <- m$source_header
    openxlsx::addWorksheet(wb,sheets$sheet[i]); openxlsx::writeData(wb,sheets$sheet[i],d)
  }
  openxlsx::saveWorkbook(wb,cfg$dataset,overwrite=TRUE)
  openxlsx::write.xlsx(data.frame(note='Synthetic metadata only'),cfg$kobo)
  # Exercise the actual entry point, not a second hand-written stage order.
  source('MAIN.R',local=environment())
  results <- readRDS('outputs/analysis/results.rds')
  stopifnot(nrow(results$results)>0,file.exists('outputs/presentation/report.html'),
    file.exists('outputs/presentation/dashboard_results.csv'))
  water <- readRDS('data/processed/indicators.rds')$main
  stopifnot(water$water_jmp_service[1]=='basic',water$water_jmp_service[2]=='basic',
    water$water_jmp_service[3]=='surface_water',is.na(water$water_jmp_service[4]),
    is.na(water$water_litres_collected_per_person_yesterday[4]))
  # Tampering with an upstream artifact must block later stages.
  cat('\ntampered',file='outputs/analysis/results_long.csv',append=TRUE)
  err <- tryCatch({validate_stage('present',cfg);NULL},error=identity)
  stopifnot(inherits(err,'error'))
  # Removing one duplicated UUID must cascade by parent index, preserving the other interview.
  duplicate <- readRDS('data/processed/prepared.rds')
  row <- duplicate$main[1,,drop=FALSE]; row[['_index']] <- '5'; row$record_id <- 'main::5'
  duplicate$main <- dplyr::bind_rows(duplicate$main,row)
  child <- duplicate$roster[1,,drop=FALSE]
  child[['_index']] <- '9'; child[['_parent_index']] <- '5'; child$record_id <- 'roster::5::9'
  duplicate$roster <- dplyr::bind_rows(duplicate$roster,child)
  saveRDS(duplicate,'data/processed/prepared.rds')
  decisions <- tibble::tibble(table='main',uuid='main::5',question=NA_character_,
    old_value=NA_character_,change_type='remove_survey',new_value=NA_character_,
    reason='Synthetic duplicate',reviewer='Synthetic test',approved='yes')
  write_csv(decisions,cfg$decisions); clean_data(cfg)
  cleaned <- readRDS('data/processed/clean.rds')
  stopifnot(nrow(cleaned$main)==4,nrow(cleaned$roster)==8,
    !'roster::5::9' %in% cleaned$roster$record_id)
  cat('Synthetic end-to-end pipeline passed. Fixture:',fixture,'\n')
})
