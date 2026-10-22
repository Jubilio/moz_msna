# Source from tests/run_tests.R after project functions have been loaded.
expect_error <- function(expr,pattern) {
  err <- tryCatch({force(expr);NULL},error=function(e)e)
  stopifnot(inherits(err,'error'),grepl(pattern,conditionMessage(err)))
}
# MAIN and the test harness must load the same real R files.
stopifnot(length(list.files('R',pattern='\\.R$'))>=10)
main_lines <- readLines('MAIN.R')
stopifnot(any(grepl("pattern='\\\\.R$'",main_lines,fixed=TRUE)))

# Unknown container selection never becomes zero water collected.
w_unknown <- w[1,,drop=FALSE]
for(size in c(10,15,20,25)) {
  w_unknown[[paste0('wash_container_size',size,'_time_int')]] <- NA_character_
  w_unknown[[paste0('wash_container_size/',size,'_litres')]] <- '0'
}
w_unknown[['wash_container_size/dnk']] <- '1'
stopifnot(is.na(water_indicators(w_unknown,cfg)$water_litres_collected_per_person_yesterday))
w_unknown[['wash_container_size/dnk']] <- '0'
w_unknown[['wash_container_size/20_litres']] <- '1'
w_unknown$wash_container_size20_time_int <- 'bad'
stopifnot(is.na(water_indicators(w_unknown,cfg)$water_litres_collected_per_person_yesterday))

# On-premises sources are configured explicitly; unknown is never on premises.
time <- tibble::tibble(wash_drinking_water_source=c('piped_dwelling','piped_compound',
  'piped_neighbour','borehole','borehole','dnk'),
  wash_drinking_water_time_int=c(NA,NA,NA,'30','31',NA))
tc <- cfg; tc$water_on_premises_sources <- c('piped_dwelling','piped_compound','piped_neighbour')
stopifnot(identical(water_time_category(time,tc),
  c('premises','premises','premises','under_30min','above_30min',NA_character_)),
  is.na(water_time_category(time,cfg)[2]))
# Explicit on-premises branch takes precedence over an irrelevant stale integer.
time$wash_drinking_water_time_yn <- c(NA,NA,NA,'water_in_plot','number_minutes',NA)
time$wash_drinking_water_time_int[4] <- NA_character_
stopifnot(water_time_category(time,tc)[4]=='premises')

# Binary and numeric-invalid responses have the same exclusion in estimates/denominators.
bin <- tibble::tibble(hh_id=letters[1:4],admin2='X',`q/a`=c('1','0','yes',NA))
den <- analysis_denominators(bin,'q/a','prop_select_one','admin2')
stopifnot(den$n_eligible==4,den$n_valid==2,den$n_missing==1,den$n_invalid==1)
rbin <- run_one_analysis(bin,'q/a','prop_select_one','admin2',cfg,'main')
stopifnot(all(rbin$n_unweighted_valid==2),abs(sum(rbin$stat)-1)<1e-8)
bin$value <- c('1','2','bad',NA)
den <- analysis_denominators(bin,'value','mean',NA_character_)
stopifnot(den$n_valid==2,den$n_invalid==1)

# Review rejects arbitrary choice strings and parent-only multiple edits.
map <- read_csv_text('config/column_map.csv'); choices <- read_csv_text('config/choices_dictionary.csv')
log <- tibble::tibble(table='main',uuid='main::1',question='wash_container_size',
  old_value='20_litres',change_type='change_response',new_value='10_litres')
expect_error(validate_decision_values(log,map,choices),'binary options')
log$question <- 'wash_drinking_water_source'; log$new_value <- 'made_up'
expect_error(validate_decision_values(log,map,choices),'canonical')

# Rebuilding the multiple parent preserves auditability and unknown siblings.
sm <- tibble::tibble(record_id='main::1',q='a',`q/a`='0',`q/b`='1')
mm <- tibble::tibble(table='main',name=c('q/a','q/b'),type='binary',option=c('a','b'))
ll <- tibble::tibble(uuid='main::1',question='q/b',change_type='change_response')
synced <- sync_multiple_choices(sm,ll,'main',mm)
stopifnot(synced$data$q=='b',nrow(synced$audit)==1)
sm[['q/a']] <- NA_character_
stopifnot(is.na(sync_multiple_choices(sm,ll,'main',mm)$data$q))

# Escape source labels before including them in the self-contained HTML report.
stopifnot(identical(html_escape('<script>&"'), '&lt;script&gt;&amp;&quot;'))
cat('Pipeline regression tests passed.\n')
# Blocker review is required, and a recorded disposition is explicit rather than automatic.
local({
  before <- getwd(); tmp <- tempfile('blocker-review-'); dir.create(tmp)
  on.exit({setwd(before);unlink(tmp,recursive=TRUE)},add=TRUE)
  setwd(tmp); dir.create('outputs/qa',recursive=TRUE)
  flags <- tibble::tibble(table='main',uuid='main::1',question='age',old_value='-1',
    issue='Invalid age',check_id='age',severity='blocker')
  retained <- list(main=tibble::tibble(record_id='main::1'))
  approved <- tibble::tibble(table=character(),uuid=character(),question=character(),change_type=character())
  expect_error(check_blocker_dispositions(flags,approved,retained,cfg),'unresolved blockers')
  approved <- tibble::tibble(table='main',uuid='main::1',question='age',change_type='no_action')
  check_blocker_dispositions(flags,approved,retained,cfg)
})
cat('Blocker disposition regression passed.\n')
# Weighted point estimates use the supplied household weights and retain a proper domain design.
weighted_cfg <- cfg; weighted_cfg$analysis_mode <- 'weighted'
weighted_cfg$sampling_design_confirmed <- TRUE
wd <- tibble::tibble(hh_id=letters[1:4],weight=c(1,1,3,3),value=c(1,3,5,7))
wr <- run_one_analysis(wd,'value','mean',NA_character_,weighted_cfg,'main')
stopifnot(abs(wr$stat-5)<1e-8,wr$n_unweighted_valid==4)
wd$weight[1] <- Inf
expect_error(make_design(wd,weighted_cfg),'weights')
cat('Weighted estimate regression passed.\n')
