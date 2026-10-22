food_indicators <- function(main,cfg) {
  fcs <- c(fsl_fcs_cereal='m3269_hh_fcs_staples',fsl_fcs_legumes='m3270_hh_fcs_pulses',
    fsl_fcs_dairy='m3271_hh_fcs_dairy',fsl_fcs_meat='m3272_hh_fcs_protein',
    fsl_fcs_veg='m3273_hh_fcs_veg',fsl_fcs_fruit='m3274_hh_fcs_fruits',
    fsl_fcs_oil='m3275_hh_fcs_fat',fsl_fcs_sugar='m3276_hh_fcs_sugar')
  rcsi <- c(fsl_rcsi_lessquality='fsl_rcsi_rely_on_less_expensive_food',
    fsl_rcsi_borrow='fsl_rcsi_borrow',fsl_rcsi_mealsize='fsl_rcsi_mealsize',
    fsl_rcsi_mealadult='fsl_rcsi_mealadult',fsl_rcsi_mealnb='fsl_rcsi_mealnb')
  assert_cols(main,c(unname(fcs),unname(rcsi)))
  x <- main
  for (v in names(fcs)) x[[v]] <- mask_days(main[[fcs[[v]]]])
  x <- append_humind(x,humind::add_fcs,cutoffs=cfg$fcs_cutoffs)
  for (v in names(rcsi)) x[[v]] <- mask_days(main[[rcsi[[v]]]])
  x <- append_humind(x,humind::add_rcsi)
  hhs <- c(fsl_hhs_nofoodhh='m2754_hh_hhs_no_food',fsl_hhs_nofoodhh_freq='m2755_hh_hhs_no_food_frequency',
    fsl_hhs_sleephungry='m2756_hh_hhs_sleep_hungry',fsl_hhs_sleephungry_freq='m2757_hh_hhs_sleep_hungry_frequency',
    fsl_hhs_alldaynight='m2758_hh_hhs_no_day_night',fsl_hhs_alldaynight_freq='m2759_hh_hhs_no_day_night_frequency')
  assert_cols(main,unname(hhs)); h <- tibble::as_tibble(main[,unname(hhs)])
  names(h) <- names(hhs); valid <- rep(TRUE,nrow(h))
  for (i in c(1,3,5)) {
    yn <- h[[i]]; freq <- h[[i+1]]
    valid <- valid & yn %in% c('yes','no') &
      ((yn=='no' & is.na(freq)) | (yn=='yes' & freq %in% c('1','2','3')))
    h[[i+1]] <- c('rarely','sometimes','often')[match(freq,c('1','2','3'))]
  }
  # Compute only valid rows; inconsistent responses remain NA and are already flagged.
  for (v in c('fsl_hhs_score','fsl_hhs_cat','fsl_hhs_cat_ipc')) x[[v]] <-
    if(v=='fsl_hhs_score') NA_real_ else NA_character_
  if (any(valid)) {
    hh <- humind::add_hhs(h[valid,,drop=FALSE])
    for (v in c('fsl_hhs_score','fsl_hhs_cat','fsl_hhs_cat_ipc')) x[[v]][valid] <- hh[[v]]
  }
  # Preserve the local strategy set rather than silently dropping the extra items.
  lc <- read_csv_text('config/lcsi_strategies.csv'); assert_cols(main,lc$variable)
  a <- do.call(cbind,lapply(lc$variable,function(v) as.character(main[[v]])))
  used_or_exhausted <- a %in% c('30','20'); dim(used_or_exhausted) <- dim(a)
  used_only <- a %in% '30'; dim(used_only) <- dim(a)
  severity <- matrix(rep(as.integer(lc$severity),each=nrow(a)),nrow=nrow(a))
  known <- matrix(a %in% c('10','20','30','9999'),nrow=nrow(a))
  aggregate_lc <- function(selected) {
    scores <- ifelse(selected,severity,0); scores[!known] <- NA_real_
    apply(scores,1,function(z) if(all(is.na(z))) NA_real_ else max(z,na.rm=TRUE))
  }
  lc_score <- aggregate_lc(used_or_exhausted)
  lc_used_score <- aggregate_lc(used_only)
  x$lcsi_local_score <- lc_score
  x$lcsi_local_category <- c('None','Stress','Crisis','Emergency')[lc_score+1]
  x$lcsi_local_used_score <- lc_used_score
  x$lcsi_local_used_category <- c('None','Stress','Crisis','Emergency')[lc_used_score+1]
  x$lcsi_local_missing_items_n <- rowSums(!known)
  # FCM needs no LCSI. FCLCM/composite require explicit validation of the local set.
  x <- append_humind(x,humind::add_fcm_phase)
  if (isTRUE(cfg$local_lcsi_mapping_confirmed)) {
    # Guidance 2026, p.12: FCLC uses strategies USED in the last 30 days only.
    x$fsl_lcsi_cat <- x$lcsi_local_used_category
    x <- append_humind(x,humind::add_fclcm_phase)
    x <- append_humind(x,humind::add_comp_foodsec)
  }
  for (p in list(c('export_fcs_score','fsl_fcs_score'),c('export_rcsi_score','fsl_rcsi_score'),
                 c('export_hhs_score','fsl_hhs_score'))) {
    if (p[1] %in% names(x)) x[[paste0(p[2],'_export_mismatch')]] <-
      !is.na(num(x[[p[1]]])) & !is.na(x[[p[2]]]) & abs(num(x[[p[1]]])-x[[p[2]]])>1e-8
  }
  x
}

water_indicators <- function(main,cfg) {
  x <- main; sizes <- c(10,15,20,25)
  vars <- paste0('wash_container_size',sizes,'_time_int'); assert_cols(x,c(vars,'hh_size'))
  counts <- matrix(NA_real_,nrow(x),length(sizes))
  for (j in seq_along(sizes)) {
    v <- vars[j]; selected <- paste0('wash_container_size/',sizes[j],'_litres'); assert_cols(x,selected)
    raw <- num(x[[v]]); flag <- num(x[[selected]])
    # Zero only for an explicitly unselected container with a blank count.
    counts[,j] <- ifelse(flag==0 & is.na(raw),0,raw)
    counts[!is.na(counts[,j]) & (counts[,j]<0 | counts[,j]!=floor(counts[,j])),j] <- NA_real_
    # Missing response to the container question remains unknown, not zero litres.
    counts[is.na(flag),j] <- NA_real_
  }
  x$water_litres_collected_yesterday <- rowSums(sweep(counts,2,sizes,'*'),na.rm=FALSE)
  hh <- num(x$hh_size); hh[is.na(hh)|hh<=0|hh!=floor(hh)] <- NA_real_
  if('hh_size_analysis_valid' %in% names(x)) hh[!x$hh_size_analysis_valid] <- NA_real_
  x$water_litres_collected_per_person_yesterday <- x$water_litres_collected_yesterday/hh
  x$water_collection_below_15l <- ifelse(is.na(x$water_litres_collected_per_person_yesterday),
    NA_character_,ifelse(x$water_litres_collected_per_person_yesterday<15,'below_15','15_or_more'))
  # Collection yesterday is a proxy. It is not a measured daily consumption indicator or HWISE.
  x <- append_humind(x,humind::add_drinking_water_source_cat)
  tm <- num(x$wash_drinking_water_time_int); tm[tm<0] <- NA_real_
  x$water_collection_time_over_30min <- ifelse(is.na(tm),NA_character_,ifelse(tm>30,'over_30','30_or_less'))
  cat <- x$wash_drinking_water_source_cat
  x$water_jmp_service <- dplyr::case_when(
    cat=='surface_water' ~ 'surface_water',cat=='unimproved' ~ 'unimproved',
    cat=='improved' & tm>30 ~ 'limited',cat=='improved' & tm<=30 ~ 'basic',
    TRUE ~ NA_character_)
  # Safely managed cannot be inferred without availability and water quality testing.
  map <- read_csv_text('config/sanitation_mapping.csv')
  sanitation <- map$class[match(x$wash_sanitation_facility,map$code)]
  if (!isTRUE(cfg$sanitation_mapping_confirmed)) sanitation[] <- NA_character_
  x$sanitation_jmp_service <- dplyr::case_when(
    sanitation=='none' ~ 'open_defecation',sanitation=='unimproved' ~ 'unimproved',
    sanitation=='improved' & x$wash_sanitation_facility_sharing_yn=='yes' ~ 'limited',
    sanitation=='improved' & x$wash_sanitation_facility_sharing_yn=='no' ~ 'basic',TRUE ~ NA_character_)
  x
}

education_indicators <- function(loop,cfg) {
  x <- loop; assert_cols(x,c('edu_ind_age','edu_access','edu_level_grade','edu_ind_gender'))
  x$age <- num(x$edu_ind_age)
  x$eligible_education <- !is.na(x$age) & x$age>=cfg$education_age_min & x$age<=cfg$education_age_max
  x$education_access <- ifelse(x$eligible_education & x$edu_access %in% c('yes','no'),x$edu_access,NA_character_)
  x$education_oos <- ifelse(is.na(x$education_access),NA_character_,
    ifelse(x$education_access=='no','out_of_school','accessing_education'))
  x$gender <- x$edu_ind_gender
  # Grade comes from the XML code, never from the inconsistent English cycle label.
  x$grade_numeric <- num(sub('^grade_','',x$edu_level_grade))
  x$grade_numeric[!grepl('^grade_[0-9]+$',x$edu_level_grade) %in% TRUE] <- NA_real_
  map <- read_csv_text('config/education_grades.csv')
  if (isTRUE(cfg$education_cycle_mapping_confirmed)) {
    ii <- match(x$edu_level_grade,map$code)
    x$education_cycle <- map$cycle[ii]
    intended <- num(map$intended_age[ii])
    x$education_overage <- ifelse(x$education_access=='yes' & !is.na(intended),
      ifelse(x$age-intended>=2,'overage_2plus','not_overage'),NA_character_)
  }
  # Raw interruption/barrier binary options are analysed on their actual question denominator.
  x
}

health_indicators <- function(loop,main) {
  x <- loop; assert_cols(x,c('health_ind_healthcare_needed','health_ind_healthcare_received'))
  need <- x$health_ind_healthcare_needed; received <- x$health_ind_healthcare_received
  x$health_access_category <- dplyr::case_when(need=='no' ~ 'no_need',
    need=='yes' & received=='yes' ~ 'needed_met',need=='yes' & received=='no' ~ 'needed_unmet',
    TRUE ~ NA_character_)
  x$health_unmet_among_need <- ifelse(need=='yes' & received %in% c('yes','no'),
    ifelse(received=='no','unmet','met'),NA_character_)
  counts <- x |> dplyr::group_by(hh_id) |> dplyr::summarise(
    health_loop_n=dplyr::n(),health_loop_unknown_n=sum(is.na(health_access_category)),
    health_ind_healthcare_needed_no_n=sum(health_access_category=='no_need',na.rm=TRUE),
    health_ind_healthcare_needed_yes_met_n=sum(health_access_category=='needed_met',na.rm=TRUE),
    health_ind_healthcare_needed_yes_unmet_n=sum(health_access_category=='needed_unmet',na.rm=TRUE),.groups='drop')
  hh <- dplyr::left_join(main,counts,by='hh_id',relationship='one-to-one')
  needhh <- hh$health_hh_any_member_required_healthcare
  valid_size <- !is.na(num(hh$hh_size)) & num(hh$hh_size)>0 & num(hh$hh_size)==floor(num(hh$hh_size))
  if('hh_size_analysis_valid' %in% names(hh)) valid_size <- valid_size & hh$hh_size_analysis_valid
  complete <- (needhh=='no' & valid_size) |
    (needhh=='yes' & valid_size & hh$health_loop_unknown_n==0 & hh$health_loop_n==num(hh$hh_size))
  complete[is.na(complete)] <- FALSE
  for (v in c('health_ind_healthcare_needed_no_n','health_ind_healthcare_needed_yes_met_n',
              'health_ind_healthcare_needed_yes_unmet_n')) {
    if(v=='health_ind_healthcare_needed_no_n') hh[[v]][needhh %in% 'no'] <- num(hh$hh_size[needhh %in% 'no'])
    else {
      # A structural skip can establish no need; do not erase contradictory observed needs.
      replace <- needhh %in% 'no' & is.na(hh[[v]])
      hh[[v]][replace] <- 0
    }
  }
  vars <- c('health_ind_healthcare_needed_no_n','health_ind_healthcare_needed_yes_met_n',
            'health_ind_healthcare_needed_yes_unmet_n')
  # The guidance uses the maximum observed severity, even when other members are unknown.
  # An all-unknown household stays unclassified. Coverage remains explicitly separate.
  observed <- vapply(seq_len(nrow(hh)),function(i)
    any(num(unlist(hh[i,vars]))>0,na.rm=TRUE),logical(1))
  for (v in c('comp_health_score','comp_health_in_need','comp_health_in_severe_need')) hh[[v]] <- NA_real_
  if (any(observed)) {
    comp <- humind::add_comp_health(hh[observed,,drop=FALSE])
    for (v in c('comp_health_score','comp_health_in_need','comp_health_in_severe_need')) hh[[v]][observed] <- comp[[v]]
  }
  hh$health_composite_complete <- complete
  hh$health_composite_partial <- !complete & !is.na(hh$comp_health_score)
  list(loop=x,main=hh)
}

compose_indicators <- function(cfg) {
  tables <- readRDS('data/processed/clean.rds')
  main <- food_indicators(tables$main,cfg)
  roster <- tables$roster |> dplyr::count(hh_id,name='roster_n')
  main <- dplyr::left_join(main,roster,by='hh_id',relationship='one-to-one')
  main$hh_size_analysis_valid <- !is.na(main$roster_n) & !is.na(num(main$hh_size)) &
    num(main$hh_size)>0 & num(main$hh_size)==main$roster_n
  main <- water_indicators(main,cfg)
  health <- health_indicators(tables$health_ind,main); main <- health$main
  tables$main <- main; tables$health_ind <- health$loop
  main <- household_disaggregation(main)
  tables$main <- main
  tables$edu_ind <- education_indicators(tables$edu_ind,cfg)
  gaps <- read_csv_text('config/methodology_gaps.csv')
  write_csv(gaps,'outputs/qa/methodology_gaps.csv')
  # p.6: publish coverage by the disaggregation plan, including wholly absent sectors.
  sectors <- c('comp_edu_score','comp_foodsec_score','comp_health_score',
               'comp_prot_score','comp_snfi_score','comp_wash_score')
  coverage_keys <- unique(c('hh_id','admin1','admin2','admin3','population_group',
                           'hoh_gender_final','hoh_age_category'))
  coverage <- main[,intersect(coverage_keys,names(main)),drop=FALSE]
  for(v in sectors) coverage[[v]] <- if(v %in% names(main)) num(main[[v]]) else rep(NA_real_,nrow(main))
  coverage$available_sector_count <- rowSums(!is.na(coverage[,sectors]))
  coverage$missing_sector_count <- length(sectors)-coverage$available_sector_count
  coverage$observed_severity_lower_bound <- apply(as.matrix(coverage[,sectors]),1,
    function(z) if(all(is.na(z))) NA_real_ else max(z,na.rm=TRUE))
  coverage$reported_severity_lower_bound <- ifelse(coverage$observed_severity_lower_bound==5,
    '4+',as.character(coverage$observed_severity_lower_bound))
  coverage$interpretation <- 'Maximum of available composites; coverage diagnostic, not an approved full MSNI'
  saveRDS(coverage,'data/processed/sector_coverage.rds')
  if (isTRUE(cfg$calculate_full_msni)) stop(
    'Full MSNI blocked: missing HWISE, lighting and non-equivalent Protection inputs. ',
    'Do not substitute zero scores. Review outputs/qa/methodology_gaps.csv.')
  saveRDS(tables,'data/processed/indicators.rds')
}
