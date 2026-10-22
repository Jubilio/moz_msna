if(!file.exists('config/project.R')) stop('Run from the project root.')
source('config/project.R')
for(p in sort(list.files('R',pattern='\\.R$',full.names=TRUE))) source(p)
stopifnot(identical(mask_days(c('0','7','8','-1','1.5','bad')),c(0,7,NA_real_,NA_real_,NA_real_,NA_real_)))
# Whitespace normalization is safe; shifted or genuinely different columns remain blocked.
sc <- schema_comparison(c('Province','  District\n','What is the head of household’s gender?'),
 c('Province','District',"What is the head of household's gender?"),'main')
stopifnot(all(sc$equivalent_after_normalization))
sc <- schema_comparison(c('District','Province'),c('Province','District'),'main')
stopifnot(!any(sc$equivalent_after_normalization))
sc <- schema_comparison(c('Province'),c('Province','District'),'main')
stopifnot(!sc$equivalent_after_normalization[2])
# Water: blank unselected container = zero, selected missing count = unknown.
w <- tibble::tibble(hh_size=c('4','4','0'),wash_drinking_water_time_int=c('30','31',NA),
 wash_drinking_water_source=c('borehole','surface_water','borehole'),
 wash_sanitation_facility=c('pit_latrine_slab','pit_latrine_wo_slab','open_defecation'),
 wash_sanitation_facility_sharing_yn=c('no','yes','no'))
for(size in c(10,15,20,25)) {
 w[[paste0('wash_container_size',size,'_time_int')]] <- if(size==20)c('3',NA,'1') else rep(NA_character_,3)
 w[[paste0('wash_container_size/',size,'_litres')]] <- if(size==20)rep('1',3) else rep('0',3)
}
ww <- water_indicators(w,cfg)
stopifnot(ww$water_litres_collected_per_person_yesterday[1]==15,
 is.na(ww$water_litres_collected_per_person_yesterday[2]),
 is.na(ww$water_litres_collected_per_person_yesterday[3]),ww$water_jmp_service[1]=='basic',
 all(is.na(ww$sanitation_jmp_service)))
# Health: unmet need known; missing/partial loops cannot imply absence of needs.
main <- tibble::tibble(hh_id=c('a','b','c'),hh_size=c('2','2','2'),
 health_hh_any_member_required_healthcare=c('yes','yes','no'))
h <- tibble::tibble(hh_id=c('a','a','b'),health_ind_healthcare_needed=c('yes','no','yes'),
 health_ind_healthcare_received=c('no',NA,'yes'))
hh <- health_indicators(h,main)$main
stopifnot(hh$comp_health_score[1]==3,hh$comp_health_score[2]==2,
 hh$health_composite_partial[2],hh$comp_health_score[3]==1)
# Education: explicitly distinguish 17-year-old eligible child from 18-year-old.
e <- tibble::tibble(edu_ind_age=c('17','18',NA),edu_access=c('yes','no','yes'),
 edu_level_grade=c('grade_7','grade_8',NA),edu_ind_gender=c('male','female','male'))
ee <- education_indicators(e,cfg)
stopifnot(ee$eligible_education[1],!ee$eligible_education[2],ee$grade_numeric[1]==7,
 is.na(ee$education_access[2]))
# Food complete zero recall and HHS no/structural skips; inconsistent HHS stays missing.
f <- tibble::tibble(hh_id=c('a','b','c'))
fm <- c('m3269_hh_fcs_staples','m3270_hh_fcs_pulses','m3271_hh_fcs_dairy',
 'm3272_hh_fcs_protein','m3273_hh_fcs_veg','m3274_hh_fcs_fruits','m3275_hh_fcs_fat','m3276_hh_fcs_sugar')
for(v in fm) f[[v]] <- c('0','7',NA_character_)
for(v in c('fsl_rcsi_rely_on_less_expensive_food','fsl_rcsi_borrow','fsl_rcsi_mealsize',
 'fsl_rcsi_mealadult','fsl_rcsi_mealnb')) f[[v]] <- c('0','7',NA_character_)
for(v in c('m2754_hh_hhs_no_food','m2756_hh_hhs_sleep_hungry','m2758_hh_hhs_no_day_night'))
 f[[v]] <- c('no','yes','no')
for(v in c('m2755_hh_hhs_no_food_frequency','m2757_hh_hhs_sleep_hungry_frequency',
 'm2759_hh_hhs_no_day_night_frequency')) f[[v]] <- c(NA_character_,'3','1')
lc <- read_csv_text('config/lcsi_strategies.csv')
for(v in lc$variable) f[[v]] <- c('10','30',NA_character_)
ff <- food_indicators(f,cfg)
stopifnot(ff$fsl_fcs_score[1]==0,ff$fsl_rcsi_score[2]==56,ff$fsl_hhs_score[1]==0,
 ff$fsl_hhs_score[2]==6,is.na(ff$fsl_hhs_score[3]),ff$lcsi_local_category[2]=='Emergency',
 is.na(ff$lcsi_local_category[3]),!'comp_foodsec_score' %in% names(ff))
# Exhausted emergency strategy affects general LCSI but must not drive USED-only FCLC.
f$hh_lcsfs_sold_house[1] <- '20'
ff_exhausted <- food_indicators(f,cfg)
stopifnot(ff_exhausted$lcsi_local_category[1]=='Emergency',
 ff_exhausted$lcsi_local_used_category[1]=='None')
# IMPACT package integration: preserve known denominator and grouping key.
d <- tibble::tibble(hh_id=c('a','b','c','d'),admin2=c('X','X','Y','Y'),
 value=c('yes','no','yes',NA_character_))
rr <- run_one_analysis(d,'value','prop_select_one','admin2',cfg,'main')
stopifnot(all(is.na(rr$stat_low)),all(rr$n_unweighted_valid[rr$group_var_value=='X']==2),
 all(rr$n_unweighted_valid[rr$group_var_value=='Y']==1))
# Derive head characteristics with branch-aware respondent substitution.
heads <- tibble::tibble(resp_hoh_yn=c('yes','no',NA_character_),
 resp_gender=c('female','male','male'),hoh_gender=c(NA_character_,'female','female'),
 resp_age=c('17','35','40'),hoh_age=c(NA_character_,'60','50'))
heads <- household_disaggregation(heads)
stopifnot(identical(heads$hoh_age_category,c('below_18','60_and_above',NA_character_)),
 identical(heads$hoh_gender_final,c('female','female',NA_character_)))
# Verify all 45 HICM cells against the user-supplied workbook's embedded matrix.
reference <- read_csv_text('config/hicm_reference_cells.csv')
cases <- tibble::tibble(fsl_fcs_cat=reference$fcs_category,
 fsl_rcsi_cat=reference$rcsi_category,fsl_hhs_cat_ipc=reference$hhs_category)
actual <- humind::add_fcm_phase(cases)
stopifnot(identical(actual$fsl_fc_phase,paste0('Phase ',reference$expected_phase,' FC')))
reference_lc <- read_csv_text('config/fclcm_reference_cells.csv')
cases_lc <- tibble::tibble(fsl_fc_phase=paste0('Phase ',reference_lc$fc_phase,' FC'),
 fsl_lcsi_cat=reference_lc$lcsi_category)
actual_lc <- humind::add_fclcm_phase(cases_lc)
stopifnot(identical(actual_lc$fclcm_phase,paste0('Phase ',reference_lc$expected_phase,' FCLC')))
cat('All synthetic boundary and IMPACT integration tests passed.\n')

source('tests/pipeline_regressions.R')
