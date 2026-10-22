household_disaggregation <- function(main) {
  assert_cols(main,c('resp_hoh_yn','resp_gender','hoh_gender','resp_age','hoh_age'))
  x <- main
  # Use respondent characteristics only when the respondent is explicitly the household head.
  x$hoh_gender_final <- dplyr::case_when(
    x$resp_hoh_yn=='yes' & x$resp_gender %in% c('male','female') ~ x$resp_gender,
    x$resp_hoh_yn=='no' & x$hoh_gender %in% c('male','female') ~ x$hoh_gender,
    TRUE ~ NA_character_)
  x$hoh_age_final <- dplyr::case_when(
    x$resp_hoh_yn=='yes' ~ num(x$resp_age),
    x$resp_hoh_yn=='no' ~ num(x$hoh_age),TRUE ~ NA_real_)
  bad <- !is.na(x$hoh_age_final) & (x$hoh_age_final<0 | x$hoh_age_final>120 |
                                   x$hoh_age_final!=floor(x$hoh_age_final))
  x$hoh_age_final[bad] <- NA_real_
  x$hoh_age_category <- dplyr::case_when(
    x$hoh_age_final<18 ~ 'below_18',
    x$hoh_age_final>=18 & x$hoh_age_final<60 ~ '18_to_59',
    x$hoh_age_final>=60 ~ '60_and_above',TRUE ~ NA_character_)
  # Keep the reported total amount separate from source-specific percentage shares.
  if('cm_income_source_total_prop' %in% names(x)) {
    x$income_reported_total_30days <- num(x$cm_income_source_total_prop)
    x$income_reported_total_30days[x$income_reported_total_30days<0] <- NA_real_
  }
  # The template's "income percentiles" heading actually lists fractions of the median.
  # No quintiles or per-source monetary amounts are inferred from percentage shares.
  x
}
