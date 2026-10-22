# Mozambique MSNA 2026: cleaning and analysis

An R project configured for `REACH_MOZ_Data_MSNA_01102026.xlsx` and `kobo-used.xlsx`. It follows the Check → Create → Review workflow of IMPACT cleaningtools and uses analysistools for survey summaries and humind for supported humanitarian indicators. This is an implementation based on the public package interfaces, not a claim of institutional methodological approval.

## Start

1. Extract the project and open `MOZ_MSNA_2026.Rproj` in RStudio.
2. Copy the original dataset into `data/raw/`. Copy the matching Kobo form to `data/raw/kobo-used.xlsx`. Keep the original files unchanged.
3. Install dependencies once:

```r
source('scripts/install_dependencies.R')
```

4. Validate the installed environment:

```r
source('tests/run_tests.R')
```

5. In the terminal, run the pipeline:

```sh
Rscript MAIN.R all
```

The 117 MB input workbook can take several minutes to import. Run individual stages to avoid reading it again:

```sh
Rscript MAIN.R prepare
Rscript MAIN.R checks
Rscript MAIN.R clean
Rscript MAIN.R indicators
Rscript MAIN.R analyse
Rscript MAIN.R review
Rscript MAIN.R present
```

## Structure

`MAIN.R` orchestrates seven stages: import and normalization, checks, reviewed cleaning, indicators, analysis, result review and presentation. See [the operating guide](docs/PIPELINE.md) for decisions, WASH adaptation, education, cache integrity and outputs. Configuration lives in `config/`. Raw files remain in `data/raw/`, cached data in `data/processed/`, and results in `outputs/`. No survey responses are included in this distribution.

`column_map.csv` maps every source header by exact sheet and column position. Headers are validated before import. Label normalization is a separate preparation step, documented through the dictionary and input hashes; it does not overwrite the source workbook. Original exported scores are retained with an `export_` prefix. One truncated Protection option is preserved as an unmapped raw column; see `docs/unmapped_columns.csv`. This particular option is not included in the automatic descriptive analysis until its mapping is resolved.

Import repair: headers are compared after normalizing case, whitespace/line breaks and apostrophe style. `read_excel()` now disables automatic whitespace trimming. Column number and order are still checked; a real change is not bypassed. Every imported sheet writes `outputs/qa/schema_comparison_<table>.csv` with the expected and actual headers. If import still stops, inspect rows marked FALSE in that report before changing the map. This diagnostic is required to identify a real file-version or header change; the generic error alone cannot establish the cause.

## Cleaning workflow

`checks` generates `outputs/qa/cleaning_flags.csv` and a flag summary. It uses cleaningtools duplicate, logic and FCS checks, plus explicit local numeric, recall, HHS and parent-link checks. Flags are questions for review, not automatic corrections.

Record-level decisions belong in `config/cleaning_decisions.csv`. `uuid` in this log is the immutable **record_id**, not the household UUID: `main::<_index>` or `<table>::<_parent_index>::<_index>`. This lets you correct one child without changing all siblings. Use the mapped XML question name. Supported actions are `change_response`, `blank_response`, `remove_survey` and `no_action`. Approved decisions require `approved=yes`, a reviewer, reason and current old_value. Use canonical Kobo codes in new_value, not translated labels. Conflicting or stale cell decisions stop execution.

`clean` validates decisions, calls cleaningtools `review_cleaning_log()` when response changes exist and `create_clean_data()`, then audits applied values and deletions. Household removals cascade to children and have a separate removal log. Missing/duplicate household identifiers and inconsistent parent links stop analysis. Raw data are never overwritten and routine reruns never overwrite decisions. Core identifiers cannot be changed through a cell decision.

FCS/rCSI values outside 0–7 integer days and inconsistent HHS responses remain missing in the analysis copy; they are flagged rather than clipped or repaired. Invalid or incomplete records do not silently become zero scores. Additional context-specific rules can be added in `R/02_checks.R`. Kobo relevancies are retained for reference; the project does not claim to evaluate every XLSForm expression or every sectoral plausibility rule.

## Indicators and limitations

Enabled: FCS (normal thresholds by default), rCSI, HHS, food consumption matrix, provisional local LCSI, water collected yesterday per person, drinking-water service classification, individual education access/out-of-school/grade/barrier/disruption outputs, individual health access and a health composite reflecting the maximum observed individual severity. Health coverage is reported separately: incomplete information can still establish a minimum observed severity, but cannot establish the absence of unobserved higher needs. An all-unknown household stays unclassified. Descriptive Nutrition, SNFI/HLP, Protection and AAP outputs use the actual collected questions.

Local LCSI uses the eleven strategies explicitly labelled Stress/Crisis/Emergency in the supplied tool. The two additional wild-food/begging items are reported descriptively and listed as unclassified. Confirm their roles and the final local strategy set with the sector team. `lcsi_local_category` includes used or exhausted strategies for general descriptive analysis. `lcsi_local_used_category` considers only strategies used in the past 30 days; this is the input for FCLC, in accordance with the 2026 guidance (p.12). The number of missing strategy responses is retained; category maxima ignore missing items and represent observed lower bounds, while all-unknown inputs remain NA. `local_lcsi_mapping_confirmed=FALSE` blocks FCLCM and the Food Security sectoral composite until reviewed.

Water collected yesterday is **not measured water consumption**. A blank count becomes zero only when its container option is explicitly unselected; missing selected counts remain unknown. The 15 L comparison is a descriptive proxy. It cannot replace HWISE-4. Collection time/source support basic/limited/unimproved/surface-water categories; safely managed service cannot be established from this dataset.

Sanitation categories are disabled until the mapping is reviewed. Emergency/communal latrines, generic flush latrines and adapted chairs do not identify a complete JMP technology. Grade/cycle/overage outputs are disabled until the Mozambique ISCED mapping is completed in `config/education_grades.csv`: the English and Portuguese labels differ for some grades. Default school-age analysis is 5–17, while the Kobo flag covers 5–18. Ages are ages at interview; school-year age correction requires a reliable interview date and agreed academic-year settings.

The full MSNI is explicitly blocked for this unreviewed country framework: HWISE-4, lighting and equivalent Protection dimensions are not complete. Education composite adaptations also need review. This is a methodological readiness guard, not a rule that every household needs six non-missing scores. The official guidance (p.6) permits the maximum of available scores with missing values ignored; it also requires discussion and agreement of framework deviations with the RSU. Core questions absent from the questionnaire must not be equated with an isolated missing household response. `config/methodology_gaps.csv` documents these limitations. Setting `calculate_full_msni=TRUE` stops with a clear message; it does not produce an unofficial reduced-sector score under the MSNI name. `sector_coverage.rds` retains each available/absent composite and the observed severity lower bound strictly as a coverage diagnostic.

The guidance requires four metrics: households in need (score >=3), households in severe need (>=4), average number of sectoral needs and sectoral need profiles. These become official MSNI outputs only after the country framework is validated. Numeric score 5 in humind is reported as **4+**, not as IPC/JIAF catastrophic Phase 5. Households outside a confirmed indicator universe (e.g. no school-age child) receive severity 1 for that indicator; unknown eligibility remains unknown. Education composite implementation remains gated while its local mappings are unresolved.

## Disaggregation, weights and denominators

Edit `cfg$groups` in `config/project.R`. A value `'admin1, admin2'` means the joint grouping; separate entries produce separate tables. Admin2 and population-group combinations are included by default. Education also includes sex disaggregation. School-age access uses children 5–17 with valid access responses; health unmet need uses individuals reporting need and valid receipt responses. Structural skips remain outside their question denominator. Individual sample sizes and distinct household counts are kept separate.

The supplied June 2026 MSNI workbook also specifies head-of-household sex and age disaggregations. The project derives `hoh_gender_final` and `hoh_age_category` (below 18, 18–59, 60+) using respondent characteristics only if the respondent is explicitly the head. These groupings are now included in the default configuration and inherited by individual rosters. If retaining an older project.R, add `'hoh_gender_final'` and `'hoh_age_category'` to cfg$groups manually. Unknown head status does not silently fall back to respondent characteristics.

The income heading in the reference is labelled "percentiles", but its categories are fractions of median income. These are different measures. Local source-specific `_prop` fields are percentage shares, while `cm_income_source_total_prop` asks a total amount in local currency. The latter is preserved separately as `income_reported_total_30days`. No per-source amounts, quintiles or median-based categories are automatically inferred before the intended income measure is confirmed.

The default is **unit-weight descriptive analysis**. No population confidence intervals are published. Results retain IMPACT fields and add independently counted valid denominators, household counts and a small-sample flag (default n<30; a reporting flag, not an automatic deletion rule). `denominators.csv` also retains eligible, valid and missing counts. Numeric means/medians and categorical proportions use their valid-response denominators.

For population inference, obtain final weights and the sample design from IOM. Fill `config/weights.csv` with unique hh_id and positive weight, choose weighted mode, specify mapped PSU/strata column names as applicable, and confirm sampling_design_confirmed. Weights are joined once at household level and inherited by loops. Where no PSU is specified, the household ID is the cluster identifier, including for children. Weighted individual estimates are individual-level estimates, not household proportions. The project does not invent population totals or calculate weights from district names alone. For a validated probability-sampling frame, IMPACT `analysistools::add_weights()` is available upstream; use it only with an appropriate design and frame.

## Outputs

- QA flags, applied cleaning log, cascade removals and methodology gaps.
- Sectoral missingness counts and proportions by the configured administrative/population disaggregations, including entirely absent sectoral composites. These are unweighted household coverage diagnostics and are included in the result workbook.
- Prepared, cleaned and indicator RDS files with original and derived fields separated.
- `results_long.csv`, `denominators.csv`, `results.rds` and `MOZ_MSNA_2026_results.xlsx`.
- Input hashes, record inventory, configuration and R session information.

These are local analytical outputs, not publication-ready anonymized datasets. Raw data, decisions, weights and generated outputs are excluded from git. Audit-duration and enumerator-performance checks require source metadata/audit files absent from this export.

## Reproducibility and validation

Installer repair: run the entire installer with `source('scripts/install_dependencies.R')`, not selected lines. The installer now sets `restart=FALSE` when initializing renv and uses `renv::install()` directly for CRAN and GitHub packages, so it does not need `remotes` or console objects named `cran`/`refs`. It reuses an already initialized project and prints success only after dependency and lockfile verification. If you ran the previous installer partially, restart R once, then source this corrected file from the project root.

Direct IMPACT dependencies are pinned in the installer. After successful installation it creates an actual `renv.lock`; subsequent sessions can use `renv::restore()`. No fabricated lockfile is supplied. Record the resolved lockfile with the project after installation.

Validation status and the exact executed checks are recorded in `docs/validation.json`. Run both `Rscript tests/run_tests.R` and `Rscript tests/synthetic_pipeline.R` after installation. The synthetic workflow does not validate the real survey data or approve the country methodology.

## Source references

### June framework workbook review

`config/framework_requirements.csv` compares 102 question references against the local schema and records exact-code matches, renamed inputs, adaptations, unavailable inputs and optional alternate modules. These counts describe schema coverage, not completed questionnaires or a count of mandatory missing indicators. `docs/framework_reference_warnings.csv` records five reference issues, including shifted sanitation code/label rows, an Education attack-omission note that conflicts with the local Kobo, income category terminology and older package references. The workbook's text approval note is not treated as proof that a deviation applies to the Mozambique assessment.

All 45 cells of its embedded Food Consumption matrix were checked against the phase lists in pinned humind v2026.4.1. The 20 Food Consumption/Livelihood Coping cells using its first four livelihood categories also match the pinned function's logic. Their reference fixtures are included in `config/hicm_reference_cells.csv` and `config/fclcm_reference_cells.csv`, with runtime tests in `tests/run_tests.R`. The example's fifth livelihood-coping category requires a contextual near-exhaustion definition and is not invented by this project. Static cross-reference checks are recorded in `docs/framework_comparison.json`; runtime validation status is recorded in `docs/validation.json`.

The June companion workbook does not override the September guidance automatically. Both sources are retained in the methodological references, and apparent conflicts are documented instead of silently translated into scoring rules.

- https://github.com/impact-initiatives/cleaningtools (commit f917e607115cb9da255d09039965a06bae1736e5)
- https://github.com/impact-initiatives/analysistools (commit d748f320c41eeaa5619be45457414ffb4f53e78a)
- https://impact-initiatives-hppu.github.io/humind/ (v2026.4.1)
- https://github.com/martivit/EduAnalysisTool (education workflow reference; no third-party code copied)
- User-supplied `REACH_2026_MSNI-guidance_20260924 (1).pdf`: pp.3 and 6 (max aggregation, missingness and subsets), p.12 (used-only LCSI for FCLC), pp.14-15 (Health), pp.16-17 (WASH), p.23 (Protection deviations), p.26 (four MSNI metrics). This primary guidance takes precedence over outdated explanatory text in package documentation.
- User-supplied `REACH_MSNA_2026_MSNI_EN_20260626.xlsx`: Disaggregation, Food Security (including both embedded matrices), Education, Protection, SNFI, WASH and Health worksheets. Reference issues are recorded with exact sheet/cell locations in `docs/framework_reference_warnings.csv`.

Read the MSNI Guidance Note 2026 and coordinate indicator adaptations with the Global MSNA/sector teams. The project implements the feasible analysis, keeps methodological decisions explicit and does not infer why any question was omitted.


## Reviewed pipeline and presentation

Blocker flags require an approved disposition before cleaning by default. Correct select-multiple binary options; parent-only changes are rejected and touched parents are rebuilt with an audit. Duplicate interview removals cascade by parent index. A shared validity rule feeds estimates and denominator reports. Successful stages have input/output hash records, preventing stale cached outputs from being reused.

WASH collection time handles confirmed on-premises sources without inventing missing questionnaire fields. Unknown/refused container selections remain unknown litres. The configurable source list defaults to `piped_dwelling`; compound/neighbour require confirmation against the local questionnaire. Education can optionally correct age using valid interview dates and confirmed school-year settings; no fallback dates are invented.

`review` checks result integrity before `present` produces labelled Excel tables (including IMPACT `presentresults` wide tables), a dashboard CSV and `outputs/presentation/report.html` with PNG charts, review status and methodological gaps. No raw record data is embedded in the report. Question and indicator labels can use English or Portuguese.

See [docs/PIPELINE.md](docs/PIPELINE.md) for the complete operating instructions and the meaning of valid, missing, invalid, individual and household denominators.
