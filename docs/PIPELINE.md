# Operating the Mozambique MSNA 2026 pipeline

## First run

Open `MOZ_MSNA_2026.Rproj` and run the entire installer:

```r
source('scripts/install_dependencies.R')
source('tests/run_tests.R')
```

Copy the unmodified household/loop export to the path in `cfg$dataset` and its matching XLSForm to `cfg$kobo`. Raw survey data and the real cleaning decision log are gitignored. The supplied dictionaries describe the mapped export; they do not prove that a different Kobo version has the same skip logic.

From the RStudio Terminal:

```sh
Rscript MAIN.R prepare
Rscript MAIN.R checks
```

Review `outputs/qa/cleaning_flags.csv`. Fill `config/cleaning_decisions.csv`, generated with an empty header by `checks`. The example file documents the format but its illustrative row must not be approved. Use immutable **record IDs**, not household UUIDs. Every approved row needs a reason and reviewer. Use original canonical Kobo codes, not display labels.

```sh
Rscript MAIN.R clean
Rscript MAIN.R indicators
Rscript MAIN.R analyse
Rscript MAIN.R review
Rscript MAIN.R present
```

`Rscript MAIN.R all` runs the same seven stages in order. It deliberately stops at `clean` if blocker flags have no reviewed disposition. After recording decisions, resume from `clean`. A flag may be acknowledged with `no_action` and a reason; this does not turn an invalid measurement into a valid indicator value. `cfg$require_blocker_review=FALSE` is an explicit configuration override for exploratory work, not a default publication setting.

## Cleaning decisions

Actions: `change_response`, `blank_response`, `remove_survey`, `no_action`.

- `uuid` identifies the exact record: `main::<_index>` or `<table>::<_parent_index>::<_index>`.
- A response edit must match the current `old_value`; conflicting decisions stop execution.
- Do not edit and delete the same record in one run.
- Household removals cascade by the removed **parent index**. This also handles duplicate household UUIDs without removing the retained interview's children.
- For select-multiple questions, correct the binary option columns (`question/option`) using `0`/`1`. Parent-only edits are rejected. Touched parents are rebuilt and audited in `rebuilt_multiple_parents.csv`; partial unknown sibling responses keep the reconstructed parent unknown.
- Changing a question does not evaluate every XLSForm relevance or recalculate every exported Kobo calculation. Review dependent fields explicitly, including roster-derived attributes in separate loops. No general XPath evaluator is implemented.

Unresolved blockers are written to `outputs/qa/unresolved_blockers.csv`. Cascade removals and applied decisions have separate audit files. The original workbook is never changed.

## WASH adaptation

`cfg$water_on_premises_sources` defaults to `piped_dwelling`. Add `piped_compound` and/or `piped_neighbour` **only after confirming their meaning and skip logic in the local instrument**. A skipped question alone does not establish availability on premises. The local schema currently contains the integer collection-time question but not the global template's `_time_yn` / `_time_sl` fields; absent questions are not fabricated.

The adapter handles integer-only exports and, if actually supplied, the standard scoping and fallback-time fields. Exact reported times must be positive integer minutes, with a configurable maximum of 600. Exactly 30 minutes is classified on the basic-service side; 31 minutes is above the threshold. On-premises sources remain identifiable without an integer time. Unknown/refused sources remain unknown.

Unknown/refused container-size selections never become zero litres. Selected containers need valid fill counts. A blank count is zero only for an explicitly unselected container. Contradictory positive counts for unselected containers remain unknown. Per-person estimates additionally require a valid household size consistent with the roster.

`wash_denominator_comparison.csv` displays eligibility, valid, missing and invalid counts for related WASH indicators. Different indicator universes may legitimately have different denominators. The review checks internal agreement; it does not force all WASH percentages to be identical. `water_jmp_service` is a service/source classification, not a water-quality laboratory result. Water collected yesterday is not measured consumption and does not replace HWISE-4.

## Education adaptation

Access/out-of-school and related outputs use individual children. Household composites are separate. `education_age_min` and `education_age_max` default to 5 and 17. Cycle and overage outputs require completed, reviewed `config/education_grades.csv`; enabling the switch with blank mappings stops with a clear error.

The default age basis is age at interview. To enable school-year correction, set a mapped household interview-date column in `education_interview_date_column`, supply `education_school_year_start_month` and confirm `education_school_year_confirmed`. Dates must be valid; missing dates are never replaced with a date from 2024 or 2025. The age basis is retained in the individual indicator dataset. National ISCED/cycle mappings and academic-year settings must be agreed before interpretation.

## Weights and denominators

The default mode is descriptive, with unit weights and no population confidence intervals. For weighted analysis, supply unique household weights, confirm the sampling design, and configure mapped PSU/strata fields. Non-finite, zero or negative weights and missing design identifiers stop execution. A household cluster is used when no separate PSU is supplied. Indicator-validity subsetting happens after creating the survey design; actual sampling parameters must still be validated against the assessment design.

For each indicator/group:

- `n_eligible`: records in the configured analysis population;
- `n_valid`: records included in the estimate;
- `n_missing`: blank responses;
- `n_invalid`: nonblank responses excluded by the numeric/binary validity rule;
- `n_valid_households`: distinct households represented by valid records.

Eligible = valid + missing + invalid. Individual records and household counts are never interchangeable. The minimum sample size is a reporting flag; no automatic suppression policy is inferred.

## Review and presentation

`review` checks duplicate result keys, proportion ranges, denominator accounting/agreement, select-one sums, household counts and removal of confidence intervals in descriptive outputs. These are integrity checks, not independent methodological approval or a second statistical estimator.

`present` requires a passed review and generates:

- `outputs/presentation/report.html`: aggregate overview, PNG charts, review and methodology gaps;
- `Results_<table>.xlsx`: labelled long results, denominators and native `presentresults` wide tables;
- `results_labelled.csv`: labelled estimates retaining original variable codes;
- `dashboard_results.csv`: estimates joined to eligibility and denominator information;
- `presentation_manifest.csv`: generated files and hashes.

Question/choice labels use `presentation_language` (`en` or `pt`). Report structure and quality-control messages are in English. Edit `presentation_labels.csv` for derived-indicator labels and `presentation_indicators` to choose overview charts. Raw responses and record identifiers are not included in the report. Outputs still require disclosure review before external sharing.

## Cache integrity and reproducibility

Successful stages write `outputs/metadata/stage_<stage>.rds`. Later stages verify code, configuration, inputs and upstream artifact hashes. A failed rerun invalidates its success record and those of downstream stages. Modified source/code/methodology requires rerunning from `prepare`; changes to cleaning decisions require rerunning from `clean`; changes to the weights file require rerunning from `analyse`. Configuration changes conservatively require a new preparation run.

The installer creates a real `renv.lock` after successful installation. Preserve that lockfile for operational runs and use `renv::restore()` subsequently. Direct IMPACT packages are pinned; transitive versions are captured in the resolved lockfile. Run metadata records R and package versions.

## Validation

```sh
Rscript tests/run_tests.R
Rscript tests/synthetic_pipeline.R
```

The synthetic test builds a separate temporary project with a small workbook matching the mapped export headers. It exercises all seven stages, empty auxiliary loops, WASH missingness and the generated presentation; it also checks stale-artifact rejection. It never reads the real survey dataset. Set `MSNA_KEEP_FIXTURE=yes` to retain its temporary output for inspection. GitHub Actions runs both test suites on pull requests and pushes to main.

Full MSNI remains gated: these engineering additions do not resolve missing HWISE-4, Protection equivalence or other country-framework gaps documented in `config/methodology_gaps.csv`.
