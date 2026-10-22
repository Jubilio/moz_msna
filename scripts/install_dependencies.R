# Run this WHOLE file from the project root:
# source('scripts/install_dependencies.R')
# All installer state is local; no console variables such as cran or refs are needed.
local({
  if(getRversion() < '4.3.0') stop('R >= 4.3 is required by the pinned presentresults version.')
  if (!file.exists('config/project.R'))
    stop('Open MOZ_MSNA_2026.Rproj and run the installer from the project root.')
  options(repos=c(CRAN='https://cloud.r-project.org'),
          timeout=max(600, getOption('timeout', 60)))
  install_project <- function() {
    if (!requireNamespace('renv', quietly=TRUE)) {
      utils::install.packages('renv', repos='https://cloud.r-project.org')
      if (!requireNamespace('renv', quietly=TRUE))
        stop('renv installation failed. Read the installation error above.')
    }
    # init() normally restarts interactive RStudio, destroying console variables.
    # Prevent that restart; reuse an existing project without reinitializing it.
    if (!file.exists('renv/activate.R')) {
      renv::init(project=getwd(), bare=TRUE, restart=FALSE,
                 repos=c(CRAN='https://cloud.r-project.org'))
    } else {
      renv::load(project=getwd())
    }
    cran_packages <- c('readxl','readr','dplyr','tidyr','purrr','stringr','tibble',
                       'rlang','survey','srvyr','openxlsx')
    github_refs <- c(
      cleaningtools='impact-initiatives/cleaningtools@f917e607115cb9da255d09039965a06bae1736e5',
      analysistools='impact-initiatives/analysistools@d748f320c41eeaa5619be45457414ffb4f53e78a',
      humind='impact-initiatives-hppu/humind@v2026.4.1',
      presentresults='impact-initiatives/presentresults@6219392a2983db6c90b5e7cb557db5e052b76fed')
    # renv supports GitHub owner/repository@ref directly; remotes is not required.
    renv::install(packages=cran_packages, project=getwd(), prompt=FALSE)
    for (ref in unname(github_refs))
      renv::install(packages=ref, project=getwd(), prompt=FALSE)
    required <- c('renv',cran_packages,names(github_refs))
    missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
    if (length(missing))
      stop('Installation incomplete. Missing: ',paste(missing,collapse=', '))
    required_functions <- list(
      cleaningtools=c('check_duplicate','check_logical','check_fcs',
                      'review_cleaning_log','create_clean_data'),
      analysistools='create_analysis',
      humind=c('add_fcs','add_rcsi','add_hhs','add_fcm_phase',
               'add_fclcm_phase','add_comp_foodsec','add_comp_health',
               'add_drinking_water_source_cat'),
      presentresults='create_table_variable_x_group')
    for (pkg in names(required_functions)) {
      missing_functions <- setdiff(required_functions[[pkg]],getNamespaceExports(pkg))
      if(length(missing_functions)) stop(pkg,': missing functions: ',
        paste(missing_functions,collapse=', '),'. Restart R and rerun this installer.')
    }
    renv::snapshot(project=getwd(),packages=required,prompt=FALSE)
    if(!file.exists('renv.lock')) stop('Snapshot did not create renv.lock.')
    lock <- renv::lockfile_read('renv.lock')
    if(length(setdiff(required,names(lock$Packages))))
      stop('The lockfile does not contain all required packages.')
    message('Dependencies installed and renv.lock verified. Next: source("tests/run_tests.R")')
    invisible(TRUE)
  }
  # An error stops source(); no success message is emitted after a failed operation.
  install_project()
})

