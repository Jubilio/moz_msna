# Run from the project root: Rscript MAIN.R all
args <- commandArgs(trailingOnly = TRUE)
stage <- if (length(args)) args[[1]] else 'all'
if (!file.exists('config/project.R')) stop('Run from the MOZ_MSNA_2026_R project root.')
source('config/project.R')
packages <- c('readxl', 'readr', 'dplyr', 'tidyr', 'purrr', 'stringr', 'tibble',
              'survey', 'srvyr', 'cleaningtools', 'analysistools', 'humind', 'openxlsx')
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop('Missing packages: ', paste(missing, collapse = ', '),
                         '. Run Rscript scripts/install_dependencies.R')
for (p in c('data/processed','outputs','outputs/qa','outputs/analysis','outputs/metadata'))
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
for (p in sort(list.files('R', pattern = '\\.R$', full.names = TRUE))) source(p)
stages <- c('prepare','checks','clean','indicators','analyse')
if (!stage %in% c(stages,'all')) stop('Valid stages: all, ', paste(stages, collapse=', '))
selected <- if (stage == 'all') stages else stage
for (s in selected) {
  message('Stage: ', s)
  switch(s, prepare = prepare_data(cfg), checks = check_data(cfg),
         clean = clean_data(cfg), indicators = compose_indicators(cfg),
         analyse = analyse_data(cfg))
}
capture.output(sessionInfo(), file='outputs/metadata/sessionInfo.txt')
writeLines(paste('Finished', format(Sys.time(), tz='UTC', usetz=TRUE)), 'outputs/metadata/run_time.txt')
