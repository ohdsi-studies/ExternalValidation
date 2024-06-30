rm(list = ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source('./summarizeExperiments.R')
library(plyr)
library(latex2exp)
library(ggpubr)

workDir <- 'C:/localdev/projects/robustness/rwe'

experimentDirs <- list(
  'Full-medium' = 'full_model_agefix_match50_test',
  'Full' = 'full_model_agefix_test',
  'Full-top100' = 'full_model_agefix_top100_test',
  'Age-Sex-medium' = 'Models_age_sex_agefix_match50_test',
  'Age-Sex' = 'Models_age_sex_agefix',
  'Age-Sex-int' = 'Models_age_sex_int_agefix',
  'LR' = 'Models_lr_50_agefix_sig',
  'LR-non-na' = 'Models_lr_50_agefix',
  'RF' = 'Models_rf_50_agefix',
  'XGBoost' = 'Models_xgb_50_agefix'
)

comparisons <- list(
  main = c('Age-Sex', 'LR', 'XGBoost','Full'),
  ageSex = c('Age-Sex', 'Age-Sex-medium', 'Age-Sex-int'),
  lr = c('LR', 'LR-non-na'),
  fullModels = c('Full', 'Full-medium', 'Full-top100')
)

dbs <- c('ccae', 'mdcd', 'mdcr', 'optum ses', 'optum ehr')

analysesMap = data.frame(analysis=1:5, analysisName=c(
  'Seizure',
  'Diarrhea',
  'Fracture',
  'GI hemorrhage',
  'Insomnia'))

dbMap <- list(
  'CCAE' = 'ccae',
  'MDCD' = 'mdcd',
  'MDCR' = 'mdcr',
  'Optum EHR' = 'optum ehr',
  'Clinformatics' = 'optum ses'
)
dbMap <- unlist(dbMap)



metrics <- c('AUROC', 'brier score', 'calibration')
diffPlots <- list()

for (metric in metrics) {
  allSummary <- summarizeAllExperiments(workDir, experimentDirs, dbs, metric)
  allResults <- merge(allSummary$results, analysesMap, by='analysis')
  
  allResults$internalDatabase  <- names(dbMap)[match(allResults$internalDatabase, dbMap)]
  allResults$externalDatabase  <- names(dbMap)[match(allResults$externalDatabase, dbMap)]
  
  failedSummary <- merge(allSummary$failed, analysesMap, by='analysis')
  failedSummary$internalDatabase  <- names(dbMap)[match(failedSummary$internalDatabase, dbMap)]
  failedSummary$externalDatabase  <- names(dbMap)[match(failedSummary$externalDatabase, dbMap)]
  
  results2 <- reshapeResults(allResults)  # TODO consider doing this from the start

  for (cname in names(comparisons)) {
    cat(metric, cname, '\n')
    plotRawResults(results2, comparisons[[cname]], metric)
    ggsave(file.path(workDir, glue('{cname} {metric}.png')), width = 7, height = 7, dpi = 300)
    plotFailedEstimations(failedSummary, comparisons[[cname]], metric)
    ggsave(file.path(workDir, glue('{cname} {metric} success.png')), dpi=300)
    d <- plotPerformenceDifference(allResults, cname, metric)
    if (cname == 'main')
      diffPlots[[metric]] <- d
  }
}

# bgcolor("White") +
  
ggarrange(
  diffPlots$AUROC, #  + rremove("xlab") 
  diffPlots$`brier score`, #  + rremove("xlab") 
  diffPlots$calibration, 
  labels = c("a)", "b)", "c)"),
  ncol = 1, nrow = 3,
  align = 'v') + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill = "White", colour = "White"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "White", colour = "White"),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.spacing=unit(c(0,0,0,0), "cm"))
ggsave(file.path(workDir, 'Figure 4 performance differences.png'), width = 7, height = 5.5, dpi = 300)

