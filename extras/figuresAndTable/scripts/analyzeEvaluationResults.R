rm(list = ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source('./summarizeExperiments.R')
library(plyr)
library(latex2exp)

workDir <- 'C:/localdev/projects/robustness/rwe'

experimentDirs <- list(
  'full-match50' = 'full_model_agefix_match50_test',
  'Full' = 'full_model_agefix_test',
  'full-top100' = 'full_model_agefix_top100_test',
  'age-sex-match50' = 'Models_age_sex_agefix_match50_test',
  'Age-Sex' = 'Models_age_sex_agefix',
  'age-sex-int-coef-n0' = 'Models_age_sex_int_agefix',
  'LR' = 'Models_lr_50_agefix_sig',
  'lr50-non-na' = 'Models_lr_50_agefix',
  'rf50' = 'Models_rf_50_agefix',
  'XGBoost' = 'Models_xgb_50_agefix'
)

comparisons <- list(
  main = c('Age-Sex', 'LR', 'XGBoost','Full') # ,
  # ageSex = c('Age-Sex', 'age-sex-match50', 'age-sex-int-coef-n0'),
  # lr = c('LR', 'lr50-non-na'),
  # forest = c('XGBoost', 'rf50'),
  # fullModels = c('Full', 'full-match50', 'full-top100')
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
  'Optum CDM' = 'optum ses'
)
dbMap <- unlist(dbMap)


for (metric in c('AUROC', 'brier score', 'calibration')) { 
  
  allSummary <- summarizeAllExperiments(workDir, experimentDirs, dbs, metric)
  allResults <- merge(allSummary$results, analysesMap, by='analysis')
  
  allResults$internalDatabase  <- names(dbMap)[match(allResults$internalDatabase, dbMap)]
  allResults$externalDatabase  <- names(dbMap)[match(allResults$externalDatabase, dbMap)]
  
  failedSummary <- merge(allSummary$failed, analysesMap, by='analysis')
  results2 <- reshapeResults(allResults)  # TODO consider doing this from the start

  for (cname in names(comparisons)) {
    plotRawResults(workDir, results2, comparisons[[cname]], cname, metric)
    plotFailedEstimations(failedSummary, comparisons[[cname]], cname, metric)
    plotPerformenceDifference(workDir, allResults, cname, metric)  # TODO check the inputs
  }
}
