rm(list = ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source('./summarizeExperiments.R')
library(plyr)

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

internalName <- 'optum ses'
externalName <- 'ccae'
analysisName <- 4
experimentDir <- experimentDirs[['LR']]

# Start running

expName <- glue('{internalName}-Analysis_{analysisName}-{externalName}')
rdsName <- file.path(workDir, experimentDir, glue('{expName}.rds'))

est <- readRDS(rdsName)

observedRiskIdx <- est$summary$metric == 'calibrationInLarge observed risk'
externalIdx <- est$summary$type == 'external'
brierScorelIdx <- est$summary$metric == 'brier score'

extObservedRisk <- as.numeric(est$summary[observedRiskIdx & externalIdx, 'value'])
extBrierScore <- as.numeric(est$summary[brierScorelIdx & externalIdx, 'value'])
extBrierSkillScore <- getBrierSkillScore(extBrierScore, extObservedRisk)

internalIdx <- est$summary$type == 'internal'
intObservedRisk <- as.numeric(est$summary[observedRiskIdx & internalIdx, 'value'])
intBrierScore <- as.numeric(est$summary[brierScorelIdx & internalIdx, 'value'])
intBrierSkillScore <- getBrierSkillScore(intBrierScore, intObservedRisk)

estimationIdx <- est$summary$type == 'estimation'
estObservedRisk <- as.numeric(est$summary[observedRiskIdx & estimationIdx, 'value'])
estBrierScore <- as.numeric(est$summary[brierScorelIdx & estimationIdx, 'value'])
estBrierSkillScore <- getBrierSkillScore(estBrierScore, estObservedRisk)

