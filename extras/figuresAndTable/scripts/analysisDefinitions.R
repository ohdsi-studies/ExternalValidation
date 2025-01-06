workDir <- 'C:/local/projects/robustness/rwe'

experimentDirs <- list(
  'LR-large' = 'full_model_agefix_test',
  'LR-large-est-medium' = 'full_model_agefix_match50_test',
  'LR-large-est-top100' = 'full_model_agefix_top100_test',
  'LR-small-est-medium' = 'Models_age_sex_agefix_match50_test',
  'LR-small' = 'Models_age_sex_agefix',
  'LR-small-est-interactions' = 'Models_age_sex_int_agefix',
  'LR-medium' = 'Models_lr_50_agefix_sig',
  'LR-medium-est-non-na' = 'Models_lr_50_agefix',
  'RF-medium' = 'Models_rf_50_agefix',
  'XGBoost-medium' = 'Models_xgb_50_agefix'
)

comparisons <- list(
  main = c('LR-small', 'LR-large', 'LR-medium', 'XGBoost-medium'),
  lrSmall = c('LR-small', 'LR-small-est-interactions', 'LR-small-est-medium'),
  lrMedium = c('LR-medium', 'LR-medium-est-non-na'),
  lrLarge = c('LR-large', 'LR-large-est-medium', 'LR-large-est-top100'),
  nonLinear = c('XGBoost-medium', 'RF-medium')
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

ylabDict <- list(
  AUROC='AUROC',
  'calibration'='Calibration',
  'brier score'='Brier score', 
  'Scaled Brier score'='Scaled Brier score')

metrics <- names(ylabDict)