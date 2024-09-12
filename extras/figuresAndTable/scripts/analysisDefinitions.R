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
