cohortTableName <- 'estimating_external_val'
outputLocation <- '' # location to save results

targetId <- 5430
outcomeIds <- c(5431, 5420, 5421, 5422, 5426)

covariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T, 
  useDemographicsAgeGroup = T, 
  useConditionGroupEraLongTerm = T, 
  useDrugGroupEraLongTerm = T, 
  endDays = -1
)

# extracting the data for a database (add loop if multiple databases)
database <- ''
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = keyring::key_get('dbms', 'all'),
  server = keyring::key_get('server', database),
  user = keyring::key_get('user', 'all'),
  password = keyring::key_get('pw', 'all'),
  port = keyring::key_get('port', 'all')#,
) 

workDatabaseSchema <- keyring::key_get('cohortDatabaseSchema', 'all')
cdmDatabaseSchema <- keyring::key_get('cdmDatabaseSchema',  database)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema =  cdmDatabaseSchema, 
  cohortDatabaseSchema = workDatabaseSchema, 
  cohortTable = cohortTableName,
  outcomeDatabaseSchema = workDatabaseSchema, 
  outcomeTable = cohortTableName, 
  targetId = targetId, 
  outcomeIds = outcomeIds
)

data <- PatientLevelPrediction::getPlpData(
  databaseDetails = databaseDetails,
  covariateSettings = covariateSettings, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings() 
)
PatientLevelPrediction::savePlpData(
  plpData = data, 
  file = file.path(outputLocation,'Data', database)
)

analysisSpecifications <- ParallelLogger::loadSettingsFromJson(file.path(studyLoc,'cohorts','prediction_phenotype.json'))
covariateSettings <- lapply(
  analysisSpecifications$sharedResources[[1]]$cohortDefinitions,
  function(x){
    PatientLevelPrediction::createCohortCovariateSettings(
      cohortName = x$cohortName, 
      cohortId = x$cohortId, 
      cohortTable = cohortTableName, 
      settingId = 1, 
      cohortDatabaseSchema =  workDatabaseSchema, 
      analysisId = 765, 
      startDay = -99999, 
      endDay = -1
    )
  }
)

covariateSettings[[length(covariateSettings) + 1]] <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T, 
  useDemographicsAgeGroup = T
)
# extracting the phenotype predictor and age/sex data

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema =  cdmDatabaseSchema, 
  cohortDatabaseSchema = workDatabaseSchema, 
  cohortTable = cohortTableName,
  outcomeDatabaseSchema = workDatabaseSchema, 
  outcomeTable = cohortTableName, 
  targetId = targetId, 
  outcomeIds = outcomeIds
)

data <- PatientLevelPrediction::getPlpData(
  databaseDetails = databaseDetails,
  covariateSettings = covariateSettings, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings() 
)
PatientLevelPrediction::savePlpData(
  plpData = data, 
  file = file.path(outputLocation, 'simpleModels', 'Data_50', database)
)

data <- PatientLevelPrediction::getPlpData(
  databaseDetails = databaseDetails,
  covariateSettings = covariateSettings[[length(covariateSettings)]], 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings() 
)
PatientLevelPrediction::savePlpData(
  plpData = data, 
  file = file.path(outputLocation, 'simpleModels', 'Data_age_sex', database)
)