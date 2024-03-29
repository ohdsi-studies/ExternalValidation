studyLoc <- '' # add
cohortDefinitionSet <- readRDS(file.path(studyLoc,'cohorts','cohortDefinitionSet.rds'))
cohortTableName <- 'estimating_external_val'

cohortTableNames <- CohortGenerator::getCohortTableNames(
  cohortTable = cohortTableName
)

database <- '' # add

# add connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = keyring::key_get('dbms', 'all'),
  server = keyring::key_get('server', database),
  user = keyring::key_get('user', 'all'),
  password = keyring::key_get('pw', 'all'),
  port = keyring::key_get('port', 'all')#,
) 

# add schemas
workDatabaseSchema <- keyring::key_get('cohortDatabaseSchema', 'all')
cdmDatabaseSchema <- keyring::key_get('cdmDatabaseSchema',  database)

# this will generate cohorts into specified schema table 
CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = cdmDatabaseSchema, 
  cohortDatabaseSchema = workDatabaseSchema, 
  cohortTableNames = cohortTableNames, 
  cohortDefinitionSet = cohortDefinitionSet, 
  incremental = F
)


