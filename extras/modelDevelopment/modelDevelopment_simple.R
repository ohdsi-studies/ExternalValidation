cohortTableName <- 'estimating_external_val'
outputLocation <- '' # location to save results

targetId <- 5430
outcomeIds <- c(5431, 5420, 5421, 5422, 5426)

# model development 
database <- '' 
print(database)

data <- PatientLevelPrediction::loadPlpData(
  file = file.path(outputLocation, 'simpleModels', 'Data_50', database)
)

for(i in 1:length(outcomeIds)){
  print(outcomeIds[i])
  resultExists <- file.exists(file.path(outputLocation, 'simpleModels', 'Models_50', database, 
                                        paste0('Analysis_',i), 'plpResult', 'runPlp.rds'))
  
  if(!resultExists){
    PatientLevelPrediction::runPlp(
      plpData = data, 
      outcomeId = outcomeIds[i], 
      analysisId = paste0('Analysis_',i),
      analysisName = paste0('Analysis_',i), 
      populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
        washoutPeriod = 365, 
        removeSubjectsWithPriorOutcome = T, 
        priorOutcomeLookback = 99999, 
        requireTimeAtRisk = F,
        riskWindowStart = 1, 
        riskWindowEnd = 365
      ), 
      splitSettings = PatientLevelPrediction::createDefaultSplitSetting(), 
      preprocessSettings = PatientLevelPrediction::createPreprocessSettings(), 
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(), 
      saveDirectory = file.path(outputLocation, 'simpleModels', 'Models_50', database)
    )
  }
}


# sex/age only
data <- PatientLevelPrediction::loadPlpData(
  file = file.path(outputLocation, 'Data_age_sex', database)
)

for(i in 1:length(outcomeIds)){
  print(outcomeIds[i])
  resultExists <- file.exists(file.path(outputLocation, 'simpleModels', 'Models_age_sex', database, 
                                        paste0('Analysis_',i), 'plpResult', 'runPlp.rds'))
  
  if(!resultExists){
    PatientLevelPrediction::runPlp(
      plpData = data, 
      outcomeId = outcomeIds[i], 
      analysisId = paste0('Analysis_',i),
      analysisName = paste0('Analysis_',i), 
      populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
        washoutPeriod = 365, 
        removeSubjectsWithPriorOutcome = T, 
        priorOutcomeLookback = 99999, 
        requireTimeAtRisk = F,
        riskWindowStart = 1, 
        riskWindowEnd = 365
      ), 
      splitSettings = PatientLevelPrediction::createDefaultSplitSetting(), 
      preprocessSettings = PatientLevelPrediction::createPreprocessSettings(), 
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(), 
      saveDirectory = file.path(outputLocation, 'simpleModels', 'Models_age_sex', database)
    )
  }
}


# external val models

valDatabases <- c()

models <- dir(
  file.path(outputLocation, 'simpleModels', 'Models_50', database), 
  pattern = 'Analysis_', full.names = T
)

for(modelLoc in models){
  plpResult <- PatientLevelPrediction::loadPlpResult(
    file.path(modelLoc, 'plpResult')
  )
  
  for(valDb in valDatabases){
    
    vdata <- PatientLevelPrediction::loadPlpData(
      file.path(outputLocation, 'simpleModels', 'Data_50', valDb)
    )
    pop <- PatientLevelPrediction::createStudyPopulation(
      plpData = vdata, 
      outcomeId = plpResult$model$modelDesign$outcomeId, 
      populationSettings = plpResult$model$modelDesign$populationSettings
    )
    
    val <- PatientLevelPrediction:::externalValidatePlp(
      plpModel = plpResult$model,
      plpData =  vdata, 
      population = pop
    )
    
    saveRDS(
      val, 
      file = file.path(outputLocation, 'simpleModels', 'Val_50', 
                       paste0(database, '-', plpResult$analysisRef$analysisId, '-',valDb)
      )
    )
  }
}


# external val age/sex models

models <- dir(
  file.path(outputLocation, 'simpleModels', 'Models_age_sex', database), 
  pattern = 'Analysis_', full.names = T
)

for(modelLoc in models){
  plpResult <- PatientLevelPrediction::loadPlpResult(
    file.path(modelLoc, 'plpResult')
  )
  
  for(valDb in valDatabases){
    
    vdata <- PatientLevelPrediction::loadPlpData(
      file.path(outputLocation, 'simpleModels', 'Data_age_sex', valDb)
    )
    pop <- PatientLevelPrediction::createStudyPopulation(
      plpData = vdata, 
      outcomeId = plpResult$model$modelDesign$outcomeId, 
      populationSettings = plpResult$model$modelDesign$populationSettings
    )
    
    val <- PatientLevelPrediction:::externalValidatePlp(
      plpModel = plpResult$model,
      plpData =  vdata, 
      population = pop
    )
    
    saveRDS(
      val, 
      file = file.path(outputLocation, 'simpleModels', 'Val_age_sex', 
                       paste0(database, '-', plpResult$analysisRef$analysisId, '-',valDb)
      )
    )
  }
}

