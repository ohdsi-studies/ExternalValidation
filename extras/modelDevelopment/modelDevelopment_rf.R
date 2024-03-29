cohortTableName <- 'estimating_external_val'
outputLocation <- '' # location to save results

targetId <- 5430
outcomeIds <- c(5431, 5420, 5421, 5422, 5426)


# developing the xgb models for the 50 vars + age/sex
database <- ''
print(database)

data <- PatientLevelPrediction::loadPlpData(
  file = file.path(outputLocation, 'Data_50', database)
)

for(i in 1:length(outcomeIds)){
  print(outcomeIds[i])
  resultExists <- file.exists(file.path(outputLocation, 'simpleModels','rf', 'Models_50', database, 
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
      modelSettings = PatientLevelPrediction::setRandomForest(
        minSamplesSplit = list(2),
        minSamplesLeaf = list(5),
        ntrees = list(500), 
        mtries = list(as.integer(9)), 
        classWeight = list(NULL, 'balanced'),
        maxSamples = list(0.9),
        maxDepth = list(4, 10)  
      ), 
      saveDirectory = file.path(outputLocation, 'simpleModels','rf', 'Models_50', database)
    )
  }
}


# external val xgb models
valDatabases <- c() # add validation databases

models <- dir(
  file.path(outputLocation, 'simpleModels','rf', 'Models_50', database), 
  pattern = 'Analysis_', full.names = T
)

for(modelLoc in models){
  plpResult <- PatientLevelPrediction::loadPlpResult(
    file.path(modelLoc, 'plpResult')
  )
  
  for(valDb in valDatabases){
    
    if(!file.exists(file.path(outputLocation, 'simpleModels','rf', 'Val_50', 
                              paste0(database, '-', plpResult$analysisRef$analysisId, '-',valDb)
    ))){
      
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
        file = file.path(outputLocation, 'simpleModels','rf', 'Val_50', 
                         paste0(database, '-', plpResult$analysisRef$analysisId, '-',valDb)
        )
      )
    }
  }
}
  

