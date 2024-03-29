cohortTableName <- 'estimating_external_val'
outputLocation <- '' # location to save results

targetId <- 5430
outcomeIds <- c(5431, 5420, 5421, 5422, 5426)

database <- '' # add model dev database

# developing the models for the full data

  data <- PatientLevelPrediction::loadPlpData(
    file = file.path(outputLocation, 'Data', database)
  )
  
  for(i in 1:length(outcomeIds)){
    print(outcomeIds[i])
    resultExists <- file.exists(file.path(outputLocation, 'Models', database, 
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
        saveDirectory = file.path(outputLocation, 'Models', database)
      )
    }
  }
  



# external val models 

  valDatabases <- c() # add list of validation databases
  
  models <- dir(
    file.path(outputLocation, 'Models', database), 
    pattern = 'Analysis_', full.names = T
  )
  
  for(modelLoc in models){
    plpResult <- PatientLevelPrediction::loadPlpResult(
      file.path(modelLoc, 'plpResult')
    )
    
    for(valDb in valDatabases){
      
      vdata <- PatientLevelPrediction::loadPlpData(
        file.path(outputLocation, 'Data', valDb)
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
        file = file.path(outputLocation, 'Val', 
          paste0(database, '-', plpResult$analysisRef$analysisId, '-',valDb)
        )
      )
    }
  }
  
