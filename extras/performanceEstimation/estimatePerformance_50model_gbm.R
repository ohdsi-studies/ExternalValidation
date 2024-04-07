# code to approximate performance
##remotes::install_github('KI-Research-Institute/plpDataAdapter')
##remotes::install_github('KI-Research-Institute/LearningWithExternalStats', ref = 'structured-logging')
library(glue)
library(plpDataAdapter)
library(LearningWithExternalStats)
library(FeatureExtraction) # v3.2.0
library(PatientLevelPrediction) # v6.0.4
library(dplyr)

databases <- c()

outputLocation <- '' 

resultLoc <- file.path(outputLocation, 'finalResults/Models_xgb')
if(!dir.exists(resultLoc)){
  dir.create(resultLoc)
}

for(internalDatabase in databases){
  exDatabases <- databases[databases != internalDatabase]
  
  
  analyses <- dir(file.path(outputLocation,'SimpleModels','xgb', 'Models', internalDatabase))
  
    for(analysis in analyses){
      internalPlpData <- tryCatch({
        PatientLevelPrediction::loadPlpData(
          file.path(outputLocation,'SimpleModels', 'Data',internalDatabase)
        )}, error = function(e){print(e); return(NULL)}
      )
      
      internalResults <- tryCatch({
        PatientLevelPrediction::loadPlpResult(
          file.path(outputLocation,'SimpleModels','xgb', 'Models', internalDatabase,analysis,'plpResult')
        )}, error = function(e){print(e); return(NULL)}
      )
      
      if(!is.null(internalResults) & !is.null(internalPlpData)){
        
        # TODO - remove this?
        covs <- internalResults$covariateSummary %>%
          filter(!is.na(.data$covariateValue)) %>%
          filter(abs(.data$covariateValue) >= 0) %>% 
          arrange(-abs(.data$covariateValue))
        
        
        estimationCovariates <- plpDataAdapter::getLRModelCovariates(internalResults)
        
        internalPlpData$covariateData$covariates <- internalPlpData$covariateData$covariates %>%
          filter(.data$covariateId %in% local(covs$covariateId) ) %>%
          collect()
        internalPlpData$covariateData$covariateRef <- internalPlpData$covariateData$covariateRef %>%
          filter(.data$covariateId %in% local(covs$covariateId) ) %>%
          collect()
        
        # Transform internal plp data and align it with predictions
        internalXY <- plpDataAdapter::transformPlpDataToDataFrame(
          plpData = internalPlpData, 
          populationSettings = internalResults$model$modelDesign$populationSettings, 
          outcomeId = internalResults$model$modelDesign$outcomeId
        )
        allignedInternal <- plpDataAdapter::allignDataAndPrediction(
          internalXY, internalResults, subsets = 'Test')
        # Compute transformation on covariate outcome pairs
        ZInt <- LearningWithExternalStats::computeTable1LikeTransformation(
          #allignedInternal$dataXY[c(estimationCovariates, 'outcome')],
          allignedInternal$dataXY,
          outcomeBalance = T,
          outcomeCol = 'outcome'
        )
        # Estimate external performance
        internalData <- list(
          z= ZInt,
          p = allignedInternal$prediction$value,
          y = allignedInternal$dataXY[['outcome']]
        )
        
        # now get all external 
        for(exDatabase in exDatabases){
          
          saveName <- paste(internalDatabase, analysis , paste0(exDatabase, '.csv'), sep = '-')
          checkName <- paste(internalDatabase, analysis , paste0(exDatabase, '.rds'), sep = '-')
          
          if(!file.exists(file.path(resultLoc, checkName))){
            
            externalPlpData <- tryCatch({
              PatientLevelPrediction::loadPlpData(
                file.path(outputLocation,'SimpleModels', 'Data', exDatabase)
              )}, error = function(e){print(e); return(NULL)}
            )
            
            externalResults <- tryCatch({
              readRDS(
                file.path( outputLocation, paste0('SimpleModels/xgb/Val'),paste0(internalDatabase,'-',analysis,'-',exDatabase))
              )}, error = function(e){print(e); return(NULL)}
            )
            
            if(!is.null(externalPlpData) & !is.null(externalResults)){
              
              
              externalPlpData$covariateData$covariates <- externalPlpData$covariateData$covariates %>%
                filter(.data$covariateId %in% local(covs$covariateId) ) %>%
                collect()
              externalPlpData$covariateData$covariateRef <- externalPlpData$covariateData$covariateRef %>%
                filter(.data$covariateId %in% local(covs$covariateId) ) %>%
                collect()
              
              externalXY <- plpDataAdapter::transformPlpDataToDataFrame(
                plpData = externalPlpData, 
                populationSettings = internalResults$model$modelDesign$populationSettings, 
                outcomeId = internalResults$model$modelDesign$outcomeId
              )
              ZExt <- LearningWithExternalStats::computeTable1LikeTransformation(
                externalXY[c(names(externalXY)[names(externalXY) %in% estimationCovariates], 'outcome')],
                outcomeBalance = T,
                outcomeCol = 'outcome'
              )
              muExt <- colMeans(ZExt)
              
              externalEstimatorSettings <- LearningWithExternalStats::createExternalEstimatorSettings(
                #divergence = 'entropy', # entropy, chi2
                #lambda = 1e-1,
                #minW = 0,
                #optimizationMethod = 'dual', # dual, primal
                reweightAlgorithm = LearningWithExternalStats::seTunedWeightOptimizer(nIter=2000), # changed from 500
                nMaxReweight = 20000, # 5000 was too small for SMD resulting in error
                stratifiedSampling = T,
                nRepetitions = 10,
                #maxProp = 100,
                maxWSMD = 0.2,
                outputDir = file.path(resultLoc, "estimating"),
                maxCores = 5
              )
              dir.create(file.path(resultLoc, "estimating"), recursive = T)
              
              # find varaibles not in external 
              missing <- colnames(internalData$z)[!colnames(internalData$z) %in% names(muExt)]
              
              if(length(missing)>0){
                muExt2 <- c(muExt, rep(0,length(missing)))
                names(muExt2)[length(muExt)+(1:length(missing))] <- missing
              } else{
                muExt2 <- muExt
              }
            
              estimatedResults <- tryCatch(
                {LearningWithExternalStats::estimateExternalPerformanceFromStatistics(
                  internalData = internalData,
                  externalStats = muExt2[colnames(internalData$z)],
                  externalEstimatorSettings = externalEstimatorSettings
                )}, error = function(e){return(list(error = e))}
              )
              
              if(!is.null(estimatedResults$estimation)){ # edited 
                
                plpMetrics <- c(
                  'populationSize','outcomeCount','AUROC','95% lower AUROC','95% upper AUROC',
                  'brier score','calibrationInLarge mean prediction','calibrationInLarge observed risk'
                )
                intPerf <- internalResults$performanceEvaluation$evaluationStatistics
                rowsInt <- (intPerf['evaluation']=='Test') & (intPerf[['metric']] %in% plpMetrics)
                cat('Internal performance:\n')
                print(format(intPerf[rowsInt, c('metric', 'value')], digits=3))
                
                extPerf <- externalResults$performanceEvaluation$evaluationStatistics
                rowsExt <- (extPerf['evaluation']=='Validation') & (extPerf[['metric']] %in% plpMetrics)
                cat('External performance:\n')
                print(format(extPerf[rowsExt, c('metric', 'value')], digits=3))
                
                
                
                showResults <- c(
                  'n',
                  'n outcome',
                  'Max weight',
                  'chi2 to uniform',
                  'kl',
                  'Max Weighted SMD',
                  'AUROC',
                  '95% lower AUROC',
                  '95% upper AUROC',
                  'n repetitions',
                  'Brier score',
                  'Global calibration mean prediction',
                  'Global calibration observed risk')
                estimationView <- estimatedResults$estimation[showResults, , drop = F]
                # Format the output:
                estimationView[, 'value'] <- apply(estimationView, 1, function(x) {sprintf('%.3g', x)})
                estimationView['n', 'value'] <- estimatedResults$estimation['n', 'value']
                print(estimationView)
                
                #rename
                allRes <- rbind(
                  cbind(extPerf[rowsExt, c('metric', 'value')], type = "external"),
                  cbind(intPerf[rowsInt, c('metric', 'value')], type = "internal"),
                  data.frame(
                    metric = c('AUROC', '95% lower AUROC', "95% upper AUROC", 
                      "brier score","calibrationInLarge mean prediction",
                      "calibrationInLarge observed risk" ),
                    value = c(
                      estimationView$value[rownames(estimationView) == 'AUROC'],
                      estimationView$value[rownames(estimationView) == '95% lower AUROC'],
                      estimationView$value[rownames(estimationView) == '95% upper AUROC'],
                      estimationView$value[rownames(estimationView) == 'Brier score'],
                      estimationView$value[rownames(estimationView) == 'Global calibration mean prediction'],
                      estimationView$value[rownames(estimationView) == 'Global calibration observed risk']
                    )  ,
                    type = rep('estimation',6)
                  )
                )
                
                allRes$targetId <- internalResults$model$modelDesign$targetId
                allRes$outcomeId <- internalResults$model$modelDesign$outcomeId
                allRes$internalDatabase <- internalDatabase 
                allRes$externalDatabase <- exDatabase 
                allRes$analysis <- analysis
                
                allRes$metric <- unlist(allRes$metric)
                allRes$value <- unlist(allRes$value)
                
                
                write.csv(x = allRes, file = file.path(resultLoc, saveName), row.names = F)
                
                write.csv(
                  x = data.frame(
                    metric = rownames(estimatedResults$estimation),
                    value = estimatedResults$estimation, 
                    targetId = internalResults$model$modelDesign$targetId, 
                    outcomeId = internalResults$model$modelDesign$outcomeId,
                    internalDatabase = internalDatabase,
                    externalDatabase = exDatabase,
                    analysis = analysis
                  ), 
                  file = file.path(resultLoc, paste0('full-',saveName)), row.names = F)
                
              } else{
                allRes <- NULL
                
              }
              
              saveRDS(list(
                summary = allRes,
                estimationResult = estimatedResults
              ), file.path(resultLoc, gsub('.csv','.rds',saveName))
              )
              
            } # not null external results
            
          } else{
            print('results exist')
          }
        } # exdatabases
      } # if not null internal results
    } # analysis
} # internal database
