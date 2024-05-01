library(glue)
library(dplyr)
library(ggplot2)
library(stringr)

#' Summarize all experiments
#' 
#' Read all results from experiment directories and summarize
#' 
#' @param workDir
#' @param experimentDirs
#' @param dbs
#' @param metric
#' 
#' @return a named list with:
#'  `results` a dataframe with columns
#'      Experiment	analysis	internalDatabase	externalDatabase	type	value.ext	value.eval
#'  `success`
#' 
summarizeAllExperiments <- function(workDir, experimentDirs, dbs, metric) {
  colNames <- c('Short name', 'n', 'n success', 'n large opt err', 'out of range stat')
  allSummary <- matrix(ncol=length(colNames), nrow=length(experimentDirs))
  colnames(allSummary) <- colNames
  rownames(allSummary) <- experimentDirs
  
  allResults <- data.frame()
  allSuccess <- data.frame()
  
  analysisExp <- data.frame()
  for (name in names(experimentDirs)) {
    summary <- summarizeExperimentsDirectory(dbs, experimentDirs[name], workDir, metric)
    nSuccess <- sum(summary$success[ ,'success'])
    nLarge <- sum(summary$success[ , 'large.opt.err'])
    nOut <- sum(summary$success[ ,'out.of.range'])
    cat('n rows', nrow(summary$success), 'n success', nSuccess, '\n')
    allSummary[experimentDirs[[name]], ] <- c(name, nrow(summary$success), nSuccess, nLarge, nOut)
    
    g <- summary$success[, c('analysis', 'success', 'large.opt.err', 'out.of.range')] %>% 
      group_by(analysis) %>%
      summarise(n.success = sum(success), n.large.opt.err = sum(large.opt.err), n.out.of.range=sum(out.of.range)) %>%
      as.data.frame()
    g[['experiment']] <- name
    analysisExp <- rbind(analysisExp, g)
    
    allSuccess <- rbind(allSuccess, cbind(name, summary$success))
    allResults <- rbind(allResults, cbind(name, summary$results))  # Add a name column and then concat

  }

  allResults[['value.ext']] <- as.numeric(allResults[['value.ext']])
  allResults[['value.eval']] <- as.numeric(allResults[['value.eval']])
  
  colnames(allResults)[1] <- 'Experiment'  # TODO do it inside
  colnames(allSuccess)[1] <- 'Experiment'
  
  # write.csv(analysisExp, file.path(workDir, 'analysis-experiments success-failure summary.csv'), row.names = F)
  write.csv(allSummary, file.path(workDir, 'experiments success-failure summary.csv'))
  
  write.csv(allResults, file.path(workDir, 'all results.csv'))
  
  keys <- c('Experiment', 'internalDatabase', 'externalDatabase', 'analysis')
  failedSummary <- allSuccess[allSuccess[['success']]==0, keys]
  
  return(list(results=allResults, failed=failedSummary))
}


#' Summarize experiment results in a directory that corresponds to a model and a set of covariates
#' 
summarizeExperimentsDirectory <- function(dbs, experimentDir, workDir, metric) {
  keys <- c('analysis', 'internalDatabase', 'externalDatabase')
  summaryColNames <- c(keys, 'type', 'value.ext', 'value.eval')  # , 'opt.err', 'Max.Weighted.SMD' , 'InternaloutcomeCount'
  
  rSummary <- matrix(nrow=0, ncol=length(summaryColNames))
  colnames(rSummary) <- summaryColNames
  successSummaryColNames <- c('internalDatabase', 'externalDatabase', 'analysis', 'success', 'large.opt.err',  'out.of.range','details')
  successSummary <- matrix(nrow=0, ncol=length(successSummaryColNames))
  colnames(successSummary) <- successSummaryColNames 
  
  for (analysisName in 1:5) {
    for (internalName in dbs) {
      for (externalName in dbs) {
        r <- summarizeSingleExperiment(internalName, analysisName, experimentDir, externalName, workDir, metric)
        rSummary <- rbind(rSummary, r$internal)
        rSummary <- rbind(rSummary, r$estimation)
        if (r$hasRDS) {
          if (r$success) {
            l <- c(internalName, externalName, analysisName, 1, 0, 0, '')
            lastSuccessEst <- r$est
          } else {
            largeOpt <- if (r$failureReason == 'Large optimization error') 1 else 0
            outOfRange <- if (r$failureReason == 'Out of range statistic') 1 else 0
            l <- c(internalName, externalName, analysisName, 0, largeOpt, outOfRange, r$failureReasonDetails)
            lastFailEst <- r$est
          }
          successSummary <- rbind(successSummary, l)
          
        }
      }
    }
  }
  successSummary <- data.frame(successSummary)
  successSummary[['success']] <- as.numeric(successSummary[['success']])
  successSummary[['large.opt.err']] <- as.numeric(successSummary[['large.opt.err']])
  successSummary[['out.of.range']] <- as.numeric(successSummary[['out.of.range']])
  return (list(success=successSummary, results=rSummary))
}



#' Summarize a single experiment that has and rds and a csv file
#' 
#' @param internalName
#' @param analysisName
#' @param experimentDir
#' @param externalName
#' @param workDir
#' 
summarizeSingleExperiment <- function(internalName, analysisName, experimentDir, externalName, workDir, metric) {
  
  res <- list(internal=NULL, estimation=NULL, hasRDS=F, hasSummary=F, success=F)
  if (internalName != externalName) {
    expName <- glue('{internalName}-Analysis_{analysisName}-{externalName}')
    rdsName <- file.path(workDir, experimentDir, glue('{expName}.rds'))
    if (file.exists(rdsName)) {
      res$hasRDS <- T
      est <- readRDS(rdsName)
      res$est <- est
      if (!is.null(est$summary)) {
        res$hasSummary <- T
        if (est$estimationResult$preDiagnosis$status == 'Success') {
          res$success <- T
          r <- read.csv(file.path(workDir, experimentDir, glue('{expName}.csv')))
          #  
          
          if (metric == 'calibration') {
            meanPredictionIdx <- r[['metric']] == "calibrationInLarge mean prediction"
            observedRiskIdx <- r[['metric']] == "calibrationInLarge observed risk"
            r1 <- r[meanPredictionIdx, ]
            r2 <- r[observedRiskIdx, ]
            rcols <- c("type", "targetId", "outcomeId", "internalDatabase", "externalDatabase", "analysis")
            r <- merge(r1, r2, by = rcols)
            r[['metric']] <- 'calibration'
            r[['value']] <- abs(r[['value.x']] - r[['value.y']])
          }
          
          # TODO - check consistensy 
          internal <- r[['type']] == 'internal'
          external <- r[['type']] == 'external'
          estimation <- r[['type']] == 'estimation'
          metricIdx <- r[['metric']] == metric
          
          internalMetric <- as.numeric(r[internal & metricIdx, 'value'])
          externalMetric <- as.numeric(r[external & metricIdx, 'value'])
          estimationMetric <- as.numeric(r[estimation & metricIdx, 'value'])
          
          keyValues <- c(analysisName, internalName, externalName)
          res$internal <- c(keyValues, 'int', externalMetric, internalMetric)
          res$estimation <- c(keyValues, 'est', externalMetric, estimationMetric)
          
        }
      }  # !is.null(est$summary)
      else {  # TODO add more scenarios
        if (is.null(est$estimationResult$preDiagnosis)) {
          res$failureReason <- 'Other error'
          res$failureReasonDetails <- as.character(est$estimationResult$error)
        }
        else {
          if(est$estimationResult$preDiagnosis$status == 'Success') {
            if (!is.null(est$estimationResult$weightingResults)) {
              res$failureReason <- 'Large optimization error'
              res$failureReasonDetails <- est$estimationResult$results['Opt err' , 'value']
            } else {
              res$failureReason <- 'Probably large smd, but dig more'
              res$failureReasonDetails <- est$estimationResult$weightingResults['Max Weighted SMD', 'value']
            }
          }
          else {
            res$failureReason <- 'Out of range statistic'
            res$failureReasonDetails <- 
              est$estimationResult$preDiagnosis$structuredLog['outOfRangeRepresentative', 'value']
          }
        }
      }
    }  # if (file.exists(rdsName))
    else {
      cat(rdsName, 'does not exist\n')
    }
  }
  return(res)
}


# TODO - go over the entire pipeline to check if we can generate results directly in this format
reshapeResults <- function(results) {
  resultsKeys <- c('Experiment', 'analysis', 'analysisName', 'type', 'internalDatabase', 'value.eval')
  internalResults <- 
    results[results[['type']]=='int', resultsKeys] %>%
    distinct()
  internalResults[['value.ext']]  <- internalResults[['value.eval']]
  internalResults[['externalDatabase']]  <- internalResults[['internalDatabase']]
  estimatedResults <- results[results[['type']]=='est', ]
  results2 <- rbind(internalResults, estimatedResults)
  results2[['analysisName']] <- as.factor(results2[['analysisName']])
  return(results2)
}



#' Plot raw results
#' 
#' @param workDir path to working directory
#' @param results a data frame with columns:
#'   `Experiment`, `externalDatabase`, `value.ext`, `value.eval`, `type`
#'    Where `type` is either int or est
#' @param models a list of models
#' @param name comparison name
#' @param metric AUROC, brier score or calibration
#' 
plotRawResults <- function(workDir, results, models, name, metric) {

  results <- results[results[['Experiment']] %in% models, ]
  
  externalEvalIdx <- results[['externalDatabase']]!=results[['internalDatabase']]

  results %>%
    ggplot(aes(x=externalDatabase, y=value.ext, group=Experiment, color = Experiment)) +
    scale_x_discrete(
      breaks = names(dbMap),
      labels = names(dbMap)
    ) + theme_bw() + 
    scale_y_continuous() +  # limits = c(0.65, 0.8)
    xlab("External database") +
    ylab("External AUC") +
    geom_point(shape = 5, size=1) +  # empty diamonds
    geom_point(data = results[results[['type']]=='int', ], shape = 18, size=2) +  # Filled diamonds
    geom_point(
      data = results[externalEvalIdx, ], mapping = aes(y=value.eval), shape = 4, size = 2, 
      position = position_nudge(x = 0.25)) + # x  
    facet_grid(internalDatabase~analysisName) +
    # scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) # ,
          # plot.margin = margin(1,1,5,1.2, "cm")
          )

  ggsave(file.path(workDir, glue('compare {name} {metric}.png')), width = 7, height = 7)
}


plotFailedEstimations <- function(failedEstimations, models, name, metric) {
  
  failedEstimations <- failedEstimations[failedEstimations[['Experiment']] %in% models, ]
  failedEstimations %>%
    ggplot(aes(x=externalDatabase, y=Experiment, group=Experiment, color = Experiment)) +
    scale_x_discrete(
      breaks = dbs,
      labels = dbs
    ) + theme_bw() + 
    scale_y_discrete() +  # limits = c(0.65, 0.8)
    xlab("External database") +
    ylab("Experiment") +
    geom_point(shape = 15, size=4) +
    facet_grid(internalDatabase~analysisName) +
    theme( legend.position = "none", 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          aspect.ratio = 1/3,
          strip.text.y.right = element_text(angle = 0))
  
  ggsave(file.path(workDir, glue('success compare {name} {metric}.png')))
}


plotPerformenceDifference <- function(workDir, allResults, cname, metric)  {
  cResults <- allResults[allResults[['Experiment']] %in% comparisons[[cname]], ]
  
  externalEvalIdx <- cResults[['externalDatabase']]!=cResults[['internalDatabase']]
  cResults <- cResults[externalEvalIdx, ]
  
  cResults[['Difference']] <- abs(cResults[['value.eval']] - cResults[['value.ext']])
  
  ylabDict <- list(AUROC='AUROC', 'brier score'='Brier', 'calibration'='Calibration')
  
  cResults %>%
    ggplot(aes(x=type, y=Difference, group=type, color = type)) +
    scale_x_discrete(
      breaks = c('int', 'est'),
      labels = c('Internal', 'Reweighting')
    ) + theme_bw() + 
    scale_y_continuous() +  # limits = c(0.65, 0.8)
    xlab('Estimation type') +
    ylab(TeX(glue("|$\\Delta$ {ylabDict[metric]}|"))) + # 
    geom_boxplot() +
    facet_grid(~internalDatabase) +
    theme(
      aspect.ratio = 2/(1+sqrt(5)),
      legend.position = "none") # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  # scale_color_brewer(palette = "Dark2")
  fileName <- file.path(workDir, glue('{metric} {cname} difference.png'))
  cat('Saving', fileName, '\n')
  ggsave(fileName, width = 7, height = 1.5)
  
  pStats <- ddply(
    cResults, .(type, internalDatabase), summarise, med = median(Difference), 
    q25 = quantile(Difference, 0.25),
    q75 = quantile(Difference, 0.75))
  cat(cname, metric, '\n')
  if (metric == 'AUROC') {
    pStats[ , c('med', 'q25', 'q75')] <- round(pStats[ , c('med', 'q25', 'q75')], 3)
    print(pStats)
  }
  else
    print(pStats)
}
