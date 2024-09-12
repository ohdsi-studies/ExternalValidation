rm(list = ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source('./summarizeExperiments.R')
source('./analysisDefinitions.R')
library(glue)
library(plyr)

metric <- 'AUROC'

#### Get the table summary ####

allSummary <- summarizeAllExperiments(workDir, experimentDirs, dbs, metric)
allResults <- merge(allSummary$results, analysesMap, by='analysis')
allResults$internalDatabase  <- names(dbMap)[match(allResults$internalDatabase, dbMap)]
allResults$externalDatabase  <- names(dbMap)[match(allResults$externalDatabase, dbMap)]
results2 <- reshapeResults(allResults)  # TODO consider doing this from the start

#### Outcome counts ####

experimentDir <- experimentDirs[['LR']]

outcomeCounts <- data.frame(matrix(nrow = 25, ncol=3))
colnames(outcomeCounts) <- c('internalDatabase', 'analysis', 'n.outcome')
i <- 0
for (analysisId in 1:5) {
  for (internalDB in names(dbMap)) {
    internalName <- dbMap[[internalDB]]
    # analysisName <- analysesMap[analysisId, 'analysisName']
    if (internalName=='optum ses')  # Make sure internal != external
      externalName <- 'optum ehr'
    else
      externalName <- 'optum ses'
    expName <- glue('{internalName}-Analysis_{analysisId}-{externalName}')
    rdsName <- file.path(workDir, experimentDir, glue('{expName}.rds'))
    est <- readRDS(rdsName)
    n <- est$summary[est$summary$metric=='outcomeCount' & est$summary$type=='internal', 'value']
    i <- i+1
    outcomeCounts[i, ] <- c(internalDB, analysisId, n)
  }
}

write.csv(outcomeCounts, file.path(workDir, 'outcomeCounts.csv'))

results3 <- merge(results2, outcomeCounts, by = c('internalDatabase', 'analysis'))
models <- c('Age-Sex', 'LR', 'XGBoost','Full')
results3 <- results3[results3$type=='est' & results3$Experiment %in% models, ]
results3$n.outcome <- as.integer(results3$n.outcome)

results3$err <- abs(results3$value.ext - results3$value.eval)

ggplot(results3, aes(x=n.outcome, y=err, group=Experiment, color=Experiment)) +
  geom_point() +
  scale_x_log10()

ggplot(results3, aes(x=n.outcome, y=err, group=n.outcome)) +
  geom_boxplot() +
  scale_x_log10()
