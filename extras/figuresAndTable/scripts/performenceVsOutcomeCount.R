rm(list = ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source('./summarizeExperiments.R')
source('./analysisDefinitions.R')
library(glue)
library(plyr)
library(latex2exp)

metric <- 'AUROC' # calibration 'brier score', 'AUROC'

ylabDict <- list(AUROC='AUROC', 'brier score'='Brier score', 'calibration'='Calibration')

#### Get the table summary ####

allSummary <- summarizeAllExperiments(workDir, experimentDirs, dbs, metric)
allResults <- merge(allSummary$results, analysesMap, by='analysis')
allResults$internalDatabase  <- names(dbMap)[match(allResults$internalDatabase, dbMap)]
allResults$externalDatabase  <- names(dbMap)[match(allResults$externalDatabase, dbMap)]
results2 <- reshapeResults(allResults)  # TODO consider doing this from the start

#### Outcome counts ####

experimentDir <- experimentDirs[['LR-medium']]

outcomeCounts <- data.frame(matrix(nrow = 25, ncol=3))
colnames(outcomeCounts) <- c('database', 'analysis', 'n.outcome')
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

outcomeCounts$n.outcome <- as.numeric(outcomeCounts$n.outcome)
outcomeCounts$Outcome.size.category = cut(
  outcomeCounts$n.outcome,
  # breaks = c(500,  1000,  2000,  5000, 10000,  20000,  50000),
  breaks = c(100 ,1000, 10000, 100000),
  right = FALSE,
  include.lowest = TRUE,
  dig.lab = 6)

write.csv(outcomeCounts, file.path(workDir, 'outcomeCounts.csv'))

for (mode in c('internal', 'external')) {
  results3 <- merge(
    results2, outcomeCounts, 
    by.x = c(glue('{mode}Database'), 'analysis'),
    by.y = c('database', 'analysis')
    )
  models <- c('LR-Age-Sex', 'LR-medium', 'XGBoost-medium','LR-large')
  results3 <- results3[results3$type=='est' & results3$Experiment %in% models, ]
  results3$n.outcome <- as.integer(results3$n.outcome)
  
  results3$err <- abs(results3$value.ext - results3$value.eval)
  
  ggplot(results3, aes(x=n.outcome, y=err, group=Experiment, color=Experiment)) +
    geom_point() +
    scale_x_log10() +
    ylab(TeX(glue("|$\\Delta$ {ylabDict[metric]}|"))) + # 
    xlab(glue('{mode} outcome counts'))
  ggsave(file.path(workDir, glue('{metric} err vs {mode} outcome counts point.png')))
  
  ggplot(results3, aes(x=n.outcome, y=err, group=n.outcome)) +
    geom_boxplot() +
    scale_x_log10() +
    ylab(TeX(glue("|$\\Delta$ {ylabDict[metric]}|"))) + # 
    xlab(glue('{mode} outcome counts'))
  ggsave(file.path(workDir, glue('{metric} err vs {mode} outcome counts box.png')))
  
  # Fixed outcome and external DB meaning that n is a function of internal DB
  ggplot(results3, aes(x=n.outcome, y=err, group = Experiment, color = Experiment)) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    facet_grid(cols = vars(analysisName), rows = vars(externalDatabase)) +
    ylab(TeX(glue("|$\\Delta$ {ylabDict[metric]}|"))) + # 
    xlab(glue('{mode} outcome counts'))
  ggsave(file.path(workDir, glue('{metric} err vs {mode} outcome counts lines outcome-external.png')))
  
  
  # Fixed internal DB and external DB meaning that n is a function of outcome
  ggplot(results3, aes(x=n.outcome, y=err, group = Experiment, color = Experiment)) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    facet_grid(cols = vars(internalDatabase), rows = vars(externalDatabase)) +
    ylab(TeX(glue("|$\\Delta$ {ylabDict[metric]}|"))) + # 
    xlab(glue('{mode} outcome counts'))
  ggsave(file.path(workDir, glue('{metric} err vs {mode} outcome counts lines internal-external.png')))
  
  # Fixed external DB meaning that n is a function of outcome and internal DB
  ggplot(results3, aes(x=n.outcome, y=err, group = Experiment, color = Experiment)) +
    geom_line() +
    geom_point() +
    scale_x_log10() +
    facet_wrap(vars(externalDatabase), nrow = 1) +
    ylab(TeX(glue("|$\\Delta$ {ylabDict[metric]}|"))) + # 
    xlab(glue('{mode} outcome counts'))
  ggsave(file.path(workDir, glue('{metric} err vs {mode} outcome counts lines external.png')))
  

  ggplot(results3, aes(x=Outcome.size.category, y=err, fill =Experiment)) +
    geom_boxplot() +
    ylab(TeX(glue("|$\\Delta$ {ylabDict[metric]}|"))) + # 
    xlab(glue('{mode} outcome counts'))
  ggsave(file.path(workDir, glue('{metric} err vs {mode} outcome counts categories.png')))
  
    
}
