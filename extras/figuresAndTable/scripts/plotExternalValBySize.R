rm(list=ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source('./analysisDefinitions.R')
library(dplyr)
library(ggplot2)
library(glue)
# library(plyr)
library(latex2exp)
library(ggpubr)
library(patchwork)

#### Add metrics ####

keyCols <- c('outcomeId', 'analysis', 'sample', 'stratified')

getCalibrationResults <- function(data) {
  calibrationResults <- merge(
    data[, c(keyCols, 'type', 'metric','value')] %>%
      filter(.data$metric == 'calibrationInLarge mean prediction'),
    data[, c(keyCols, 'type', 'metric','value')] %>%
      filter(.data$metric == 'calibrationInLarge observed risk'),
    by = c(keyCols, 'type')
  )
  calibrationResults$metric <- 'calibration'
  calibrationResults$value = calibrationResults$value.x / calibrationResults$value.y
  return(calibrationResults[ , c(keyCols, 'type', 'metric', 'value')])
}


getScaledBrierResults <- function(data) {
  calibrationResults <- merge(
    data[, c(keyCols, 'type', 'metric','value')] %>%
      filter(.data$metric == 'brier score'),
    data[, c(keyCols, 'type', 'metric','value')] %>%
      filter(.data$metric == 'calibrationInLarge mean prediction'),
    by = c(keyCols, 'type')
  )
  calibrationResults$metric <- 'Scaled brier score'
  pHat <- calibrationResults$value.y
  calibrationResults$value = 1 - calibrationResults$value.x / (pHat*(1-pHat))
  return(calibrationResults[ , c(keyCols, 'type', 'metric', 'value')])
}


#' Get diff results
#'
#' @param data
#' 
#' @return a data frame
#' 
getDiffResults <- function(data, testType) {
  d <- data[, c(keyCols, 'type', 'metric','value')]
  res <- merge(
    d %>% filter(.data$type == 'estimation'),
    d %>% filter(.data$type == testType),
    by = c(keyCols, 'metric')
  )
  res$sample <- factor(res$sample, levels = sort(unique(res$sample))
  )
  res$diff <- abs(res$value.x-res$value.y)
  return(res)
}

workDir <- 'C:/localdev/projects/robustness/size-test'

exteranlSamplingData <- read.csv(file.path(workDir, 'optum_full_ccae_sample.csv'), row.names = 1)
interanlSamplingData <- read.csv(file.path(workDir, 'optum_sample_ccae_full.csv'), row.names = 1)

# cat('Internal', unique(exteranlSamplingData$internalDatabase), 'external',unique(exteranlSamplingData$externalDatabase), '\n')
# cat('Internal', unique(interanlSamplingData$internalDatabase), 'external',unique(interanlSamplingData$externalDatabase), '\n')

arrangeByMetricDiff <- function(data, samplingtype) {
  results <- list()
  
  results$`AUROC` <- data %>% filter(.data$metric == 'AUROC')
  results$`Calibration` <- getCalibrationResults(data)
  results$`Brier score` <- data %>% filter(.data$metric == 'brier score')
  results$`Scaled brier score` <- getScaledBrierResults(data)
  
  if (samplingtype == 'External data sampling')
    testType <- 'Validation'
  else
    testType <- 'external'

  diffs <- list()
  for (n in names(results)) {
    diffs[[n]] <- getDiffResults(results[[n]], testType = testType)
    diffs[[n]]$samplingType <- factor(
      samplingtype, levels = c('Internal data sampling', 'External data sampling'))
    diffs[[n]]$metric <- factor(n, levels = ylabDict)
  }
  
  r <- diffs[[1]]
  for (i in 2:4)
    r <- rbind(r, diffs[[i]])
  
  return(r)
}

exteranlSamplingDiffs <- arrangeByMetricDiff(exteranlSamplingData, 'External data sampling')
interanlSamplingDiffs <- arrangeByMetricDiff(interanlSamplingData, 'Internal data sampling')

samplingDiffs <- rbind(interanlSamplingDiffs, exteranlSamplingDiffs)

rCounts <- samplingDiffs %>%
  filter(metric=='AUROC') %>%
  group_by(samplingType, sample) %>%
  dplyr::summarise(n=n()) %>%
  as.data.frame()
print(rCounts)

samplingDiffsFiltered <- samplingDiffs %>% filter(!(samplingType == 'Internal data sampling' & sample==1000) )

ggplot(
  data = samplingDiffsFiltered,
  aes(x = sample, y = abs(diff))) +
  geom_boxplot(fill = 'royalblue3') +
  facet_grid(metric ~ samplingType, scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab(TeX(glue("|$\\Delta$ score|"))) +
  xlab('Sample size') +
  theme(
    panel.background=element_rect(fill = "White", colour = "Gray"),
    panel.grid.major.y=element_line(color = 'gray95'),
    panel.grid.minor.y=element_line(color = 'gray95'),
    panel.grid.major.x=element_line(color = 'gray95'),
    panel.grid.minor.x=element_line(color = 'gray95')
  )

ggsave(file.path(workDir, 'Figure 5 size tests.png'), width = 7, height = 6, dpi = 300)



