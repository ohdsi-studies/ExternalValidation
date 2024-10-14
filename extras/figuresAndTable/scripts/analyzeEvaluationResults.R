rm(list = ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
source('./summarizeExperiments.R')
source('./analysisDefinitions.R')
library(plyr)
library(latex2exp)
library(ggpubr)

diffPlots <- list()

for (metric in metrics) {
  allSummary <- summarizeAllExperiments(workDir, experimentDirs, dbs, metric)
  allResults <- merge(allSummary$results, analysesMap, by='analysis')
  
  allResults$internalDatabase  <- names(dbMap)[match(allResults$internalDatabase, dbMap)]
  allResults$externalDatabase  <- names(dbMap)[match(allResults$externalDatabase, dbMap)]
  
  failedSummary <- merge(allSummary$failed, analysesMap, by='analysis')
  failedSummary$internalDatabase  <- names(dbMap)[match(failedSummary$internalDatabase, dbMap)]
  failedSummary$externalDatabase  <- names(dbMap)[match(failedSummary$externalDatabase, dbMap)]
  
  results2 <- reshapeResults(allResults)  # TODO consider doing this from the start

  for (cname in names(comparisons)) {
    cat(metric, cname, '\n')
    plotRawResults(results2, comparisons[[cname]], metric)
    ggsave(file.path(workDir, glue('{cname} {metric}.png')), width = 7, height = 7, dpi = 300)
    plotFailedEstimations(failedSummary, comparisons[[cname]], metric)
    ggsave(file.path(workDir, glue('{cname} {metric} success.png')), dpi=300)
    d <- plotPerformenceDifference(allResults, cname, metric)
    if (cname == 'main')
      diffPlots[[metric]] <- d
  }
}

# bgcolor("White") +
  
ggarrange(
  diffPlots$AUROC, #  + rremove("xlab") 
  diffPlots$calibration,
  diffPlots$`brier score`, #  + rremove("xlab") 
  diffPlots$`Scaled brier score`,
  labels = c("a)", "b)", "c)", "d)"),
  ncol = 1, nrow = 4,
  align = 'v') + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill = "White", colour = "White"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "White", colour = "White"),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.spacing=unit(c(0,0,0,0), "cm"))
ggsave(file.path(workDir, 'Figure 4 performance differences.png'), width = 7, height = 8, dpi = 300)

