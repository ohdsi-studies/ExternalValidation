rm(list = ls())
library(glue)
library(dplyr)

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

# The table 1 was done on the target population before excluding outcomes, 
# so the number of patients in table 1 will differ slightly compared to the population sizes in 
# ‘outcomerate_ohdsi_formatted.csv’. 
data.file <- "../data/external_est_perf/table1_ohdsi_formatted.csv"
# outcome file includes population size and outcome rate for each analysis and for each outcome
outcome.file <- "../data/external_est_perf/outcomerate_ohdsi_formatted.csv"
table1.file <- "../data/table1.csv"
shortnames.file <- "../data/table1_ohdsi_shortnames.csv"


data.df <- read.csv(data.file, stringsAsFactors = F)
data.df[is.na(data.df)] <- 0
outcome.df <- read.csv(outcome.file, stringsAsFactors = F)
shortnames.df <- read.csv(shortnames.file, stringsAsFactors = F)

dbIds    <-c("ccae", "mdcd", "mdcr", "optum.ehr", "optum.ses" )
dbNames <- c("CCAE", "MDCD", 'MDCR', "Optum EHR", "Optum SES")
ns <- c(2365324,	660158,	205789,	3309284,	1678579)  # Communicated by email on 02/06/2023
age0.19  <- c("age group:   0 -   4", "age group:   5 -   9", "age group:  10 -  14", "age group:  15 -  19")
age20.64 <- c("age group:  20 -  24", "age group:  25 -  29", "age group:  30 -  34", "age group:  35 -  39", 
              "age group:  40 -  44", "age group:  45 -  49", "age group:  50 -  54", "age group:  55 -  59", 
              "age group:  60 -  64" )
age.ge65 <- c("age group:  65 -  69", "age group:  70 -  74", "age group:  75 -  79", "age group:  80 -  84", 
              "age group:  90 -  94", "age group:  85 -  89", "age group:  95 -  99", "age group: 100 - 104", 
              "age group: 105 - 109") 

shortnames.df <- shortnames.df[!is.na(shortnames.df['shortName']), ]
phenotypes.df <- merge(shortnames.df, data.df, by = 'covariateName')


Table1 <- data.frame(matrix(nrow = 0, ncol = 2*length(dbIds)+1))
idx1 <- (1:length(dbIds))*2
idx2 <- idx1+1
colnames(Table1)[idx1] <- dbIds
colnames(Table1)[idx2] <- ''

Table1['N', idx1] <- ns
Table1['N', 1] <- 'N'
femaleRates <- data.df[data.df$covariateName == "gender = FEMALE", dbIds]
Table1['female', idx2] <- femaleRates*100
Table1['female', idx1] <- femaleRates*ns
Table1['female', 1] <- 'Female'

for (s in c('age0.19', 'age20.64', 'age.ge65')) {
  ageRates <- colSums(data.df[data.df$covariateName %in% eval(parse(text = s)), dbIds])
  ageRates <- ageRates
  Table1[s, idx2] <- ageRates * 100
  Table1[s, idx1] <- round(ageRates * ns)
  Table1[s, 1] <- s
}
outcome.df$database <- gsub(' ', '.', outcome.df$database)
stopifnot(all(outcome.df$database == dbIds))
outcomes <- c('seizure', 'diarrhea', 'fracture', 'GI.bleed', 'insomnia')

for (i in 1:length(outcomes)) {
  n_o <- outcome.df[, glue("Analysis_{i}_outcomeCount")]
  Table1[outcomes[i],  idx1] <- n_o
  n_pop <- outcome.df[, glue("Analysis_{i}_populationSize")]
  # Table1[glue('np-{outcomes[i]}'),  idx1] <- n_pop
  Table1[outcomes[i],  idx2] <- n_o/n_pop*100
  Table1[outcomes[i], 1] <- outcomes[i]
}

Table1[idx2] <- lapply(Table1[idx2], round, 1)
# df <- sapply(df, as.character)
Table1[is.na(df)] <- ''
colnames(Table1)[1] <- ''
write.csv(format(Table1, nsmall=1), table1.file, row.names = F)

###################################################################################################
# Export to Latex 

outFile <- file(file.path('../data/table1-auto.tex'), 'w')
writeLines(
  "
  \\begin{table}[ht]
  \\centering
  \\begin{tabular}{lrlrlrlrlrl}
  \\toprule",
  con = outFile
)
# Header
l <- paste(dbNames, collapse = "} & \\multicolumn{2}{c}{", sep = '')
l <- paste("  & \\multicolumn{2}{c}{", l, "} \\\\", collapse = '', sep = '')
writeLines(l, con = outFile)
writeLines("  \\midrule", con = outFile)
# N
l <- paste(Table1['N', dbIds], collapse = "} & \\multicolumn{2}{c}{", sep = '')
l <- paste("  N & \\multicolumn{2}{c}{", l, "} \\\\", collapse = '', sep = '')
writeLines(l, con = outFile)

row.dict <- list(
  Female="Female",
  "age0.19" = "Age $\\leq$19",
  "age20.64" = "Age 20-64",
  "age.ge65" = "Age $\\geq$65",
  "seizure" = "Seizure",
  "diarrhea" = "Diarrhea",
  "fracture" = "Fracture",
  "GI.bleed" = "GI bleed",
  "insomnia" = "Insomnia"
)

for (i in 2:nrow(Table1)) {
  l  <- paste("  ", row.dict[Table1[i, 1]], collapse = '', sep = '')
  for (j in 1:length(dbIds)) {
    l <- paste(l, glue("\t& {Table1[i, j*2]} & ({Table1[i, j*2+1]}\\%)"), collapse = '', sep = '')
  }
  l <- paste(l, '\\\\')
  writeLines(l, con = outFile)
}


# End of table
writeLines(
  "
  \\botrule
  \\end{tabular}
  \\caption{}
  \\label{tab:table1}
  \\end{table}",
  con = outFile
)
close(outFile)

# Phenotypes
phentype.table <- data.frame(matrix(nrow = nrow(phenotypes.df), ncol = 2*length(dbIds)+1))
colnames(phentype.table)[idx1] <- dbIds
colnames(phentype.table)[idx2] <- ''
phentype.table[ , idx1] <- as.matrix(phenotypes.df[, 3:(length(dbIds)+2)]) %*% ns
phentype.table[ , idx2] <- phenotypes.df[, 3:(length(dbIds)+2)]*100
phentype.table[ , 1] <- phenotypes.df[, 'shortName']
phentype.table[idx2] <- lapply(phentype.table[idx2], round, 1)
write.csv(phentype.table, '../data/phenotypes.csv', row.names = F)
