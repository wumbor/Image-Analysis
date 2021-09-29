# This script This script collates results for all analysis under the mMORPH project i.e. miRDistribution & miRKinetics
#Written by Victor Kumbol, September 2021




#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(lubridate)


source("C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/GenericFunctions.R", chdir = TRUE)


#Define functions in this script
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


processPooledData <- function(DataToSummarise, resultsSheet, variablesToKeep) {
  #this function imports data filtered from a particular parameter
  
  filteredResults <- list() 
  tableNames <- c()
  for (i in 1:length(DataToSummarise$ExperimentID)) {
    filteredResults <- append(filteredResults, list(read.xlsx(DataToSummarise$Result.File.Path[i], resultsSheet)))
    tableNames <- append(tableNames, DataToSummarise$ExperimentID[i])
  }
  names(filteredResults) <- tableNames #rename filtered results to corresponding ExperimentIDs
  filteredResults <- filteredResults %>% #merge all the tables by rows and id by ExperimentID
    bind_rows(.id = "ExperimentID")
  
  DurationInfo <- DataToSummarise %>% #Tag each entry based on it's duration of stimulation
    select(ExperimentID, Duration.of.stimulation) %>%
    separate(Duration.of.stimulation, sep = " ", into = c("Duration", "Unit")) %>%
    select(ExperimentID, Duration) %>%
    mutate(Duration = as.numeric(Duration))
  filteredResults <- inner_join(filteredResults, DurationInfo, by = "ExperimentID" )
  
  #add information on treatments
  if("Treatment" %in% names(filteredResults)){
    filteredResults <- filteredResults %>% #Convert treatments to factors to capture dose levels
      mutate(Treatment=parseStandardName(Treatment))
    filteredResults$Treatment <- factor(filteredResults$Treatment, levels = TreatmentLevels)
  } else {
    TreatmentInfo <- DataToSummarise %>% #Tag each entry based on it's treatment
      select(ExperimentID, miRNAs.tested) %>%
      mutate(miRNAs.tested = trimws(miRNAs.tested)) %>%
      rename(Treatment = miRNAs.tested)
    filteredResults <- inner_join(filteredResults, TreatmentInfo, by = "ExperimentID")
  }
  
  filteredResults <- filteredResults[, variablesToKeep] #retain only relevant columns
  return(filteredResults)
}
#----------------------------------------------------------------------------------------------------


analyseMirDistribution <- function(pooledData) {
  #This function summarises the pooled data and exports the graphs and data tables
  
  pooledData <- pooledData %>% #summarise data for each treatment across wells
    group_by(ExperimentID, Treatment, Duration, Region) %>% 
    summarise(NormalisedMean=mean(Normalized.Fluorescence), Mean = mean(MeanFluorescence)) %>%
    arrange(Duration)
  
  #specify output files
  outputWorkbookName = paste(pooledResultsFolder, unique(DataToSummarise$Model.System), "_",unique(DataToSummarise$Parameter.Analyzed),".xlsx", sep = "")
  totalUptakeGraphFile <- paste(pooledResultsFolder, unique(DataToSummarise$Parameter.Analyzed), "_Total", ".tiff", sep = "")
  compartmentsUptakeGraphFile <- paste(pooledResultsFolder, unique(DataToSummarise$Parameter.Analyzed), "_Compartments", ".tiff", sep = "")
  
  #filter out relevant data sets
  totalUptakeData <- pooledData %>%
    filter(Region=="Total")
  compartmentsUptakeData <- pooledData %>%
    filter(Region!="Total")
  
  #export data to excel sheets
  if (file.exists(outputWorkbookName)){   #overwrite any existing results file
    file.remove(outputWorkbookName)
  } 
  write.xlsx(as.data.frame(totalUptakeData), file = outputWorkbookName, sheetName = "Total.Uptake")
  write.xlsx(as.data.frame(compartmentsUptakeData), file = outputWorkbookName, sheetName = "Compartments.Uptake", append = TRUE)
  
  #plot graphs
  p1 <-pooledData %>% #plot graph of total uptake into cells
    filter(Region=="Total") %>%
    ggline(x= "Duration", y= "NormalisedMean", color ="Treatment", palette = "npg", ylab = "Relative Fluorescence Intensity", numeric.x.axis = TRUE, xticks.by = 1, size = 1, title = "Total miRNA Uptake")
  
  p2 <- pooledData %>% #plot graph of uptake facted by subcellular compartments
    filter(Region!="Total") %>%
    ggline(x= "Duration", y= "NormalisedMean", color ="Treatment",facet.by = "Region", palette = "npg", ylab = "Relative Fluorescence Intensity", numeric.x.axis = TRUE, xticks.by = 1, size = 1, title = "miRNA Uptake into Various Compartments")
  
  ggexport(p1, filename = totalUptakeGraphFile, width = 1500, height = 750, res=100)
  ggexport(p2, filename = compartmentsUptakeGraphFile, width = 1500, height = 750, res=100)
  
  
  # pooledData %>% #plot graph of uptake faceted by treatments
  #   filter(Region!="Total") %>%
  #   ggline(x= "Duration", y= "NormalisedMean", color ="Region",facet.by = "Treatment", palette = "npg", ylab = "Relative Fluorescence Intensity", numeric.x.axis = TRUE, xticks.by = 1, size = 1, title = "miRNA Uptake into Various Compartments")
}
#---------------------------------------------------------------------------------------------------


analyseMirKinetics <- function(pooledData) {
  
  uniqueTreatments <- unique(pooledData$Treatment)
  pooledDataPerTreatment <- list()
  for (i in seq_along(uniqueTreatments)) {
    treatmentData <- pooledData %>%
      filter(Treatment == uniqueTreatments[i]) 
    pooledDataPerTreatment <- append(pooledDataPerTreatment, list(treatmentData))
  }
  names(pooledDataPerTreatment) <- uniqueTreatments
  
  #iterate over data sets and plot graphs
  for (i in seq_along(pooledDataPerTreatment)) {
    resultsGraphFile <- paste(pooledResultsFolder, uniqueTreatments[i], ".tiff", sep = "")
    treatment <- uniqueTreatments[i]
    Graphs <- plotMiRKineticsGraphs(pooledDataPerTreatment[[i]], DurationOfInterest = 4, errorType = "mean_se", treatment, resultsGraphFile)
  }
}
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


#PART TO ADAPT FOR ANALYSIS
#specify project, model system and parameter to analyse
projectTitle <- "mMORPH" 
parameterToAnalyze <- "MiRDistribution"
modelSystem = "NeuronsWT"




#define output folder and results file based on project under analysis
if(projectTitle == "mMORPH"){
  pooledResultsFile <- metaResultFilemMORPH
  pooledResultsFolder <- pooledResultsFoldermMORPH
  resultsSheetIndex <- 1
} else if (projectTitle == "mMASSAY") {
  pooledResultsFile <- metaResultFilemASSAY
  pooledResultsFolder <- pooledResultsFoldermASSAY
  resultsSheetIndex <- 2
}


#read in data for experiment details and parse columns appropriately
metaExperimentDetails <-read.xlsx(metaExperimentDetailsFile, 4, encoding = "UTF-8", colClasses =  c(rep("character", 8), "Date", "Date")) %>%
  mutate(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>%
  distinct()
#read in data for pooled results, parse columns and remove duplicates
pooledExperimentResults <- read.csv(file = pooledResultsFile, header = TRUE, sep = ",", fileEncoding = "UTF-8", colClasses = c(rep("character", 4))) %>%
  mutate(Analysis.Date = dmy_hm(Analysis.Date)) %>%
  distinct(ExperimentID, Parameter.Analyzed, Result.File.Path, .keep_all = TRUE)
#join the two tables to create master data sheet
masterDataSheet <- inner_join(pooledExperimentResults, metaExperimentDetails, by = c("ExperimentID", "Parameter.Analyzed"))


#run analysis file based on set parameter to analyse 
if(parameterToAnalyze =="MiRDistribution"){
  variablesToKeep <- c("ExperimentID","Well", "Treatment","Duration", "Region", "MeanFluorescence", "Normalized.Fluorescence")
  DataToSummarise <- masterDataSheet %>% #filter for only relevant data
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze)
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyseMirDistribution(pooledData)
  
} else if(parameterToAnalyze =="MiRKinetics"){
  variablesToKeep <- c("ExperimentID", "Treatment", "ChannelLabel","AbsoluteTime.h", "Region", "Mean", "NormalisedMean")
  DataToSummarise <- masterDataSheet %>%
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze)
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyseMirKinetics(pooledData)
}
