# This script This script collates results for all analysis under the mMORPH project i.e. miRDistribution & miRKinetics
#Written by Victor Kumbol, September 2021




#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(lubridate)
library(rstatix)
library(broom)

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
    ggline(x= "Duration", y= "NormalisedMean", color ="Treatment", palette = "npg", ylab = "Relative Fluorescence Intensity", numeric.x.axis = TRUE, xticks.by = 1, size = 1,  add = c("mean_sd", "jitter"), title = "Total miRNA Uptake")
  
  p2 <- pooledData %>% #plot graph of uptake facted by subcellular compartments
    filter(Region!="Total") %>%
    ggline(x= "Duration", y= "NormalisedMean", color ="Treatment",facet.by = "Region", palette = "npg", ylab = "Relative Fluorescence Intensity", numeric.x.axis = TRUE, xticks.by = 1, size = 1, add = c("mean_sd", "jitter"), title = "miRNA Uptake into Various Compartments")
  
  ggexport(p1, filename = totalUptakeGraphFile, width = 1000, height = 750, res=100)
  ggexport(p2, filename = compartmentsUptakeGraphFile, width = 2250, height = 750, res=100)
  
  
  # pooledData %>% #plot graph of uptake faceted by treatments
  #   filter(Region!="Total") %>%
  #   ggline(x= "Duration", y= "NormalisedMean", color ="Region",facet.by = "Treatment", palette = "npg", ylab = "Relative Fluorescence Intensity", numeric.x.axis = TRUE, xticks.by = 1, size = 1, title = "miRNA Uptake into Various Compartments")
}
#---------------------------------------------------------------------------------------------------


analyseMirKinetics <- function(pooledData) {
  #update list of experiments to be analysed here
  #AlexaList <- c("2021_11_6_VK01", "2021_10_9_VK01", "2021_11_12_VK01", "2021_11_17_VK")
  miR92List <- c("2021_10_13_VK03", "2021_10_9_VK", "2021_11_6_VK")
  #miR124List <- c("2021_10_13_VK01", "2021_11_12_VK", "2021_11_6_VK02", "2021_11_17_VK01")
  miR124List <- c("2021_11_12_VK", "2021_11_6_VK02", "2021_11_17_VK01")
  AlexaList <- c("2021_11_6_VK01", "2021_11_12_VK01", "2021_11_17_VK")
  selectExperimentsList <- list(AlexaList, miR92List, miR124List)
  
  pooledDataPerTreatment <- list()
  for (i in seq_along(selectExperimentsList)) {
    treatmentData <- pooledData %>%
      filter(ExperimentID%in%selectExperimentsList[[i]])
    treatment <- unique(treatmentData$Treatment)
    resultsGraphFile <- paste(pooledResultsFolder, treatment, ".tiff", sep = "")
    plotMiRKineticsGraphs(treatmentData, DurationOfInterest = 4, errorType = "mean_se", treatment, resultsGraphFile)
  }
}

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
analyseFluorescence <- function(pooledData) {
  #This function summarises the pooled data and exports the graphs and data tables
  
  #specify output files
  outputWorkbookName = paste(pooledResultsFolder, unique(DataToSummarise$Model.System), "_",unique(DataToSummarise$Parameter.Analyzed),".xlsx", sep = "")
  fluorescenceGraphFile <- paste(pooledResultsFolder, unique(DataToSummarise$Parameter.Analyzed), ".tiff", sep = "")

  #export data to excel sheets
  if (file.exists(outputWorkbookName)){   #overwrite any existing results file
    file.remove(outputWorkbookName)
  } 
  write.xlsx(as.data.frame(pooledData), file = outputWorkbookName, sheetName = "Summary")
  
  #plot graphs
  p1 <- pooledData %>%  #plot graph of total relative uptake of miRNAs
    ggbarplot(x="Treatment", y = "RawMean", add = c("mean", "jitter"), size = 1,  ylab = "Relative Fluorescence Intensity")
  
  Graph <- annotate_figure(p1, top = text_grob(DataToSummarise$Parameter.Analyzed, face = "bold", size = 14))
  ggexport(Graph, filename = fluorescenceGraphFile, width = 750, height = 750, res=100)
}
#---------------------------------------------------------------------------------------------------

analyzeUptakeInhibition <- function(pooledData) {
  
  #Normalise mean fluorescence for each experiment to the corresponding DMSO value
  exportData <- pooledData %>%
    group_by(ExperimentID, Treatment, ChannelLabel) %>%
    mutate(NormalizedMean = (Mean/Mean[str_detect(Medium, "DMSO")])) 
  
  #rearrange data for exporting
  exportData <- exportData %>%
    select(ExperimentID, Treatment, Medium, Mean, NormalizedMean, ChannelLabel) %>%
    pivot_wider(names_from = c("Medium"), values_from = c("Mean", "NormalizedMean")) %>%
    arrange(Treatment, ChannelLabel)
  
  #specify output files
  outputWorkbookName = paste(pooledResultsFolder, unique(DataToSummarise$Model.System), "_",unique(DataToSummarise$Parameter.Analyzed),".xlsx", sep = "")
  
  #export data
  write.xlsx(as.data.frame(exportData), file = outputWorkbookName, sheetName = "Summary")
  
  
}






#----------------------------------------------------------------------------------------------------


#PART TO ADAPT FOR ANALYSIS
#specify project, model system and parameter to analyse
projectTitle <- "mMORPH" 
parameterToAnalyze <- "UptakeInhibition"
modelSystem = "NeuronsWT"




#define output folder and results file based on project under analysis
if(projectTitle == "mMORPH"){
  pooledResultsFile <- metaResultFilemMORPH
  pooledResultsFolder <- pooledResultsFoldermMORPH
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
  resultsSheetIndex <- 1
  variablesToKeep <- c("ExperimentID","Well", "Treatment","Duration", "Region", "MeanFluorescence", "Normalized.Fluorescence")
  DataToSummarise <- masterDataSheet %>% #filter for only relevant data
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze)
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyseMirDistribution(pooledData)
} 


if(parameterToAnalyze =="MiRKinetics"){
  resultsSheetIndex <- 1
  variablesToKeep <- c("ExperimentID", "Treatment", "ChannelLabel","AbsoluteTime.h", "Region", "Mean", "NormalisedMean")
  DataToSummarise <- masterDataSheet %>%
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze)
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyseMirKinetics(pooledData)
} 


if(parameterToAnalyze =="UptakeInhibition"){
  resultsSheetIndex <- 2
  variablesToKeep <- c("ExperimentID", "Treatment", "Medium", "ChannelLabel", "Area", "Mean", "RawIntDen", "NormIntDen", "Count")
  DataToSummarise <- masterDataSheet %>%
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze)
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyzeUptakeInhibition(pooledData)
} 

if(parameterToAnalyze %in% c("SynaptophysinFluorescence")){
  resultsSheetIndex <- 2
  variablesToKeep <- c("ExperimentID", "Treatment", "RawMean","UnsaturatedMean")
  DataToSummarise <- masterDataSheet %>%
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze)
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyseFluorescence(pooledData)
}





pooledData <- pooledData %>%
  group_by(ExperimentID, Treatment, ChannelLabel) %>%
  mutate(NormalizedMean = (Mean/Mean[str_detect(Medium, "DMSO")])) 




pooledData %>%
  #filter(ChannelLabel!="pHrodoRed") %>%
  ggbarplot(x="Medium", y = "NormalizedMean", add = c("mean_sd", "jitter"), size = 1, color = "Medium", palette = "npg", position = position_dodge(0.9), facet.by = c("Treatment", "ChannelLabel"))





pooledData %>%
  filter(ChannelLabel=="miRNA") %>%
  # filter(!(ExperimentID %in% c("2021_12_1_VK", "2021_12_2_VK"))) %>%
  group_by(Treatment, Medium, ChannelLabel) %>%
  summarise(mean(NormalizedMean))







# 
# summary <- pooledData %>%
#   group_by(Treatment, ChannelLabel, Medium) %>%
#   summarise(mean(Mean), mean(RawIntDen))
#   


testData <- pooledData %>%
  filter(ChannelLabel=="miRNA") %>%
  #filter(!(ExperimentID %in% c("2021_12_1_VK", "2021_12_2_VK"))) %>%
  select(ExperimentID, Treatment, Medium, NormalizedMean,  ChannelLabel) %>%
  pivot_wider(names_from = c("Medium"), values_from = c("NormalizedMean")) %>%
  arrange(Treatment, ChannelLabel)

# t.test(testData$DMSO[[2]], testData$DYNA[[2]])

statTest <- testData %>% 
  #filter(Treatment!="unstimulated") %>%
  group_by(Treatment) %>% 
  do(tidy(t.test(.$DMSO, .$DYNA)))
