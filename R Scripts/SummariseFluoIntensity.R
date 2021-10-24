# This script analyses data from the CountNucleiMaster macro to plot graphs  of fluorescence measurements 
#Written by Victor Kumbol, January 2021
#Modifed by Victor Kumbol, October 2021

#prepare workspace
myarg <- commandArgs()
workingDirectory <- as.character(myarg[6])
setwd(workingDirectory)  #set working dir based on command line arguments

#load required libraries
library(tidyverse)
library(xlsx)
library(ggpubr)
library(ggplot2)
library(ggrepel)

source("C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/GenericFunctions.R", chdir = TRUE)

####################################################################################################
#define functions
normalize <- function(value, maxi) {
  normalized_value = (value/maxi)*100
  return(normalized_value)
}


#define custom functions

#function to test for outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


importFluoData <- function(microscopyDataFile, microscopySequenceFile) {
  
  #read in data from files
  imageSequenceHeaderLine <- (grep("Condition", read_lines(microscopySequenceFile, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1 #Specify header line 
  imageSequence <- read.csv(file = microscopySequenceFile, colClasses = c(rep("numeric", 2), rep("character", 2)), header = TRUE, sep = ",", skip = imageSequenceHeaderLine)
  imageData <- read.csv(file = microscopyDataFile, colClasses = c("numeric", "character", rep("numeric", 6)), header = TRUE, sep = ",")
  
  imageSequence <- imageSequence %>% #process image sequence data
    rename(Original_Filename = Filename) %>%
    arrange(Original_Filename)
  
  imageData <- imageData %>%  #process image data
    rename(Processed_Filename = Label) %>%
    arrange(Processed_Filename) %>%
    select(-X)
  
  importedData <- bind_cols(imageSequence, imageData) %>% #merge sequence and count tables 
    separate(Condition, sep = "_", into = c("Treatment", "Field")) %>%
    mutate(Field = as.numeric(Field)) %>%
    mutate(Treatment = parseStandardName(trimws(Treatment)))
  importedData$Treatment <- factor(importedData$Treatment, levels = TreatmentLevels)
  return(importedData)
}


isAnySaturated <- function(DataToProcess, maxGrayValue) {
  #this function checks if any of the images contain saturated pixels
  return(sum(DataToProcess$Max>=maxGrayValue))
}




processFluoData <- function(combinedData, analysisLogFile, resultsExcelFile, maxGrayValue) {
  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  unsaturatedcombinedData <- combinedData %>%
    filter(Max < maxGrayValue)
  
  combinedData <- combinedData %>% #report outliers
    mutate(Suspected.Outliers=Original_Filename) %>% 
    group_by(Treatment) %>% 
    mutate(is_outlier=ifelse(is_outlier(Mean), Mean, as.numeric(NA)))
  combinedData$Suspected.Outliers[which(is.na(combinedData$is_outlier))] <- as.numeric(NA)
  combinedData <- combinedData %>%
    select(-is_outlier)
  
  #summarise the mean for every coverslip
  combinedDataPerCoverslip <- combinedData %>%   
    group_by(Treatment, Coverslip) %>%
    summarise(Mean = mean(Mean))
  combinedDataPerCoverslipUnsaturated <- unsaturatedcombinedData %>%
    group_by(Treatment, Coverslip) %>%
    summarise(Mean = mean(Mean)) 
  
  #calculate mean of null group
  nullGroupStats <- combinedDataPerCoverslip %>% 
    filter(Treatment=="Null") %>%
    summarise(AvgMean = mean(Mean))
  nullGroupStatsUnsaturated <- combinedDataPerCoverslipUnsaturated %>% 
    filter(Treatment=="Null") %>%
    summarise(AvgMean = mean(Mean))
  
  #Normalize all data to null group mean
  combinedDataPerCoverslip <- combinedDataPerCoverslip %>% #
    mutate(NormalizedMean = normalize(Mean, nullGroupStats$AvgMean))
  combinedDataPerCoverslipUnsaturated <- combinedDataPerCoverslipUnsaturated %>% 
    mutate(NormalizedMean = normalize(Mean, nullGroupStatsUnsaturated$AvgMean)) 
  
  
  #Prepare summary report for each treatment
  summaryReport <- combinedDataPerCoverslip %>% 
    group_by(Treatment) %>%
    summarise(RawMean = mean(NormalizedMean))
  summaryReportUnsaturated <- combinedDataPerCoverslipUnsaturated %>%
    group_by(Treatment) %>%
    summarise(RawMean = mean(NormalizedMean))
  combinedSummaryReport <- full_join(summaryReport, summaryReportUnsaturated, by="Treatment")
  
  analysisParameters <- read.csv(file = analysisLogFile, sep = ",", header = TRUE) #read data from analysis logfile
  analysisParameters <- analysisParameters %>%
    add_row(Parameter="AnySaturated?", Value=as.character(isAnySaturated(DataToProcess, maxGrayValue)))
  
  #save the results to an excel spreadsheet
  write.xlsx(as.data.frame(combinedData), file = resultsExcelFile, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, showNA = FALSE)
  write.xlsx(as.data.frame(combinedSummaryReport), file = resultsExcelFile, sheetName = "Fluorescence Summary", col.names = TRUE, row.names = FALSE, append = TRUE, showNA = FALSE)
  write.xlsx(as.data.frame(analysisParameters), file = resultsExcelFile, sheetName = "Analysis Parameters", col.names = FALSE, row.names = FALSE, append = TRUE, showNA = FALSE)
  return(combinedData)
}



plotFluoGraphs <- function(DataToPlot, resultsGraphFile) {
  scaledWidth = 150*length(unique(DataToPlot$Treatment)) #scale width to accomodate treatments
  
  p <- ggplot(DataToPlot, aes(y=Mean, x=Treatment)) + geom_boxplot() +  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+ geom_text_repel(aes(label = Suspected.Outliers), na.rm = TRUE, show.legend = FALSE) + theme_pubr() + plottheme
  
  Graph <- annotate_figure(p, top = text_grob(experimentId, face = "bold", size = 14))
  ggexport(Graph, filename = resultsGraphFile, width = scaledWidth, height = 750, res=100)
}

####################################################################################################

#define key global variables
experimentId <- str_split(workingDirectory, "/", simplify = TRUE)
experimentId <- experimentId[length(experimentId)]

#Specify input files
microscopySequenceFile <- paste(experimentId, "_Image_Capture.txt", sep = "")
microscopyDataFile <- paste(experimentId, "_Fluo_Intensity.csv", sep = "")
analysisLogFile <- paste(experimentId, "_AnalysisLog.txt", sep = "")
analysisParameters <- read.csv(file = analysisLogFile, sep = ",", header = TRUE) #read data from analysis logfile

#Specify output files
ParameterAnalyzed <- trimws(analysisParameters$Value[str_detect(analysisParameters$Parameter, "Parameter Analyzed")])
resultsExcelFile <- paste(experimentId, ParameterAnalyzed, "Fluorescence.xlsx", sep = "_") 
resultsGraphFile <- paste(experimentId, ParameterAnalyzed, "Fluorescence.tiff", sep = "_")

#Run Analysis
DataToProcess <- importFluoData(microscopyDataFile, microscopySequenceFile) #import data
DataToPlot <- processFluoData(DataToProcess, analysisLogFile, resultsExcelFile, maxGrayValue=4095) #process data
plotFluoGraphs(DataToPlot, resultsGraphFile) #plot graphs


#update meta results sheet
updateMetaResults(experimentId, ParameterAnalyzed = paste(ParameterAnalyzed, "Fluorescence"), resultsExcelFile, metaResultsFile = metaResultFilemMORPH) #update metaresults sheet
