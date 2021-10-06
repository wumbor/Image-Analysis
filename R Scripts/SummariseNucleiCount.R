# This script analyses data from the CountNucleiMaster macro to plot graphs cell counts
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

#function to test for outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


importCountData <- function(microscopyDataFile, microscopySequenceFile) {
  
  #read in data from files
  imageSequenceHeaderLine <- (grep("Condition", read_lines(microscopySequenceFile, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1 #Specify header line 
  imageSequence <- read.csv(file = microscopySequenceFile, colClasses = c(rep("numeric", 2), rep("character", 2)), header = TRUE, sep = ",", skip = imageSequenceHeaderLine)
  imageData <- read.csv(file = microscopyDataFile, colClasses = c("character", rep("numeric", 4)), header = TRUE, sep = ",")
  
  imageSequence <- imageSequence %>% #process image sequence data
    rename(Original_Filename = Filename) %>%
    arrange(Original_Filename)
  
  imageData <- imageData %>%  #process image count data
    rename(Processed_Filename = Slice) %>%
    arrange(Processed_Filename) %>%
    select(Processed_Filename, Count)
  
  importedData <- bind_cols(imageSequence, imageData) %>% #merge sequence and count tables 
    separate(Condition, sep = "_", into = c("Treatment", "Field")) %>%
    mutate(Field = as.numeric(Field)) %>%
    mutate(Treatment = parseStandardName(trimws(Treatment)))
  importedData$Treatment <- factor(importedData$Treatment, levels = TreatmentLevels)
  return(importedData)
}




processCountData <- function(combinedData, analysisLogFile, resultsExcelFile) {
  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  combinedData <- combinedData %>% #report outliers
    mutate(Suspected.Outliers=Processed_Filename) %>% 
    group_by(Treatment) %>% 
    mutate(is_outlier=ifelse(is_outlier(Count), Count, as.numeric(NA)))
  combinedData$Suspected.Outliers[which(is.na(combinedData$is_outlier))] <- as.numeric(NA)
  combinedData <- combinedData %>%
    select(-is_outlier)
  
  combinedDataPerCoverslip <- combinedData %>%   #summarise the mean and median for every coverslip
    group_by(Treatment, Coverslip) %>%
    summarise(Mean = mean(Count), Median = median(Count))
  
  nullGroupStats <- combinedDataPerCoverslip %>% #calculate mean and median of null group
    filter(Treatment=="Null") %>%
    summarise(AvgMean = mean(Mean), AvgMedian = mean(Median))
  
  combinedDataPerCoverslip <- combinedDataPerCoverslip %>% #Normalize all counts to null group stats
    mutate(NormalizedMean = normalize(Mean, nullGroupStats$AvgMean), NormalizedMedian = normalize(Median, nullGroupStats$AvgMedian))
  
  summaryReport <- combinedDataPerCoverslip %>% #Generate summary results
    group_by(Treatment) %>%
    summarise(NormalizedMean = mean(NormalizedMean), NormalizedMedian = mean(NormalizedMedian))
  
  
  analysisParameters <- read.csv(file = analysisLogFile, sep = ",", header = TRUE) #read data from analysis logfile
  
  #save the results to an excel spreadsheet
  write.xlsx(as.data.frame(combinedData), file = resultsExcelFile, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, showNA = FALSE)
  write.xlsx(as.data.frame(summaryReport), file = resultsExcelFile, sheetName = "Nuclei Count Summary", col.names = TRUE, row.names = FALSE, append = TRUE, showNA = FALSE)
  write.xlsx(as.data.frame(analysisParameters), file = resultsExcelFile, sheetName = "Analysis Parameters", col.names = FALSE, row.names = FALSE, append = TRUE, showNA = FALSE)
  
  return(combinedData)
}



plotCountGraphs <- function(DataToPlot, resultsGraphFile) {
  scaledWidth = 100*length(unique(DataToPlot$Treatment)) #scale width to accomodate treatments
  
  p <- ggplot(DataToPlot, aes(y=Count, x=Treatment)) + geom_boxplot() +  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+ geom_text_repel(aes(label = Suspected.Outliers), na.rm = TRUE, show.legend = FALSE) + theme_pubr() + plottheme
  
  Graph <- annotate_figure(p, top = text_grob(experimentId, face = "bold", size = 14))
  ggexport(Graph, filename = resultsGraphFile, width = scaledWidth, height = 750, res=100)
}

####################################################################################################

#define key global variables
experimentId <- str_split(workingDirectory, "/", simplify = TRUE)
experimentId <- experimentId[length(experimentId)]

#Specify input files
microscopySequenceFile <- paste(experimentId, "_Image_Capture.txt", sep = "")
microscopyDataFile <- paste(experimentId, "_Nuclei_Count.csv", sep = "")
analysisLogFile <- paste(experimentId, "_AnalysisLog.txt", sep = "")

#Specify output files
resultsExcelFile <- paste(experimentId, "_Nuclei_Count.xlsx", sep = "")
resultsGraphFile <- paste(experimentId, "_Nuclei_Count.tiff", sep = "")

#Run Analysis
DataToProcess <- importCountData(microscopyDataFile, microscopySequenceFile) #import data
DataToPlot <- processCountData(DataToProcess, analysisLogFile, resultsExcelFile) #process data
plotCountGraphs(DataToPlot, resultsGraphFile) #plot graphs


#update meta results sheet
updateMetaResults(experimentId, ParameterAnalyzed = "NeuNNucleiCount", resultsExcelFile, metaResultsFile = metaResultFilemASSAY) #update metaresults sheet

