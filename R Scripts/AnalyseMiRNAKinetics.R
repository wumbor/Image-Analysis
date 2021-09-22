# This script analyses the data output of the miRKinetics macro to generate miRNA uptake kinetics graphs
# Written by Victor Kumbol, August 2021

#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)


source("C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/GenericFunctions.R", chdir = TRUE)


#define custom functions
extractImageParameter <- function(parameterName, imageParameters) { 
  parameter <- imageParameters$Value[str_detect(imageParameters$Parameter, parameterName)]
  parameter <- trimws(parameter)
  parameter <- str_split(parameter, " ", simplify = TRUE)
  parameter <- parameter[1]
  return(parameter)
}


processKineticsData <- function(imageDataFolder) {
  
  originalWorkingDirectory <- getwd()
  setwd(imageDataFolder)
  
  #retrieve list of results files
  FilesToProcess <- list.files(imageDataFolder, pattern = "*.csv")
  ProcessedData <- list()
  
  #iterate over the files in the folder and process each
  for (i in 1:length(FilesToProcess)) {
    raw_data <- read.csv(FilesToProcess[i])
    
    raw_data <- raw_data %>%
      select(Channel, Area, Mean, RawIntDen) %>%
      separate(Channel, into = c("Channel", "Slice"), sep = "-") %>%
      mutate(Channel = trimws(Channel))
    
    summary_data <- raw_data %>%
      select(-RawIntDen)
    
    #append results to the ProcessedData list
    ProcessedData <- append(ProcessedData, list(summary_data))
  }
  names(ProcessedData) <- FilesToProcess
  
  
  #merge results into one
  ProcessedData <- bind_rows(ProcessedData, .id="Filename")
  ProcessedData <- ProcessedData %>%
    rename(Parameter = Channel) %>%
    mutate(Parameter = str_replace(Parameter, "red inverse", "redInverse")) %>%
    separate(Filename, into = c("Filename", "Channel.FrameID"), sep = "_miRKinetics_" ) %>%
    separate(Channel.FrameID, into = c("Channel", "FrameID"), sep = "-") %>%
    separate(FrameID, into = c("FrameID", NA), sep = "\\.") %>%
    mutate(FrameID = as.numeric(FrameID), Slice = as.numeric(Slice))
  
  setwd(originalWorkingDirectory)
  return(ProcessedData)
}






exportResults <- function(combinedData, timeOffset, frameInterval, sliceSelection, resultsExcelFile) { #This function exports data tables
  
  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  combinedDataPerSlice <- combinedData %>% 
    filter(Slice %in% sliceSelection) %>%
    select(-Area) %>%
    filter(!is.na(Mean)) %>% #slight edit here to handle NAs
    pivot_wider(names_from = Parameter, values_from = Mean) %>% 
    mutate(prop = red/(red + redInverse)) %>% #compute other parameters
    pivot_longer(red:prop, names_to = "Parameter", values_to = "Value") %>%
    mutate(RecordingTime.h = ((FrameID-1)*frameInterval)/60) %>%
    mutate(AbsoluteTime.h = RecordingTime.h + (timeOffset/60)) %>%
    group_by(Filename, Channel, Parameter, Slice) %>%
    mutate(NormalisedValue = (Value/first(Value))) %>%
    ungroup() 
  combinedDataPerSlice <- left_join(combinedDataPerSlice, ParameterLabels, by = "Parameter")
  combinedDataPerSlice <- left_join(combinedDataPerSlice, ChannelLabels, by = "Channel")
  
  combinedDataPerFrame <- combinedDataPerSlice %>%
    select(-c(Parameter, Channel, RecordingTime.h)) %>%
    filter(Region != "Non-Endosomal") %>%
    group_by(Filename, AbsoluteTime.h, ChannelLabel, Region) %>%
    summarise(Mean = mean(Value),  Mean.SD = sd(Value), NormalisedMean = mean(NormalisedValue), NormalisedMean.SD = sd(NormalisedValue))
  
  write.xlsx(as.data.frame(combinedDataPerFrame), file = resultsExcelFile, sheetName = "combinedDataPerFrame", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  kineticsPlots <- list()
  combinedDataPerSlice <- combinedDataPerSlice %>%
    filter(AbsoluteTime.h <= 4) #plot only 4h results
  
  #Plot graph of proportion with SD
  p1 <- combinedDataPerSlice %>%
    filter(Region %in% c("Endosomal Proportion")) %>%
    filter(ChannelLabel%in% c("miRNA", "DAPI", "phRodored")) %>%
    ggline(x= "AbsoluteTime.h", y = "Value", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Endosomal/Non-Endosomal Proportion", add = "mean_sd")
  kineticsPlots <- append(kineticsPlots, list(p1))
  
  #Plot graph of Mean Fluorescence with SD
  p2 <- combinedDataPerSlice %>%
    filter(Region=="Endosomal") %>%
    ggline(x= "AbsoluteTime.h", y = "Value", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Mean Fluorescence Intensity", add = "mean_sd")
  kineticsPlots <- append(kineticsPlots, list(p2))
  
  #Plot graph of Normalised Mean Fluorescence with SE
  p3 <- combinedDataPerSlice %>%
    filter(Region=="Endosomal") %>%
    ggline(x= "AbsoluteTime.h", y = "NormalisedValue", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Normalised Fluorescence Intensity", add = "mean_sd")
  kineticsPlots <- append(kineticsPlots, list(p3))
  
  kineticsPlots <- ggarrange(plotlist=kineticsPlots, nrow = 3)
  return(kineticsPlots)
}








#initialise workspace and allow user to define working directory
workingDirectory <- choose.dir(default = getwd(), caption = "Select Source folder")
setwd(workingDirectory)
experimentId <- str_split(workingDirectory, "\\\\", simplify = TRUE)
experimentId <- experimentId[length(experimentId)]

#Specify input files
imageParametersFile <- paste(experimentId, "_Image_Capture.txt", sep = "")
imageDataFolder <- paste(workingDirectory, "\\", experimentId, "_Results", sep = "")
imageParametersHeaderLine <- (grep("Parameter", read_lines(imageParametersFile, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1

#Read analysis parameters from file
imageParameters <- read.csv(file = imageParametersFile, header = TRUE, sep = ":", skip = imageParametersHeaderLine)
treatment <- extractImageParameter("Treatment", imageParameters)
frameInterval <- as.numeric(extractImageParameter("Frame Interval", imageParameters))
timeOffset <- as.numeric(extractImageParameter("Time Offset", imageParameters))
sliceSelectionRange <- extractImageParameter("Slice Selection", imageParameters)
sliceSelectionRange <- str_split(sliceSelectionRange, "-", simplify = TRUE)
sliceSelection <- seq(sliceSelectionRange[1], sliceSelectionRange[2],by=1)



#define designations for parameters
ChannelLabels <- tibble(Channel = c("red", "green", "blue"), ChannelLabel = c("phRodored", "miRNA", "DAPI"))
ParameterLabels <- tibble(Parameter = c("red", "redInverse", "prop"), Region = c("Endosomal", "Non-Endosomal", "Endosomal Proportion"))
resultsExcelFile <- paste(treatment, "_Kinetics.xlsx", sep = "")
resultsGraphFile <- paste(treatment, "_Kinetics.tiff", sep = "")





#run data analysis
DataToExport  <- processKineticsData(imageDataFolder)
Graphs <- exportResults(DataToExport, timeOffset, frameInterval, sliceSelection, resultsExcelFile)
tiff(resultsGraphFile, width = 1500, height = 2250, res=100)
annotate_figure(Graphs, top = text_grob(treatment, face = "bold", size = 14))
garbage <- dev.off()

#update meta results sheet
ParameterAnalyzed <- "MiRKinetics"
metaResultsFile <- metaResultFilemMORPH
updateMetaResults(experimentId, ParameterAnalyzed, resultsExcelFile, metaResultsFile)

