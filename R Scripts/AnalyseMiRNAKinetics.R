# This script analyses data from the miRDistribution macro to plot graphs of relative distribution of uptaken miRNAs
#Written by Victor Kumbol, August 2021



#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(bayestestR)


#define designations for channels
treatment <- "miR-92a"
channelMarkers <- tibble(Channel = c("red", "redInverse", "green", "blue"), Region = c("Endosomal miRNA", "Non-Endosomal miRNA", "Endosomal DAPI", "Endosomal phRodored"))
resultsExcelFile <- paste(treatment, "Kinetics.xlsx", sep = "")
resultsGraphFile <- paste(treatment, "Kinetics.tiff", sep = "")


frameInterval <- 2 #interval between frames
timeOffset <- 6/60 #time offset after addition of miRNA mix

# #retrieve image sequence information from file
# imageSequence <- read.csv("Image_Capture.txt")
# imageSequence <- imageSequence %>%
#   select(Filename, Treatment, Well)



#create a function to summarise the results from all files into on tibble
processUptakeData <- function(working_directory) {
  setwd(working_directory)
  
  #retrieve list of results files
  FilesToProcess <- list.files(working_directory, pattern = "*.csv")
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
    separate(Filename, into = c("ExpID", "FrameID"), sep = "_miRKinetics_" ) %>%
    separate(FrameID, into = c("FrameID", NA), sep = "\\.") %>%
    mutate(FrameID = as.numeric(FrameID), Slice = as.numeric(Slice)) %>%
    mutate(Channel = str_replace(Channel, "red inverse", "redInverse"))
  
  return(ProcessedData)
}
  
  




exportResults <- function(combinedData, timeOffset, frameInterval, resultsExcelFile) { #This function exports data tables
  
  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  combinedDataPerSlice <- combinedData %>% 
    select(-Area) %>%
    pivot_wider(names_from = Channel, values_from = Mean) %>% 
    mutate(prop = red/(red + redInverse)) %>% #compute other parameters
    pivot_longer(red:prop, names_to = "Channel", values_to = "Value") %>%
    mutate(RecordingTime.h = (FrameID*frameInterval)/60) %>%
    mutate(AbsoluteTime.h = RecordingTime.h + timeOffset)
  combinedDataPerSlice <- left_join(combinedDataPerSlice, channelMarkers, by = "Channel")
  
  
  combinedDataPerFrame <- combinedData %>%
    group_by(ExpID, FrameID, Channel) %>%
    summarise(Area = mean(Area), Mean = mean(Mean)) %>%
    group_by(ExpID, Channel) %>%
    mutate(NormalisedMean = (Mean/first(Mean))) %>%
    mutate(RecordingTime.h = (FrameID*frameInterval)/60) %>%
    mutate(AbsoluteTime.h = RecordingTime.h + timeOffset)
  combinedDataPerFrame <- left_join(combinedDataPerFrame, channelMarkers, by = "Channel")
   
  # write.xlsx(as.data.frame(combinedDataPerSlice), file = resultsExcelFile, sheetName = "combinedDataPerSlice", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  write.xlsx(as.data.frame(combinedDataPerFrame), file = resultsExcelFile, sheetName = "combinedDataPerFrame", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  
  #Plot graph of proportion
  # combinedDataPerSlice %>%
  #   filter(Channel %in% c("prop")) %>%
  #   ggline(x= "AbsoluteTime.h", y = "Value", color ="#006600",  numeric.x.axis = TRUE, xticks.by = 1, ylab = "Endosomal/Non-Endosomal Proportion", add = "mean")
  # 
  
  #Plot graph of Endosomal and Non-Endosomal miRNA
  # combinedDataPerSlice %>%
  #   filter(Channel %in% c("red", "redInverse")) %>%
  #   ggline(x= "AbsoluteTime.h", y = "Value", linetype = "Region", shape = "Region", color ="#006600", numeric.x.axis = TRUE, xticks.by = 1, ylab = "Mean Fluorescence Intensity", add = "mean", point.size = 1)
  
  #Plot Normalised Fluorescence
  kineticsPlot <- combinedDataPerFrame %>%
    ggline(x= "AbsoluteTime.h", y = "NormalisedMean", ylab = "Relative Fluorescence Intensity", linetype = "Region", shape = "Region", color ="#006600", numeric.x.axis = TRUE, xticks.by = 1)
  
  return(kineticsPlot)
}






#request user to select a source folder 
working_directory <- choose.dir(default = getwd(), caption = "Select Source folder")
DataToExport  <- processUptakeData(working_directory)
Graphs <- exportResults(DataToExport, timeOffset, frameInterval, resultsExcelFile)

tiff(resultsGraphFile, width = 1300, height = 750, res=100)
annotate_figure(Graphs, top = text_grob(treatment, face = "bold", size = 14))
garbage <- dev.off()


