# This script analyses data from the miRDistribution macro to plot graphs of kinetics of miRNA uptake
#Written by Victor Kumbol, August 2021


#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(bayestestR)


#define key global variables
experimentId <- str_split(workingDirectory, "/", simplify = TRUE)
experimentId <- experimentId[length(experimentId)]


#define designations for parameters
treatment <- "miR-92"
ChannelLabels <- tibble(Channel = c("red", "green", "blue"), ChannelLabel = c("phRodored", "miRNA", "DAPI"))
ParameterLabels <- tibble(Parameter = c("red", "redInverse", "prop"), Region = c("Endosomal", "Non-Endosomal", "Endosomal Proportion"))
resultsExcelFile <- paste(treatment, "Kinetics.xlsx", sep = "")
resultsGraphFile <- paste(treatment, "Kinetics.tiff", sep = "")


frameInterval <- 30 #interval between frames
timeOffset <- 6/60 #time offset after addition of miRNA mix
sliceSelection <- c(10, 11, 12, 13, 15, 16)
# #retrieve image sequence information from file
# imageSequence <- read.csv("Image_Capture.txt")
# imageSequence <- imageSequence %>%
#   select(Filename, Treatment, Well)



#create a function to summarise the results from all files into on tibble
processUptakeData <- function(workingDirectory) {
  setwd(workingDirectory)
  
  #retrieve list of results files
  FilesToProcess <- list.files(workingDirectory, pattern = "*.csv")
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
  
  
  return(ProcessedData)
}






exportResults <- function(combinedData, timeOffset, frameInterval, resultsExcelFile) { #This function exports data tables
  
  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  combinedDataPerSlice <- combinedData %>% 
    select(-Area) %>%
    pivot_wider(names_from = Parameter, values_from = Mean) %>% 
    mutate(prop = red/(red + redInverse)) %>% #compute other parameters
    pivot_longer(red:prop, names_to = "Parameter", values_to = "Value") %>%
    mutate(RecordingTime.h = ((FrameID-1)*frameInterval)/60) %>%
    mutate(AbsoluteTime.h = RecordingTime.h + timeOffset)
  combinedDataPerSlice <- left_join(combinedDataPerSlice, ParameterLabels, by = "Parameter")
  combinedDataPerSlice <- left_join(combinedDataPerSlice, ChannelLabels, by = "Channel")
  
  combinedDataPerFrame <- combinedData %>%
    group_by(Filename, FrameID, Channel, Parameter) %>%
    summarise(Area = mean(Area), Mean = mean(Mean)) %>%
    group_by(Filename, Channel, Parameter) %>%
    mutate(NormalisedMean = (Mean/first(Mean))) %>%
    mutate(RecordingTime.h = ((FrameID-1)*frameInterval)/60) %>%
    mutate(AbsoluteTime.h = RecordingTime.h + timeOffset)
  combinedDataPerFrame <- left_join(combinedDataPerFrame, ParameterLabels, by = "Parameter")
  combinedDataPerFrame <- left_join(combinedDataPerFrame, ChannelLabels, by = "Channel")
  
  # write.xlsx(as.data.frame(combinedDataPerSlice), file = resultsExcelFile, sheetName = "combinedDataPerSlice", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  # write.xlsx(as.data.frame(combinedDataPerFrame), file = resultsExcelFile, sheetName = "combinedDataPerFrame", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  kineticsPlots <- list()
  #Plot graph of proportion
  p1 <- combinedDataPerSlice %>%
    filter(Region %in% c("Endosomal Proportion")) %>%
    filter(ChannelLabel%in% c("miRNA", "DAPI", "phRodored")) %>%
    ggline(x= "AbsoluteTime.h", y = "Value", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Endosomal/Non-Endosomal Proportion", add = "mean_se")
  kineticsPlots <- append(kineticsPlots, list(p1))
  
  #Plot graph of Mean Fluorescence with SE
  p2 <- combinedDataPerSlice %>%
    filter(Region=="Endosomal") %>%
    ggline(x= "AbsoluteTime.h", y = "Value", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Mean Fluorescence Intensity", add = "mean_se")
  kineticsPlots <- append(kineticsPlots, list(p2))
  
  
  #Plot graph of Endosomal and Non-Endosomal miRNA
  # combinedDataPerSlice %>%
  #   filter(Region %in% c("Endosomal", "Non-Endosomal")) %>%
  #   ggline(x= "AbsoluteTime.h", y = "Value", color ="ChannelLabel", facet.by = "ChannelLabel",palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Mean Fluorescence Intensity", add = "mean", point.size = 1)
  
  #Plot Normalised Fluorescence
  # kineticsPlot <- combinedDataPerFrame %>%
  #   filter(Region=="Endosomal") %>%
  #   ggline(x= "AbsoluteTime.h", y = "Mean", ylab = "Relative Fluorescence Intensity", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1)
  # 
  kineticsPlots <- ggarrange(plotlist=kineticsPlots, nrow = 2)
  return(kineticsPlots)
}






#request user to select a source folder 
workingDirectory <- choose.dir(default = getwd(), caption = "Select Source folder")
DataToExport  <- processUptakeData(workingDirectory)

DataToExport <- DataToExport %>% #filter for selected slices
  filter(Slice %in% sliceSelection)

Graphs <- exportResults(DataToExport, timeOffset, frameInterval, resultsExcelFile)

tiff(resultsGraphFile, width = 1500, height = 1500, res=100)
annotate_figure(Graphs, top = text_grob(treatment, face = "bold", size = 14))
garbage <- dev.off()







# #Play ground
combinedData <- DataToExport

combinedDataPerSlice %>%
  filter(Slice %in% sliceSelection)%>%
  filter(Region=="Endosomal") %>%
  ggline(x= "AbsoluteTime.h", y = "Value", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Mean Fluorescence Intensity", add = "mean_se")


# combinedDataPerSlice2 <- combinedData %>%
#   pivot_wider(names_from = Parameter, values_from = Area:Mean) %>%
#   mutate(Mean_prop = Mean_red/(Mean_red + Mean_redInverse), Area_prop = Area_red/(Area_red + Area_redInverse)) %>% #compute other parameters
#   pivot_longer(Area_red:Area_prop, names_to = "Parameter", values_to = "Value") %>%
#   mutate(RecordingTime.h = ((FrameID-1)*frameInterval)/60) %>%
#   mutate(AbsoluteTime.h = RecordingTime.h + timeOffset)
# 

# combinedDataPerSlice2 %>%
#   filter(Parameter %in% c("Area_prop", "Mean_prop")) %>%
#   filter(Channel%in% c("blue", "green", "red")) %>%
#   ggline(x= "AbsoluteTime.h", y = "Value", facet.by ="Channel", color = "Channel",  palette =  c("#0000FF", "#00FF00", "#FF0000"), shape = "Parameter", numeric.x.axis = TRUE, xticks.by = 1, ylab = "Endosomal/Non-Endosomal Proportion", add = "mean_se")
# 
# 
# combinedDataPerSlice2 %>%
#   filter(Parameter %in% c("Area_prop", "Mean_red")) %>%
#   filter(Channel%in% c("blue", "green", "red")) %>%
#   ggline(x= "AbsoluteTime.h", y = "Value", facet.by ="Channel", color = "Parameter",  palette =  c("#0000FF", "#00FF00"),  shape = "Parameter", linetype = "Parameter", numeric.x.axis = TRUE, xticks.by = 1, ylab = "Endosomal/Non-Endosomal Proportion", add = "mean_se")
# 
# 

SliceDistribution <- combinedDataPerSlice2 %>%
  filter(Channel=="green") %>%
  filter(Parameter=="Mean_red") %>%
  filter(FrameID==7) 

SliceDistribution <- combinedDataPerSlice2 %>%
  filter(Channel=="green") %>%
  filter(Parameter=="Mean_red") %>%
  filter(FrameID==7) %>%
  ggbarplot(x="FrameID", y="Value", add = c("mean_sd", "jitter"), label = TRUE)
