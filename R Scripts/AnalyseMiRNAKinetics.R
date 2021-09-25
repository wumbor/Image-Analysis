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


importKineticsData  <- function(imageDataFolder) {
  originalWorkingDirectory <- getwd()
  setwd(imageDataFolder)
  FilesToProcess <- list.files(imageDataFolder, pattern = "*.csv") #retrieve list of results files
  importedData <- list()
  
  for (i in 1:length(FilesToProcess)) { #iterate over the files in the folder and import each
    raw_data <- read.csv(FilesToProcess[i])
    raw_data <- raw_data %>%
      select(Channel, Area, Mean, RawIntDen) %>%
      separate(Channel, into = c("Channel", "Slice"), sep = "-") %>%
      mutate(Channel = trimws(Channel))
    summary_data <- raw_data %>%
      select(-RawIntDen)
    #append results to the importedData list
    importedData <- append(importedData, list(summary_data))
  }
  names(importedData) <- FilesToProcess
  
  #merge results into one
  importedData <- bind_rows(importedData, .id="Filename")
  importedData <- importedData %>%
    rename(Parameter = Channel) %>%
    mutate(Parameter = str_replace(Parameter, "red inverse", "redInverse")) %>%
    separate(Filename, into = c("Filename", "Channel.FrameID"), sep = "_miRKinetics_" ) %>%
    separate(Channel.FrameID, into = c("Channel", "FrameID"), sep = "-") %>%
    separate(FrameID, into = c("FrameID", NA), sep = "\\.") %>%
    mutate(FrameID = as.numeric(FrameID), Slice = as.numeric(Slice))
  
  setwd(originalWorkingDirectory)
  return(importedData)
}



processKineticsData <- function(combinedData, timeOffset, frameInterval, sliceSelection, resultsExcelFile) { #This function processes the imported data table and exports them
  
  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  combinedDataPerSlice <- DataToProcess %>% 
    filter(Slice %in% sliceSelection) %>%
    pivot_wider(names_from = Parameter, values_from = c(Mean, Area)) %>% 
    mutate(Mean_prop = Mean_red/(Mean_red + Mean_redInverse), Area_prop = Area_red/(Area_red + Area_redInverse)) %>% #compute other parameters
    select(-Area_red, -Area_redInverse) %>%
    pivot_longer(Mean_red:Area_prop, names_to = "Parameter", values_to = "Value") %>%
    mutate(RecordingTime.h = ((FrameID-1)*frameInterval)/60) %>%
    mutate(AbsoluteTime.h = RecordingTime.h + (timeOffset/60)) %>%
    group_by(Filename, Channel, Parameter, Slice) %>%
    mutate(NormalisedValue = (Value/first(Value))) %>%
    ungroup() %>%
    rename(Mean = Value) %>%
    rename(NormalisedMean = NormalisedValue)
  combinedDataPerSlice <- left_join(combinedDataPerSlice, ParameterLabels, by = "Parameter")
  combinedDataPerSlice <- left_join(combinedDataPerSlice, ChannelLabels, by = "Channel")
  
  
  combinedDataPerFrame <- combinedDataPerSlice %>%
    select(-c(Parameter, Channel, RecordingTime.h)) %>%
    filter(Region != "Non-Endosomal") %>%
    group_by(Filename, AbsoluteTime.h, ChannelLabel, Region) %>%
    summarise(Mean = mean(Mean), NormalisedMean = mean(NormalisedMean))
  
  write.xlsx(as.data.frame(combinedDataPerFrame), file = resultsExcelFile, sheetName = "combinedDataPerFrame", col.names = TRUE, row.names = FALSE, append = TRUE)
 
  return(combinedDataPerSlice)
}




plotCorrelationGraphs <- function(dataForPlots, channelOfInterest, DurationOfInterest, errorType, resultsGraphFile) {
  correlationPlots <- list()
  dataForPlots <- dataForPlots %>%
    filter(AbsoluteTime.h <= DurationOfInterest) #plot only 4h results
  
  a <- dataForPlots %>% #plot graph of area vs time
    filter(Region =="Endosomal/Non-Endosomal Area Proportion") %>%
    filter(ChannelLabel == channelOfInterest) %>%
    ggline(x="AbsoluteTime.h", y = "Mean", numeric.x.axis = TRUE, xticks.by = 1, add = errorType, ylab = "Endosomal Area Proportion")
  correlationPlots <- append(correlationPlots, list(a))
  
  b <- dataForPlots %>% #plot graph of normalised fluorescence intensity of miRNA and phrodoRed vs time
    filter(Region %in% c("Endosomal")) %>%
    filter(ChannelLabel %in% c("miRNA", "phRodored")) %>%
    ggline(x= "AbsoluteTime.h", y = "NormalisedMean", color = "Channel",  palette =  c("#00FF00", "#FF0000"),  numeric.x.axis = TRUE, xticks.by = 1, add = errorType, ylab = "Endosomal Fluoresence Intensity")
  correlationPlots <- append(correlationPlots, list(b))
  
  
  combinedDataForCorrelation <- dataForPlots %>%
    select(Filename:Mean, ChannelLabel) %>%
    filter(Parameter %in% c("Mean_red", "Area_prop")) %>%
    pivot_wider(names_from = Parameter, values_from = Mean) %>%
    group_by(Filename, FrameID, ChannelLabel) %>%
    summarise(Mean_red = mean(Mean_red), Area_prop = mean(Area_prop))
  
  c <- combinedDataForCorrelation %>% #plot correlation scatter plot 
    filter(ChannelLabel==channelOfInterest) %>%
    ggscatter(x="Area_prop", y="Mean_red", add = "reg.line", add.params = list(color = "blue", fill = "lightgray"), conf.int = TRUE, cor.coef = TRUE, cor.coeff.args = list(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 0.3, label.sep = "\n"), title = "miRNA Scatter Plot", ylab = "miRNA Fluorescence Intensity", xlab = "Endosomal/Non-Endosomal Area Proportion")
  correlationPlots <- append(correlationPlots, list(c))
  
  d <- combinedDataForCorrelation %>% #plot correlation scatter plot 
    filter(ChannelLabel=="phRodored") %>%
    ggscatter(x="Area_prop", y="Mean_red", add = "reg.line", add.params = list(color = "blue", fill = "lightgray"), conf.int = TRUE, cor.coef = TRUE, cor.coeff.args = list(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 0.3, label.sep = "\n"), title = "phRodoRed Scatter Plot", ylab = "phRodored Fluorescence Intensity", xlab = "Endosomal/Non-Endosomal Area Proportion")
  correlationPlots <- append(correlationPlots, list(d))
  
  correlationPlots <- ggarrange(plotlist=correlationPlots, nrow = 4)
  correlationPlots <- annotate_figure(correlationPlots, top = text_grob("Correlation Analysis", face = "bold", size = 14))
  ggexport(correlationPlots, filename = resultsGraphFile, width = 1500, height = 3000, res=100)
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
ParameterLabels <- tibble(Parameter = c("Mean_red", "Mean_redInverse", "Mean_prop", "Area_prop"), Region = c("Endosomal", "Non-Endosomal", "Endosomal/Non-Endosomal miRNA Proportion", "Endosomal/Non-Endosomal Area Proportion"))
resultsExcelFile <- paste(treatment, "_Kinetics.xlsx", sep = "")
resultsGraphFile <- paste(treatment, "_Kinetics.tiff", sep = "")
correlationGraphFile <- paste(treatment, "_Correlation.tiff", sep = "")




#run data analysis
DataToProcess  <- importKineticsData (imageDataFolder)
DataForPlots <- processKineticsData(DataToProcess, timeOffset, frameInterval, sliceSelection, resultsExcelFile)

#plot graphs
plotMiRKineticsGraphs(DataForPlots, DurationOfInterest = 4, errorType = "mean_sd", treatment, resultsGraphFile)
plotCorrelationGraphs(DataForPlots, channelOfInterest = "miRNA", DurationOfInterest = 4, errorType = "mean_sd", correlationGraphFile)

#update meta results sheet
ParameterAnalyzed <- "MiRKinetics"
metaResultsFile <- metaResultFilemMORPH
updateMetaResults(experimentId, ParameterAnalyzed, resultsExcelFile, metaResultsFile)

