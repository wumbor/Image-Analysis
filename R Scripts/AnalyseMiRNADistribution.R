# This script analyses data from the miRDistribution macro to plot graphs of relative distribution of uptaken miRNAs
#Written by Victor Kumbol, August 2021


#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
#library(bayestestR)

source("C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/GenericFunctions.R", chdir = TRUE)

#create a function to summarise the results from all files into on tibble
importDistributionData <- function(imageDataFolder) {
  originalWorkingDirectory <- getwd()
  setwd(imageDataFolder)
  
  FilesToProcess <- list.files(imageDataFolder) #retrieve list of results files
  ImportedData <- list()
  for (i in 1:length(FilesToProcess)) { #iterate over the files in the folder and process each
    raw_data <- read.csv(FilesToProcess[i])
    raw_data <- raw_data %>%
      select(Channel, Area, Mean, RawIntDen) %>%
      separate(Channel, into = c("Channel", "Slice"), sep = "-") %>%
      mutate(Channel = trimws(Channel))
    
    summary_data <- raw_data %>%
      group_by(Channel) %>%
      summarise(TotalArea = mean(Area), MeanFluorescence = mean(Mean))
    ImportedData <- append(ImportedData, list(summary_data)) #append results to the ImportedData list
  }
  names(ImportedData) <- FilesToProcess
  
  ImportedData <- bind_rows(ImportedData, .id="Filename") #merge results into one
  setwd(originalWorkingDirectory)
  return(ImportedData)
}



processDistributionData <- function(combinedData, channelMarkers, resultsExcelFile) {
  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  combinedData <- combinedData%>% # Organise information on wells and treatments
    mutate(Label = Filename) %>%
    separate(Label, into = c("Well", "Label"), sep = "-") %>%
    separate(Label, into = c("Treatment", "Index", NA), sep = "_")
  combinedData <- inner_join(combinedData, channelMarkers, by = "Channel") #Add information on channels
  
  combinedDataPerWell <- combinedData %>%  #summarise data per well i.e. unique n's
    select(-TotalArea) %>%
    group_by(Well, Treatment, Region) %>%
    summarise(MeanFluorescence = mean(MeanFluorescence))
  
  combinedDataPerWell <- combinedDataPerWell %>%  #normalise fluorescence to untreated wells
    group_by(Region) %>%
    mutate(Normalized.Fluorescence = MeanFluorescence/MeanFluorescence[(str_which(Treatment, "Null"))]) %>%
    ungroup()
  
  write.xlsx(as.data.frame(combinedDataPerWell), file = resultsExcelFile, sheetName = "combinedDataPerWell", col.names = TRUE, row.names = FALSE, append = TRUE) #export results to excel sheet
  
  return(combinedDataPerWell)
}



plotMiRDistributionGraphs <- function(dataForPlots, resultsGraphFile) {
  distributionPlots <- list()
  p1 <- dataForPlots %>%  #plot graph of total relative uptake of miRNAs
    filter(Region =="Total") %>%
    ggbarplot(x="Treatment", y = "Normalized.Fluorescence", add = c("mean", "jitter"), size = 1,  ylab = "Relative Fluorescence Intensity", yticks.by = 1)

  p2<- dataForPlots %>% #plot graph of relative distribution of uptaken miRNAs
    filter(Region !="Total") %>%
    ggbarplot(x="Treatment", y = "Normalized.Fluorescence", color = "Region", palette = "npg", position = position_dodge(0.9), add = c("mean", "jitter"), size = 1, point.size = 5, ylab = "Relative Fluorescence Intensity", yticks.by = 1)

  distributionPlots <- append(distributionPlots, list(p1))
  distributionPlots <- append(distributionPlots, list(p2))
  distributionPlots <- ggarrange(plotlist=distributionPlots, nrow = 2)
  
  Graphs <- annotate_figure(distributionPlots, top = text_grob(experimentId, face = "bold", size = 14))
  ggexport(Graphs, filename = resultsGraphFile, width = 1000, height = 1500, res=100)
}








#initialise workspace and allow user to define working directory
workingDirectory <- choose.dir(default = getwd(), caption = "Select Source folder")
setwd(workingDirectory)
experimentId <- str_split(workingDirectory, "\\\\", simplify = TRUE)
experimentId <- experimentId[length(experimentId)]

#Specify input files
imageParametersFile <- paste(experimentId, "_Image_Capture.txt", sep = "")
imageDataFolder <- paste(workingDirectory, "\\", "Results", sep = "")

#Specify output files
resultsExcelFile <- paste(experimentId, "_Distribution.xlsx", sep = "")
resultsGraphFile <- paste(experimentId, "_Distribution.tiff", sep = "")


#define designations for channels
channelMarkers <- tibble(Channel = c("blue", "red", "fred", "composite"), Region = c("DAPI", "Neurofilament", "NeuN","Total"))


#run data analysis
DataToProcess  <- importDistributionData(imageDataFolder)
DataForPlots <- processDistributionData(DataToProcess, channelMarkers, resultsExcelFile)

#Plot graphs
plotMiRDistributionGraphs(DataForPlots, resultsGraphFile)

#update meta results sheet
updateMetaResults(experimentId, ParameterAnalyzed = "MiRDistribution", resultsExcelFile, metaResultsFile = metaResultFilemMORPH)
