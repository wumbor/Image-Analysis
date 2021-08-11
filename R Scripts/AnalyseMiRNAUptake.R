# This script analyses data from the miRDistribution macro to plot graphs of relative distribution of uptaken miRNAs
#Written by Victor Kumbol, August 2021



#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(bayestestR)


#define designations for channels
channelMarkers <- tibble(Channel = c("blue", "red", "composite"), Region = c("Nuclear", "Neurites", "Total"))
results_excel_file <- "Results.xlsx"

#retrieve image sequence information from file
imageSequence <- read.csv("Image_Capture.txt")
imageSequence <- imageSequence %>%
  select(Filename, Treatment, Well)



#create a function to summarise the results from all files into on tibble
processUptakeData <- function(working_directory) {
  setwd(working_directory)
  
  #retrieve list of results files
  FilesToProcess <- list.files(working_directory)
  ProcessedData <- list()
  
  #iterate over the files in the folder and process each
  for (i in 1:length(FilesToProcess)) {
    raw_data <- read.csv(FilesToProcess[i])
    
    raw_data <- raw_data %>%
      select(Channel, Area, Mean, RawIntDen) %>%
      separate(Channel, into = c("Channel", "Slice"), sep = "-") %>%
      mutate(Channel = trimws(Channel))
    
    summary_data <- raw_data %>%
      group_by(Channel) %>%
      summarise(TotalArea = mean(Area), MeanFluorescence = mean(Mean))
    
    #append results to the ProcessedData list
    ProcessedData <- append(ProcessedData, list(summary_data))
  }
  names(ProcessedData) <- FilesToProcess
  
  
  #merge results into one
  ProcessedData <- bind_rows(ProcessedData, .id="Filename")
  return(ProcessedData)
}
  
  




#request user to select a results folder 
working_directory <- choose.dir(default = getwd(), caption = "Select Results folder")
combinedData  <- processUptakeData(working_directory)

#Add information on filenames and channels
combinedData <- inner_join(combinedData, imageSequence, by = "Filename")
combinedData <- inner_join(combinedData, channelMarkers, by = "Channel")

#summarise data per well i.e. unique n's
combinedDataPerWell <- combinedData %>%
  select(-TotalArea) %>%
  group_by(Well, Treatment, Region) %>%
  summarise(MeanFluorescence = mean(MeanFluorescence))

#normalise fluorescence to untreated wells
combinedDataPerWell <- combinedDataPerWell %>%
  group_by(Region) %>%
  mutate(Normalized.Fluorescence = MeanFluorescence/MeanFluorescence[(str_which(Treatment, "Null"))]) %>%
  ungroup()



#plot graph of total relative uptake of miRNAs
combinedDataPerWell %>%
  filter(Region =="Total") %>%
  ggbarplot(x="Treatment", y = "Normalized.Fluorescence", add = c("mean", "jitter"), size = 1,  ylab = "Relative Fluorescence Intensity", yticks.by = 1)

#plot graph of relative distribution of uptaken miRNAs
combinedDataPerWell %>%
  filter(Region !="Total") %>%
  ggbarplot(x="Treatment", y = "Normalized.Fluorescence", color = "Region", palette =  c("#0000FF", "#FF0000"), position = position_dodge(0.9), add = c("mean", "jitter"), size = 1, ylab = "Relative Fluorescence Intensity", yticks.by = 1)

#save the results to an excel file
write.xlsx(combinedDataPerWell, file = results_excel_file)

