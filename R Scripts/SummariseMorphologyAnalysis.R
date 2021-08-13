#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)

source("C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/GenericFunctions.R")


#DEFINE FUNCTIONS

processMorphologyData <- function(DataToProcess, microscopySequenceFile, analysisLogFile) { #This function summarises the morphology results into one tibble

  imageData <- read.csv(DataToProcess)   #Read in data from input files
  imageSequence <- read.csv(file = microscopySequenceFile, colClasses = c(rep("numeric", 2), rep("character", 2)), header = TRUE, sep = ",", skip = imgSequenceHeaderLine)
  # analysisParameters <- read.csv(file = analysisLogFile, sep = ",", header = TRUE)

  imageSequence <- imageSequence %>%   #process image sequence data
    rename(Original_Filename = Filename) %>%
    arrange(Original_Filename)
  
  imageData <- imageData %>%   #process image raw data
    rename(Processed_Filename = Filename) %>%
    arrange(Processed_Filename)
  
  ProcessedData <- bind_cols(imageSequence, imageData) %>% #merge image sequence and image raw data tables and rearrange data to preferred presentation order
    separate(Condition, sep = "_", into = c("Treatment", "Field")) %>%
    mutate(Field = as.numeric(Field)) %>%
    mutate(Treatment = parseStandardName(trimws(Treatment)))
  ProcessedData$Treatment <- factor(ProcessedData$Treatment, levels = TreatmentLevels)
  
  ProcessedData <- ProcessedData %>%   #Calculate key parameters from raw data
    select(-TotalSomaArea, -AverageSomaArea) %>%
    mutate(TotalNeuriteArea=(TotalNeuriteArea*(scaleFactor^2)), TotalNeuriteLength = (TotalNeuriteLength*scaleFactor)) %>% #convert values from pixels to microns
    mutate(NeuriteLengthPerNeuron = (TotalNeuriteLength/SomaCount), NeuriteAreaPerNeuron = (TotalNeuriteArea/SomaCount)) #Calculate derived parameters
  
  return(ProcessedData)
}




exportResults <- function(combinedData, resultsExcelFile, metaResultsFile) { #This function exports data tables and graphs

  if (file.exists(resultsExcelFile)){   #overwrite any existing results file
    file.remove(resultsExcelFile)
  } 
  
  write.xlsx(as.data.frame(combinedData), file = resultsExcelFile, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = TRUE)   #Export the raw data 
  
  combinedDataPerWell <- combinedData %>%   #summarise the mean of key parameters for every coverslip
    filter(SomaCount!=1) %>% #filter erroneous data i.e. where only one soma was detected
    group_by(Treatment, Coverslip) %>%
    summarise(MeanSomaCount = mean(SomaCount), TotalSomaCount = sum(SomaCount), MeanNeuriteLengthPerNeuron = mean(NeuriteLengthPerNeuron), MeanNeuriteCount = mean(NeuriteCount), MeanNeuriteAreaPerNeuron = mean(NeuriteAreaPerNeuron), MeanAttachmentPoints = mean(AttachmentPointsCount), MeanEndPoints = mean(EndPointsCount))
  
  summaryReport <- combinedDataPerWell %>%    #Export the summary report
    group_by(Treatment) %>%
    summarise(MeanSomaCount = mean(MeanSomaCount), TotalSomaCount = sum(TotalSomaCount), NeuriteLengthPerNeuron = mean(MeanNeuriteLengthPerNeuron), NeuriteCount = mean(MeanNeuriteCount), NeuriteAreaPerNeuron = mean(MeanNeuriteAreaPerNeuron), AttachmentPoints = mean(MeanAttachmentPoints), EndPoints = mean(MeanEndPoints))
  write.xlsx(as.data.frame(summaryReport), file = resultsExcelFile, sheetName = "Morphology Analysis Summary", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  suspectedOutliers <- combinedData %>% #Detect and report suspected error files in a separate sheet
    filter(SomaCount==1) 
  if(count(suspectedOutliers)<1){
    suspectedOutliers <- data.frame("Report" = "NO FILES EXCLUDED", stringsAsFactors = FALSE)
  } 
  write.xlsx(suspectedOutliers, file = resultsExcelFile, sheetName = "Excluded Files", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  # write.xlsx(analysisParameters, file = resultsExcelFile, sheetName = "Analysis Parameters", col.names = FALSE, row.names = FALSE, append = TRUE) #Export analysis parameters
  
  meta_result <- data.frame("ExperimentID" = experimentId, "Parameter.Analyzed" = parameterAnalyzed, "Result.File.Path" = paste (getwd(), resultsExcelFile, sep = "/"), "Analysis Date" = as.character(Sys.time()), stringsAsFactors = FALSE) #Export key experiment details to pooled data sheet
    write_csv(meta_result, metaResultsFile, na = "NA", append = TRUE, col_names = TRUE) #export the meta results
  
    parametersList  <- combinedDataPerWell %>% #Iterate over the list of analysed parameters and plot graphs
      ungroup()%>%
      select(MeanSomaCount:MeanEndPoints, -TotalSomaCount)
    parametersList <- names(parametersList)
    morphologyPlots <- list()
    for (i in 1:length(parametersList)) {
      p<- combinedDataPerWell %>%
        ggbarplot(x="Treatment", y = parametersList[i], add = c("mean", "jitter"), size = 1) + plottheme
      morphologyPlots <- append(morphologyPlots, list(p))
    }
    morphologyPlots <- ggarrange(plotlist=morphologyPlots)
    return(morphologyPlots)
}





#PREPARE WORKSPACE


# myarg <- commandArgs() #set working dir based on command line arguments
# workingDirectory <- as.character(myarg[6])


workingDirectory <- choose.dir(default = getwd(), caption = "Select Source folder") #request working directory from user and convert to universal format
workingDirectory <- str_replace_all(workingDirectory, "\\\\", "/")

setwd(workingDirectory)


#define key global variables
experimentId <- str_split(workingDirectory, "/", simplify = TRUE)
experimentId <- experimentId[length(experimentId)]
scaleFactor <- 0.161 #factor for conversion of pixel to microns @ 40X Magnification at Olympus Microscope
parameterAnalyzed <- "MorphologyAnalysis"


#Specify input files
microscopySequenceFile <- paste(experimentId, "_Image_Capture.txt", sep = "")
imgSequenceHeaderLine <- (grep("Condition", read_lines(microscopySequenceFile, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1 #Specify header line in microscopySequenceFile
analysisLogFile <- paste(experimentId, "_AnalysisLog.txt", sep = "")

#Specify output files
resultsExcelFile <- paste(experimentId, "_Morphology_Analysis.xlsx", sep = "")
resultsGraphFile <- paste(experimentId, "_Morphology_Analysis.tiff", sep = "")
metaResultsFile <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/Pooled_Data.csv"









DataToExport  <- processMorphologyData("MorphologyAnalysis.csv", microscopySequenceFile, analysisLogFile)
Graphs <- exportResults(DataToExport, resultsExcelFile, metaResultsFile)


tiff(resultsGraphFile, width=1400, height=900, res=100)
annotate_figure(Graphs, top = text_grob(experimentId, face = "bold", size = 14))
garbage <- dev.off()

