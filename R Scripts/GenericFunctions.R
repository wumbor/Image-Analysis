#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)


####################################################################################################
#PARSING TREATMENT NAMES
#function to parse common names of treatments to standard names
parseStandardName <- function(input_string) {
  codeNameVector <-  c("miR#4", "miR#5", "miR#13", "miR#19", "miR#27", "TL8", "miR92a", "miR92","miR124", "Alexa")
  standardNameVector <-  c("Control oligo", "miR-124-5p", "miR-9-5p", "miR-501-3p", "miR-92a-1-5p", "TL8-506", "miR-92a-1-5p", "miR-92a-1-5p", "miR-124-5p", "Alexa488")
  codeDoseVector <-  c("\\(L\\)", "\\(LM\\)", "\\(M\\)", "\\(H\\)",  "\\(XH\\)")
  standardDoseVector <- c("(1)", "(3)", "(5)", "(10)",  "(20)")
  
  output <- input_string
  i = 1
  for(i in 1:length(codeNameVector)){
    output <- str_replace(output, codeNameVector[i], standardNameVector[i])
  }
  
  i = 1
  for(i in 1:length(codeDoseVector)){
    output <- str_replace(output, codeDoseVector[i], standardDoseVector[i])
  }
  return(output)
}

TreatmentLevels <- c("Null", "Control oligo", "Control oligo(5)", "Control oligo(10)","Alexa488", "miR-124-5p", "miR-124-5p(1)", "miR-124-5p(3)", "miR-124-5p(5)", "miR-124-5p(10)", "miR-124-5p(20)", "miR-9-5p", "miR-9-5p(1)", "miR-9-5p(3)", "miR-9-5p(5)", "miR-9-5p(10)", "miR-9-5p(20)", "miR-501-3p", "miR-501-3p(1)","miR-501-3p(3)", "miR-501-3p(5)", "miR-501-3p(10)", "miR-501-3p(20)","miR-92a-1-5p", "miR-92a-1-5p(1)", "miR-92a-1-5p(3)", "miR-92a-1-5p(5)", "miR-92a-1-5p(10)", "miR-92a-1-5p(20)", "let7b", "LOX", "R848", "TL8-506")


#GRAPH FORMATTING SECTION
#General formatting for all graphs
plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 




#META RESULTS SECTION
#This function updates the metaresults file with experiment details
updateMetaResults <- function(experimentId, ParameterAnalyzed, resultsExcelFile, metaResultsFile) {
  metaResult <- data.frame("ExperimentID" = experimentId, "Parameter.Analyzed" = ParameterAnalyzed, "Result.File.Path" = paste(getwd(), resultsExcelFile, sep = "/"), "Analysis.Date" = as.character(Sys.time()), stringsAsFactors = FALSE)
  
  if (file.exists(metaResultsFile)){ #append to an existing file, if any
    write_csv(as.data.frame(metaResult), metaResultsFile, append = TRUE, col_names = FALSE)
  } else {
    write_csv(as.data.frame(metaResult), metaResultsFile, append = TRUE, col_names = TRUE)
  }
}

metaExperimentDetailsFile <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/Experimental_Overview.xlsx"
metaResultFilemMORPH <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/Pooled_Data.csv"
metaResultFilemASSAY <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/Pooled_Data.csv"

pooledResultsFoldermMORPH <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/Pooled Data/"
pooledResultsFoldermASSAY <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/Pooled Data/"

IJMacrosDir <- "C:/Users/Victor Kumbol/Documents/GitHub/New-Image-Analysis/ImageJ Macros/"
RScriptsDir <- "C:/Users/Victor Kumbol/Documents/GitHub/New-Image-Analysis/R Scripts/"


Encoding(metaResultFilemMORPH) <- "UTF-8"
Encoding(metaResultFilemASSAY) <- "UTF-8"
Encoding(metaExperimentDetailsFile) <- "UTF-8"
Encoding(pooledResultsFoldermMORPH) <- "UTF-8"
Encoding(pooledResultsFoldermASSAY) <- "UTF-8"
Encoding(IJMacrosDir) <- "UTF-8"
Encoding(RScriptsDir) <- "UTF-8"








####################################################################################################

#MIRKINETICS SECTION
plotMiRKineticsGraphs <- function(dataForPlots, DurationOfInterest, errorType, treatment, resultsGraphFile) {
  kineticsPlots <- list()
  dataForPlots <- dataForPlots %>%
    filter(AbsoluteTime.h <= DurationOfInterest) #plot only 4h results
  
  caption <- paste(treatment, " n=", length(unique(dataForPlots$ExperimentID)), sep = "") #generate a unique title 

  #Plot graph of Mean Endosomal area
  p1 <- dataForPlots %>%
    filter(Region %in% c("Endosomal Area")) %>%
    filter(ChannelLabel%in% c("miRNA", "DAPI", "phRodored")) %>%
    ggline(x= "AbsoluteTime.h", y = "Mean", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Mean Endosomal Area", add = errorType, title = caption)
  kineticsPlots <- append(kineticsPlots, list(p1))
  
  
  
  #Plot graph of Mean Fluorescence 
  p2 <- dataForPlots %>%
    filter(Region=="Endosomal") %>%
    ggline(x= "AbsoluteTime.h", y = "Mean", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Mean Fluorescence Intensity", add = errorType, title = caption)
  kineticsPlots <- append(kineticsPlots, list(p2))
  
  #Plot graph of Normalised Mean Fluorescence 
  p3 <- dataForPlots %>%
    filter(Region=="Endosomal") %>%
    ggline(x= "AbsoluteTime.h", y = "NormalisedMean", color ="ChannelLabel", palette =  c("#0000FF", "#00FF00", "#FF0000"), numeric.x.axis = TRUE, xticks.by = 1, ylab = "Normalised Fluorescence Intensity", add = errorType, title = caption)
  p3 <- ggpar(p3, ylim = c(1, 2)) #use a common y-scale for all treatments
  kineticsPlots <- append(kineticsPlots, list(p3))
  
  kineticsPlots <- ggarrange(plotlist=kineticsPlots, nrow = 3)
  #Graphs <- annotate_figure(kineticsPlots, top = text_grob(treatment, face = "bold", size = 14))
  ggexport(kineticsPlots, filename = resultsGraphFile, width = 1500, height = 2250, res=100)
}

###################################################################################################
#IJ Macro Section
##  Define key global variables
IJExePath <- shQuote("C:/Users/Victor Kumbol/Fiji/ImageJ-win64.exe")
cmdOptions <- "-batch"

