#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(lubridate)
library(rstatix)

source("C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/GenericFunctions.R", chdir = TRUE)


#Define functions in this script
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


processPooledData <- function(DataToSummarise, resultsSheet, variablesToKeep) {
  #this function imports data filtered from a particular parameter
  
  filteredResults <- list() 
  tableNames <- c()
  for (i in 1:length(DataToSummarise$ExperimentID)) {
    filteredResults <- append(filteredResults, list(read.xlsx(DataToSummarise$Result.File.Path[i], resultsSheet)))
    tableNames <- append(tableNames, DataToSummarise$ExperimentID[i])
  }
  names(filteredResults) <- tableNames #rename filtered results to corresponding ExperimentIDs
  filteredResults <- filteredResults %>% #merge all the tables by rows and id by ExperimentID
    bind_rows(.id = "ExperimentID")
  
  DurationInfo <- DataToSummarise %>% #Tag each entry based on it's duration of stimulation
    select(ExperimentID, Duration.of.stimulation) %>%
    separate(Duration.of.stimulation, sep = " ", into = c("Duration", "Unit")) %>%
    select(ExperimentID, Duration) %>%
    mutate(Duration = as.numeric(Duration))
  filteredResults <- inner_join(filteredResults, DurationInfo, by = "ExperimentID" )
  
  #add information on treatments
  if("Treatment" %in% names(filteredResults)){
    filteredResults <- filteredResults %>% #Convert treatments to factors to capture dose levels
      mutate(Treatment=parseStandardName(Treatment))
    filteredResults$Treatment <- factor(filteredResults$Treatment, levels = TreatmentLevels)
  } else {
    TreatmentInfo <- DataToSummarise %>% #Tag each entry based on it's treatment
      select(ExperimentID, miRNAs.tested) %>%
      mutate(miRNAs.tested = trimws(miRNAs.tested)) %>%
      rename(Treatment = miRNAs.tested)
    filteredResults <- inner_join(filteredResults, TreatmentInfo, by = "ExperimentID")
  }
  
  filteredResults <- filteredResults[, variablesToKeep] #retain only relevant columns
  return(filteredResults)
}
# #---------------------------------------------------------------------------------------------------
# 
# 
analyseTUNLNucleiCount <- function(pooledData) {
  
  pooledData$Treatment <- factor(pooledData$Treatment, levels = TreatmentLevels)
  
  pooledData %>%
    #kruskal_test(NormalizedMean ~ Treatment) %>%
    dunn_test(NormalizedRatio ~ Treatment, p.adjust.method = "bonferroni")

  resultsGraphFile <- paste(pooledResultsFolder, modelSystem,"-", parameterToAnalyze, ".tiff", sep = "")
  
   #plot graphs
   scaledWidth = 100*length(unique(pooledData$Treatment)) #scale width to accomodate treatments
   Graph <- pooledData %>%
     ggbarplot(x="Treatment", y="NormalizedRatio", add=c("mean_se", "jitter"))  + plottheme
   ggexport(Graph, filename = resultsGraphFile, width = scaledWidth, height = 750, res=100)
   
   pooledDataExport <- pooledData %>%
     pivot_wider(names_from = Treatment, values_from = c(NormalizedRatio, NormalizedMean))
   
   write_csv(pooledDataExport, resultsExcelFile)
   
}




analyseNeuNNucleiCount <- function(pooledData) {
  
  pooledData$Treatment <- factor(pooledData$Treatment, levels = TreatmentLevels)
  # 
  # pooledData %>%
  #   #kruskal_test(NormalizedMean ~ Treatment) %>%
  #   dunn_test(NormalizedRatio ~ Treatment, p.adjust.method = "bonferroni")

  #plot graphs
  scaledWidth = 100*length(unique(pooledData$Treatment)) #scale width to accomodate treatments
  Graph <- pooledData %>%
    ggbarplot(x="Treatment", y="NormalizedMean", add=c("mean_se", "jitter"))  + plottheme
  ggexport(Graph, filename = resultsGraphFile, width = scaledWidth, height = 750, res=100)
  
  pooledDataExport <- pooledData %>%
    pivot_wider(names_from = Treatment, values_from = NormalizedMean)

  write_csv(pooledDataExport, resultsExcelFile)

}



# #----------------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------------


#PART TO ADAPT FOR ANALYSIS
#specify project and parameter to analyse
projectTitle <- "mASSAY" 
parameterToAnalyze <- "NeuNNucleiCount"
modelSystem = "NeuronsT7KO"



#define output folder and results file based on project under analysis
if(projectTitle == "mMORPH"){
  pooledResultsFile <- metaResultFilemMORPH
  pooledResultsFolder <- pooledResultsFoldermMORPH
} else if (projectTitle == "mASSAY") {
  pooledResultsFile <- metaResultFilemASSAY
  pooledResultsFolder <- pooledResultsFoldermASSAY
  resultsExcelFile <- paste(pooledResultsFolder, modelSystem,"-", parameterToAnalyze, ".csv", sep = "")
  resultsGraphFile <- paste(pooledResultsFolder, modelSystem,"-", parameterToAnalyze, ".tiff", sep = "")
  
}






#read in data for experiment details and parse columns appropriately
metaExperimentDetails <-read.xlsx(metaExperimentDetailsFile, 4, encoding = "UTF-8", colClasses =  c(rep("character", 8), "Date", "Date")) %>%
  mutate(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>%
  distinct()
#read in data for pooled results, parse columns and remove duplicates
pooledExperimentResults <- read.csv(file = pooledResultsFile, header = TRUE, sep = ",", fileEncoding = "UTF-8", colClasses = c(rep("character", 4))) %>%
  mutate(Analysis.Date = dmy_hm(Analysis.Date)) %>%
  distinct(ExperimentID, Parameter.Analyzed, Result.File.Path, .keep_all = TRUE)
#join the two tables to create master data sheet
masterDataSheet <- inner_join(pooledExperimentResults, metaExperimentDetails, by = c("ExperimentID", "Parameter.Analyzed"))


#run analysis file based on set parameter to analyse 
if(parameterToAnalyze =="TUNLNucleiCount"){
  resultsSheetIndex <- 2
  variablesToKeep <- c("ExperimentID", "Treatment","NormalizedMean", "NormalizedRatio")
  DataToSummarise <- masterDataSheet %>%
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze) %>%
    filter(Duration.of.stimulation=="5 days")
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyseTUNLNucleiCount(pooledData)
}


if(parameterToAnalyze =="NeuNNucleiCount"){
  resultsSheetIndex <- 2
  variablesToKeep <- c("ExperimentID", "Treatment","NormalizedMean")
  DataToSummarise <- masterDataSheet %>%
    filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze) %>%
    filter(Duration.of.stimulation=="5 days")
  pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)
  analyseNeuNNucleiCount(pooledData)
}




#Prepare Data for Statistical Analysis

#Get pooledData per miRNA

miR124Data <- pooledData %>%
  filter(Treatment %in% c("Null", "Control oligo", "LOX", "miR-124-5p(1)", "miR-124-5p(5)", "miR-124-5p(10)"))

miR92Data <- pooledData %>%
  filter(Treatment %in% c("Null", "Control oligo", "LOX",  "miR-92a-1-5p(1)", "miR-92a-1-5p(5)", "miR-92a-1-5p(10)"))


miR124Table <- miR124Data %>%
  pivot_wider(names_from = Treatment, values_from = NormalizedMean)


miR124Data %>%
  kruskal_test(NormalizedMean ~ Treatment) %>%
  #dunn_test(NormalizedMean ~ Treatment, p.adjust.method = "bonferroni")

miR124Data %>%
  dunn_test(NormalizedMean ~ Treatment, p.adjust.method = "holm")















#filter all relevant data sets for chosen model system and analysis parameter
DataToSummarise <- masterDataSheet %>%
  filter(Model.System == modelSystem, Parameter.Analyzed == parameterToAnalyze)
pooledData <- processPooledData(DataToSummarise, resultsSheetIndex, variablesToKeep)


#Create unique output filenames based on the parameters selected
outputWorkbookName = unique(DataToSummarise$Model.System)
outputFileName <- paste(unique(DataToSummarise$Model.System), unique(DataToSummarise$Parameter.Analyzed), sep = "_")
#timecourseSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "TimeCourse", sep = ".")
# DRCSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "DRC", sep = ".")

#specify output files
#timecourseGraphFile <- paste(pooledResultsFolder, outputFileName, "_TimeCourse", ".tiff", sep = "")
# DRC_graph_file <- paste(pooledResultsFolder , outputFileName, "_DRC", ".tiff", sep = "")
#resultsExcelFile <- paste(pooledResultsFolder, outputWorkbookName, ".xlsx", sep = "")




# doseForTimecourse <- "10"
# if (modelSystem == "NeuronsWT"){
#   miR_candidates <- c("miR-124-5p", "miR-92a-1-5p") 
#   day_for_DRC <- "7"
# } else if (modelSystem == "SY5YWT"){
#   miR_candidates <- c("miR-9-5p", "miR-501-3p") 
#   day_for_DRC <- "4"
# }
# 
# 





# 
# Graphs <- plotMiRKineticsGraphs(pooledDataPerTreatment[[1]], DurationOfInterest = 4, errorType = "mean_se")
# resultsGraphFile <- paste(pooledResultsFolder, uniqueTreatments[1], ".tif", sep = "")
# tiff(resultsGraphFile, width = 1500, height = 2250, res=100)
# annotate_figure(Graphs, top = text_grob(uniqueTreatments[1], face = "bold", size = 14))
# garbage <- dev.off()
# 
# 
# Graphs <- plotMiRKineticsGraphs(pooledDataPerTreatment[[1]], DurationOfInterest = 4, errorType = "mean_se")
# Graphs <- annotate_figure(Graphs, top = text_grob(uniqueTreatments[1], face = "bold", size = 14))
# 
# resultsGraphFile <- paste(pooledResultsFolder, uniqueTreatments[1], ".tiff", sep = "")
# ggexport(Graphs, filename = resultsGraphFile, width = 1500, height = 2250, res=100)
# 
# 
# 


# #DOSE RESPONSE CURVE AND ALL DAY ANALYSIS
# #This part is processes the DRC data
# filteredResults <- filteredResults%>%
#   arrange(Treatment)
# 
# #filter out the results for each day
# duration_list <- unique(filteredResults$Duration)
# all_days_summary <- list()
# all_days_data <- list()
# 
# for (i in 1:length(duration_list)) {
#   
#   all_days_temp_data <-filteredResults %>%
#     filter(str_detect(Duration, duration_list[i]))
#   
#   all_days_data <- append(all_days_data, list(all_days_temp_data)) #save raw data for each day
#   
#   all_days_temp_data <- all_days_temp_data %>%
#     select(ExperimentID, Treatment, NormalizedMean) %>%
#     pivot_wider(names_from = Treatment, values_from = NormalizedMean)
#   
#   all_days_temp_data <- add_column(all_days_temp_data, Duration = rep(duration_list[i], length(all_days_temp_data$ExperimentID)), .before = "ExperimentID")
#   
#   all_days_summary<- append(all_days_summary, list(all_days_temp_data)) #save summary data for each day
#   
# }
# 
# all_days_tibble <- tibble(Duration = duration_list, AllDays.RawData = all_days_data, AllDays.Data = all_days_summary) 
# all_days_tibble <- all_days_tibble %>%
#   arrange(Duration)
# 
# 
# #filter out the data for DRC graph
# DRC_tibble <- all_days_tibble %>%
#   filter(Duration == day_for_DRC)
# 
# all_days_tibble  <- all_days_tibble %>%
#   filter(Duration != day_for_DRC)
# 
# # DRC_tibble <- DRC_tibble$AllDays.RawData[[1]]
# 
# 
# 
# 
# 
# 
# 
# # save the results to an excel spreadsheet
# if (file.exists(resultsExcelFile)){ #if the file already exists, delete any similar named sheet if found
#   
#   wb <- loadWorkbook(resultsExcelFile)
#   if(any(str_detect(names(getSheets(wb)), allDaysSheetName))){
#     removeSheet(wb, sheetName = allDaysSheetName)
#     saveWorkbook(wb, resultsExcelFile)
#     wb <- loadWorkbook(resultsExcelFile)
#   }
#   
# } else {
#   wb <- createWorkbook(type = "xlsx")
# }
# 
# newSheet <- createSheet(wb, sheetName = allDaysSheetName)
# newRowindex <- 2
# for (i in 1:n_distinct(all_days_tibble)) {
#   addDataFrame(all_days_tibble$AllDays.Data[[i]], newSheet, startRow = newRowindex)
#   newRowindex <- newRowindex + n_distinct(all_days_tibble$AllDays.Data[[i]]) + 4
# }
# saveWorkbook(wb, resultsExcelFile)
# 
# 
# 
# # save the results to an excel spreadsheet
# if (file.exists(resultsExcelFile)){ #if the file already exists, delete any similar named sheet if found
#   
#   wb <- loadWorkbook(resultsExcelFile)
#   if(any(str_detect(names(getSheets(wb)), DRCSheetName))){
#     removeSheet(wb, sheetName = DRCSheetName)
#     saveWorkbook(wb, resultsExcelFile)
#     wb <- loadWorkbook(resultsExcelFile)
#   }
#   
# } else {
#   wb <- createWorkbook(type = "xlsx")
# }
# newRowindex <- 2
# newSheet <- createSheet(wb, sheetName = DRCSheetName)
# addDataFrame(DRC_tibble$AllDays.Data[[1]], newSheet, startRow = newRowindex)
# saveWorkbook(wb, resultsExcelFile)
# 
# 
# 
# 
# #plot the pooled result in a bar graph
# scaled_width = 100*length(unique(DRC_tibble$AllDays.RawData[[1]]))
# tiff(DRC_graph_file, width=scaled_width, height=800, res=100)
# plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggbarplot(DRC_tibble$AllDays.RawData[[1]], x = "Treatment", y = "NormalizedMean", add = c("mean", "jitter"), size = 1, ylab = "Normalised NeuN+ Cell Count (%)", lab.hjust = 0.5, title = "Dose Response Curves") + plottheme
# garbage <- dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #TIME COURSE ANALYSIS (COMPLETE)
# #This part proceses the timecourse data
# filteredResults <- filteredResults%>%
#   arrange(Duration)
# 
# #filter out the results for each miRNA
# miR_candidates_tc <- miR_candidates
# timecourse_summary <- list()
# timecourse_data <- list()
# 
# for (i in 1:length(miR_candidates)) {
#   miR_candidates_tc[i] <- paste(miR_candidates[i], "\\(", doseForTimecourse, "\\)", sep = "")
#   
#   miR_candidates_tc_data <- filteredResults %>%
#     filter(str_detect(Treatment, miR_candidates_tc[i])) 
#   
#   timecourse_data <- append(timecourse_data, list(miR_candidates_tc_data)) #save raw data
#   
#   miR_candidates_tc_data <- miR_candidates_tc_data %>%
#     select(ExperimentID, Duration, NormalizedMean) %>%
#     pivot_wider(names_from = Duration, values_from = NormalizedMean) %>%
#     add_column(miRNA.Analyzed= rep(miR_candidates[i], length(miR_candidates_tc_data$ExperimentID)), .before = "ExperimentID")
#   
#   timecourse_summary <- append(timecourse_summary, list(miR_candidates_tc_data)) 
#   
# }
# 
# timecourse_tibble <- tibble(Names = miR_candidates, TimeCourse.RawData = timecourse_data, TimeCourse.Data = timecourse_summary) 
# 
# 
# #save the results to an excel spreadsheet
# if (file.exists(resultsExcelFile)){ #if the file already exists, delete any similar named sheet if found
#   
#   wb <- loadWorkbook(resultsExcelFile)
#   if(any(str_detect(names(getSheets(wb)), timecourseSheetName))){
#     removeSheet(wb, sheetName = timecourseSheetName)
#     saveWorkbook(wb, resultsExcelFile)
#     wb <- loadWorkbook(resultsExcelFile)
#   }
#   
# } else {
#   wb <- createWorkbook(type = "xlsx")
# }
# 
# newSheet <- createSheet(wb, sheetName = timecourseSheetName)
# newRowindex <- 2
# for (i in 1:n_distinct(timecourse_tibble)) {
#   addDataFrame(timecourse_tibble$TimeCourse.Data[[i]], newSheet, startRow = newRowindex)
#   newRowindex <- newRowindex + n_distinct(timecourse_tibble$TimeCourse.Data[[i]]) + 4
# }
# saveWorkbook(wb, resultsExcelFile)
# 
# 
# 
# #plot the pooled result in a bar graph
# scaled_width = 50*length(unique(filteredResults$Treatment))
# tiff(timecourseGraphFile, width=scaled_width, height=800, res=100)
# plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
# 
# timecourse_graphs <- list()
# 
# for (i in 1:n_distinct(timecourse_tibble)) {
#   
#   timecourse_graph_title <- paste(timecourse_tibble$Names[[i]], "Timecourse", sep = " ")
#   timecourse_graph <- ggbarplot(timecourse_tibble$TimeCourse.RawData[[i]], x = "Duration", y = "NormalizedMean", add = c("mean", "jitter"), size = 1, ylab = "Normalised NeuN+ Cell Count (%)", xlab = "Duration (days)", lab.hjust = 0.5, title = timecourse_graph_title) + plottheme
#   timecourse_graphs <- append(timecourse_graphs, list(timecourse_graph)) 
# }
# ggarrange(plotlist = timecourse_graphs, ncol = 2, common.legend = TRUE, legend = "bottom")
# garbage <- dev.off()
# 
# 

