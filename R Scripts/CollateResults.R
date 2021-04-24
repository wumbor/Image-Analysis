#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(lubridate)


#prepare workspace
parent_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/Pooled Data/"

#specify input files
meta_experiment_details_file <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/Experimental_Overview.xlsx"
pooled_experiment_results_file <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/Pooled_Data.csv"


TreatmentLevels <- c("Null", "Control oligo", "miR-124-5p", "miR-124-5p(1)", "miR-124-5p(3)", "miR-124-5p(5)", "miR-124-5p(10)", "miR-124-5p(20)", "miR-9-5p", "miR-9-5p(1)", "miR-9-5p(3)", "miR-9-5p(5)", "miR-9-5p(10)", "miR-9-5p(20)", "miR-501-3p", "miR-501-3p(1)","miR-501-3p(3)", "miR-501-3p(5)", "miR-501-3p(10)", "miR-501-3p(20)","miR-92a-1-5p", "miR-92a-1-5p(1)", "miR-92a-1-5p(3)", "miR-92a-1-5p(5)", "miR-92a-1-5p(10)", "miR-92a-1-5p(20)", "let7b", "LOX", "R848", "TL8-506")


ControlTreatments <- c("Null", "Control oligo", "LOX", "R848", "TL8-506")


#READ IN DATA FROM INPUT FILES TO CREATE A MASTER DATA SHEET
#read in data for experiment details and parse columns appropriately
meta_experiment_details <-read.xlsx(meta_experiment_details_file, 4, encoding = "UTF-8", colClasses =  c(rep("character", 8), "Date", "Date")) %>%
  mutate(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>%
  distinct()

#read in data for pooled results, parse columns and remove duplicates
pooled_experiment_results <- read.csv(file = pooled_experiment_results_file, header = TRUE, sep = ",", fileEncoding = "UTF-8", colClasses = c(rep("character", 4))) %>%
  mutate(Analysis.Date = parse_date_time(Analysis.Date, "%Y-%m-%d %H:%M:%S", truncated = 3)) %>%
  distinct(ExperimentID, Parameter.Analyzed, Result.File.Path, .keep_all = TRUE)

#join the two tables to create master data sheet
master_data_sheet <- inner_join(pooled_experiment_results, meta_experiment_details, by = c("ExperimentID", "Parameter.Analyzed"))



#Use conditionals to select filter parameters i.e. Model system and Parameter analyzed
model_system <- "SY5YWT"
parameter_to_analyze <- "NeuNNucleiCount"
dose_for_timecourse <- "5"



if (model_system == "NeuronsWT"){
  miR_candidates <- c("miR-124-5p", "miR-92a-1-5p") 
  day_for_DRC <- "7"
} else if (model_system == "SY5YWT"){
  miR_candidates <- c("miR-9-5p", "miR-501-3p") 
  day_for_DRC <- "4"
}




#filter all relevant data sets for chosen model system and analysis parameter
DataToSummarise <- master_data_sheet %>%
  filter(Model.System == model_system, Parameter.Analyzed == parameter_to_analyze)


# #Create unique filenames based on the parameters selected
# outputWorkbookName = unique(DataToSummarise$Model.System)
# outputFileName <- paste(unique(DataToSummarise$Model.System), unique(DataToSummarise$Parameter.Analyzed), sep = "_")
# timecourseSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "TimeCourse", sep = ".")
# allDaysSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "AllDays", sep = ".")
# DRCSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "DRC", sep = ".")

#specify output files
# timecourse_graph_file <- paste(parent_directory, outputFileName, "_TimeCourse", ".tiff", sep = "")
# DRC_graph_file <- paste(parent_directory, outputFileName, "_DRC", ".tiff", sep = "")
# results_excel_file <- paste(parent_directory, outputWorkbookName, ".xlsx", sep = "")



#create a list containing all the  results from the filtered experiments
filtered_results <- list()
table_names <- c()
for (i in 1:length(DataToSummarise$ExperimentID)) {
  filtered_results <- append(filtered_results, list(read.xlsx(DataToSummarise$Result.File.Path[i], 2)))
  table_names <- append(table_names, DataToSummarise$ExperimentID[i])
}

#rename elements of the filtered results list to corresponding ExperimentID
names(filtered_results) <- table_names

#THIS PART NEEDS TO BE ADAPTED FOR THE IN VIVO DATA
for (i in 1:length(filtered_results)) {
  filtered_results[[i]] <- select(filtered_results[[i]], Treatment, NormalizedMean, NormalizedMedian)  #retain only relevant columns
}

#merge all the tables by rows and id by ExperimentID
filtered_results <- filtered_results %>%
  bind_rows(.id = "ExperimentID")

#Tag each entry based on it's duration of stimulation
DurationInfo <- DataToSummarise %>%
  select(ExperimentID, Duration.of.stimulation) %>%
  separate(Duration.of.stimulation, sep = " ", into = c("Duration", "Unit")) %>%
  select(ExperimentID, Duration)

filtered_results <- inner_join(filtered_results, DurationInfo, by = "ExperimentID" )

#Convert treatments to factors to capture dose levels
filtered_results$Treatment <- factor(filtered_results$Treatment, levels = TreatmentLevels)







ControlTreatmentsData <- filtered_results %>%
  filter(str_detect(Duration, day_for_DRC)) %>% #filter out data for the relevant day used for DRCs
  filter(Treatment %in% ControlTreatments) #filter out data for control treatments


#COLLATE DRC DATA
collated_DRC_data <- list()

for (i in 1:length(miR_candidates)) {
  
  SelectmiRNAData <- filtered_results %>%
    filter(str_detect(Duration, day_for_DRC)) %>% 
    filter(str_detect(Treatment, miR_candidates[i])) #filter out data for the relevant miRNA
  
  #merge the two data sets 
  PlotData <- bind_rows(ControlTreatmentsData, SelectmiRNAData)
  
  PlotData <- PlotData %>%
    arrange(Treatment) %>%
    select(ExperimentID, Treatment, NormalizedMean) %>%
    pivot_wider(names_from = Treatment, values_from = NormalizedMean)
  
  
  #save raw data
  collated_DRC_data <- append(collated_DRC_data, list(PlotData)) 
}




#COLLATE TIMECOURSE DATA
filtered_results <- filtered_results%>%
  arrange(Duration)


collated_timecourse_data <- list()
miR_candidates_tc <- miR_candidates


for (i in 1:length(miR_candidates)) {
  miR_candidates_tc[i] <- paste(miR_candidates[i], "\\(", dose_for_timecourse, "\\)", sep = "")
  
  miR_candidates_tc_data <- filtered_results %>%
    filter(str_detect(Treatment, miR_candidates_tc[i])) 
  
  PlotData <- bind_rows(ControlTreatmentsData, miR_candidates_tc_data)
  
  PlotData <- PlotData %>%
    arrange(Duration, Treatment) %>%
    select(ExperimentID, Treatment, Duration, NormalizedMean) %>%
    pivot_wider(names_from = Duration, values_from = NormalizedMean)
  
  collated_timecourse_data<- append(collated_timecourse_data, list(PlotData)) 
  
}


collated_tibble <- tibble(Names = miR_candidates, DRC.data = collated_DRC_data, TimeCourse.Data = collated_timecourse_data) 







#save the results to an excel spreadsheet

for (i in 1:length(collated_tibble$Names)) {
  if (file.exists(results_excel_file)){ #if the file already exists, delete any similar named sheet if found
    
    wb <- loadWorkbook(results_excel_file)
    if(any(str_detect(names(getSheets(wb)), collated_tibble$Names[i]))){
      removeSheet(wb, sheetName = collated_tibble$Names[i])
      saveWorkbook(wb, results_excel_file)
      wb <- loadWorkbook(results_excel_file)
    }
    
  } else {
    wb <- createWorkbook(type = "xlsx")
  }
  
  newSheet <- createSheet(wb, sheetName = collated_tibble$Names[i])
  newRowindex <- 2
  
  addDataFrame(collated_tibble$DRC.data[[i]], newSheet, startRow = newRowindex)
  newRowindex <- newRowindex + n_distinct(collated_tibble$DRC.data[[i]]) + 5
  addDataFrame(collated_tibble$TimeCourse.Data[[i]], newSheet, startRow = newRowindex)
  saveWorkbook(wb, results_excel_file)

}















#plot the pooled result in a bar graph

#specify multiple comparisons to be performed
# multiple_comparisons <- list(c("Null", "Conrol oligo"), c("Null", "miR-9-5p(10)"))
# 
# 
# scaled_width = 100*n_distinct(PlotData$Treatment)
# 
# tiff(DRC_graph_file, width=scaled_width, height=800, res=100)
# plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
# ggbarplot(PlotData, x = "Treatment", y = "NormalizedMean", add = c("mean", "jitter"), size = 1, ylab = "Percentage Relative Neuronl Viability(%)", lab.hjust = 0.5) + plottheme +  stat_compare_means(method = "anova") + stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Null", p.adjust.method = "bonferroni", hide.ns = TRUE)  
# 
# 
# garbage <- dev.off()
# 
# 
# 
# 
# 
# #Multicomp Package gives similar results as GraphPad for Dunnett
# res.aov <- aov(NormalizedMean ~ Treatment, data = PlotData)
# summary(glht(res.aov, linfct = mcp(Treatment = "Dunnett")))

