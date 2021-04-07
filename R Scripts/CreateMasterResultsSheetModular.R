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
model_system <- "NeuronsWT"
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


#Create unique filenames based on the parameters selected
outputWorkbookName = unique(DataToSummarise$Model.System)
outputFileName <- paste(unique(DataToSummarise$Model.System), unique(DataToSummarise$Parameter.Analyzed), sep = "_")
timecourseSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "TimeCourse", sep = ".")
allDaysSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "AllDays", sep = ".")
DRCSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), "DRC", sep = ".")

#specify output files
timecourse_graph_file <- paste(parent_directory, outputFileName, "_TimeCourse", ".tiff", sep = "")
DRC_graph_file <- paste(parent_directory, outputFileName, "_DRC", ".tiff", sep = "")
results_excel_file <- paste(parent_directory, outputWorkbookName, ".xlsx", sep = "")


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







#DOSE RESPONSE CURVE AND ALL DAY ANALYSIS
#This part is processes the DRC data
filtered_results <- filtered_results%>%
  arrange(Treatment)

#filter out the results for each day
duration_list <- unique(filtered_results$Duration)
all_days_summary <- list()
all_days_data <- list()

for (i in 1:length(duration_list)) {

  all_days_temp_data <-filtered_results %>%
    filter(str_detect(Duration, duration_list[i]))
  
  all_days_data <- append(all_days_data, list(all_days_temp_data)) #save raw data for each day
  
  all_days_temp_data <- all_days_temp_data %>%
    select(ExperimentID, Treatment, NormalizedMean) %>%
    pivot_wider(names_from = Treatment, values_from = NormalizedMean)
  
  all_days_temp_data <- add_column(all_days_temp_data, Duration = rep(duration_list[i], length(all_days_temp_data$ExperimentID)), .before = "ExperimentID")
  
  all_days_summary<- append(all_days_summary, list(all_days_temp_data)) #save summary data for each day
  
}

all_days_tibble <- tibble(Duration = duration_list, AllDays.RawData = all_days_data, AllDays.Data = all_days_summary) 
all_days_tibble <- all_days_tibble %>%
  arrange(Duration)


#filter out the data for DRC graph
DRC_tibble <- all_days_tibble %>%
  filter(Duration == day_for_DRC)

all_days_tibble  <- all_days_tibble %>%
  filter(Duration != day_for_DRC)

# DRC_tibble <- DRC_tibble$AllDays.RawData[[1]]







# save the results to an excel spreadsheet
if (file.exists(results_excel_file)){ #if the file already exists, delete any similar named sheet if found
  
  wb <- loadWorkbook(results_excel_file)
  if(any(str_detect(names(getSheets(wb)), allDaysSheetName))){
    removeSheet(wb, sheetName = allDaysSheetName)
    saveWorkbook(wb, results_excel_file)
    wb <- loadWorkbook(results_excel_file)
  }
  
} else {
  wb <- createWorkbook(type = "xlsx")
}

newSheet <- createSheet(wb, sheetName = allDaysSheetName)
newRowindex <- 2
for (i in 1:n_distinct(all_days_tibble)) {
  addDataFrame(all_days_tibble$AllDays.Data[[i]], newSheet, startRow = newRowindex)
  newRowindex <- newRowindex + n_distinct(all_days_tibble$AllDays.Data[[i]]) + 4
}
saveWorkbook(wb, results_excel_file)



# save the results to an excel spreadsheet
if (file.exists(results_excel_file)){ #if the file already exists, delete any similar named sheet if found
  
  wb <- loadWorkbook(results_excel_file)
  if(any(str_detect(names(getSheets(wb)), DRCSheetName))){
    removeSheet(wb, sheetName = DRCSheetName)
    saveWorkbook(wb, results_excel_file)
    wb <- loadWorkbook(results_excel_file)
  }
  
} else {
  wb <- createWorkbook(type = "xlsx")
}
newRowindex <- 2
newSheet <- createSheet(wb, sheetName = DRCSheetName)
addDataFrame(DRC_tibble$AllDays.Data[[1]], newSheet, startRow = newRowindex)
saveWorkbook(wb, results_excel_file)




#plot the pooled result in a bar graph
scaled_width = 100*length(unique(DRC_tibble$AllDays.RawData[[1]]))
tiff(DRC_graph_file, width=scaled_width, height=800, res=100)
plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

ggbarplot(DRC_tibble$AllDays.RawData[[1]], x = "Treatment", y = "NormalizedMean", add = c("mean", "jitter"), size = 1, ylab = "Normalised NeuN+ Cell Count (%)", lab.hjust = 0.5, title = "Dose Response Curves") + plottheme
garbage <- dev.off()











#TIME COURSE ANALYSIS (COMPLETE)
#This part proceses the timecourse data
filtered_results <- filtered_results%>%
  arrange(Duration)

#filter out the results for each miRNA
miR_candidates_tc <- miR_candidates
timecourse_summary <- list()
timecourse_data <- list()

for (i in 1:length(miR_candidates)) {
  miR_candidates_tc[i] <- paste(miR_candidates[i], "\\(", dose_for_timecourse, "\\)", sep = "")
  
  miR_candidates_tc_data <- filtered_results %>%
    filter(str_detect(Treatment, miR_candidates_tc[i])) 
  
  timecourse_data <- append(timecourse_data, list(miR_candidates_tc_data)) #save raw data
  
  miR_candidates_tc_data <- miR_candidates_tc_data %>%
    select(ExperimentID, Duration, NormalizedMean) %>%
    pivot_wider(names_from = Duration, values_from = NormalizedMean) %>%
    add_column(miRNA.Analyzed= rep(miR_candidates[i], length(miR_candidates_tc_data$ExperimentID)), .before = "ExperimentID")
  
  timecourse_summary <- append(timecourse_summary, list(miR_candidates_tc_data)) 
  
}

timecourse_tibble <- tibble(Names = miR_candidates, TimeCourse.RawData = timecourse_data, TimeCourse.Data = timecourse_summary) 


#save the results to an excel spreadsheet
if (file.exists(results_excel_file)){ #if the file already exists, delete any similar named sheet if found
  
  wb <- loadWorkbook(results_excel_file)
  if(any(str_detect(names(getSheets(wb)), timecourseSheetName))){
    removeSheet(wb, sheetName = timecourseSheetName)
    saveWorkbook(wb, results_excel_file)
    wb <- loadWorkbook(results_excel_file)
  }
  
} else {
  wb <- createWorkbook(type = "xlsx")
}
  
newSheet <- createSheet(wb, sheetName = timecourseSheetName)
newRowindex <- 2
for (i in 1:n_distinct(timecourse_tibble)) {
  addDataFrame(timecourse_tibble$TimeCourse.Data[[i]], newSheet, startRow = newRowindex)
  newRowindex <- newRowindex + n_distinct(timecourse_tibble$TimeCourse.Data[[i]]) + 4
}
saveWorkbook(wb, results_excel_file)



#plot the pooled result in a bar graph
scaled_width = 50*length(unique(filtered_results$Treatment))
tiff(timecourse_graph_file, width=scaled_width, height=800, res=100)
plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

timecourse_graphs <- list()

for (i in 1:n_distinct(timecourse_tibble)) {
  
  timecourse_graph_title <- paste(timecourse_tibble$Names[[i]], "Timecourse", sep = " ")
  timecourse_graph <- ggbarplot(timecourse_tibble$TimeCourse.RawData[[i]], x = "Duration", y = "NormalizedMean", add = c("mean", "jitter"), size = 1, ylab = "Normalised NeuN+ Cell Count (%)", xlab = "Duration (days)", lab.hjust = 0.5, title = timecourse_graph_title) + plottheme
  timecourse_graphs <- append(timecourse_graphs, list(timecourse_graph)) 
}
ggarrange(plotlist = timecourse_graphs, ncol = 2, common.legend = TRUE, legend = "bottom")
garbage <- dev.off()



