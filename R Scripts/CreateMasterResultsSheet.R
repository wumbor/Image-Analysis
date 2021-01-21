#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(lubridate)

#prepare workspace
parent_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/40X Magnification/Pooled Data/"

#specify input files
meta_experiment_details_file <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/Experimental_Overview.xlsx"
pooled_experiment_results_file <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/40X Magnification/Pooled_Data.csv"


#READ IN DATA FROM INPUT FILES TO CREATE A MASTER DATA SHEET
#read in data for experiment details and parse columns appropriately
meta_experiment_details <-read.xlsx(meta_experiment_details_file, 4, encoding = "UTF-8", colClasses =  c(rep("character", 8), "Date", "Date")) %>%
  mutate(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) 

#read in data for pooled results, parse columns and remove duplicates
pooled_experiment_results <- read.csv(file = pooled_experiment_results_file, header = TRUE, sep = ",", fileEncoding = "UTF-8", colClasses = c(rep("character", 3))) %>% 
  distinct(ExperimentID, Parameter.Analyzed, Result.File.Path)

#join the two tables to create master data sheet
master_data_sheet <- inner_join(pooled_experiment_results, meta_experiment_details, by = c("ExperimentID", "Parameter.Analyzed"))


#THIS IS THE PART THAT NEEDS CHANGING
#Filter out the desired results by key parameters i.e. model system, duration of treatment, parameter analyzed
DataToSummarise <- master_data_sheet %>%
  filter(Duration.of.stimulation == "5 days", Model.System =="NeuronsWT", Parameter.Analyzed =="NeuNNucleiCount")


#Create a unique filename based on the parameters selected
outputFileName <- paste(unique(DataToSummarise$Model.System), unique(DataToSummarise$Parameter.Analyzed), unique(DataToSummarise$Duration.of.stimulation), sep = "_") 
outputSheetName = paste(unique(DataToSummarise$Parameter.Analyzed), unique(DataToSummarise$Duration.of.stimulation), sep = ".")
outputWorkbookName = unique(DataToSummarise$Model.System)


#create a list containing all the  results from the filtered experiments
filtered_results <- list()
table_names <- c()
for (i in 1:length(DataToSummarise$ExperimentID)) {
  filtered_results <- append(filtered_results, list(read.xlsx(DataToSummarise$Result.File.Path[i], 2)))
  table_names <- append(table_names, DataToSummarise$ExperimentID[i])
}

for (i in 1:length(filtered_results)) {
  filtered_results[[i]] <- select(filtered_results[[i]], Treatment, Codename, Normalized_Mean)  #retain only Treatment, CodeName & Normalized_Mean columns
}

#rename elements of the filtered results list to corresponding ExperimentID
names(filtered_results) <- table_names 

#merge all the tables by rows and id by ExperimentID
filtered_results <- filtered_results %>%
  bind_rows(.id = "ExperimentID")

#Rearrange data to preferred presentation order
TreatmentLevels <- c("Null", "Control oligo", "miR-124-5p", "miR-124-5p(1)", "miR-124-5p(3)", "miR-124-5p(5)", "miR-124-5p(10)", "miR-124-5p(20)", "miR-9-5p", "miR-9-5p(1)", "miR-9-5p(3)", "miR-9-5p(5)", "miR-9-5p(10)", "miR-9-5p(20)", "miR-501-3p", "miR-501-3p(1)","miR-501-3p(3)", "miR-501-3p(5)", "miR-501-3p(10)", "miR-501-3p(20)","miR-92a-1-5p", "miR-92a-1-5p(1)", "miR-92a-1-5p(3)", "miR-92a-1-5p(5)", "miR-92a-1-5p(10)", "miR-92a-1-5p(20)", "let7b", "LOX", "R848", "TL8")

filtered_results$Treatment <- factor(filtered_results$Treatment, levels = TreatmentLevels)
filtered_results <- filtered_results%>%
  arrange(Treatment)


#flip the rows into columns
filtered_results_table <- filtered_results %>%
  select(ExperimentID, Treatment, Normalized_Mean) %>%
  pivot_wider(names_from = Treatment, values_from = Normalized_Mean)

#specify ouptut files
results_graph_file <- paste(parent_directory, outputFileName, ".tiff", sep = "")
results_excel_file <- paste(parent_directory, outputWorkbookName, ".xlsx", sep = "")


#save the results to an excel spreadsheet
if (file.exists(results_excel_file)){ 
  wb <- loadWorkbook(results_excel_file)
    if(any(str_detect(names(getSheets(wb)), outputSheetName))){ #delete the sheet if it already exists
    removeSheet(wb, sheetName = outputSheetName)  
    saveWorkbook(wb, results_excel_file)
  }
  write.xlsx(filtered_results_table, file = results_excel_file, sheetName = outputSheetName, col.names = TRUE, row.names = TRUE, append = TRUE)
  
} else {
  write.xlsx(filtered_results_table, file = results_excel_file, sheetName = outputSheetName, col.names = TRUE, row.names = TRUE, append = FALSE)
}

#plot the pooled result in a bar graph
scaled_width = 100*length(unique(filtered_results$Treatment))
tiff(results_graph_file, width=scaled_width, height=800, res=100)
ggbarplot(filtered_results, x = "Treatment", y = "Normalized_Mean", add = c("mean_se", "jitter"), title = outputFileName, size = 1, ylab = unique(DataToSummarise$Parameter.Analyzed), lab.hjust = 0.5) + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
garbage <- dev.off()
