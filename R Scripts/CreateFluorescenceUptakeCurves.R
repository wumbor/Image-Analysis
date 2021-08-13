#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(bayestestR)

#prepare workspace
working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/2021_06_3_VK"

setwd(working_directory)
FilesToProcess <- c("ROI Dot 1 Time Profile Data.csv", "ROI Dot 2 Time Profile Data.csv", "ROI Dot 3 Time Profile Data.csv")
summary_graph <- "Intensity_Profiles.tiff"


#Create a function which receives the filename as input and generates a tibble with the normalised data
processProfileData <- function(filename) {
  #Read in data from file
  raw_profile_data <- read.csv(filename, col.names = c("Time.h", "Blue", "Red", "Green"))
  
  #Pivot the data longer
  processed_profile_data <- raw_profile_data %>%
    pivot_longer(cols = Blue:Green, names_to = "Channel", values_to = "Value")
  
  #Calculate Normalised Values
  processed_profile_data <- processed_profile_data %>%
    group_by(Channel) %>%
    mutate(Normalized.To.Initial = (1*Value)/first(Value)) %>%
    ungroup()
  
  return(processed_profile_data)
}


#Iterate over list of files to process and combine the into one dataset
combinedData <- list()

for (i in 1:length(FilesToProcess)) {
  combinedData <- append(combinedData, list(processProfileData(FilesToProcess[i])))
}

combinedData <- bind_rows(combinedData, .id = "ID")



#Process the combined datasets for plotting graphs
combinedData <- combinedData  %>%
  rename(Raw.Value = Value, Normalized.Value = Normalized.To.Initial) %>%
  pivot_longer(Raw.Value:Normalized.Value, names_to = "Parameter", values_to = "Value" )


#Plot Raw Value graph
graph1 <- combinedData %>%
  filter(Parameter == "Raw.Value") %>%
  ggline(x="Time.h", y="Value", color = "Channel", palette = c("#0000FF", "#FF0000", "#00FF00"), add = "mean_se", numeric.x.axis = TRUE, xticks.by = 1)

#Plot Normalised Value graph
graph2 <- combinedData %>%
  filter(Parameter == "Normalized.Value") %>%
  ggline(x="Time.h", y="Value", color = "Channel", palette = c("#0000FF", "#FF0000", "#00FF00"), add = "mean_se", numeric.x.axis = TRUE, xticks.by = 1)

graphs <- list(graph1, graph2)





tiff(summary_graph, width=1600, height=1000, res=100)
#ggarrange(plotlist = graphs, nrow = 2, common.legend = TRUE)
combinedData %>%
  filter(Parameter == "Raw.Value") %>%
  ggline(x="Time.h", y="Value", color = "Channel", palette = c("#0000FF", "#FF0000", "#00FF00"), add = "mean_se", numeric.x.axis = TRUE, xticks.by = 1, point.size = 0.5, ylab = "Normalised Fluorescence Intensity")
garbage <- dev.off()














# ORIGINAL SCRIPT
#prepare workspace
# working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/2019_06_1_TW/Third_Point"
# setwd(working_directory)
# 
# intensity_profile_data_file <- "Intensity_Profile.xlsx"
# intensity_profile_graph_raw <- paste(working_directory, "/Colocalisation_Curves_Raw", ".tiff", sep = "")
# intensity_profile_graph_normalized_to_max <- paste(working_directory, "/Colocalisation_Curves_Normalized_Max", ".tiff", sep = "")
# intensity_profile_graph_normalized_to_initial <- paste(working_directory, "/Colocalisation_Curves_Normalized_Initial", ".tiff", sep = "")
# intensity_profile_output_file <- paste(working_directory, "/Intensity_Profile_Output", ".xlsx", sep = "")
# 
# #define useful functions
# #converts strings from the DE format to ISO numeric
# convertDEtoISO <- function(input_string){
#   output_numeric <- as.numeric(str_replace(input_string, "\\,", "\\."))
#   return(output_numeric)
# }
# 
# 
# #read in the raw data from an Excel sheet
# intensity_profile_raw <- read.xlsx(intensity_profile_data_file, 1)
# 
# 
# 
# 
# #ANALYSIS FOR TWO LINE PROFILES
# 
# #rename variables, covert strings to numbers and convert time to minutes and hours
# intensity_profile_raw <- intensity_profile_raw %>%
#   rename(Red_Ch_X = Ch1, Red_Ch_Y = Ch2, Green_Ch_X = Ch3, Green_Ch_Y = Ch4, Original_Filename = Name) %>%
#   mutate_at(c("Distance", "Red_Ch_X", "Red_Ch_Y", "Green_Ch_X", "Green_Ch_Y"), convertDEtoISO) %>%
#   mutate(Time.h = round((Time/3600000), digits = 2)) %>%
#   filter(!(is.na(Green_Ch_X)|is.na(Green_Ch_Y)|is.na(Red_Ch_X)|is.na(Red_Ch_Y))) #filter out NAs
# 
# 
# #Calculate the mean, max and AUCs for the intensity plots 
# intensity_profile_summary <- intensity_profile_raw %>%
#   group_by(Time.h) %>%
#   summarise(Mean_Red = (mean(Red_Ch_X)+mean(Red_Ch_Y))/2, Mean_Green = (mean(Green_Ch_X)+mean(Green_Ch_Y))/2, AUC_Red = (area_under_curve(Distance, Red_Ch_X)+area_under_curve(Distance, Red_Ch_Y))/2, AUC_Green = (area_under_curve(Distance, Green_Ch_X)+area_under_curve(Distance, Green_Ch_Y))/2)
# 
# 
# #Rearrange the data into one column
# intensity_profile_graph_data <- intensity_profile_summary %>%
#   pivot_longer(Mean_Red:AUC_Green, names_to = "Parameter")
# 
# #Inlcude the Parameter and Channel as separate variables
# intensity_profile_graph_data <- intensity_profile_graph_data %>%
#   rename(Parameter.Channel = Parameter) %>%
#   separate(Parameter.Channel, into = c("Parameter", "Channel"), remove = FALSE)
# 
# 
# intensity_profile_graph_data <- intensity_profile_graph_data %>%
#   group_by(Parameter.Channel) %>%
#   mutate(Normalized.To.Initial = (100*value)/first(value), Normalized.To.Max = (100*value)/max(value)) %>%
#   ungroup()
# 
# intensity_profile_summary <- intensity_profile_graph_data %>%
#   pivot_wider(id_cols = Time.h, names_from = Parameter.Channel, values_from = c(value, Normalized.To.Initial, Normalized.To.Max), names_sep = "__")
# 
# 
# 
# 
# #Plot the raw intensity values on a graph
# plot_raw <- intensity_profile_graph_data %>%
#   filter(!(Parameter == "Max")) %>%
#   ggline(x="Time.h", y="value", facet.by = "Parameter", color = "Channel", palette = c("#00FF00", "#FF0000"))
# 
# tiff(intensity_profile_graph_raw, width=1800, height=800, res=100)
# ggpar(plot_raw, xlab = "Time (h)", ylab = "Fluorescence Intensity (AU)", x.text.angle = 45, yticks.by = 2000)
# garbage <- dev.off()
# 
# 
# #Plot the intensity values normalized to the max on a graph
# plot_normalized_to_max <- intensity_profile_graph_data %>%
#   filter(!(Parameter == "Max")) %>%
#   ggline(x="Time.h", y="Normalized.To.Max", facet.by = "Parameter", color = "Channel", palette = c("#00FF00", "#FF0000"))
# 
# tiff(intensity_profile_graph_normalized_to_max, width=1800, height=800, res=100)
# ggpar(plot_normalized_to_max, xlab = "Time (h)", ylab = "Normalized Intensity (%)", x.text.angle = 45, yticks.by = 10)
# garbage <- dev.off()
# 
# 
# #Plot the intensity values normalized to the initial value on a graph
# plot_normalized_to_initial <- intensity_profile_graph_data %>%
#   filter(!(Parameter == "Max")) %>%
#   ggline(x="Time.h", y="Normalized.To.Initial", facet.by = "Parameter", color = "Channel", palette = c("#00FF00", "#FF0000"))
# 
# tiff(intensity_profile_graph_normalized_to_initial, width=1800, height=800, res=100)
# ggpar(plot_normalized_to_initial, xlab = "Time (h)", ylab = "Normalized Intensity (%)", x.text.angle = 45, yticks.by = 50)
# garbage <- dev.off()
# 
# 
# 
# 
# #save the results to an excel spreadsheet
# if (file.access(intensity_profile_output_file)){ #overwrite any existing file
#   write.xlsx(intensity_profile_raw, file = intensity_profile_output_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = TRUE)
# } else {
#   write.xlsx(intensity_profile_raw, file = intensity_profile_output_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = FALSE)
# }
# write.xlsx(intensity_profile_summary, file = intensity_profile_output_file, sheetName = "Summary", col.names = TRUE, append = TRUE)
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
# 
# 
# 
# 
# 
# 
# #THIS SECTION PLOTS THE MEAN OF SEVERAL EXPERIMENTS
# 
# first_n_file <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/2019_06_1_TW/Second_Point/Intensity_Profile_Output.xlsx"
# second_n_file <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/2019_06_1_TW/Third_Point/Intensity_Profile_Output.xlsx"
# third_n_file <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/2019_06_1_TW/Fourth_Point/Intensity_Profile_Output.xlsx"
# 
# 
# #read in the raw data from an Excel sheet
# first_n_data <- read.xlsx(first_n_file, 2)
# second_n_data <- read.xlsx(second_n_file, 2)
# third_n_data <- read.xlsx(third_n_file, 2)
# 
# combined_data <- bind_rows(first_n_data, second_n_data, third_n_data, .id = "ID")
# 
# 
# 
# 
# 
# 
# #Rearrange the data into fewer rows
# combined_data_graph <- combined_data %>%
#   pivot_longer(value__Mean_Red:Normalized.To.Max__AUC_Green, names_to = "Parameter")
# 
# #Inlcude the Parameter and Channel as separate variables
# combined_data_graph  <- combined_data_graph  %>%
#   select(ID, Time.h:value) %>%
#   rename(Parameter.Channel = Parameter) %>%
#   separate(Parameter.Channel, into = c("Parameter", "Channel"), sep = "__") %>%
#   mutate(Parameter = str_replace(Parameter, "value", "Raw.Data")) %>%
#   rename(Normalized.Status = Parameter) %>%
#   separate(Channel, into = c("Parameter", "Channel"), sep = "_")
# 
# 
# 
# 
# #Plot the intensity values normalized to the initial value on a graph
# summary_graph_normalized_to_initial <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/2019_06_1_TW/Summary.tiff"
# 
# summary_plot_normalized_to_initial <- combined_data_graph %>%
#   filter(Normalized.Status == "Normalized.To.Initial") %>%
#   ggline(x="Time.h", y="value", facet.by = "Parameter", color = "Channel", palette = c("#00FF00", "#FF0000"), add = "mean_se")
# 
# tiff(summary_graph_normalized_to_initial, width=1800, height=800, res=100)
# ggpar(summary_plot_normalized_to_initial, xlab = "Time (h)", ylab = "Normalized Intensity (%)", x.text.angle = 45)
# garbage <- dev.off()
# 
# 
# 
# 
# 
# 
# 
# #PLOTS FOR LAB MEETING
# summary_graphs <- list()
# 
# mean_raw_graph <- combined_data_graph %>%
#   filter((Normalized.Status == "Raw.Data")&(Parameter =="Mean")) %>%
#   ggline(x="Time.h", y="value", color = "Channel", palette = c("#00FF00", "#FF0000"), ylab = "Mean Fluorescence Intentisty (AU)", add = "mean_se", x.text.angle = 45, xlab = "Time (h)", title ="Raw Intensity Values" )
# 
# mean_normalised_graph <- combined_data_graph %>%
#   filter((Normalized.Status == "Normalized.To.Initial")&(Parameter =="Mean")) %>%
#   ggline(x="Time.h", y="value", color = "Channel", palette = c("#00FF00", "#FF0000"), ylab = "Normalized Mean Fluorescene (%)", add = "mean_se", x.text.angle = 45, xlab = "Time (h)", title = "Normalized to Initial Values")
# 
# summary_graphs <- append(summary_graphs, list(mean_raw_graph))
# summary_graphs <- append(summary_graphs, list(mean_normalised_graph))
# 
# tiff(summary_graph_normalized_to_initial, width=1800, height=800, res=100)
# ggarrange(plotlist = summary_graphs, ncol = 2)
# garbage <- dev.off()
# 
# 
# 
