#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)

#prepare workspace
#working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/PhD Project-DESKTOP-3SHP9SG/mMORPH/R Scripts/2020_08_3_VK"
#setwd(working_directory)

myarg <- commandArgs()
working_directory <- as.character(myarg[6])
setwd(working_directory)  #set working dir based on command line arguments

#define key global variables
experiment_id <- str_split(working_directory, "/", simplify = TRUE)
experiment_id <- experiment_id[length(experiment_id)]

#Specify input files
microscopy_sequence_file <- paste(experiment_id, "_Image_Capture.txt", sep = "")
threshold_analysis_file <- paste(experiment_id, "_Threshold_Analysis.csv", sep = "")

#Specify output files
results_excel_file <- paste(experiment_id, "_Threshold_Results.xlsx", sep = "")
#results_graph_file <- paste(experiment_id, "_Graph.tiff", sep = "")

#Specify header line in microscopy_sequence_file
sequence_header_line <- (grep("Condition", read_lines(microscopy_sequence_file, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1
threshold_header_line <- (grep("Filename", read_lines(threshold_analysis_file, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1


#read in file data
image_sequence <- read.csv(file = microscopy_sequence_file, header = TRUE, sep = ",", skip = sequence_header_line)
threshold_data <- read.csv(file = threshold_analysis_file, header = TRUE, sep = ",", skip = threshold_header_line)


#process image sequence data
image_sequence <- image_sequence %>%
  rename(Original_Filename = Filename) %>%
  mutate(Original_Filename = as.character(Original_Filename)) %>%
  arrange(Original_Filename)

#process threshold data
threshold_data <- threshold_data %>%
  rename(Processed_Filename = Filename) %>%
  mutate(Processed_Filename = as.character(Processed_Filename)) %>%
  arrange(Processed_Filename)

#merge image sequence and threshold data tables
combined_data <- bind_cols(image_sequence, threshold_data) %>%
  separate(Condition, sep = "_", into = c("Treatment", "Field")) %>%
  mutate(Field = as.numeric(Field))


# #calculate stats of complete data 
# overall_stats <- combined_data %>%
#   summarise(avg=mean(Lower.Threshold), med=median(Lower.Threshold))

#calculate stats of null group
null_group_stats <- combined_data %>%
  filter(str_detect(Treatment, "Null")) %>%
  summarise(avg=mean(Lower.Threshold), med=median(Lower.Threshold))

#cat("CountThreshold = ", as.integer(null_group_stats$avg), "\n")
cat(as.integer(null_group_stats$avg))
#cat("Null Group Median = ", as.integer(null_group_stats$med), "\n")
# cat("Overall Average = ", as.integer(overall_stats$avg), "\n")
# cat("Overall Median = ", as.integer(overall_stats$med), "\n")

