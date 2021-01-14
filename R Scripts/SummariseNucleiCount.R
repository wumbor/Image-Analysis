#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)

#prepare workspace
myarg <- commandArgs()
working_directory <- as.character(myarg[6])
setwd(working_directory)  #set working dir based on command line arguments



#define key global variables
experiment_id <- str_split(working_directory, "/", simplify = TRUE)
experiment_id <- experiment_id[length(experiment_id)]

#Specify input files
microscopy_sequence_file <- paste(experiment_id, "_Image_Capture.txt", sep = "")
microscopy_data_file <- paste(experiment_id, "_Nuclei_Count.csv", sep = "")

#Specify output files
results_excel_file <- paste(experiment_id, "_Results.xlsx", sep = "")
results_graph_file <- paste(experiment_id, "_Graph.tiff", sep = "")

#Specify header line in microscopy_sequence_file
header_line <- (grep("Condition", read_lines(microscopy_sequence_file, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1




#define functions
normalize <- function(value, maxi) {
  normalized_value = (value/maxi)*100
  return(normalized_value)
}


#read in file data
image_sequence <- read.csv(file = microscopy_sequence_file, header = TRUE, sep = ",", skip = header_line)
image_data <- read.csv(file = microscopy_data_file, header = TRUE, sep = ",")

#process image sequence data
image_sequence <- image_sequence %>%
  rename(Original_Filename = Filename) %>%
  mutate(Original_Filename = as.character(Original_Filename)) %>%
  arrange(Original_Filename)

#process image count data
image_data <- image_data %>%
  rename(Processed_Filename = Slice) %>%
  mutate(Processed_Filename = as.character(Processed_Filename)) %>%
  arrange(Processed_Filename) %>%
  select(-Mode, -Median, -Mean)

#merge image sequence and image count tables
combined_data <- bind_cols(image_sequence, image_data) %>%
  separate(Condition, sep = "_", into = c("Treatment", "Field")) %>%
  mutate(Field = as.numeric(Field))

#calculate mean and median of null group
null_group_stats <- combined_data %>%
  filter(str_detect(Treatment, "Null")) %>%
  summarise(avg=mean(Count), med=median(Count))

#Normalize counts
combined_data <- combined_data %>%
  mutate(Mean_Normalized_Count = normalize(Count, null_group_stats$avg)) %>%
  mutate(Median_Normalized_Count = normalize(Count, null_group_stats$med)) %>%
  arrange(desc(Treatment))


#Calculate group means and medians
combined_data_summary <- combined_data %>%
  group_by(Treatment) %>%
  summarise(Normalized_Mean = mean(Mean_Normalized_Count), Normalized_Median=median(Median_Normalized_Count))

#Arrange summarised data in preferred format
TreatmentLevels <- c("Null", "miR#4", "miR#5", "miR#5(L)", "miR#5(M)", "miR#5(H)", "miR#13", "miR#13(L)", "miR#13(M)", "miR#13(H)", "miR#19", "miR#19(L)", "miR#19(M)", "miR#19(H)", "miR#27", "miR#27(L)", "miR#27(M)", "miR#27(H)", "let7b", "LOX", "R848")
combined_data_summary <- combined_data_summary %>%
  mutate(Treatment = trimws(Treatment)) 

combined_data_summary$Treatment <- factor(combined_data_summary$Treatment, levels = TreatmentLevels)
combined_data_summary <- combined_data_summary %>%
  arrange(Treatment)

#write data to file
if (file.access(results_excel_file)){
  write.xlsx(combined_data, file = results_excel_file, sheetName = "AutoThreshold Nuclei Count", col.names = TRUE, row.names = FALSE, append = TRUE)
} else {
  write.xlsx(combined_data, file = results_excel_file, sheetName = "AutoThreshold Nuclei Count", col.names = TRUE, row.names = FALSE, append = FALSE)
}
write.xlsx(combined_data_summary, file = results_excel_file, sheetName = "Nuclei Count Summary", col.names = TRUE, append = TRUE)



#save graph of results
tiff(results_graph_file, width=900, height=800, res=100)
mean_plot <- ggbarplot(combined_data, x = "Treatment", y = "Mean_Normalized_Count", add = c("mean_se", "jitter"), color = "Treatment", palette = "npg", size = 1)
median_plot <- ggbarplot(combined_data, x = "Treatment", y = "Median_Normalized_Count", add = c("mean_se", "jitter"), color = "Treatment", palette = "npg", size = 1)
ggarrange(mean_plot, median_plot, labels = c("Normalised by Mean", "Normalised by Median"), ncol = 2, common.legend = TRUE, legend = "bottom")
garbage <- dev.off()





