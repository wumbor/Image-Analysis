#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)

#prepare workspace
myarg <- commandArgs()
print("Initial working directory was")
getwd()

setwd(myarg[6])
working_directory <- as.character(myarg[6])

print("New working directory is")
getwd()



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
  summarise(Normalized_Mean = mean(Mean_Normalized_Count), Normalized_Median=median(Median_Normalized_Count)) %>%
  arrange(desc(Treatment))

#write data to file
write.xlsx(combined_data, file = results_excel_file, sheetName = "Nuclei Count", col.names = TRUE, row.names = FALSE, append = FALSE)
write.xlsx(combined_data_summary, file = results_excel_file, sheetName = "Nuclei Count Summary", col.names = TRUE, append = TRUE)



#save graph of results
tiff(results_graph_file, width=900, height=800, res=100)
mean_plot <- ggbarplot(combined_data, x = "Treatment", y = "Mean_Normalized_Count", add = c("mean_se", "jitter"), color = "Treatment", palette = "npg", size = 1)
median_plot <- ggbarplot(combined_data, x = "Treatment", y = "Median_Normalized_Count", add = c("mean_se", "jitter"), color = "Treatment", palette = "npg", size = 1)
ggarrange(mean_plot, median_plot, labels = c("Normalised by Mean", "Normalised by Median"), ncol = 2, common.legend = TRUE, legend = "bottom")
dev.off()