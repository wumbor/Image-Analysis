#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)

#prepare workspace
myarg <- commandArgs()
working_directory <- as.character(myarg[6])
setwd(working_directory)  #set working dir based on command line arguments


#TESTING SECTION
# working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/R Scripts/2020_08_6B_VK"
# setwd(working_directory)


#define key global variables
experiment_id <- str_split(working_directory, "/", simplify = TRUE)
experiment_id <- experiment_id[length(experiment_id)]

#Specify input files
microscopy_sequence_file <- paste(experiment_id, "_Image_Capture.txt", sep = "")
microscopy_data_file <- paste(experiment_id, "_Nuclei_Count.csv", sep = "")
analysis_log_file <- paste(experiment_id, "_AnalysisLog.txt", sep = "")

#Specify output files
results_excel_file <- paste(experiment_id, "_Nuclei_Count.xlsx", sep = "")
results_graph_file <- paste(experiment_id, "_Nuclei_Count.tiff", sep = "")
meta_results_file <- paste(dirname(getwd()), "Pooled_Data.csv", sep = "/") 

#Specify header line in microscopy_sequence_file
img_sequence_header_line <- (grep("Condition", read_lines(microscopy_sequence_file, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1



#define functions
normalize <- function(value, maxi) {
  normalized_value = (value/maxi)*100
  return(normalized_value)
}


parseStandardName <- function(input_string) {
  codeNameVector <-  c("miR#4", "miR#5", "miR#13", "miR#19", "miR#27", "TL8")
  standardNameVector <-  c("Control oligo", "miR-124-5p", "miR-9-5p", "miR-501-3p", "miR-92a-1-5p", "TL8-506")
  codeDoseVector <-  c("\\(L\\)", "\\(LM\\)", "\\(M\\)", "\\(H\\)",  "\\(XH\\)")
  standardDoseVector <- c("(1)", "(3)", "(5)", "(10)",  "(20)")
  
  output <- input_string
  # output <- str_replace(output, "miR#5", "miR-124-5p")
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

TreatmentLevels <- c("Null", "Control oligo", "miR-124-5p", "miR-124-5p(1)", "miR-124-5p(3)", "miR-124-5p(5)", "miR-124-5p(10)", "miR-124-5p(20)", "miR-9-5p", "miR-9-5p(1)", "miR-9-5p(3)", "miR-9-5p(5)", "miR-9-5p(10)", "miR-9-5p(20)", "miR-501-3p", "miR-501-3p(1)","miR-501-3p(3)", "miR-501-3p(5)", "miR-501-3p(10)", "miR-501-3p(20)","miR-92a-1-5p", "miR-92a-1-5p(1)", "miR-92a-1-5p(3)", "miR-92a-1-5p(5)", "miR-92a-1-5p(10)", "miR-92a-1-5p(20)", "let7b", "LOX", "R848", "TL8")


#read in data from files
image_sequence <- read.csv(file = microscopy_sequence_file, colClasses = c(rep("numeric", 2), rep("character", 2)), header = TRUE, sep = ",", skip = img_sequence_header_line)
image_data <- read.csv(file = microscopy_data_file, colClasses = c("character", rep("numeric", 4)), header = TRUE, sep = ",")
analysis_parameters <- read.csv(file = analysis_log_file, sep = ",", header = TRUE)



#process image sequence data
image_sequence <- image_sequence %>%
  rename(Original_Filename = Filename) %>%
  arrange(Original_Filename)

#process image count data
image_data <- image_data %>%
  rename(Processed_Filename = Slice) %>%
  arrange(Processed_Filename) %>%
  select(-Mode, -Median, -Mean)

#merge image sequence and image count tables and rearrange data to preferred presentation order
combined_data <- bind_cols(image_sequence, image_data) %>%
  separate(Condition, sep = "_", into = c("Treatment", "Field")) %>%
  mutate(Field = as.numeric(Field)) %>%
  mutate(Treatment = parseStandardName(trimws(Treatment))) 
combined_data$Treatment <- factor(combined_data$Treatment, levels = TreatmentLevels)



#summarise the mean and median for every coverslip
combinedDataPerCoverslip <- combined_data %>%
  group_by(Treatment, Coverslip) %>%
  summarise(Mean = mean(Count), Median = median(Count)) 


#calculate mean and median of null group
null_group_stats <- combinedDataPerCoverslip %>%
  filter(Treatment=="Null") %>%
  summarise(AvgMean = mean(Mean), AvgMedian = mean(Median)) 


#Normalize all counts to null group stats
combinedDataPerCoverslip <- combinedDataPerCoverslip %>%
  mutate(NormalizedMean = normalize(Mean, null_group_stats$AvgMean), NormalizedMedian = normalize(Median, null_group_stats$AvgMedian))


summaryReport <- combinedDataPerCoverslip %>%
  group_by(Treatment) %>%
  summarise(NormalizedMean = mean(NormalizedMean), NormalizedMedian = mean(NormalizedMedian))


#save the results to an excel spreadsheet
if (file.access(results_excel_file)){ #overwrite any existing file
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = TRUE)
} else {
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = FALSE)
}
write.xlsx(summaryReport, file = results_excel_file, sheetName = "Nuclei Count Summary", col.names = TRUE, append = TRUE)
write.xlsx(analysis_parameters, file = results_excel_file, sheetName = "Analysis Parameters", col.names = FALSE, row.names = FALSE, append = TRUE)


#Outlier Detection
# dt <- combined_data
# var <- combined_data$Count
# var_name <- eval(substitute(var),eval(dt))
# outlier <- boxplot.stats(var_name)$out
# 
# suspected_outliers <- combined_data %>%
#   filter(Count %in% outlier)
# 
# if(length(outlier)){
#   write.xlsx(suspected_outliers, file = results_excel_file, sheetName = "Suspected Outliers", col.names = TRUE, row.names = FALSE, append = TRUE)
# } 

#save graph of results
scaled_width = 100*length(unique(summaryReport$Treatment))
tiff(results_graph_file, width=scaled_width, height=800, res=100)

plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

mean_plot <- ggbarplot(combinedDataPerCoverslip, x = "Treatment", y = "NormalizedMean", add = c("mean", "jitter"), size = 1, ylab = "Normalised NeuN+ Cell Count (%)", lab.hjust = 0.5, title = "Normalized by Mean") + plottheme

median_plot <- ggbarplot(combinedDataPerCoverslip, x = "Treatment", y = "NormalizedMedian", add = c("mean", "jitter"), size = 1, ylab = "Normalised NeuN+ Cell Count (%)", lab.hjust = 0.5, title = "Normalized by Median") + plottheme

ggarrange(mean_plot, median_plot, ncol = 2, common.legend = TRUE, legend = "bottom")
garbage <- dev.off()



# Save key experiment details (Experiment#, Parameter Analyzed, Path to Results file) to pooled data sheet
meta_result <- data.frame("ExperimentID" = experiment_id, "Parameter.Analyzed" = "NeuNNucleiCount", "Result.File.Path" = paste (getwd(), results_excel_file, sep = "/"), stringsAsFactors = FALSE)


if (file.exists(meta_results_file)){ #append to an existing file, if any
  write_csv(meta_result, meta_results_file, na = "NA", append = TRUE, col_names = FALSE)
} else {
  write_csv(meta_result, meta_results_file, na = "NA", append = TRUE, col_names = TRUE)
}









