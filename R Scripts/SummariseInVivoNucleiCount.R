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
# working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/40X Magnification/2021_01_1_VK/2021_01_1A_VK"
# setwd(working_directory)


#define key global variables
experiment_id <- str_split(working_directory, "/", simplify = TRUE)
experiment_id <- experiment_id[length(experiment_id)]
field_area <- 0.037 #area of field in square millimeters at 40X Magnification at Olympus Microscope
main_project_folder <- dirname(dirname(dirname(getwd())))

#Specify input files
microscopy_sequence_file <- paste(experiment_id, "_Image_Capture.txt", sep = "")
microscopy_data_file <- paste(experiment_id, "_Nuclei_Count.csv", sep = "")
analysis_log_file <- paste(experiment_id, "_AnalysisLog.txt", sep = "")

#Specify output files
results_excel_file <- paste(experiment_id, "_Nuclei_Count.xlsx", sep = "")
results_graph_file <- paste(experiment_id, "_Nuclei_Count.tiff", sep = "")
meta_results_file <- paste(main_project_folder, "Pooled_Data.csv", sep = "/")

#Specify header line in microscopy_sequence_file
img_sequence_header_line <- (grep("Condition", read_lines(microscopy_sequence_file, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1


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

TreatmentLevels <- c("Null", "Control oligo", "miR-124-5p", "miR-124-5p(1)", "miR-124-5p(3)", "miR-124-5p(5)", "miR-124-5p(10)", "miR-124-5p(20)", "miR-9-5p", "miR-9-5p(1)", "miR-9-5p(3)", "miR-9-5p(5)", "miR-9-5p(10)", "miR-9-5p(20)", "miR-501-3p", "miR-501-3p(1)","miR-501-3p(3)", "miR-501-3p(5)", "miR-501-3p(10)", "miR-501-3p(20)","miR-92a-1-5p", "miR-92a-1-5p(1)", "miR-92a-1-5p(3)", "miR-92a-1-5p(5)", "miR-92a-1-5p(10)", "miR-92a-1-5p(20)", "let7b", "LOX", "R848", "TL8-506")


#read in data from files
image_sequence <- read.csv(file = microscopy_sequence_file, colClasses = c(rep("character", 4)), header = TRUE, sep = ",", skip = img_sequence_header_line)
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

combined_data <- combined_data %>%
  mutate(Hemisphere = case_when(Field <= 6 ~ "Left", Field > 6 ~ "Right")) %>%
  mutate(CountPerSqMM = Count/field_area) %>%
  select(Mouse.ID:Treatment, Hemisphere, Field:Count, CountPerSqMM, Total.Area:X.Area)

#summarise the mean and median for each hemisphere
combinedDataPerHemisphere<- combined_data %>%
  group_by(Treatment, Hemisphere) %>%
  summarise(MeanCountPerSqMM = mean(CountPerSqMM), MedianCountPerSqMM = median(CountPerSqMM))

summaryReport <- combinedDataPerHemisphere %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(MeanCountPerSqMM), Median = mean(MedianCountPerSqMM))


#save the results to an excel spreadsheet
if (file.access(results_excel_file)){ #overwrite any existing file
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = TRUE)
} else {
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = FALSE)
}
write.xlsx(summaryReport, file = results_excel_file, sheetName = "Nuclei Count Summary", col.names = TRUE, append = TRUE)
write.xlsx(analysis_parameters, file = results_excel_file, sheetName = "Analysis Parameters", col.names = FALSE, row.names = FALSE, append = TRUE)


#save graph of results
scaled_width = 150*length(unique(summaryReport$Treatment))
tiff(results_graph_file, width=scaled_width, height=800, res=100)

plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

mean_plot <- ggbarplot(combinedDataPerHemisphere, x = "Treatment", y = "MeanCountPerSqMM", add = c("mean", "jitter"), size = 1, ylab = "NeuN+ Cell Count Per Square MM", lab.hjust = 0.5, title = "Mean Count Per Square MM") + plottheme

median_plot <- ggbarplot(combinedDataPerHemisphere, x = "Treatment", y = "MedianCountPerSqMM", add = c("mean", "jitter"), size = 1, ylab = "NeuN+ Cell Count Per Square MM", lab.hjust = 0.5, title = "Median Count Per Square MM") + plottheme

combined_plot <- ggarrange(mean_plot, median_plot, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(combined_plot, top = text_grob(experiment_id, face = "bold", size = 14))
garbage <- dev.off()



# Save key experiment details (Experiment#, Parameter Analyzed, Path to Results file) to pooled data sheet
meta_result <- data.frame("ExperimentID" = experiment_id, "Parameter.Analyzed" = "NeuNNucleiCount", "Result.File.Path" = paste (getwd(), results_excel_file, sep = "/"), stringsAsFactors = FALSE)


if (file.exists(meta_results_file)){ #append to an existing file, if any
  write_csv(meta_result, meta_results_file, na = "NA", append = TRUE, col_names = FALSE)
} else {
  write_csv(meta_result, meta_results_file, na = "NA", append = TRUE, col_names = TRUE)
}



#Detecct and report suspected outliers in a separate sheet
dt <- combined_data
var <- combined_data$Count
var_name <- eval(substitute(var),eval(dt))
outlier <- boxplot.stats(var_name)$out

suspected_outliers <- combined_data %>%
  filter(Count %in% outlier)

if(length(outlier)){
  write.xlsx(suspected_outliers, file = results_excel_file, sheetName = "Suspected Outliers", col.names = TRUE, row.names = FALSE, append = TRUE)
} else {
  suspected_outliers <- data.frame("ExperimentID" = experiment_id, "Report" = "NO SUSPECTED OUTLIERS DETECTED", stringsAsFactors = FALSE)
  write.xlsx(suspected_outliers, file = results_excel_file, sheetName = "Suspected Outliers", col.names = TRUE, row.names = FALSE, append = TRUE)
}
