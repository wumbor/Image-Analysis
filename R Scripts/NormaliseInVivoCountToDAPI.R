#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)

#prepare workspace
#working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/40X Magnification/2021_01_1_VK/2021_01_1C_VK"
working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/40X Magnification/2021_04_5_VK"
setwd(working_directory)

#define key global variables
experiment_id <- str_split(working_directory, "/", simplify = TRUE)
experiment_id <- experiment_id[length(experiment_id)]


#Specify input files
#dapi_results_file <- paste(working_directory, "2021_01_1A_VK_Nuclei_Count_DAPI.xlsx", sep = "/")
#neun_results_file <- paste(working_directory, "2021_01_1A_VK_Nuclei_Count_NeuN.xlsx", sep = "/")
dapi_results_file <- paste(experiment_id, "_Nuclei_Count_DAPI.xlsx", sep = "")
neun_results_file <- paste(experiment_id, "_Nuclei_Count_NeuN.xlsx", sep = "")

#Read in data
dapi_data <- read.xlsx(dapi_results_file, 1)
neun_data <- read.xlsx(neun_results_file, 1)

#Specify output files
results_excel_file <- paste(experiment_id, "_Normalised_Count.xlsx", sep = "")
results_graph_file <- paste(experiment_id, "_Normalised_Count.tiff", sep = "")


#process individual datasheets
neun_data <- neun_data %>%
  mutate(NeuNCountPerSqMM = CountPerSqMM) %>%
  select(Mouse.ID:Processed_Filename, NeuNCountPerSqMM)

dapi_data <- dapi_data %>%
  mutate(DAPICountPerSqMM = CountPerSqMM) %>%
  select(Mouse.ID:Processed_Filename, DAPICountPerSqMM)

#combine and process combined data
combined_data <- inner_join(neun_data, dapi_data, by = c("Mouse.ID", "Section.ID", "Treatment", "Hemisphere", "Field", "Original_Filename"))
combined_data$Treatment <- factor(combined_data$Treatment, levels = TreatmentLevels)

combined_data <- combined_data %>%
  arrange(Treatment) %>%
  mutate(CountNormalisedToDAPI = NeuNCountPerSqMM/DAPICountPerSqMM) %>%
  filter(DAPICountPerSqMM>0)


#summarise the mean and median for each hemisphere
combinedDataPerHemisphere <- combined_data %>%
  group_by(Mouse.ID, Hemisphere) %>%
  summarise(MeanCountNormalisedToDAPI = mean(CountNormalisedToDAPI), MedianCountNormalisedToDAPI = median(CountNormalisedToDAPI))

summaryReport <- combinedDataPerHemisphere %>%
  group_by(Mouse.ID) %>%
  summarise(Mean = mean(MeanCountNormalisedToDAPI), Median = mean(MedianCountNormalisedToDAPI))



#save graph of results
scaled_width = 150*length(unique(summaryReport$Treatment))
tiff(results_graph_file, width=scaled_width, height=800, res=100)

plottheme <- theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

mean_plot <- ggbarplot(combinedDataPerHemisphere, x = "Treatment", y = "MeanCountNormalisedToDAPI", add = c("mean", "jitter"), size = 1, ylab = "NeuN+/DAPI Cell Count", lab.hjust = 0.5, title = "Normalised Mean Count") + plottheme

median_plot <- ggbarplot(combinedDataPerHemisphere, x = "Treatment", y = "MedianCountNormalisedToDAPI", add = c("mean", "jitter"), size = 1, ylab = "NeuN+/DAPI Cell Count", lab.hjust = 0.5, title = "Normalised Median Count") + plottheme

combined_plot <- ggarrange(mean_plot, median_plot, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(combined_plot, top = text_grob(experiment_id, face = "bold", size = 14))
garbage <- dev.off()


#save the results to an excel spreadsheet
if (file.access(results_excel_file)){ #overwrite any existing file
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = TRUE)
} else {
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = FALSE)
}
write.xlsx(summaryReport, file = results_excel_file, sheetName = "Normalised Count Summary", col.names = TRUE, append = TRUE)

