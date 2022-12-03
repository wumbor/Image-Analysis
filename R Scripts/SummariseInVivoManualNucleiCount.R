#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(bayestestR)



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

field_area <- 0.0146 #area of field in square millimeters at 64X Magnification at Olympus Microscope



working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mASSAY/64X Magnification/Blinded InVivo - 06.08.21/"

setwd(working_directory)

#Specify input files
microscopy_sequence_file <- "Image_Capture.txt"
microscopy_blinding_file <- "Blinding_Data.txt"
microscopy_data_file <- "Nuclei_Count.csv"


#Specify output files
results_excel_file <- "Nuclei_Count.xlsx"
results_graph_file <- "Nuclei_Count.tiff"
results_csv_file <- "Nuclei_Count_Summary.csv"

#Specify header line in microscopy_sequence_file
img_sequence_header_line <- (grep("Mouse.ID", read_lines(microscopy_sequence_file, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1



#read in data from files
image_sequence <- read.csv(file = microscopy_sequence_file, colClasses = c(rep("character", 4)), header = TRUE, sep = ",", skip = img_sequence_header_line)
image_blinding <- read.csv(file = microscopy_blinding_file, colClasses = c(rep("character", 3)), header = TRUE, sep = ",", skip = 0)
image_data <- read.csv(file = microscopy_data_file, colClasses = c("character", rep("numeric", 4)), header = TRUE, sep = ",")


#process image sequence data
image_sequence <- image_sequence %>%
  rename(Original_Filename = Filename) %>%
  arrange(Original_Filename)

#process image count data
image_data <- image_data %>%
  rename(Processed_Filename = Slice) %>%
  arrange(Processed_Filename) %>%
  select(-Mode, -Median, -Mean)


combined_data <- bind_cols(image_sequence, image_data) %>%
  select(-Treatment) %>%
  rename(Blind.Code = Mouse.ID)


unblinded_data <- left_join(combined_data, image_blinding, by = "Blind.Code")

unblinded_data <- unblinded_data %>%
  mutate(Treatment = parseStandardName(trimws(Treatment)))%>%
  mutate(Mouse.ID = trimws(Mouse.ID))

unblinded_data$Treatment <- factor(unblinded_data$Treatment, levels = TreatmentLevels)

combined_data <- unblinded_data %>%
  arrange(Treatment) %>%
  mutate(CountPerSqMM = Count/field_area)


combinedDataPerMouse <- combined_data %>%
  group_by(Treatment, Mouse.ID) %>%
  summarise(Mean = mean(CountPerSqMM))

#save the results to an excel spreadsheet
if (file.access(results_excel_file)){ #overwrite any existing file
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = TRUE)
} else {
  write.xlsx(combined_data, file = results_excel_file, sheetName = "Raw Data", col.names = TRUE, row.names = FALSE, append = FALSE)
}

write.xlsx(combinedDataPerMouse, file = results_excel_file, sheetName = "Summary", col.names = TRUE, row.names = FALSE)


write.csv(combinedDataPerMouse, results_csv_file)











#New Section
# unblinded_data1 <- unblinded_data 
# unblinded_data2 <- unblinded_data 
# unblinded_data <- bind_rows(unblinded_data1, unblinded_data2)
# 
# combined_data <- unblinded_data %>%
#   arrange(Treatment) %>%
#   mutate(CountPerSqMM = Count/field_area) %>%
#   mutate(Mouse.ID = trimws(Mouse.ID)) %>%
#   mutate(Section.ID = trimws(Section.ID))
# 
# 
# complete_set <- c("J1", "J2", "J3", "J5")
# combined_data <- combined_data %>%
#   filter(Blind.Code %in% complete_set) %>%
#   # filter(!(Section.ID == "L3")) %>%
#   group_by(Treatment, Section.ID) %>%
#   summarise(CountPerSqMM = mean(CountPerSqMM))
# 
# combinedDataPerMouse <- combined_data %>%
#   group_by(Treatment, Mouse.ID, Section.ID) %>%
#   summarise(CountPerSqMM = mean(CountPerSqMM))
# 
# ggbarplot(details, x = "Treatment", y = "CountPerSqMM", add = c("mean", "jitter"), size = 1, ylab = "CountPerSqMM", lab.hjust = 0.5) 