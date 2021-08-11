#load required libraries
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(bayestestR)



working_directory <- "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mELECTRIC/Slice Recording - 25.09.2020/E-55 30s puffs"

# E-55 30s puffs
setwd(working_directory)






data_file = "E55-Sequence5-26.asc"
# E53-CCIVneu-10
# E55-Sequence5-22

output_file <- str_split(data_file, "\\.", simplify = TRUE)
output_file <- output_file[[1]]
output_file <- paste(output_file, ".tiff", sep = "")

header_line <- (grep("Index", read_lines(data_file, skip = 0, skip_empty_rows = FALSE, n_max = -1L, na = character()))) - 1

ephys_data <- read.csv(data_file, skip = header_line)

ephys_data <- ephys_data %>%
  select(Time.s., Imon1.A., Vmon.1.V.) %>%
  mutate(Imon1.pA. = Imon1.A.*1E12)

ephys_data_sample <- ephys_data %>%
  filter(row_number() %% 20 == 1)

# write_csv(ephys_data, output_file)

#Plot and export graph
tiff(output_file, width=1000, height=600, res=100)
graph <- ggline(ephys_data_sample, x= "Time.s.", y = "Imon1.pA.", numeric.x.axis = TRUE)
ggpar(graph, ylim = c(-100, 100))

garbage <- dev.off()
# 
# data <- read.xlsx(data_file, 4, sheetName = "mtlr7", startRow = 44, endRow = 53, encoding = "UTF-8"