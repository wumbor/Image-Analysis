#The aim of this script is to compare the automated counts obtained from the TUNL count with manual count data to optimize the macro


##  Load required libraries and functions
library(tidyverse)
library("xlsx")
library(ggpubr)
library(ggsci)
library(ggplot2)
library(ggrepel)


#initialise workspace and allow user to define working directory
workingDirectory <- choose.dir(default = getwd(), caption = "Select Source folder")
workingDirectory <- gsub("\\\\", "/", workingDirectory)
setwd(workingDirectory)
experimentId <- str_split(workingDirectory, "/", simplify = TRUE)
experimentId <- experimentId[length(experimentId)]


#Specify input files
resultsExcelFile <- paste(experimentId, "_TUNL_Count.xlsx", sep = "")


#load manual count data
manualCountDirectory <- paste(workingDirectory, "Manual Counts", sep = "/")
setwd(manualCountDirectory)
rawData <- read.xlsx(resultsExcelFile, 1)

#reset working directory
setwd(workingDirectory)

#plot graphs
rawData %>%
  ggbarplot(x="Treatment", y="Count", add = c("mean_sd", "jitter"))

ggscatter(rawData, x="Count", y="Manual.Count",  add = "reg.line",  # Add regression line
          add.params = list(color = "blue", fill = "lightgray"))+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3
  )


#in Summary the automated counts are a better reflection of ground truth


