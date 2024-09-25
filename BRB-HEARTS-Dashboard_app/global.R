
### SCRIPT METADATA ####

# program name :        brb_hearts_001
# project:              Barbados HEARTS Dashboard
# analyst:              Kern ROCKE
# date create:          24 Sept 24
# date modified:        24 Sept 24
# task:                 Create outputs for HEARTS Dashboard

# Set working directory
dir <- "/Users/kernrocke/Library/Mobile Documents/com~apple~CloudDocs/Github/BRB-HTN-Dashboard"
data_name <- "BRB_Hearts_Dashboard_Data.csv"

# Load libraries
#List of libaries needed
libs <- c("readxl", "tidyverse", "openxlsx", "writexl", "magrittr", "reshape2", 
          "lubridate", "uuid", "stringr", "RcppUUID", "data.table", "anytime", 
          "dplyr", "tidyr")

#Install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

#Load libraries
invisible(lapply(libs, library, character.only = T))

#Load data into environment
data <- read.csv(paste0(dir,"/Data/",data_name))

### DATA CLEANING

# Remove NULL from systolic and diastolic values
data$most_recent_systoic <- as.numeric(as.character(data$most_recent_systoic))
data$most_recent_diasystoic <- as.numeric(as.character(data$most_recent_diasystoic))

# Data Quality check - Flagging erogenous BP readings
data$SBP_keep[data$most_recent_systoic<70]=0
data$SBP_keep[data$most_recent_systoic>300]=0
data$SBP_keep[data$most_recent_systoic>=70 & data$most_recent_systoic<=300]=1

data$DBP_keep[data$most_recent_diasystoic<40]=0
data$DBP_keep[data$most_recent_diasystoic>200]=0
data$DBP_keep[data$most_recent_diasystoic>=40 & data$most_recent_diasystoic<=200]=1

# Create Hypertension control categories

## Isolated SBP
data$ISO_SBP[data$most_recent_systoic>=140 & data$most_recent_diasystoic<=90]=1
data$ISO_SBP[data$most_recent_systoic>=140 & data$most_recent_diasystoic>90]=0
data$ISO_SBP[data$most_recent_systoic<140 & data$most_recent_diasystoic>90]=0
data$ISO_SBP[data$most_recent_systoic<140 & data$most_recent_diasystoic<=90]=0

## Isolated DBP
data$ISO_DBP[data$most_recent_systoic>=140 & data$most_recent_diasystoic<90]=0
data$ISO_DBP[data$most_recent_systoic>=140 & data$most_recent_diasystoic>=90]=0
data$ISO_DBP[data$most_recent_systoic<140 & data$most_recent_diasystoic>=90]=1
data$ISO_DBP[data$most_recent_systoic<140 & data$most_recent_diasystoic<90]=0

# Hypertension Uncontrolled
data$HTN_control[data$most_recent_systoic>=140 & data$most_recent_diasystoic>=90]=1
data$HTN_control[data$most_recent_systoic>=140 & data$most_recent_diasystoic<90]=0
data$HTN_control[data$most_recent_systoic<140 & data$most_recent_diasystoic>=90]=0
data$HTN_control[data$most_recent_systoic<140 & data$most_recent_diasystoic<90]=0

# Hypertension Control
data$HTN_control[data$most_recent_systoic>=140 & data$most_recent_diasystoic>=90]=0
data$HTN_control[data$most_recent_systoic>=140 & data$most_recent_diasystoic<90]=0
data$HTN_control[data$most_recent_systoic<140 & data$most_recent_diasystoic>=90]=0
data$HTN_control[data$most_recent_systoic<140 & data$most_recent_diasystoic<90]=1

# Extract year and month
data$year <- format(as.Date(data$last_visited_date, format="%m/%d/%Y"),"%Y")
data$month <- format(as.Date(data$last_visited_date, format="%m/%d/%Y"),"%m")
