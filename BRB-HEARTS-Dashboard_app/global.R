
### SCRIPT METADATA ####

# program name :        brb_hearts_001
# project:              Barbados HEARTS Dashboard
# analyst:              Kern ROCKE
# date create:          24 Sept 24
# date modified:        24 Sept 24
# task:                 Create outputs for HEARTS Dashboard

# Set working directory
#dir <- "/Users/kernrocke/Library/Mobile Documents/com~apple~CloudDocs/Github/BRB-HTN-Dashboard"
dir <- "/home/kernrocke/Documents/Github/BRB-HTN-Dashboard"

data_name <- "BRB_Hearts_Dashboard_Data.csv"

#Useful code for the future
#controlled_sbp <- data %>% filter(HTN_control == 1) %>% group_by(last_visited_polyclinic) %>% summarize(minimum = min(most_recent_systoic, na.rm = TRUE), mean = mean(most_recent_systoic, na.rm = TRUE), maximum = max(most_recent_systoic, na.rm = TRUE))

# Load libraries
#List of libaries needed
libs <- c("dplyr")

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
data$HTN_uncontrol[data$most_recent_systoic>=140 & data$most_recent_diasystoic>=90]=1
data$HTN_uncontrol[data$most_recent_systoic>=140 & data$most_recent_diasystoic<90]=0
data$HTN_uncontrol[data$most_recent_systoic<140 & data$most_recent_diasystoic>=90]=0
data$HTN_uncontrol[data$most_recent_systoic<140 & data$most_recent_diasystoic<90]=0

# Hypertension Control
data$HTN_control[data$most_recent_systoic>=140 & data$most_recent_diasystoic>=90]=0
data$HTN_control[data$most_recent_systoic>=140 & data$most_recent_diasystoic<90]=0
data$HTN_control[data$most_recent_systoic<140 & data$most_recent_diasystoic>=90]=0
data$HTN_control[data$most_recent_systoic<140 & data$most_recent_diasystoic<90]=1

# Extract year and month
data$year <- format(as.Date(data$last_visited_date, format="%m/%d/%Y"),"%Y")
data$month <- format(as.Date(data$last_visited_date, format="%m/%d/%Y"),"%m")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#### SUMARRY STATISTICS

#-------------------------------------------------------------------------------

#Systolic Blood Pressure

#Create data frame with minimum, mean and maximum BP measures
controlled_sbp <- data %>% filter(HTN_control == 1 & SBP_keep == 1)  %>% summarize(minimum = min(most_recent_systoic, na.rm = TRUE), mean = mean(most_recent_systoic, na.rm = TRUE), maximum = max(most_recent_systoic, na.rm = TRUE))
uncontrolled_sbp <- data %>% filter(HTN_uncontrol == 1 & SBP_keep == 1)  %>% summarize(minimum = min(most_recent_systoic, na.rm = TRUE), mean = mean(most_recent_systoic, na.rm = TRUE), maximum = max(most_recent_systoic, na.rm = TRUE))
ISO_SBP_sbp <- data %>% filter(ISO_SBP == 1 & SBP_keep == 1)  %>% summarize(minimum = min(most_recent_systoic, na.rm = TRUE), mean = mean(most_recent_systoic, na.rm = TRUE), maximum = max(most_recent_systoic, na.rm = TRUE))
ISO_DBP_sbp <- data %>% filter(ISO_DBP == 1 & SBP_keep == 1)  %>% summarize(minimum = min(most_recent_systoic, na.rm = TRUE), mean = mean(most_recent_systoic, na.rm = TRUE), maximum = max(most_recent_systoic, na.rm = TRUE))

controlled_sbp$group <- "Controlled"
uncontrolled_sbp$group <- "Uncontrolled"
ISO_SBP_sbp$group <- "Isolated systolic"
ISO_DBP_sbp$group <- "Isolated diastolic"


#Merge data frames of summary statistics
summary_sbp <- bind_rows(controlled_sbp, uncontrolled_sbp, ISO_SBP_sbp, ISO_DBP_sbp)
summary_sbp$mean = round(summary_sbp$mean, 0)
summary_sbp <- summary_sbp %>% select(group,minimum,mean,maximum)

#-------------------------------------------------------------------------------

#Diastolic Blood Pressure

#Create data frame with minimum, mean and maximum BP measures
controlled_dbp <- data %>% filter(HTN_control == 1 & DBP_keep == 1)  %>% summarize(minimum = min(most_recent_diasystoic, na.rm = TRUE), mean = mean(most_recent_diasystoic, na.rm = TRUE), maximum = max(most_recent_diasystoic, na.rm = TRUE))
uncontrolled_dbp <- data %>% filter(HTN_uncontrol == 1 & DBP_keep == 1)  %>% summarize(minimum = min(most_recent_diasystoic, na.rm = TRUE), mean = mean(most_recent_diasystoic, na.rm = TRUE), maximum = max(most_recent_diasystoic, na.rm = TRUE))
ISO_SBP_dbp <- data %>% filter(ISO_SBP == 1 & DBP_keep == 1)  %>% summarize(minimum = min(most_recent_diasystoic, na.rm = TRUE), mean = mean(most_recent_diasystoic, na.rm = TRUE), maximum = max(most_recent_diasystoic, na.rm = TRUE))
ISO_DBP_dbp <- data %>% filter(ISO_DBP == 1 & DBP_keep == 1)  %>% summarize(minimum = min(most_recent_diasystoic, na.rm = TRUE), mean = mean(most_recent_diasystoic, na.rm = TRUE), maximum = max(most_recent_diasystoic, na.rm = TRUE))

controlled_dbp$group <- "Controlled"
uncontrolled_dbp$group <- "Uncontrolled"
ISO_SBP_dbp$group <- "Isolated systolic"
ISO_DBP_dbp$group <- "Isolated diastolic"


#Merge data frames of summary statistics
summary_dbp <- bind_rows(controlled_dbp, uncontrolled_dbp, ISO_SBP_dbp, ISO_DBP_dbp)
summary_dbp$mean = round(summary_dbp$mean, 0)
summary_dbp <- summary_dbp %>% select(group,minimum,mean,maximum)

#-------------------------------------------------------------------------------

#Merge summary statistics data frames
summary_sbp$BP_type <- "SBP"
summary_dbp$BP_type <- "DBP"

summary_stats <- bind_rows(summary_sbp, summary_dbp)

#Remove excess data frames
rm(controlled_sbp,uncontrolled_sbp,ISO_SBP_sbp,ISO_DBP_sbp,controlled_dbp,uncontrolled_dbp,ISO_SBP_dbp,ISO_DBP_dbp,summary_sbp,summary_dbp)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Create data frame for estimates for prevalence of HEARTS indicators

