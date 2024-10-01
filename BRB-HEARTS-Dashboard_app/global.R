
### SCRIPT METADATA ####

# program name :        brb_hearts_001
# project:              Barbados HEARTS Dashboard
# analyst:              Kern ROCKE
# date create:          24 Sept 24
# date modified:        30 Sept 24
# task:                 Create outputs for HEARTS Dashboard

# Set working directory
dir <- "/Users/kernrocke/Library/Mobile Documents/com~apple~CloudDocs/Github/BRB-HTN-Dashboard"
#dir <- "/home/kernrocke/Documents/Github/BRB-HTN-Dashboard"

data_name <- "BRB_Hearts_Dashboard_Data.csv"

#Useful code for the future
#controlled_sbp <- data %>% filter(HTN_control == 1) %>% group_by(last_visited_polyclinic) %>% summarize(minimum = min(most_recent_systoic, na.rm = TRUE), mean = mean(most_recent_systoic, na.rm = TRUE), maximum = max(most_recent_systoic, na.rm = TRUE))

# Load libraries
#List of libaries needed
libs <- c("dplyr", "ggplot2", "janitor")

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

#Remove empty rows of dtaa
data <- data[!is.na(data$encounter_id), ]

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

data$month[data$month=="01" ]="Jan"
data$month[data$month=="02" ]="Feb"
data$month[data$month=="03" ]="Mar"
data$month[data$month=="04" ]="Apr"
data$month[data$month=="05" ]="May"
data$month[data$month=="06" ]="Jun"
data$month[data$month=="07" ]="Jul"
data$month[data$month=="08" ]="Aug"
data$month[data$month=="09" ]="Sep"
data$month[data$month=="10" ]="Oct"
data$month[data$month=="11" ]="Nov"
data$month[data$month=="12" ]="Dec"

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


# Create data frames with 2023 and 2024 data

data$HTN_control[data$HTN_control=="1" ]="Conrol"
data$HTN_control[data$HTN_control=="0" ]="Unconrolled"

data_2023 <- data[data$year == 2023, ]
data_2024 <- data[data$year == 2024, ]



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Graph Preparations

# Create data frame for estimates for prevalence of HEARTS indicators
# Group by month and calculate counts for each HTN control categories
htn_control_counts <- data_2023 %>%
  filter(!is.na(HTN_control) & !is.na(month)) %>%
  group_by(month, HTN_control) %>%
  summarize(count = n())

# Calculate total count for each month
total_counts <- htn_control_counts %>%
  group_by(month) %>%
  summarize(total = sum(count))

# Join total counts with original data and calculate percentages
htn_control_percentages <- left_join(htn_control_counts, total_counts, by = "month") %>%
  mutate(percentage = round((count / total) * 100, 2))

#-------------------------------------------------------------------------------

# Create data frame for estimates for prevalence of HEARTS indicators by 
# Group by polyclinic and calculate counts for each HTN control categories
htn_control_poly <- data_2023 %>%
  filter(!is.na(HTN_control) & !is.na(month)) %>%
  group_by(last_visited_polyclinic, HTN_control) %>%
  summarize(count = n())

# Calculate total count for each month
total_poly <- htn_control_poly %>%
  group_by(last_visited_polyclinic) %>%
  summarize(total = sum(count))

# Join total counts with original data and calculate percentages
htn_control_poly_per <- left_join(htn_control_poly, total_poly, by = "last_visited_polyclinic") %>%
  mutate(percentage = round((count / total) * 100, 2))


# Create data frame for estimates for patient counts
# Group by month and calculate counts 
patient_counts <- data_2023 %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarize(count = n())


#-------------------------------------------------------------------------------

# GRAPH Outputs

# First plot of HTN Control 
htn_control_percentages$month <- factor(htn_control_percentages$month, levels = c("Jan", "Feb", "Mar", "Apr", 
                                                                                  "May", "Jun", "Jul", "Aug", 
                                                                                  "Sep", "Oct", "Nov", "Dec"))

# Second plot of HTN Control 
htn_control_poly_pers$last_visited_polyclinic <- factor(htn_control_poly_pers$last_visited_polyclinic, levels = c("Bradford Taitt", "David Thompshon", "Edgar Cochrane", "Eunice Gibson", 
                                                                                                                  "Fredrick Miler", "General Practice", "Horse Hill", "Maurice Byer", 
                                                                                                                  "Randall Phillips", "St.Andrew Outpatient", "St.Phillip", "Winston Scott"))
# Third plot of HTN Control 
patient_counts$month <- factor(patient_counts$month, levels = c("Jan", "Feb", "Mar", "Apr", 
                                                                "May", "Jun", "Jul", "Aug", 
                                                                "Sep", "Oct", "Nov", "Dec"))

# Graph 1 - Hypertension Control Rates by Month
ggplot(htn_control_percentages, aes(x = month, y = percentage, fill = HTN_control)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Hypertension Control Rates by Month", x = "Month", y = "Percentage (%)") +
  guides(fill=guide_legend(title=" Hypertension Control")) +
  scale_y_continuous(breaks=seq(0, 60, by = 5), limits = c(0, 55)) +
  scale_fill_manual(values = c("#2ca25f","#e34a33")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.line.x = element_line(colour = "black")) +
  theme(axis.line.y = element_line(colour = "black"))


# Graph 2 - Hypertension Control Rates by Polyclinic
ggplot(htn_control_poly_per, aes(x = last_visited_polyclinic, y = percentage, fill = HTN_control)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Hypertension Control Rates by Polyclinics", x = "Polyclinic", y = "Percentage (%)") +
  guides(fill=guide_legend(title=" Hypertension Control")) +
  scale_y_continuous(breaks=seq(0, 65, by = 5), limits = c(0, 65)) +
  scale_fill_manual(values = c("#2ca25f","#e34a33")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.line.x = element_line(colour = "black")) +
  theme(axis.line.y = element_line(colour = "black"))

# Graph 3 - Patient counts
ggplot(patient_counts, aes(x = month, y = count)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#3182bd" ) +
  geom_text(aes(label=count), vjust=-0.3, size=3.5) +
  labs(title = "Patient Coverage by Month", x = "Month", y = "Number of Patients") +
  scale_y_continuous(breaks=seq(0, 7000, by = 1000), limits = c(0, 7000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.line.x = element_line(colour = "black")) +
  theme(axis.line.y = element_line(colour = "black"))

  
