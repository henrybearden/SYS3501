# Name: Henry Bearden
# Computing ID: zuf3ys
# LLM tool used: ChatGPT


#------------------------------------------------------#
#----------------------- Task 1 -----------------------#
#------------------------------------------------------#
#PROMPT: Generate code to set the working directory to "~/Desktop/SYS3501" 
#and set up my data folder called "~/Desktop/SYS3501/Data/TrainData"

# CODE
# Set the working directory
setwd("~/Desktop/SYS3501")

# Define the data folder path
data_folder <- "~/Desktop/SYS3501/Data/TrainData"

# Verify the working directory and data folder path
cat("Current working directory:", getwd(), "\n")
cat("Data folder path:", data_folder, "\n")

# Prompt 1.2: Load and aggregate the data in all CSV files
#from my data folder and store it in "aggregated_data"

# CODE

# Load required library
library(dplyr)
library(readr)

# Get list of all CSV files in the data folder
csv_files <- list.files(path = "~/Desktop/SYS3501/Data/TrainData", 
                        pattern = "*.csv", 
                        full.names = TRUE)

# Read and combine all CSV files
aggregated_data <- csv_files %>%
  lapply(function(file) {
    read_csv(file, col_types = cols(.default = "c"))  # Convert all columns to character
  }) %>%
  bind_rows()

# Print the dimensions of the aggregated data
print(paste("Aggregated data dimensions:", 
            nrow(aggregated_data), "rows,", 
            ncol(aggregated_data), "columns"))


#------------------------------------------------------#
#----------------------- Task 2 -----------------------#
#------------------------------------------------------#
# Prompt 2: Generate code to check the shape of aggregated_data and 
# neatly print the column names and data types for each column of 
# aggregated_data


# Check the shape of aggregated_data
cat("Shape of aggregated_data:", dim(aggregated_data), "\n\n")

# Print column names and data types neatly
cat("Column names and data types:\n")
column_info <- data.frame(
  Column = names(aggregated_data),
  Type = sapply(aggregated_data, class)
)
print(column_info, row.names = FALSE)


#------------------------------------------------------#
#----------------------- Task 3 -----------------------#
#------------------------------------------------------#
# Prompt 3.1: Generate code to display the data types for the contents of the 
# TIMEHR, TIMEMIN, AMPM, and CASINJ columns in aggregated_data.

# Print the dimensions of the aggregated data
cat("Dimensions of aggregated data:", dim(aggregated_data), "\n")

# Print column names
cat("Column names:\n")
print(names(aggregated_data))

# Print data types for specific columns
columns_to_check <- c("TIMEHR", "TIMEMIN", "AMPM", "CASINJ")
sapply(aggregated_data[columns_to_check], class)

# Prompt 3.2: Plot the CASINJ over 24h, using the time information 
#included in the TIMEHR, TIMEMIN, and AMPM (AM or PM) columns.
# Be sure to ensure that CASINJ, TIMEHR, and TIMEMIN are integers first.

# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Convert TIMEHR, TIMEMIN, and CASINJ from character to integer
aggregated_data <- aggregated_data %>%
  mutate(
    TIMEHR = as.integer(TIMEHR),
    TIMEMIN = as.integer(TIMEMIN),
    CASINJ = as.integer(CASINJ)
  )

# Step 2: Create a 24-hour time format
aggregated_data <- aggregated_data %>%
  mutate(
    # Convert 12-hour TIMEHR to 24-hour format based on AMPM
    TIMEHR_24 = ifelse(AMPM == "PM" & TIMEHR != 12, TIMEHR + 12, TIMEHR),
    TIMEHR_24 = ifelse(AMPM == "AM" & TIMEHR == 12, 0, TIMEHR_24),
    
    # Combine TIMEHR and TIMEMIN into a decimal hour format for plotting
    TIME_DECIMAL = TIMEHR_24 + TIMEMIN / 60
  )

# Step 3: Plot CASINJ over 24-hour time
ggplot(aggregated_data, aes(x = TIME_DECIMAL, y = CASINJ)) +
  geom_point() +
  labs(
    title = "CASINJ Over 24-Hour Time",
    x = "Time (24-hour format)",
    y = "CASINJ"
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +  # Set x-axis to show full hours
  theme_minimal()

# Prompt 3.3: I want to visualize total CASINJ by hour. Extract the hour, 
# calculate the hourly sum of CASINJ, and plot CASINJ by hour

# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Convert TIMEHR, TIMEMIN, and CASINJ from character to integer (if not already done)
aggregated_data <- aggregated_data %>%
  mutate(
    TIMEHR = as.integer(TIMEHR),
    TIMEMIN = as.integer(TIMEMIN),
    CASINJ = as.integer(CASINJ)
  )

# Step 2: Create a 24-hour time format (if not already done)
aggregated_data <- aggregated_data %>%
  mutate(
    # Convert 12-hour TIMEHR to 24-hour format based on AMPM
    TIMEHR_24 = ifelse(AMPM == "PM" & TIMEHR != 12, TIMEHR + 12, TIMEHR),
    TIMEHR_24 = ifelse(AMPM == "AM" & TIMEHR == 12, 0, TIMEHR_24)
  )

# Step 3: Summarize total CASINJ by hour
hourly_summary <- aggregated_data %>%
  group_by(TIMEHR_24) %>%
  summarize(total_CASINJ = sum(CASINJ, na.rm = TRUE))

# Step 4: Plot total CASINJ by hour
ggplot(hourly_summary, aes(x = TIMEHR_24, y = total_CASINJ)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total CASINJ by Hour",
    x = "Hour (24-hour format)",
    y = "Total CASINJ"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Set x-axis to show hours
  theme_minimal()
getwd()

# EXTRA CREDIT ASSIGNMENT

# The goal is to gain insights about the correlation between position of the 
# first car involved (POSITON1) and total accident damage (ACCDMG).

# Total damage (ACCDMG) was plotted against the position of the 
# first train involved in a given accident, excluding positions greater than 300
# to improve data clarity and visualization.

# As Demonstrated by the scatter plot, the accidents with higher total damage 
# consistently have cars first involved earlier on the train. The top 20 most 
# costly accidents occurred when the first car involved was in the first 
# 5 cars on the train. 

# Through further analysis of this data, the FRA could develop responses to 
# this correlation that limit the costly effects of train accidents. 
# For example, developing improved braking systems and impact bracing 
# technology could help protect cars at the front of the train by targeting 
# the potential situations that make them most vulnerable to accidents.

# Load required libraries
library(ggplot2)
library(dplyr)

# Assuming 'aggregated_data' contains the relevant columns
# Convert POSITON1 and ACCDMG to numeric if they are not already
aggregated_data <- aggregated_data %>%
  mutate(
    POSITON1 = as.numeric(POSITON1),
    ACCDMG = as.numeric(ACCDMG)
  )

# Filter out rows where POSITON1 is greater than 300
filtered_data <- aggregated_data %>%
  filter(POSITON1 <= 300)

# Plot scatter plot of POSITON1 vs ACCDMG
ggplot(filtered_data, aes(x = POSITON1, y = ACCDMG)) +
  geom_point(color = "darkred") +
  labs(
    title = "Car Position in Train (POSITON1) vs Total Damage (ACCDMG)",
    x = "Car Position in Train (POSITON1)",
    y = "Total Damage (ACCDMG)"
  ) +
  theme_minimal()

# End of assigment

