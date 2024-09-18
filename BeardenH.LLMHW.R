# Name: Henry Bearden
# Computing ID: zuf3ys
# LLM tool used: http://172.29.39.82:8082 Coding Assistant


#------------------------------------------------------#
#----------------------- Task 1 -----------------------#
#------------------------------------------------------#
#PROMPT: Generate code to set the working directory to "~/Desktop/SYS Prog" 
#and set up my data folder called "~/Desktop/SYS Prog/Data/TrainData"

# CODE
# Set the working directory
setwd("~/Desktop/SYS Prog")

# Define the data folder path
data_folder <- "~/Desktop/SYS Prog/Data/TrainData"

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
csv_files <- list.files(path = "~/Desktop/SYS Prog/Data/TrainData", 
                        pattern = "*.csv", 
                        full.names = TRUE)

# Read and combine all CSV files
aggregated_data <- csv_files %>%
  lapply(read_csv) %>%
  bind_rows()

# Print the dimensions of the aggregated data
print(paste("Aggregated data dimensions:", 
            nrow(aggregated_data), "rows,", 
            ncol(aggregated_data), "columns"))


#------------------------------------------------------#
#----------------------- Task 2 -----------------------#
#------------------------------------------------------#
# Prompt 2: Generate code to check the shape of aggregated_data and 
#neatly print the column names and data types for each column of aggregated-data


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
# Prompt 3.1: Generate code to show the data types of the TIMEHR, TIMEMIN, AMPM, 
# and CASINJ columns in aggregated_data.

# Print the dimensions of the aggregated data
cat("Dimensions of aggregated data:", dim(aggregated_data), "\n")

# Print column names
cat("Column names:\n")
print(names(aggregated_data))

# Print data types for specific columns
cat("\nData types for specific columns:\n")
print(sapply(aggregated_data[c("TIMEHR", "TIMEMIN", "AMPM", "#", "CASINJ")], class))

# ... existing code ...

# Prompt 3.2: The time information are in TIMEHR (integer), TIMEMIN (integer), 
# and AMPM (character: AM or PM) columns. Plot the CASINJ (integer) over 24h 
# time in R.

# Load neccessary libraries 
library(dplyr)
library(ggplot2)

# Create the TIME_24HR variable in one step
aggregated_data <- aggregated_data %>%
  mutate(TIME_24HR = as.POSIXct(sprintf("%02d:%02d %s", TIMEHR, TIMEMIN, AMPM), format="%I:%M %p"))

# Plot
ggplot(aggregated_data, aes(x = TIME_24HR, y = CASINJ)) +
  geom_line() +
  labs(title = "CASINJ over 24-hour Time",
       x = "Time (24-hour format)",
       y = "CASINJ") +
  theme_minimal()

# Prompt 3.3: The plot is not what I expected. What if I want to visualize total CASINJ by hour?

# Extract the hour and calculate the sum of CASINJ by hour
aggregated_data_by_hour <- aggregated_data %>%
  mutate(HOUR = format(TIME_24HR, "%H")) %>%  # Extract hour as a new column
  group_by(HOUR) %>%
  summarize(total_CASINJ = sum(CASINJ, na.rm = TRUE))  # Sum CASINJ for each hour

# Plot the total CASINJ by hour
ggplot(aggregated_data_by_hour, aes(x = as.numeric(HOUR), y = total_CASINJ)) +
  geom_line() +
  geom_point() +  # Optionally add points to show specific values at each hour
  labs(title = "Total CASINJ by Hour",
       x = "Hour of the Day",
       y = "Total CASINJ") +
  scale_x_continuous(breaks = 0:23) +  # Show all hours from 0 to 23
  theme_minimal()


getwd()