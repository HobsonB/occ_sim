dev.off()
rm(list=ls())
cat("\014")

#----Occupant-Centric Controls Function 4: Preferred Temperature Setpoint----

# Function for extracting information relating to occupant-centric controls based on:

# H. B. Gunay, W. O'Brien, I. Beausoleil-Morrison, and J. Bursill, 
# "Development and implementation of a thermostat learning algorithm," 
# Sci. Technol. Built Environ., vol. 24, no. 1, pp. 43-56, 2018.

# Part of IEA EBC Annex 79 Subtask 4 Activity 2.2.1

# Author(s): Brodie W. Hobson

# Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)
if ("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
require(plyr)

# Specify the file path of .csv file, must have \\ between levels of directory
file_path <- "roomData.csv"

# Specify date format used in .csv
date_format = "%Y-%m-%d %H:%M"

# Specify the confidence for the upper and lower temperature setpoints
confidence <- 0.9

#----Read in data----

# Pass file path information to variable
# Data should be a .csv with dates in the first column followed by motion data, temperature, and keypress for each room in order
# The date data should be in a single, continuous timestep applicable to data
# The column names of the should reflect the room number, or have some identifying information

# Read in data from the specified file path
temp <- read.csv(file = file_path)

# Convert date format to POSIXct
dateStr <- as.POSIXct(temp[,1], format = date_format, tz = "GMT")

# Isolate outdoor air temperature, room temperature, and keypresses
dat_Tin <- temp[,grep('temp',colnames(temp))]
dat_md <- temp[,grep('md',colnames(temp))]
dat_key <- temp[,grep('key',colnames(temp))]
dat_Tout <- temp[,2]

# Find the timestep of the data for calculation purposes, first and second timesteps must be representative of whole time series
timestep_min <- as.numeric(difftime(dateStr[2],dateStr[1]))

# Create a blank list to store metrics (i.e., temperatures, keypresses)
threshold_up <- vector()
threshold_down <- vector()
num_keypress_up <- vector()
num_keypress_down <- vector()

threshold_cool <- vector()
threshold_heat <- vector()
num_keypress_cool <- vector()
num_keypress_heat <- vector()

# Create a loop for each room

for (n in 1:ncol(dat_key)){
  
  #----Heating Seasons----
  
  # Create a temporary data frame to store the keypress, temperature, and outdoor temperature from a single office for analysis
  dat <- cbind.data.frame(dateStr, dat_md[,n], dat_key[,n], dat_Tin[,n], dat_Tout)
  colnames(dat) <- c("Time", colnames(dat_md[n]), colnames(dat_key[n]), colnames(dat_Tin[n]), "Tout")
  
  # Find the temperature readings at the timestep when the thermostat is pressed up
  keypress_up <- RoundTo(dat[which(wday(dat[,1]) %in% c(2:6) & dat[,3] == 4 & dat[,5] <13),4], multiple = 0.5, FUN = round)
  
  # Find the number of times the thermostat is pressed up
  num_keypress_up[length(num_keypress_up)+1] <- as.integer(length(keypress_up))
  
  # If the thermostat has less than 10 interactions, take the preferred temperature as the mean of the temperature during occupied period
  if(length(keypress_up)<10){
    
    keypress_up <- RoundTo(dat[which(wday(dat[,1]) %in% c(2:6) & dat[,2] == 1 & dat[,5] <13),4], multiple = 0.5, FUN = round)
    
  }
  
  # Record the temperature above which heating is not requested, using the confidence interval
  threshold_up[length(threshold_up)+1] <- Quantile(keypress_up, probs = 1-confidence)

  # Find the temperature readings at the timestep when the thermostat is pressed down
  keypress_cool <- RoundTo(dat[which(wday(dat[,1]) %in% c(2:6) & dat[,3] == 3 & dat[,5] <13),4], multiple = 0.5, FUN = round)
  
  # Find the number of times the thermostat is pressed down
  num_keypress_cool[length(num_keypress_cool)+1] <- as.integer(length(keypress_cool))
  
  # If the thermostat has less than 10 interactions, take the preferred temperature as the mean of the temperature during occupied period
  if(length(keypress_cool)<10){
    
    keypress_cool <- RoundTo(dat[which(wday(dat[,1]) %in% c(2:6) & dat[,2] == 1 & dat[,5] <13),4], multiple = 0.5, FUN = round)
    
  }
  
  # Record the temperature above which heating is not requested, using the confidence interval
  threshold_cool[length(threshold_cool)+1] <- Quantile(keypress_cool, probs = 1-confidence)
  
  #----Cooling Seasons----

  # Find the temperature readings at the timestep when the thermostat is pressed down
  keypress_down <- RoundTo(dat[which(wday(dat[,1]) %in% c(2,6) & dat[,3] == 3 & dat[,5] >=13),4], multiple = 0.5, FUN = round)
  
  # Find the number of times the thermostat is pressed down
  num_keypress_down[length(num_keypress_down)+1] <- as.integer(length(keypress_down))
  
  # If the thermostat has less than 10 interactions, take the preferred temperature as the mean of the temperature during occupied period
  if(length(keypress_down)<10){
    
    keypress_down <- RoundTo(dat[which(wday(dat[,1]) %in% c(2,6) & dat[,2] == 1 & dat[,5] >=13),4], multiple = 0.5, FUN = round)
    
  }
  
  # Record the temperature below which cooling is not requested, using the confidence interval
  threshold_down[length(threshold_down)+1] <- Quantile(keypress_down, probs = confidence)

  #Find the temperature readings at the timestep when the thermostat is pressed down
  keypress_heat <- RoundTo(dat[which(wday(dat[,1]) %in% c(2,6) & dat[,3] == 4 & dat[,5] >=13),4], multiple = 0.5, FUN = round)
  
  #Find the number of times the thermostat is pressed down
  num_keypress_heat[length(num_keypress_heat)+1] <- as.integer(length(keypress_heat))
  
  # If the thermostat has less than 10 interactions, take the preferred temperature as the mean of the temperature during occupied period
  if(length(keypress_heat)<10){
    
    keypress_heat <- RoundTo(dat[which(wday(dat[,1]) %in% c(2,6) & dat[,2] == 1 & dat[,5] >=13),4], multiple = 0.5, FUN = round)
    
  }
  
  # Record the temperature below which cooling is not requested, using the confidence interval
  threshold_heat[length(threshold_heat)+1] <- Quantile(keypress_heat, probs = confidence)
  
}

#----Quick summary statistics----

# Find the number of hours each office is occupied
occ_hours <- as.vector(colSums(dat_md)/4)

# Find the number of keypresses in each office
keypress_tot <- colSums(dat_key)

# Find the mean duration between keypresses
duration <- occ_hours/keypress_tot
duration <- duration[which(!is.infinite(duration))]
mean(duration)

# Bind results together into a single dataframe
results <- rbind.data.frame(RoundTo(threshold_up, multiple = 0.5, FUN = round),as.integer(num_keypress_up),
                            RoundTo(threshold_down, multiple = 0.5, FUN = round),as.integer(num_keypress_down),
                            RoundTo(threshold_cool, multiple = 0.5, FUN = round),as.integer(num_keypress_cool),
                            RoundTo(threshold_heat, multiple = 0.5, FUN = round),as.integer(num_keypress_heat))

rownames(results) <- c("Heating Season","Keypress Up","Cooling Season","Keypress Down","Heating","Down","Cooling","Up")
colnames(results) <- colnames(dat_md)

# Output a .csv containing the keypress and temperature data
write.csv(results, "OCC4.csv")