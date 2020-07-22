dev.off()
rm(list=ls())
cat("\014")

#----Occupant-Centric Controls Function 5: Preferred Illuminance Setpoint----

# Function for extracting information relating to occupant-centric controls based on:

# H. B. Gunay, W. O'Brien, I. Beausoleil-Morrison, and S. Gilani, 
# "Development and implementation of an adaptive lighting and blinds control algorithm," 
# Build. Environ., vol. 113, pp. 185-199, 2017.

# Part of IEA EBC Annex 79 Subtask 4 Activity 2.2.1

# Author(s): Brodie W. Hobson

# Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)

# Specify the file path of .csv file, must have \\ between levels of directory
file_path <- "roomData.csv"

# Specify date format used in .csv
date_format = "%Y-%m-%d %H:%M"

# Specify the confidence for the upper and lower occupancy profiles
confidence <- 0.9
  
#----Read in data----

# Pass file path information to variable
# Data should be a .csv with dates in the first column followed by light on/off, lux, and motion data for each room in order
# The date data should be in a single, continuous timestep applicable to data
# The column names of the should reflect the room number, or have some identifying information

# Read in data from the specified file path
temp <- read.csv(file = file_path)

# Convert date format to POSIXct
dateStr <- as.POSIXct(temp[,1], format = date_format, tz = "GMT")

# Isolate light keypress, motion detector, and lux readings
dat_light <- temp[,grep('light',colnames(temp))]
dat_md <- temp[,grep('md',colnames(temp))]
dat_lux <- temp[,grep('lux',colnames(temp))]

# Find the timestep of the data for calculation purposes, first and second timesteps must be representative of whole time series
timestep_min <- as.numeric(difftime(dateStr[2],dateStr[1]))

# Create blank vectors to store parameters
threshold <- vector()
keypress_on <- vector()
mdl_fit <- list()

# Create a loop to find the occupied period of each room
for (n in 1:ncol(dat_lux)){
  
  # Create a temporary data frame to store the light keypress, motion detector, and lux readings from a single office for analysis
  dat <- cbind.data.frame(dateStr, dat_md[,n], dat_light[,n], dat_lux[,n])
  colnames(dat) <- c("Time", colnames(dat_md[n]), colnames(dat_light[n]), colnames(dat_lux[n]))
  
  # Find the lux readings at the timesteps when the light are on and the room is occupied
  luxOn <- dat[which(wday(dat[,1]) %in% c(2:6) & dat[,2]==1 & c(dat[-1,3],dat[1,3])==1),4]
  
  # Create a dataframe with the light on (1) for the y-axis and the lux levels
  luxOn <- cbind.data.frame(luxOn, rep(1, length(luxOn)))
  
  # Name the columns so they can be combined vertically
  colnames(luxOn) <- c("lux","binary")
  
  # Find the lux readings when the lights are off and the room is occupied
  luxOff <- dat[which(wday(dat[,1]) %in% c(2:6) & dat[,2]==1 & c(dat[-1,3],dat[1,3])==0 & dat[,4]!=0),4]
  
  # Create a dataframe with the light off (0) for the y-axis and the lux levels
  luxOff <- cbind.data.frame(luxOff, rep(0, length(luxOff)))
  
  # Name the columns so they can be combined vertically
  colnames(luxOff) <- c("lux","binary")
  
  # Combine the columns vertically so they can be plotted
  temp_dat <- rbind.data.frame(luxOn,luxOff)
  
  # Create a binomial logistic regression model using the data
  glm_mdl <- glm(binary~., family = binomial(link='logit'), data = temp_dat)
  
  # Fit the data to the logistic regression model to get the shape of the curve
  mdl_fit[[length(mdl_fit)+1]] <- predict(glm_mdl, newdata = data.frame(lux = c(0:1000)), type = "response")
  
  keypress_on[length(keypress_on)+1] <- round(nrow(luxOn)/sum(nrow(luxOn)+nrow(luxOff)), digits = 2)
    
  # Find the quantile for lux measurements when light is turned on if always above the confidence threshold
  
  if(is.na(mdl_fit[which(mdl_fit[,2] >= confidence)[1],1])){
    
    threshold[length(threshold)+1] <- RoundTo(which(unlist(mdl_fit[n]) <= 0.1)[1]-1, multiple = 10, FUN = 'round')
    
  }else{
    
    threshold[length(threshold)+1] <- RoundTo(mdl_fit[which(mdl_fit[,2] >= confidence)[1],1], multiple = 10, FUN = round)
    
  }
    
}

# Store results from binomial logistic regression model for plotting
light_glm_forPlot <- as.data.frame(matrix(unlist(mdl_fit), ncol = ncol(dat_lux), byrow = F))
colnames(light_glm_forPlot) <- colnames(dat_lux)
write.csv(light_glm_forPlot,"light_glm.csv", row.names = F)

# Bind together results from each room into a single dataframe
results <- rbind.data.frame(threshold,keypress_on)
rownames(results) <- c("Light Off","Keypresses")
colnames(results) <- colnames(dat_light)

# Output a .csv containing lighting thresholds for each office
write.csv(results, "OCC5.csv")