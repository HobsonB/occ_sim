dev.off()
rm(list=ls())
cat("\014")

#----Occupant-Centric Controls Function 2: Building Level Occupancy Profiles----

# Function for extracting information relating to occupant-centric controls based on:

# X. Feng, D. Yan, and T. Hong, 
# "Simulation of occupancy in buildings," 
# Energy Build., vol. 87, no. January, pp. 348-359, 2015.

# Part of IEA EBC Annex 79 Subtask 4 Activity 2.2.1

# Author(s): Brodie W. Hobson

# Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)

# Specify the file path of .csv file, must have \\ between levels of directory
file_path <- "roomData.csv"

# Specify date format used in .csv
date_format = "%Y-%m-%d %H:%M"

# Specify the confidence for the upper and lower occupancy profiles
confidence <- 0.9

#----Read in data----

# Pass file path information to variable
# Data should be a .csv with dates in the first column and motion detector data in the following columns
# The date data should be in a single, continuous timestep applicable to all motion detectors
# The column names of the motion detector data should reflect the room number, or have some identifying information

# Read in data from the specified file path
temp <- read.csv(file = file_path)

# Convert date format to POSIXct
dateDat <- cbind.data.frame(as.POSIXct(temp[,1], format = date_format, tz = "GMT"))

wdayIndex <- wday(levels(cut(dateDat[,1], breaks = "1 day")))

# Find the timestep of the data for calculation purposes, first and second timesteps must be representative of whole time series
timestep_min <- minute(dateDat[2,1])-minute(dateDat[1,1])

# Find building level motion detector activations
allRooms <- cbind.data.frame(temp[,grep('md',colnames(temp))])

# Create a loop to find the occupied period of each room

# Create an empty list of weekly schedules to be populated for each of the 10 combinations of 9 random occupants
weekly_sch <- list()

for(k in 1:10){
  
  # Pseudo-random number generation
  set.seed(k)
  sample_ind <- sort(sample(c(1:29),9))
  
  # Bind sample data to a single dataframe
  dat <- cbind.data.frame(dateDat,allRooms[,sample_ind])
  
  colnames(dat) <- c("Time",colnames(allRooms[,sample_ind]))
  
  # Create empty vector to store each buildings schedule
  bldg_sch <- rep(0,nrow(dateDat))
  
  for (n in 2:10){
    
    #----Subset the data into each day----
    
    # Create a blank list to store a subset of data for each day
    temp_zone <- list()
    
    # Loop for each year, from first observed to last observed
    for (i in c(year(dat[1,1]):year(dat[nrow(dat),1]))){
      
      # Loop for each day of the year, from first observed to last observed
      for (j in c(yday(dat[which(year(dat[,1])==i)[1],1]):yday(dat[max(which(year(dat[,1])==i)),1]))){
        
        # Create a subset for each day of each year
        temp <- subset(dat[,n], (yday(dat[,1]) == j) & (year(dat[,1]) == i))
        
        #----Arrival and Departure Distributions----
        
        trig <- which(temp!=0)
        
        if(length(which(diff(trig, lag = 4)==4))>2){
          
          # Isolate timesteps where motion detector is triggered
          trig <- which(temp!=0)
          
          # First arrival time is the first time the zone is occupied for two 15-minute timesteps consecutively
          arrive <- as.numeric(trig[which(diff(trig, lag = 2)==2)[1]])
          
          # Last departure time in the last time the zone is occupied for two 15-minute timesteps consecutively
          depart <- as.numeric(trig[which(diff(trig, lag = 2)==2)[length(which(diff(trig, lag = 2)==2))]+2])
          
          temp_zone[[length(temp_zone)+1]] <- c(rep(0,arrive),rep(1,depart-arrive),rep(0,96-depart))
          
        }else{
          
          # If no one arrives or departs, record as NA
          temp_zone[[length(temp_zone)+1]] <- rep(0,96)
          
        }
        
      }
    }
    rm(i,j)
    
    temp_zone <- unlist(temp_zone)
    
    bldg_sch <- bldg_sch + temp_zone
    
  }
  
  bldg_sch <- matrix(bldg_sch, ncol = length(wdayIndex), byrow = F)
  
  temp <- list()
  
  # Determine occupancy during each weekday at each timestep at desired confidence interval
  for(m in c(2:6)){
  
    temp[[length(temp)+1]] <- floor(rowQuantiles(bldg_sch[,which(wdayIndex==m)], probs = confidence))
    
  }
  
  weekly_sch[[length(weekly_sch)+1]] <- unlist(temp)

  
}
  
weekly_sch <- as.data.frame(matrix(unlist(weekly_sch), ncol = k, byrow = F))

colnames(weekly_sch) <- c(1:k)

# Output a .csv containing each of the 10 weekly schedules 
write.csv(weekly_sch,"weekly_sch.csv",row.names = F)