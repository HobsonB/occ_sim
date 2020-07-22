dev.off()
rm(list=ls())
cat("\014")

#----Occupant-Centric Controls Function 3: Zone Level First/Last Arrival and Last Departure----

# Function for extracting information relating to occupant-centric controls based on:

# H. B. Gunay, W. O'Brien, I. Beausoleil-Morrison, W. Shen, G. R. Newsham, and I. Macdonald, 
# "The effect of zone level occupancy characteristics on adaptive controls," 
# Build. Simul., no. August, pp. 949-957, 2017.

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

# Specify the confidence for the arrival and departure times
confidence <- 0.9

#----Read in data----

# Pass file path information to variable
# Data should be a .csv with dates in the first column and motion detector data in the following columns
# The date data should be in a single, continuous timestep applicable to all motion detectors
# The column names of the motion detector data should reflect the room number, or have some identifying information
  
# Read in data from the specified file path
temp <- read.csv(file = file_path)

# Convert date format to POSIXct
dat <- cbind.data.frame(as.POSIXct(temp[,1], format = date_format, tz = "GMT"))

# Isolate motion detector data type
dat <- cbind.data.frame(dat, temp[,grep('md',colnames(temp))])

# Find the timestep of the data for calculation purposes, first and second timesteps must be representative of whole time series
timestep_min <- minute(dat[2,1])-minute(dat[1,1])

# Create a blank list to store time metrics (i.e., earliest arrival, latest arrival, latest departure)
tim_metrics <- list() 
arrive_cdf_forPlot <- list()
depart_cdf_forPlot <- list()

# Loop for all columns of data (minus timestamp)
for(n in 2:ncol(dat)){
  
  #----Subset the data into each day----
  
  # Create a blank list to store a subset of data for each day
  arrive <- vector()
  depart <- vector()
  
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
        arrive[length(arrive)+1] <- as.numeric(trig[which(diff(trig, lag = 2)==2)[1]])
        
        # First arrival time is the first time the zone is occupied for two 15-minute timesteps consecutively
        depart[length(depart)+1] <- as.numeric(trig[which(diff(trig, lag = 2)==2)[length(which(diff(trig, lag = 2)==2))]+2])
        
      }else{
        
        # If no one arrives or departs, record as NA
        arrive[length(arrive)+1] <- NA
        
        depart[length(depart)+1] <- NA
        
      }
      
    }
  }
  rm(i,j)
  
  # Create index of workdays to remove weekends
  wdayIndex <- wday(levels(cut(dat[,1], breaks = "1 day")))
  
  # Convert the arrival times into number between 0 and 24
  arrive <- arrive[which(wdayIndex %in% c(2:6))]*timestep_min/60
  
  # Convert the departure times into number between 0 and 24
  depart <- depart[which(wdayIndex %in% c(2:6))]*timestep_min/60
  
  # Record fraction of days that are occupied for each office
  occ_wday <- sum(!is.na(arrive))/length(arrive)
  
  #----Arrival and Departure Time Extraction----
  
  # Find the normalized cumulative probability distribution for arrival
  arrive_cdf <- cumsum(hist(arrive, breaks = seq(from = 0, to = 24, by = timestep_min/60), plot = F)$counts)/length(arrive)
  
  arrive_cdf_forPlot[[length(arrive_cdf_forPlot)+1]] <- arrive_cdf
  
  # Find the normalized cumulative probability distribution for departure
  depart_cdf <- cumsum(hist(depart, breaks = seq(from = 0, to = 24, by = timestep_min/60), plot = F)$counts)/sum(!is.na(depart))
  
  depart_cdf_forPlot[[length(depart_cdf_forPlot)+1]] <- depart_cdf
  
  # Find the earliest arrivial time
  arrive_erly <- seconds_to_period((((which(arrive_cdf >= 1-confidence)[1]))*timestep_min/60)*3600)
  arrive_erly <- sprintf('%02d:%02d', arrive_erly@hour, minute(arrive_erly))
  
  # Find the latest expected arrival
  arrive_lat <- seconds_to_period((which(arrive_cdf*length(arrive)/max(arrive_cdf*length(arrive))>=confidence)[1]*timestep_min/60)*3600)
  arrive_lat <- sprintf('%02d:%02d', arrive_lat@hour, minute(arrive_lat))
  
  # Find the latest expected departure
  depart_lat <- seconds_to_period((which(depart_cdf > confidence)[1]*timestep_min/60)*3600)
  depart_lat <- sprintf('%02d:%02d', depart_lat@hour, minute(depart_lat))
  
  # Store these metrics for each room in a list
  tim_metrics[[length(tim_metrics)+1]] <- c(occ_wday, arrive_erly,arrive_lat,depart_lat)
}

# Output a .csv containing the arrival cumulative distribution function for plotting
arrive_cdf_forPlot <- as.data.frame(matrix(unlist(arrive_cdf_forPlot), ncol = ncol(dat)-1, byrow = F))
colnames(arrive_cdf_forPlot) <- colnames(dat)[2:length(colnames(dat))]
write.csv(arrive_cdf_forPlot,"arrive_cdf.csv", row.names = F)

# Output a .csv containing the departure cumulative distribution function for plotting
depart_cdf_forPlot <- as.data.frame(matrix(unlist(depart_cdf_forPlot), ncol = ncol(dat)-1, byrow = F))
colnames(depart_cdf_forPlot) <- colnames(dat)[2:length(colnames(dat))]
write.csv(depart_cdf_forPlot,"depart_cdf.csv", row.names = F)

# Output a .csv containing the earliest/latest arrival and latest departure for each of the 10 random combinations of 9 different occupants
tim_metrics <- as.data.frame(matrix(unlist(tim_metrics), ncol = length(tim_metrics), byrow = F))
colnames(tim_metrics) <- colnames(dat)[2:length(colnames(dat))]
rownames(tim_metrics) <- c("Occupied Fraction", "Earliest Arrival","Latest Arrival","Latest Departure")
write.csv(tim_metrics,"arrive_depart_lim.csv")