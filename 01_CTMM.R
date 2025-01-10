# Author: Abigail Turnlund & Tatiana Proboste- adapted from Yang et al.(2023) & Wilber et al.(2021)
# date: 31/10/23

#Libraries
library(tidyverse)
library(ctmm)
library(data.table)
library(parallel)
library(dplyr)
library(readr)

# Load Data

dat <- readRDS("data.rds") %>%
  mutate(Year:= as.numeric(format(dtg_utc, '%Y'))) %>%
  mutate(Year=as.numeric(Year)) %>%
  dplyr::select(ID, population, yr, datetime, Longitude, Latitude)

dat <- data.frame(dat)
dat$ID <- as.character(dat$ID)


#  CTMM -------------------------------------------------------------------
#Creates a log you can check progress with when ctmm function is running
sink("ctmm.log")

## Convert each pig trajectory to 5 minute CTMM trajectory using the ctmm package
## Save the predictions from CTMM on 5 minute scale

# This creates the function we will use on our data ~ < 1 minute
### Added comments on the right side of code on lines that need to be tailored to data
fit_ctmm_model = function(id, dat, interp_vals){
  
  ## Fit CTMM model for each pig
  
  cat("Working on individual", id, "\n")
  
  tdat = dat[ID == id] ### "ID" should match ID column in Data
  tdat = tdat[, .(datetime, Longitude, Latitude)] # These variables should match the date, long, and lat columns spelling & capitalization in Data
  colnames(tdat) = c("timestamp", "location.long", "location.lat") 
  tdat = tdat[order(timestamp), ]
  tdat = tdat[!duplicated(timestamp)]
  
  # Convert to telemetry object for use in CTMM
  crs_str = "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  telm = as.telemetry(tdat, projection=crs_str)
  
  # Fit with CTMM
  guess = ctmm.guess(telm, interactive=F)
  fit = ctmm.fit(telm, guess)
  
  # Predict 5 minute intervals (60*5 seconds)
  mint = min(telm$t)
  maxt = max(telm$t)
  ind = (interp_vals >= mint) & (interp_vals <= maxt)
  newt = interp_vals[ind]
  
  pred = predict(fit, data=telm, t=newt, complete = TRUE)
  pred_dat = data.table(id=id, x=pred$x, y=pred$y, 
                        time_s=pred$t, longitude=pred$longitude,
                        latitude=pred$latitude, datetime=pred$timestamp)
  fwrite(pred_dat, paste0("ctmm_FeralPig_data/", "traj_", id, ".csv")) ### Make sure that this folder exists in your project folder or it will give an error when you run function with data
  cat("Completed individual", id, "\n")
}

#Fix Date and Time in Data
dat[, datetime:=as.POSIXct(strptime(datetime, format="%Y-%m-%d %H:%M:%S", tz="GMT"))] # "datetime" variable name needs to match date column spelling and capitalization in data

# Create column with numeric date and time values
dat[, t:=as.numeric(datetime)] # datetime variable name needs to match datetime column in data

#List of unique individuals
unq_Indiv = unique(dat$ID)

#Minimum time
mint = min(dat$t)

#Maximum time
maxt = max(dat$t)

# Time is on the second scale; creating a sequence of numbers 
### starting at the minimum time to the maximum time in integrals of 5 min
interp_vals = seq(mint, maxt, by=60*5)

# Run CMMT on data --------------------------------------------------------

# Can change mc.cores depending on computer used, needs more than 2
### For 99 individuals on 8 cores -> ~4 hours run time
mclapply(unq_Indiv, fit_ctmm_model, dat, interp_vals, mc.cores=8)

#Ends log report
sink()

##### Convert all files in cmmt_FeralPigs_Data folder to one file (~1 minute)


Updated_Raw_data <- list.files(path = "New_path/", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows ### This new file will have the following columns: id, x, y, & time_s
                            ### id is the pig identifier
                            ### x is the UTM Easting for the GPS fix (meters)
                            ### y is the UTM Northing for the GPS fix (meters)
                            ### time_s is continuous time
                            ### Longitude
                            ### Latitude
                            ### datetime is the time stamp of the time_s (continuous time)

Updated_Raw_data_old <- list.files(path = "ctmm_FeralPig_data/", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
###This new file will have the following columns: id, x, y, & time_s
### id is the pig identifier
### x is the UTM Easting for the GPS fix (meters)
### y is the UTM Northing for the GPS fix (meters)
### time_s is continuous time
### Longitude
### Latitude
### datetime is the time stamp of the time_s (continuous time)

#Convert column into character
Updated_Raw_data1 <- Updated_Raw_data

Updated_Raw_data1 <- data.frame(Updated_Raw_data1)

Updated_Raw_data1$id <- as.character(Updated_Raw_data1$id)

colnames(Updated_Raw_data_old)[which(names(Updated_Raw_data_old) == "longitutde")] <- "LG1"
colnames(Updated_Raw_data_old)[which(names(Updated_Raw_data_old) == "longitude")] <- "LG2"

Updated_Raw_data_old <- Updated_Raw_data_old %>%
  mutate(longitude = coalesce(LG1, LG2)) %>%
  select(id, x, y, time_s, latitude, longitude, datetime)

Updated_Raw_data <- rbind(Updated_Raw_data_old, Updated_Raw_data1)

#### Adding more metadata to Updated_Raw_data 
#### Adding back population metadata variable

#### Let's group by Pig Population with our original Data
#List of unique individuals
unq_Indiv = unique(dat$ID)

Pig_population <- dat %>% group_by(population) %>%
  summarize(type = paste(sort(unique(ID))))

#Rename the pig sample name column ('type') to match the Updated_Raw_data pig sample name column ('id')
names(Pig_population)[names(Pig_population) == "type"] <- "id"

#Add population information to Updated_Raw_data file ~ 2 minutes run time
Updated_Raw_data <- merge(Updated_Raw_data, Pig_population, by = "id")

#Prep data table
Updated_Raw_data <- as.data.table(Updated_Raw_data)

## If is.data.table(Updated_Raw_data) is FALSE, then below code will give error
### as.data.table() will fix this potential error

#Add a new 'year' column
Updated_Raw_data[, yr:= as.numeric(format(datetime,'%Y'))]

#Let's fix 'id' column name to match future code
names(Updated_Raw_data)[names(Updated_Raw_data) == "id"] <- "ID"

Data_For_Contact <- Updated_Raw_data %>%
  select(ID, latitude, longitude, time_s, population)

saveRDS(Data_For_Contact, file = "CTMM_Contact_11_30_23.RDS")

## Save this Data
saveRDS(Updated_Raw_data, file = "CTMM_7_12_23.RDS")






