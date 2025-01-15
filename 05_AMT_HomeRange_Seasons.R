# Calculating Home Range & Overlap
# Author: Abigail Turnlund & Tatiana Proboste
# Date: 10/11/2023 

#Load libraries
library(dplyr)
library(ggplot2) 
library(amt)
library(readr)


# -------------------------------------------------------------------------
# this code was run for direct and for indirect networks and can be modified 
# using different data set created with different thresholds.
# Here we wanted to determine which animals overlap their home range
# within the direct and indirect networks and also by season.


# Load data
Pigs <- readRDS("CTMM.RDS") 

#Add in season column
Pigs <- Pigs %>%
  dplyr::mutate(month = lubridate::month(datetime)) %>%
  dplyr::mutate(season = dplyr::case_when(
    month %in% c(12, 1, 2) ~ "Summer",
    month %in% c(3, 4, 5) ~ "Autumn",
    month %in% c(6, 7, 8) ~ "Winter",
    month %in% c(9, 10, 11) ~ "Spring"
  ))

# Make a dataframe
Pigs <- as.data.frame(Pigs)

# Add a unique identifier with ID, population, year and season info
Pigs <- Pigs %>% 
  mutate(id = paste(ID, population, yr, season, sep = "-"))


# Create a list of year, population & season that we can loop through
params <- Pigs %>%  group_by(yr, population, season) %>% 
  distinct() %>% 
  summarise(num_unique_ids = n_distinct(ID)) %>% 
  filter(num_unique_ids >3) %>%   # this is to eliminate the matrix that are smaller than 3x3
  dplyr::select(year=yr, population=population, season=season) 

# Create Output Folder
output1 <- "outputs/"
dir.create(output1)
outputs <- "outputs/year_population_season_spatial_home_range comparison/"
dir.create(outputs)

# Function that will estiamte & compare home ranges
Home_Range_compare = function(year, pop, szn, outputs) {
  cat(paste0(year, " - ", szn, " - ", pop, "\n"))

# Make an amt `track` object with our sample data set
Pigs_track <- make_track(
    Pigs %>% dplyr::filter(yr == year, population == pop, season == szn),
    x, y, datetime, id = id, population = population, yr = yr, season = season)

# Writing this CSV to check that the data was filtered correctly
  write.csv(Pigs_track, file = paste0(outputs, year, "_",
                                      szn, "_",
                                      pop, "_",
                                      "turtletrack", ".csv"))
  
  cat(paste0("Turtle_track", "\n")) #Checkpoint 
  
# Nest the object by ID
  Pigs_track1 <- Pigs_track %>%
    nest(data = -"id") %>%
    arrange(id)
  
  cat(paste0("Turtle_track1", "\n")) #Checkpoint
  
# Add kde list-column to Pigs_track using `map`
  
# Now, let’s estimate home ranges using density kernels and then estimate 
# how much the kernels overlap in area between each animal in our data 
# set using the hr method.
  
 Pigs_track2 <- Pigs_track1 %>% 
    mutate(kde = map(data, function(x)
      # levels are for which proportions (0.95 = 95%)
      x %>% hr_kde(levels = c(0.95))))
  
  cat(paste0("Turtle_track2", "\n")) #Checkpoint
  
# Calculate overlap between all individuals
  turtle_hr_overlap <- hr_overlap(Pigs_track2$kde,
                                  labels = Pigs_track2$id, 
                                  which = "all",
                                  conditional = FALSE)
  
  cat(paste0("Turtle_overlap", "\n")) #Checkpoint
  
# Save results
  write.csv(turtle_hr_overlap, file =  paste0(outputs, 
                                              year, "_",
                                              szn, "_",
                                              pop, "_",
                                              "results", ".csv"))
  
  return(turtle_hr_overlap)
  
}


# Loop through all the params
for (i in 1:nrow(params)) {
  
  cat(paste0(params[i,]$year, "\n"))
  
  Home_Range_compare(params[i,]$year,
                     params[i,]$population,
                     params[i,]$season,
                     outputs) 
}

# -------------------------------------------------------------------------
# Let's combine all the csv files we created into one large file
##### Convert all files in cmmt_FeralPigs_Data folder to one file

Home_Range_Data <- list.files(path = outputs, full.names = TRUE, pattern = "results.csv") %>%
  lapply(read_csv) %>%
  bind_rows ###This new file will have the following columns: ...1, from, to, level, overlap

#Fix up Columns
library(stringr)

Home_Range_Data[c('ID', 'population','yr', 'season')] <- str_split_fixed(Home_Range_Data$from, '-', 4)

Home_Range_Data[c('ID2', 'Other')] <- str_split_fixed(Home_Range_Data$to, '-', 2)

Home_Range_Data <- Home_Range_Data[c('yr', 'population', 'season',
                                     'ID', 'ID2', 'overlap')]

# -------------------------------------------------------------------------
# Overlap groups ----------------------------------------------------------
# -------------------------------------------------------------------------
# keep only the dyads that overlap more than 50%
# we assumed that animals that overlap 50% are from the same sounder

areaDF_50 <- Home_Range_Data %>% filter(overlap > 0.5) %>% 
  filter(overlap < 1) %>% 
  filter(ID != ID2)

saveRDS(areaDF_50,"areaDF_direct_seasons.rds")