# Author: Abigail Turnlund & Tatiana Proboste
# Date: 10/11/2023 

# This code works for direct contact or indirect contact.

library(tidyverse)
library(stringr)
library(dplyr)

dyads <- read_rds('dyads_Ctmm_Direct.rds') # all dyads 
hm <- read_rds("areaDF_direct_season.rds") %>% 
  rename(ID1=ID)  # this has the information of dyads that belong to a sounder
dem <- read_rds("Demographic_data.RDS") %>% 
  dplyr::select(-Population) 


dem$sex<- stringr::str_trim(tolower(dem$sex))

# load the full data to calculate how many days records we have per ID
days_per_ID <- read_rds("spatialtemp_direct_season.rds") %>% 
  mutate(day_month = format(datetime, "%m-%d")) %>% 
  group_by(ID, yr, season) %>% 
  summarise(num_days = n_distinct(day_month))

# Classify as between and within sounders based on the home range (>50% overlap)
# each dyadic is classified as within sounders if they are from the same sounder between spunders
# if they are from different sounders based on 50% overlap
# between sounders  = 0
# within sounders = 1

# join both side, ID1 that match with ID1 and ID1 that match with ID2
hm$yr <- as.numeric(hm$yr)

contact <- dyads %>%  
  left_join(hm)

contact <- left_join(contact, hm, by=c('ID1'='ID2', "population", 'yr', 'ID2'='ID1', 'season'))

contact[c('ID1', 'ID2')] <- str_split_fixed(contact$dyadID, '-', 2)

# combine proportion x and y in one column 
# 0 = between sounders
# 1 = within sounders

contact <- contact %>%   
  mutate(contact_type = as.character(ifelse( 
    is.na(overlap.x) & is.na(overlap.y), 0, 1))
  ) 

dat <- contact %>%
  left_join(days_per_ID, by = c("ID1" = "ID", 'yr') ) %>%
  mutate(num_days_ID1 = num_days) %>%
  left_join(days_per_ID, by = c("ID2" = "ID", 'yr')) %>%
  mutate(num_days_ID2 = num_days.y) %>%
  mutate(min_num_days = pmin(num_days_ID1, num_days_ID2)) %>% 
  dplyr::select( -num_days.y, -num_days.x) %>% 
  mutate(contact_rate = nObs/min_num_days) 

# join the sex to categorise if the dyad is female-female or male-male or female-male

dat <- left_join(dat, dem, by=c('ID1'='ID'))
dat <- left_join(dat, dem, by=c('ID2' = 'ID'))
dat <- dat %>%  mutate(dyad_sex = paste0 (sex.x, sep='_', sex.y))

# Create a new column and update values
dat$combined_sex <- ifelse(dat$dyad_sex %in% c("female_male", "male_female"), "mixed", dat$dyad_sex)

# check how many different dyads we have
length(unique(dat$dyadID)) 

# Contact rate was calculated:
# number of observations or contact per Dyad divided 
contact_rates_data <- dat %>% 
  group_by(dyadID) %>% 
  dplyr::select(population, yr, ID1, ID2, dyadID, nObs, min_num_days,
                contact_rate, contact_type, combined_sex, distance_m, season) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(contact_rate=as.numeric(contact_rate))


# Contact Rate Table ------------------------------------------------------

Contact_Rate_Table <- contact_rates_data %>% 
  group_by(population, yr, season) %>% 
  filter(min_num_days > 0) %>% # Filtering pigs that are there for less than 6 months
  mutate(Total_n = n_distinct(unlist(across(ID1:ID2)))) %>% # Creating a unique count of individuals for every population/year
  ungroup() 

Contact_Rate_Table <- Contact_Rate_Table %>%
  group_by(population, yr, combined_sex, season, contact_type) %>%
  mutate(N = n_distinct(unlist(across(ID1:ID2)))) %>% # Creating a unique count of individuals for every population/year/contact type/sex category
  ungroup()

Contact_Rate_Table <-  Contact_Rate_Table %>%
  group_by(population, yr, season, combined_sex, contact_type) %>%
  mutate(mean = mean(contact_rate), # Calculating Mean
         SD = sd(contact_rate)) %>% # Calculating Standard Deviation
  reframe(yr = yr, 
          population = population,
          season = season, 
          contact_type = contact_type,
          combined_sex = combined_sex,
          Total_n = Total_n, # Total # of individuals in each population per year
          N = N, #Total # of individuals in each combined_sex per contact type per population per year
          mean = mean,
          SD = SD) %>%
  ungroup() %>%
  distinct() # remove repeat rows

write.csv(Contact_Rate_Table, file = "Contact_Rate_Direct_Seasons.csv")

