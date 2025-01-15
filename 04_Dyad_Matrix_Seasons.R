# Author: Abigail Turnlund & Tatiana Proboste
# date: 08/12/23

#Libraries
library(tidyverse)
library(sf)
library(adehabitatLT)
library(spatsoc)
library(asnipe)
library(tmap)
library(data.table)

# load the data
df <- readRDS("spatialtemp_df_5m_ctmm_direct_season.rds")


# edge list generation ----------------------------------------------------

### separate by 0.00315 degrees or ~350m 
### seperate by 0.000018 degrees or ~ 2m
### separate by 0.000045 degrees of ~ 5m

edges <- edge_dist(
  df,
  threshold = 0.000018,
  id = 'ID',
  coords = c('x', 'y'),
  timegroup = 'timegroup',
  returnDist = TRUE,
  fillNA = TRUE,
  splitBy = c('population', 'yr', 'season')
)

# Dyads -------------------------------------------------------------------

# identifier for edge lists.
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')

# Dyad stats
# Get the unique dyads by timegroup
# NOTE: we are explicitly selecting only where dyadID is not NA
dyads <- unique(edges[!is.na(dyadID)], by = c('timegroup', 'dyadID'))

# Set the order of the rows
data.table::setorder(dyads, timegroup)

## Count number of timegroups dyads are observed together
dyads <- dyads %>%
  group_by(dyadID, yr, population, season) %>%
  mutate(nObs = n())

# add distance in metres
dyads <- dyads %>% 
  mutate(distance_m = distance*110000) 

saveRDS(dyads , "dyads_Ctmm.rds")
