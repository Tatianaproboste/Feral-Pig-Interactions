# Author: Abigail Turnlund & Tatiana Proboste
# date: 08/12/23

library(tidyverse)
library(igraph)
library(ggraph)
library(asnipe)
library(sna)
set.seed(1233)

# Get the demography 
dem <- readRDS("data.RDS")
dem$Population <- stringr::str_trim(tolower(dem$Population))

# get spatial temporal matrix
df <- readRDS("spatialtemp.rds") 
df$pop <- stringr::str_trim(tolower(df$population)) 
df <- df %>%  dplyr::select(-population)

#Create Output Folder
OUTPUT_FOLDER <- "year_population_networks/"
dir.create(OUTPUT_FOLDER)

# -------------------------------------------------------------------------
#Create list of every Year & Population Combination
params <- df %>%  group_by(yr, pop) %>% 
  distinct() %>% 
  summarise(num_unique_ids = n_distinct(ID)) %>% 
  filter(num_unique_ids >3) %>%   # this is to eliminate the matrix that are smaller than 3x3
  dplyr::select(year=yr, population=pop) 

#Create Empty Lists
matrix_list <- list() 
network_list <- list()

# global network ----------------------------------------------------------
# Global Clustering Coefficient = “ratio of triangles to connected triples”
# 
# Local Clustering Coefficient = for each node, the proportion of their neighbors
# that are connected to each other
# 
# Average Local Clustering Coefficient: If Ci
# is the proportion of two nodes connected to node i that are also connected to each other 
# (i.e., the Local Clustering Coefficient)

# the for look calculate the individual matrix for each year
# and save each year in an specific matrix_list$year
network_calculate = function(year, population, OUPUT_FOLDER) {
  cat(paste0(year, " - ", population, "\n"))
  SAVE_FOLDER <- paste0(OUTPUT_FOLDER, year, "_", population, "/")
  cat(SAVE_FOLDER)
  cat("\n")
  dir.create(SAVE_FOLDER)
  a <-  spatsoc::get_gbi(
    df %>% dplyr::filter(yr == year, pop == population),
    group='group',
    id= 'ID'
  )
  # For loop to get networks per year
  b <- asnipe::get_network(
    a,
    data_format = "GBI",
    association_index = "SRI")
  c <- igraph::graph.adjacency(b,'undirected',
                               weighted = TRUE,
                               diag = TRUE)
  # Transitivity measures the probability that the adjacent vertices
  # of a vertex are connected. This is sometimes also called the clustering coefficient.
  # the ratio of #the count of triangles and connected triples in the graph.
  mean_distance <- mean_distance(c)
  density <- edge_density(c)
  number_vertices=vcount(c)
  number_edges=ecount(c) # The size of the graph
  global_transitivity <- transitivity(c, "global")
  average_local_transitivity <- transitivity(c, "localaverage")
  betweenness_vals <- centr_betw(c)$centralization
  # Table with global measures ----------------------------------------------
  results <- data.frame(
    Component = c("Mean distance","Number of nodes","Number of edges", 
                  "Edge Density", "Global Transitivity", "Average Local Transitivity"),
    value = c(mean_distance,number_vertices, number_edges, density, 
              global_transitivity, average_local_transitivity)
  )
  
  colnames(results)[-1] <- paste("Value")
  # Plot  -------------------------------------------------------------------
  d <- tidygraph::as_tbl_graph(c) %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(centrality=igraph::evcent(c)$vector) %>%
    dplyr::mutate(degree=igraph::degree(c)) %>%
    dplyr::mutate(ID = names(igraph::degree(c))) %>%
    dplyr::mutate(strength = graph.strength(c)) %>%
    dplyr::mutate(betweenness=igraph::betweenness(c)) %>%
    left_join(dem, by = 'ID')
  
  # table wit local measures
  results_local_measures <- data.frame(cbind(data.frame(d), year)) %>%
    rename(Year = year)
  
  d %>%
    ggraph(layout = "fr") +  #kk ; linear
    geom_edge_arc(alpha = 0.4, aes(width = weight),
                  colour= "gray20",
                 lineend = "round",
                  strength = .1) +
    geom_node_point(aes(size = degree, color=sex))+
    theme_graph(background = "white") +
    guides(edge_width = FALSE,
           edge_alpha = FALSE)+
    labs(title = paste0(population, sep=" ", year)) -> plot
  tiff(paste0(SAVE_FOLDER,"plot.tif"), res=300, width = 10, height =10, units= 'cm')
  print(plot)
  dev.off()
  # Save results
  write_excel_csv(results, file = paste0(SAVE_FOLDER, "results.csv"))
 
   return(results %>%
           tidyr::pivot_wider(names_from='Component',
                              values_from='Value') %>%
           dplyr::mutate(Year = year) %>%
           dplyr::mutate(Population = population) %>%
           dplyr::left_join(results_local_measures, by=c("Year", "Population"))
  )
}

#Create Empty Data Frame
networks_results <- data.frame()

# Loop --------------------------------------------------------------------
# Loop through all the combinations listed in params

for (i in 1:nrow(params)) {
  networks_results <- rbind(networks_results,
                            network_calculate(params[i,]$year,
                                              params[i,]$population)
  )
}

#Save Results
saveRDS(networks_results, "network_local_and_global_measures.rds")
