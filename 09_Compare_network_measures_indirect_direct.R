## Network measures compare direct and indirect
# Author: Abigail Turnlund & Tatiana Proboste
# date: 08/12/23

# Load necessary library
library(purrr)
library(tidyverse)

# Function to read CSV file from a directory and add a new column
read_csv_from_dir <- function(dir) {
  file_path <- file.path(dir, "results.csv")
  if (file.exists(file_path)) {
    data <- read.csv(file_path)
    data$filename <- basename(dir)  # Add a new column with the name of the file
    return(data)
  } else {
    return(NULL)
  }
}

#  Direct Network ---------------------------------------------------------

# Define the parent directory
parent_dir <- "./Direct Contact Results/"

# List all subdirectories
sub_dirs <- list.dirs(path = parent_dir, recursive = FALSE)

# Use map_df to read all CSV files and bind them into one data frame
results <- map_df(sub_dirs, read_csv_from_dir)

# Split the filename column into year and population
results <- transform(results,
                     year = sapply(strsplit(filename, "_"), `[`, 1),
                     population = sapply(strsplit(filename, "_"), `[`, 2))

# If you want the year column to be numeric
results$year <- as.numeric(results$year) 
results_direct <- results %>% 
  mutate(theshold="5m")  # here we can select the threshold of the network we want to work with.


# Indirect Network ------------------------------------------------------------------------

# Define the parent directory
parent_dir <- "./Indirect Contact Results/"

# List all subdirectories
sub_dirs <- list.dirs(path = parent_dir, recursive = FALSE)

# Use map_df to read all CSV files and bind them into one data frame
in_results <- map_df(sub_dirs, read_csv_from_dir)

# Split the filename column into year and population
in_results <- transform(in_results,
                     year = sapply(strsplit(filename, "_"), `[`, 1),
                     population = sapply(strsplit(filename, "_"), `[`, 2))

# If you want the year column to be numeric
in_results$year <- as.numeric(in_results$year) 
in_results <- in_results %>% 
  mutate(theshold="5m")  # here we can select the threshold of the network we want to work with.

# Combine results from direct and indirect networks
all <- rbind(ind_results, results_direct)

# Plot - analyse direct vs indirect ----------------------------------------------
# Convert the 'year' column to a factor
all$year <- as.factor(all$year)

all <- all %>%  
  filter(Component!="Number of nodes") 

# Plot
library(hrbrthemes)

tiff(filename="./Plots/network_comparison_Ind_Dir.tiff", units="in", width=10, height=6, res=600)
ggplot(all, aes(x=type, y=Value, color=Component)) +
  geom_jitter(color="#882255", size=0.4, alpha=0.9) +
  geom_boxplot(color="#882255", alpha=0.5)+
  facet_wrap(~Component, scales="free") +
  # scale_color_manual(values = my_palette) +
  theme_ipsum()+
  labs(title="Global Network Measures for Direct and Indirect Networks",
       x="",
       y="Value",
       color="Component") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10),
        legend.title = element_text(face="bold", size=10),
        legend.text = element_text(size=10),
        strip.text = element_text(face="bold", size=12),
        axis.text = element_text(size=10))
dev.off()




