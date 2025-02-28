# Create a database
# This is a made up database to show how the data need to be structured for the code to work


# Load necessary library
library(data.table)

# Set seed for reproducibility
set.seed(123)

# Define parameters
num_replicates <- 100  # Number of times each individual appears
num_individuals <- 5  # Number of unique individuals
total_records <- num_replicates * num_individuals  # Total dataset size

# Create unique individual IDs
individual_ids <- paste0("f_", sample(150800:151000, num_individuals, replace = FALSE))

# Define the start and end time range
start_time <- as.POSIXct("2022-12-01 00:00:00", tz = "GMT")
end_time <- as.POSIXct("2022-12-31 23:59:59", tz = "GMT")

# Generate timestamps (randomly spaced between start and end)
timestamps <- sort(sample(seq(start_time, end_time, by = "sec"), total_records, replace = TRUE))


# Generate the dataset
dat <- data.table(
  ID = factor(rep(individual_ids, each = num_replicates)),  # Factor for ID column
  population = sample(c("oak", "maple", "birch", "willow"), total_records, replace = TRUE),
  yr = "2022",  # Keep year as character
  datetime = timestamps,  # Already in POSIXct format
  Longitude = round(runif(total_records, min = 150.5000, max = 151.0000), 6),
  Latitude = round(runif(total_records, min = -31.9200, max = -31.8200), 6)
)

# Ensure datetime is in POSIXct format (redundant, but guarantees consistency)
dat[, datetime := as.POSIXct(datetime, tz = "GMT")]

# Print summary
str(dat)

# Preview data
head(dat)


