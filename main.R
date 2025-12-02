library(dplyr)
library(readr)

source("estimation.R")
source("optimization.R")

# Read in the bike data
bike_data <- read_csv("Data/sample_bike.csv")

# Set the number of bikes to place
fleet_small <- 50
fleet_med <- 100
fleet_large <- 200

# Find the estimated arrival rates
estimated_rates <- estimate_arrival_rates(bike_data)

# Find optimized placements
placement_small <- optimize_placement(estimated_rates, fleet_small)
print("Finished small")
placement_med <- optimize_placement(estimated_rates, fleet_med)
print("Finished medium")
placement_large <- optimize_placement(estimated_rates, fleet_large)
print("Finished large")

write_csv(placement_small, "results/placement_small.csv")
write_csv(placement_med, "results/placement_med.csv")
write_csv(placement_large, "results/placement_large.csv")