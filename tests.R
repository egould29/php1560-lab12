library(dplyr)
library(testthat)

source("estimation.R")
source("optimization.R")

# Create a subset of the sample bike data to test on
bike_subset <- bike_data[sample(1:nrow(bike_data), 100), ]
test_rates <- estimate_arrival_rates(bike_subset)
stations <- get_stations(test_rates)

# Setup for randomize_placement
dummy_num_stations <- round(runif(1, min = 1, max = 100))
dummy_num_bikes <- round(runif(1, min = 1, max = 1000))
dummy_placement <- randomize_placement(dummy_num_stations, dummy_num_bikes)
# Tests for randomize_placement
testthat::expect_equal(length(dummy_placement), dummy_num_stations)
testthat::expect_equal(sum(dummy_placement), dummy_num_bikes)
testthat::expect_equal(sum(dummy_placement < 0), 0)

# Setup for run_simulation
n_bikes <- 100
test_placement <- data.frame(station = stations,
                             bikes = randomize_placement(length(stations),
                                                         n_bikes))
test_success_rate <- run_simulation(test_rates, test_placement)
# Ensure that success rates are in [0, 1]
testthat::expect_equal(sum(between(test_success_rate$success_rate, 0, 1)),
                       nrow(test_success_rate))
# Ensure that there are no NAs
testthat::expect_equal(length(complete.cases(test_success_rate)),
                       nrow(test_success_rate))

# Tests for optimize_placement
opt_placement <- optimize_placement(test_rates, n_bikes)
testthat::expect_equal(sum(!(opt_placement$station %in% stations)), 0)
testthat::expect_equal(sum(opt_placement$bikes), n_bikes)