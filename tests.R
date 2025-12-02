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

#testing arrivals_per_pair
test_arrival_per_pair <- function(){
  bike_data <- read_csv("/Users/romer/Desktop/php1560/Data/sample_bike.csv")
  arrival_rates <- estimate_arrival_rates(bike_data)
  
  #Testing that the maximum hour in the output does not exceed the
  #maximum estimated hour between trips in the subset
  subset <- arrival_rates[1:8, ]
  output <- arrivals_per_pair(subset)
  max_hour_subset <- max(subset$hour) 
  max_hour_output <- floor(max(output$time))
  testthat::expect_true(max_hour_output <= max_hour_subset)
  return("Done testing arrival_per_pair")
}


#testing simulate_demand
test_simulate_demand <- function() {
  bike_data <- read_csv("/Users/romer/Desktop/php1560/Data/sample_bike.csv")
  arrival_rates <- estimate_arrival_rates(bike_data)
  
  #Testing that the arrival times are between 0 and 24
  new_arrivals <- simulate_demand(arrival_rates)
  min_hour <- floor(min(new_arrivals$time))
  max_hour <- floor(max(new_arrivals$time))
  testthat::expect_true(min_hour >= 0 && max_hour <= 24)
  return("Done testing simulate_demand")
}


#testing simulate_day
test_simulate_day <- function() {
  stations <- seq(from=4, to=24, by=1) #length 21
  num_bikes <- rep(5, 21)
  incorrect_placement <- data.frame(
    start_station = stations,
    bikes = num_bikes
  )
  #Testing a thrown error if the placement argument does not contain correct
  #column names
  testthat::expect_error(simulate_day(new_arrivals, incorrect_placement))
  return("Done testing simulate_day")
}

test_arrival_per_pair()
test_simulate_demand()
test_simulate_day()
