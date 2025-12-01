library(dplyr)
library(tidyverse)
library(testthat)

source("estimation.R")
set.seed(5)

#' @description A helper function that returns each unique station in the data
#' 
#' @param data The arrival rates data
#' 
#' @return A vector containing each unique station in the arrival data
get_stations <- function(data) {
  # TODO: check input
  return(levels(as.factor(c(data$start_station, data$end_station))))
}

#' @description Randomizes placement for num_bikes at num_stations
#' 
#' @param num_stations The number of stations to place bikes at
#' @param num_bikes The number of bikes to place
#' 
#' @return A vector containing the number of bikes to be placed at each station.
randomize_placement <- function(num_stations, num_bikes) {
  # TODO: check input
  result <- rep.int(0, num_stations)
  
  while (num_bikes > 0) {
    i <- round(runif(1, min = 1, max = num_stations))
    result[i] <- result[i] + 1
    
    num_bikes <- num_bikes - 1
  }
  
  return(result)
}

#' @description Simulates one day of bike trips n number of times and calculates
#' the rate of successful trips by station.
#' 
#' @param data A data frame containing the estimated arrival rate data.
#' @param placement The initial daily placement of bikes.
#' @param n The number of times to simulate a day with this placement, default
#' of 100.
#' 
#' @return A data frame containing the total number of attempted and successful
#' trips, and the rate of successful trips, from each starting station.
run_simulation <- function(data, placement, n = 20) {
  # TODO: check input
  success_rate <- data.frame(start_station = c(), successes = c(), trips = c())
  
  while (n > 0) {
    sim_results <- simulate_day(simulate_demand(data), placement) %>%
      group_by(start_station) %>%
      summarize(successes = sum(successful_trip), trips = n())
    
    success_rate <- rbind(success_rate, sim_results)
    n <- n - 1
  }
  
  success_rate <- success_rate %>%
    group_by(start_station) %>%
    summarize(successes = sum(successes),
              trips = sum(trips),
              success_rate = sum(successes) / sum(trips))
  
  return(success_rate)
}

#' @description Finds the optimal configuration of bike placements at the start
#' of the day.
#' 
#' @param data A dataframe representing the estimated arrival rates for each
#' pair of stations by hour.
#' @param num_bikes The number of bikes to place.
#' @param n The number of reconfigurations in the optimization process, with
#' a default of 100.
#' 
#' @return A dataframe containing the optimal number of bikes to place at each
#' station to start the day.
optimize_placement <- function(data, num_bikes, n = 20) {
  # Check the inputs
  if (!is.data.frame(data) | !is.numeric(num_bikes)) {
    stop("Invalid input: incorrect type(s)")
  } else if (num_bikes < 0 | num_bikes %% 1 != 0) {
    stop("Invalid input: 'num_bikes' must be a positive integer")
  } else if (sum(c("start_station", "end_station", "hour", "avg_trips",
                   "avg_avail", "mu_hat") %in% colnames(data)) != 6) {
    stop("Invalid input: data does not have correct column names")
  }
  
  # Initialize a data frame to monitor the total number of successful trips
  # with the current configuration.
  # happiness <- data.frame(config = 1:n, total_success_rate = rep(0, n))
  
  # Start with a randomized configuration of bike placements.
  stations <- get_stations(data)
  placement <- data.frame(station = stations,
                          bikes = randomize_placement(length(stations),
                                                      num_bikes))
  
  for (i in 1:n) {
    # Run the simulation with the current placements.
    sim_results <- run_simulation(data, placement)
    
    # Update cumulative success rate for this configuration.
    # happiness[i, "total_success_rate"] <-
      # sum(sim_results$successes) / sum(sim_results$trips)
    
    # Find the stations with the highest and lowest success rates.
    highest <- slice_max(sim_results, success_rate,
                         with_ties = FALSE)$start_station
    lowest <- slice_min(sim_results, success_rate,
                        with_ties = FALSE)$start_station
    
    # Move at least one quarter of bikes from the station with the highest
    # success rate to the station with the lowest success rate.
    num_to_move <- ceiling(0.25 * filter(placement, station == highest)$bikes)
    placement[placement$station == highest, 2] <-
      placement[placement$station == highest, 2] - num_to_move
    placement[placement$station == lowest, 2] <-
      placement[placement$station == lowest, 2] + num_to_move
    
    # print(c("finished", i))
  }
  
  return(arrange(placement, station))
}