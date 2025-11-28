
#'Estimated arrival rates
#'
#'@description estimates the arrival rates from the input bike data
#'
#'@param data a df containing the bike data.
#'@return a df containing the estimated arrival rates between two stations
#'for a given hour
estimate_arrival_rates <- function(data) {
  
  # compute the average number of trips per hour between each pair
  x_hat <- data %>%
    mutate(hour = hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarise(avg_trips = n() / n_distinct(as_date(start_time)), 
              .groups = "drop") 
  
  # pivot longer to get change in count 
  data$end_station <- as.character(data$end_station)
  trips_long <- data %>%
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    select(station, time, hour, change)
  
  # add hour markers so we can get cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1),seq(0,23,1)+0.9999999)
  stations <- unique(trips_long$station)
  hr_pts <- expand.grid(time = dates, hour = hours, 
                        station = stations) %>%
    mutate(time = as.POSIXct(time) + hour*60*60,
           hour = hour(time))
  hr_pts$change <- 0
  trips_long <- rbind(trips_long, hr_pts)
  
  # find average availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>% 
    mutate(count = cumsum(change),
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    summarize(time_avail = 
                sum(difftime(time, lag(time), units="hours")*(count > 0), 
                    na.rm = TRUE)) %>%
    summarize(avg_avail = mean(time_avail)) %>%
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4)) %>%
    ungroup()
  
  # join the data and compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  #Converting any NAs to 0
  mu_hat$mu_hat[is.na(mu_hat$mu_hat)] <- 0
  
  return(mu_hat)
}

#' Arrivals per pair
#'
#'@description A helper method that estimates new arrival times 
#'between a single pair of stations. 
#'
#'@param subset A subset df from output of estimated_arrival_rates that 
#'contains the arrival rates between two stations. Must have a start_station, 
#'end_station, and mu_hat column. All values are numeric.
#'@return A df with three numeric columns: start_station, end_station,
#' and time, which contains the specific arrival time between the stations 
#' at every hour
arrivals_per_pair <- function(subset) {
  
  start_station <- subset$start_station[1]
  end_station <- subset$end_station[1]
  lambda_max <- max(subset$mu_hat)
  curr_time <- 0
  new_arrivals <- c()
  
  #Unique case for row 539 where rate between stations was 0
  if (lambda_max == 0) {
    return()
  }
  
  while (curr_time < 24) {
    #updating curr_time with the next arrival time
    curr_time <- curr_time + rexp(1, rate=lambda_max)
    curr_hour <- floor(curr_time)
    
    #Getting corresponding lambda from the hour
    if (any(subset$hour %in% curr_hour)) {
      lambda <- subset$mu_hat[subset$hour == curr_hour]
    }
    else {
      lambda <- 0
    }
    
    #flipping coin to see if we keep that arrival
    if (rbinom(1, 1, prob=lambda/lambda_max) == 1) {
      new_arrivals <- c(new_arrivals, curr_time)
    }
  }
  
  #creating new df
  n <- length(new_arrivals)
  df <- data.frame(
    start_station = rep(start_station, n),
    end_station = rep(end_station, n),
    time = new_arrivals
  )
  return(df)
}

#'Simulate demand
#'
#'@description Simulates the demand/specific arrival time between 
#'every pair of stations in the df from the output of estimate_arrival_rates. 
#'
#'@param rates A numeric df, specifically the output of 
#'estimated_arrival_rates. Must have a start_station, end_station, 
#'and mu_hat column.
#'@return A df that contains the arrival times between two stations. 
#'Has columns start_station, end_station, and time
simulate_demand <- function(rates) {
  
  new_arrivals <- data.frame(
    start_station = integer(),
    end_station = integer(),
    time = numeric()
  )
  n <- nrow(rates)
  i <- 1
  
  #Looping through entire rates df
  while (i <= n) {
    
    curr_start <- rates$start_station[i]
    curr_end   <- rates$end_station[i]
    j <- i
    
    #Determining how many rows we can skip (skip rows with the same
    #start and end stations)
    while (j + 1 <= n &&
           rates$start_station[j + 1] == curr_start &&
           rates$end_station[j + 1] == curr_end) {
      j <- j + 1
    }
    
    #Estimating new arrival times for one unique pair of
    #start and end stations at a time
    subset <- rates[i:j, ]
    curr_new_arrivals <- arrivals_per_pair(subset)
    new_arrivals <- rbind(new_arrivals, curr_new_arrivals)
    
    #Skipping to row with the next new end station
    i <- j + 1
  }
  
  return(new_arrivals)
}

#'Simulate day
#'
#'@description Determines which trips are successful given the initial
#' placement of bikes at each station. Updates the placement of bikes if 
#' the trip can occur
#'
#'@param demand The output of simulate_demand. A numeric df that contains 
#'columns for start_station, end_station, and time
#'@param placement A numeric df that contains the initial placement of bikes
#' at each station (bikes at hour 0)
#'@return A numeric df. The same df as what was passed as demand, but with an 
#'additional column, successful_trip, that indicates if the trip can occur
simulate_day <- function(demand, placement) {
  
  sorted_df <- arrange(demand, time)
  successful_trips <- c() #will be appended to demand as the new column
  n <- nrow(sorted_df)
  
  #Looping through every trip in the sorted demand df
  for (i in 1:n) {
    start <- sorted_df$start_station[i]
    end <- sorted_df$end_station[i]
    bikes_avail <- placement$bikes[placement$station == start]
    
    if (bikes_avail > 0) {
      successful_trips <- c(successful_trips, 1) #1 = successful trip
      #moving bikes from start station to end station
      placement$bikes[placement$station == start] <-
        placement$bikes[placement$station == start] - 1
      placement$bikes[placement$station == end] <-
        placement$bikes[placement$station == end] + 1
    }
    else {
      successful_trips <- c(successful_trips, 0) #0 = unsuccessful trip
    }
  }
  
  sorted_df <- cbind(sorted_df, successful_trip = successful_trips)
  sorted_df <- arrange(sorted_df, start_station)
  return(sorted_df)
}


############################################################################
# Load the sample dataset
bike_data <- read_csv("../Data/sample_bike.csv")

# Estimate arrival rates
arrival_rates <- estimate_arrival_rates(bike_data)
#print(arrival_rates, n = 10)

# Estimating new arrivals
new_arrivals <- simulate_demand(arrival_rates)
#print(new_arrivals)

# Creating example initial placement of bikes, 5 at each station (4-24)
stations <- seq(from=4, to=24, by=1) #length 21
num_bikes <- rep(5, 21)
init_placement <- data.frame(
  station = stations,
  bikes = num_bikes
)

# Estimating trips and successes in a single day
trips_in_one_day <- simulate_day(new_arrivals, init_placement)
#print(trips_in_one_day)
