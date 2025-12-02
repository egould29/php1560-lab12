library(dplyr)
library(gt)
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
placement_med <- optimize_placement(estimated_rates, fleet_med)
placement_large <- optimize_placement(estimated_rates, fleet_large)

# Write outputs to results folder
write_csv(placement_small, "results/placement_small.csv")
write_csv(placement_med, "results/placement_med.csv")
write_csv(placement_large, "results/placement_large.csv")

# Generate visualizations
# Table 1: Small-Fleet Placement
tbl_small <- placement_small %>%
  gt() %>%
  tab_header(title = "Placement of Bikes for Fleet Size 50") %>%
  cols_label(station ~ "Station", bikes ~ "Bikes")

# Table 2: Medium-Fleet Placement
tbl_med <- placement_med %>%
  gt() %>%
  tab_header(title = "Placement of Bikes for Fleet Size 100") %>%
  cols_label(station ~ "Station", bikes ~ "Bikes")

# Table 3: Large-Fleet Placement
tbl_large <- placement_large %>%
  gt() %>%
  tab_header(title = "Placement of Bikes for Fleet Size 200") %>%
  cols_label(station ~ "Station", bikes ~ "Bikes")

# Plot 1: Highly-Connected Stations
station_connections <- estimated_rates %>%
  group_by(start_station, end_station) %>%
  summarize(avg_daily_trips = sum(mu_hat))

fill_colors <-c("white",
                "#d5e9f7",
                "#98bcd4",
                "#19244f")
vals <- c("0", "8", "15", "30")
quants <- ecdf(station_connections$avg_daily_trips)(vals)
station_connections$tile_val <- ecdf(station_connections$avg_daily_trips)(station_connections$avg_daily_trips)

p1 <- ggplot(station_connections,
             aes(x = as.factor(start_station), y = as.factor(end_station),
                 fill = tile_val)) +
  geom_tile(color = "white") +
  ggtitle(label = "Estimated Daily Trips Between Stations") +
  scale_fill_gradientn(labels = vals,
                       colors = fill_colors,
                       breaks = quants,
                       name = "Number of Trips",
                       na.value = "lightgray") +
  theme_minimal() +
  scale_y_discrete(name = "End Station") +
  coord_equal() +
  scale_x_discrete(name = "Start Station") +
  theme(legend.position = 'bottom',
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(size = 10),
        panel.grid = element_blank())

# Plot 2: Demand Heatmap
station_demand <- estimated_rates %>%
  group_by(start_station, hour) %>%
  summarize(avg_demand = sum(mu_hat))

vals <- c("0", "5", "13", "25")
quants <- ecdf(station_demand$avg_demand)(vals)
station_demand$tile_val <- ecdf(station_demand$avg_demand)(station_demand$avg_demand)

p2 <- ggplot(station_demand,
             aes(x = hour, y = start_station, fill = tile_val)) +
  geom_tile(color = "white") +
  ggtitle(label = "Estimated Hourly Demand") +
  scale_fill_gradientn(labels = vals,
                       colors = fill_colors,
                       breaks = quants,
                       name = "Average Number of Trips Attempted",
                       na.value = "lightgray") +
  scale_y_discrete(position = 'left', name = "station", limits=rev) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0.5), 
                     breaks = c(0, 6, 12, 18, 23),
                     name = "hour") +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(size = 10),
        axis.text.x=element_text(size=10, hjust=0,vjust=0.2),
        panel.grid = element_blank())