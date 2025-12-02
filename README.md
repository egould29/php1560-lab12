# php1560-lab12
Lab 12 PHP 1560

To run the code, simply run main.R which will call necessary functions
from the other files that we source from at the top of the file. 

estimation.R:
This file contains 4 functions: estimate_arrival_rates, 
arrivals_per_pair, simulate_demand, and simulate_day. The main purpose
of this file is to simulate a day of trips and categorize trips that can
or cannot happen given some initial placement of bikes. Thus, the functions
of this file are mainly used in the optimization.R file

optimization.R:
This file contains 4 functions: get_station, randomize_placement, 
run_simulation, and optimize_placement. This file calls functions from 
estimation.R to optimize the initial of placement okf bikes such that the total
number of successful trips is maximized. Here, multiple days are simulated
to see where bikes are and are not being used, and using that information
to update the placement of bikes accordingly

tests.R:
This file tests the functionality of the functions in the previous files.
Simple tests were conducted to ensure that the output and behavior of the 
functions are what we expect.

main.R:
The main script that should be ran to get results. Here, we use different fleet
sizes, or number of bikes to place, and save the optimal placement of bikes
at each station as a csv file in our results folder. We also include code to
create figures that will provide further insight into the simulation,
where and when trips were most popular, and other information that could help
the owner place bikes.