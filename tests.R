library(testthat)

source("estimation.R")
source("optimization.R")

# TODO: test randomize_placement
testthat::expect_equal(length(randomize_placement(10, 100)), 10)
testthat::expect_true(sum(randomize_placement(10, 100)), 100)

# TODO: test run_simulation
# TODO: test optimize_placement