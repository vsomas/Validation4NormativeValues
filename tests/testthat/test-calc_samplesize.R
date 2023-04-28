# Test case 1: test if function returns an error when required inputs are missing
test_that("calc_samplesize returns an error when required inputs are missing", {
  # Set input values with missing values
  conf <- 0.95
  mean <- NULL
  sd <- 2
  ME <- NULL
  agecat <- c("18-24", "25-34", "35-44")
  # Call function and expect an error
  expect_error(calc_samplesize(conf = conf, mean = mean, sd = sd, ME = ME, agecat = agecat), "arguments imply differing number of rows: 3, 1, 0")
})
