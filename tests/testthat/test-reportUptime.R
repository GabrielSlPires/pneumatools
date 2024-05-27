# Test cases
test_that("reportUptime handles intervals within limits correctly", {
  database <- open_pneumatron_database("data-examples/example-v4.csv")

  result <- reportUptime(database)
  expect_equal(nrow(result), 200)
  expect_true(sum(result$duration > 0) == 192)

  result <- reportUptime(database, lower_interval = 1800, upper_interval = 2020)
  expect_equal(nrow(result), 21)
  expect_equal(sum(result$duration > 0), 19)
})

test_that("reportUptime works only with PneumatronDatabase class", {
  expect_error(reportUptime(mtcars), "unable to find an inherited method")
})
