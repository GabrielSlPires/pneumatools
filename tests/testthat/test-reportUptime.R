# Test cases
test_that("reportUptime handles intervals within limits correctly", {
  database <- open_pneumatron_database("data-examples/example-v4.csv")

  result <- reportUptime(database)
  expect_equal(nrow(result), 10)

  result <- reportUptime(database, interval = 960)
  expect_equal(nrow(result), 201)
  expect_equal(sum(result$duration > 0), 200)
})

test_that("reportUptime works only with PneumatronDatabase class", {
  expect_error(reportUptime(mtcars), "unable to find an inherited method")
})
