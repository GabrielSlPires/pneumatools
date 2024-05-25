test_that("PneumatronDatabase initializes correctly with valid data", {
  df <- data.frame(
    id = c(1, 1, 1, 1, 1),
    measure = 1:5,
    log_line = 1:5,
    pressure = c(100, 40, 40.7, 41.5, 41.9),
    datetime = as.POSIXct(c("2024-05-13 13:54:26", "2024-05-13 13:54:26", "2024-05-13 13:54:27", "2024-05-13 13:54:27", "2024-05-13 13:54:28"))
  )
  obj <- PneumatronDatabase(df)
  expect_s4_class(obj, "PneumatronDatabase")
})

test_that("Initialization fails with missing required columns", {
  df <- data.frame(
    id = c(1, 1, 1, 1, 1),
    measure = 1:5,
    # log_line is missing
    pressure = c(100, 40, 40.7, 41.5, 41.9),
    datetime = as.POSIXct(c("2024-05-13 13:54:26", "2024-05-13 13:54:26", "2024-05-13 13:54:27", "2024-05-13 13:54:27", "2024-05-13 13:54:28"))
  )
  expect_error(PneumatronDatabase(df), "At least one required column is missing:")
})

test_that("Initialization fails wrong datetime format", {
  df <- data.frame(
    id = c(1, 1, 1, 1, 1),
    measure = 1:5,
    log_line = 1:5,
    pressure = c(100, 40, 40.7, 41.5, 41.9),
    datetime = c("2024-05-13 13:54:26", "2024-05-13 13:54:26", "2024-05-13 13:54:27", "2024-05-13 13:54:27", "2024-05-13 13:54:28")
  )
  expect_error(PneumatronDatabase(df), "Column datetime must be a POSIXct")
})

test_that("Initialization fails with non-data.table/data.frame input", {
  expect_error(PneumatronDatabase(data = 1:10), "Data must be a data.table, data.frame, or list")
})

test_that("Summary provides correct output for non-standard log pressure count", {
  data <- open_pneumatron_database("data-examples/example-v2.csv", "V2")

  expect_output(summary(data), "Pneumatron devices summary:")
  expect_output(summary(data), "Measurements with non-standard log pressure count:")
  expect_error(expect_output(summary(data), "Measurements that need to be checked:"))
  expect_error(expect_output(summary(data), "Interruption Warnings:"))
})

test_that("Summary provides correct output for interruption warnings", {
  data <- open_pneumatron_database("data-examples/example-v3.csv", "V3")

  expect_output(summary(data), "Pneumatron devices summary:")
  expect_output(summary(data), "Measurements with non-standard log pressure count:")
  expect_error(expect_output(summary(data), "Measurements that need to be checked:"))
  expect_output(summary(data), "Interruption Warnings:")
})

test_that("get_data provides correct output", {
  database <- open_pneumatron_database("data-examples/example-v4.csv")
  data <- get_data(database)

  expect_s3_class(data, "data.table")
})

test_that("get_data only works with PneumatronDatabase", {
  expect_error(get_data("database"), "unable to find an inherited method")
})
