test_that("PneumatronData initializes correctly with valid data", {
  df <- data.frame(
    id = c(1, 1, 1, 1, 1),
    measure = 1:5,
    log_line = 1:5,
    pressure = c(100, 40, 40.7, 41.5, 41.9)
  )
  obj <- PneumatronData(df)
  expect_s4_class(obj, "PneumatronData")
})

test_that("Initialization fails with missing required columns", {
  df <- data.frame(
    id = c(1, 1, 1, 1, 1),
    measure = 1:5,
    # log_line is missing
    pressure = c(100, 40, 40.7, 41.5, 41.9)
  )
  expect_error(PneumatronData(df), "missing:")
})

test_that("Initialization fails with non-data.table/data.frame input", {
  expect_error(PneumatronData(data = 1:10), "Data must be a data.table, data.frame, or list")
})
