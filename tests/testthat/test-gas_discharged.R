test_that("GasDischarged class initializes correctly with valid data.table", {
  valid_data <- data.frame(
    id = 1:10,
    measure = rnorm(10),
    datetime = Sys.time() + 1:10,
    gd_ul = rnorm(10)
  )

  gas_discharged <- GasDischarged(data = valid_data)

  expect_s4_class(gas_discharged, "GasDischarged")
  expect_equal(gas_discharged@data, data.table::data.table(valid_data))
})

test_that("GasDischarged class throws error with invalid data type", {
  expect_error(GasDischarged(data = "not a data.table"), "Data must be a data.table or data.frame")
})

test_that("GasDischarged class throws error with missing required columns", {
  invalid_data <- data.frame(
    id = 1:10,
    measure = rnorm(10),
    datetime = Sys.time() + 1:10
  )

  expect_error(GasDischarged(data = invalid_data), "At least one required column is missing")
})


test_that("calculateGasDischarged calculates correctly with valid data", {
  pneumatron_db <- open_pneumatron_database("data-examples/example-v4.csv")

  gas_discharged <- calculateGasDischarged(pneumatron_db)

  expect_s4_class(gas_discharged, "GasDischarged")
  expect_true("gd_ul" %in% names(gas_discharged@data))
})

test_that("calculateGasDischarged handles fixed atmospheric pressure correctly", {
  pneumatron_db <- open_pneumatron_database("data-examples/example-v4.csv")

  gas_discharged <- calculateGasDischarged(pneumatron_db, fixed_p_atm = FALSE)

  expect_s4_class(gas_discharged, "GasDischarged")
  expect_true("gd_ul" %in% names(gas_discharged@data))
})

test_that("calculateGasDischarged handles fixed temperature correctly", {
  pneumatron_db <- open_pneumatron_database("data-examples/example-v4.csv")
  gas_discharged <- calculateGasDischarged(pneumatron_db, fixed_temp = FALSE)

  expect_s4_class(gas_discharged, "GasDischarged")
  expect_true("gd_ul" %in% names(gas_discharged@data))
})
