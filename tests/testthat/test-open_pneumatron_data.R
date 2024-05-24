# Tests for open_pneumatron_data function
test_that("open_pneumatron_data handles invalid file path", {
  expect_error(open_pneumatron_data("nonexistent.csv", "V4"))
})

test_that("open_pneumatron_data handles main formats correctly", {
  expect_s4_class(open_pneumatron_data("data-examples/example-v4.csv", "V4"), "PneumatronData")
  expect_s4_class(open_pneumatron_data("data-examples/example-v3.csv", "V3"), "PneumatronData")
  expect_s4_class(open_pneumatron_data("data-examples/example-v2.csv", "V2"), "PneumatronData")
})

# Test correct format detection
test_that("Correct format is detected", {
  expect_equal(detect_data_format("data-examples/example-v2.csv"), "V2")
  expect_equal(detect_data_format("data-examples/example-v3.csv"), "V3")
  expect_equal(detect_data_format("data-examples/example-v4.csv"), "V4")
})

# Test unsupported format error
test_that("Unsupported format gives an error", {
  test_temp_dir <- tempdir()
  write.csv(data.frame(matrix(ncol = 25, nrow = 1)), file.path(test_temp_dir, "unsupported.csv"), row.names = FALSE)
  expect_error(detect_data_format(file.path(test_temp_dir, "unsupported.csv")), "Unsupported data format provided")
  unlink(test_temp_dir, recursive = TRUE)
})

# Test file not found error
test_that("File not found gives an error", {
  expect_error(detect_data_format("nonexistent.csv"), "does not exist")
})

# Test open multiple files
test_that("File not found gives an error", {
  expect_error(open_pneumatron_data(c("data-examples/example-v2.csv", "data-examples/example-v3.csv")), "file_path must be a single character string")
})
