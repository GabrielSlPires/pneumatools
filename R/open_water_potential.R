#' Open and Parse Water Potential Data
#'
#' Reads a water potential dataset from a CSV file, parses the timestamp column,
#' filters out rows with missing IDs, and returns a cleaned data table.
#'
#' @param file_path Character. The path to the CSV file to be read.
#' @param time_parser Function. A function from `lubridate` to parse the `time` column
#' (e.g., `lubridate::dmy_hm`, `lubridate::ymd_hms`). Default is `lubridate::dmy_hm`.
#'
#' @return A data table with parsed time column and rows without `id` values removed,
#' sorted by time in ascending order.
#'
#' @examples
#' \dontrun{
#' water <- open_water_potential("data/experiment_1.csv")
#' water <- open_water_potential("data/experiment_1.csv", lubridate::ymd_hms)
#' }
#' @import data.table
#' @import lubridate
#' @export
open_water_potential <- function(file_path, time_parser = lubridate::dmy_hm) {
  df <- data.table::fread(file_path, fill = TRUE)

  # Ensure column names are 'id', 'time', and 'pot'
  if (!all(c("id", "time", "pot") %in% colnames(df))) {
    stop("Columns 'id', 'time', and 'pot' are required in the dataset.")
  }

  df$time <- time_parser(df$time)
  df <- df[!is.na(id)]  # Filter rows where 'id' is not NA

  # Check if 'pot' is always negative
  if (any(df$pot >= 0, na.rm = TRUE)) {
    warning("Some values in 'pot' are not negative.")
  }

  data.table::setorder(df, time)  # Order data by time in ascending order
  return(df)
}
