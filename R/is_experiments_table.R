#' Check if a Data Frame is in Pneumatron Experiments Format
#'
#' This function checks if a given data frame conforms to the expected format for Pneumatron experiments.
#'
#' @param x A data frame to be checked.
#'
#' @return A logical value indicating whether the data frame is in the Pneumatron experiments format.
#' @details
#' The function verifies that the input data frame has the required columns:
#' "id", "pneumatron_id", "start_datetime", "end_datetime", and "pneumatron_file".
#' Additionally, it checks that:
#' - "id" is numeric
#' - "pneumatron_id" is numeric
#' - "start_datetime" is of class POSIXct
#' - "end_datetime" is of class POSIXct
#' - "pneumatron_file" is a character vector
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#' id = 1:5,
#' pneumatron_id = 1:5,
#' start_datetime = as.POSIXct(Sys.time() + 1:5),
#' end_datetime = as.POSIXct(Sys.time() + 6:10),
#' pneumatron_file = c("file1.csv", "file2.csv", "file3.csv", "file4.csv", "file5.csv")
#' )
#' is_experiments_table(df) # returns TRUE
#' }
#' @export
is_experiments_table <- function(x) {
  if (!inherits(x, "data.frame")) return(FALSE)
  required_colnames <- c(
    "id", "pneumatron_id", "start_datetime", "end_datetime", "pneumatron_file"
  )
  check_cols <- length(setdiff(required_colnames, colnames(x))) == 0
  check_id <- is.numeric(x$id)
  check_pneumatron_id <- is.numeric(x$pneumatron_id)
  check_start_datetime <- inherits(x$start_datetime, "POSIXct")
  check_end_datetime <- inherits(x$end_datetime, "POSIXct")
  check_pneumatron_file <- is.character(x$pneumatron_file)
  return(all(
    check_cols,
    check_id,
    check_pneumatron_id,
    check_start_datetime,
    check_end_datetime,
    check_pneumatron_file
  ))
}


