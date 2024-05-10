#' Class PneumatronData
#'
#' This class is designed for managing large datasets from Pneumatron devices efficiently.
#' It utilizes `data.table` for data manipulation, ensuring fast processing and easy handling of data.
#'
#' @slot file_path Character vector, represents the file path for the data file.
#' @slot data_format Character vector, specifies the Pneumatron data format of the data (e.g., V2, V3, V4).
#' @examples
#' # To convert a data.frame to PneumatronData, first ensure your data.frame includes
#' # the required columns: id, measure, log_line, and pressure. Here's an example:
#' df <- data.frame(
#'   id = c(1, 1, 1, 1, 1),
#'   measure = c(1, 2, 3, 4, 5),
#'   log_line = c(1, 2, 3, 4, 5),
#'   pressure = c(100, 40, 40.7, 41.5, 41.9)
#' )
#' # Now, convert the data.frame to a PneumatronData object.
#' pneumatron_data <- PneumatronData(df)
#' print(pneumatron_data)
#'
#' @export
PneumatronData <- setClass(
  Class = "PneumatronData",
  slots = c(
    file_path = "character",
    data_format = "character"
  ),
  prototype = prototype(
    file_path = NA_character_,
    data_format = NA_character_
  ),
  contains = c("data.table")
)

setMethod("initialize", "PneumatronData", function(.Object, data, ...) {
  required_columns <- c("id", "measure", "log_line", "pressure")

  # Check if 'data' is data.table, if not, try to convert it
  if (!inherits(data, "data.table")) {
    if (is.data.frame(data) || is.list(data)) {
      data.table::setDT(data)
    } else {
      stop("Data must be a data.table, data.frame, or list")
    }
  }

  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("At least one required column is missing:", paste(missing_cols, collapse = ", "))
  }

  .Object <- callNextMethod(.Object, data, ...)
  return(.Object)
})
