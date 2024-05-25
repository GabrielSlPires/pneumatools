#' Class PneumatronDatabase
#'
#' This class is designed for managing large datasets from Pneumatron devices efficiently.
#' It utilizes `data.table` for data manipulation, ensuring fast processing and easy handling of data.
#'
#' @importFrom methods setClass new
#' @importFrom data.table :=
#' @slot data data.table, represents the data stored in a data.table.
#' @slot file_path Character vector, represents the file path for the data file.
#' @slot data_format Character vector, specifies the Pneumatron data format of the data (e.g., V2, V3, V4).
#' @rdname PneumatronDatabase
setClass(
  Class = "PneumatronDatabase",
  slots = c(
    data = "data.table",
    file_path = "character",
    data_format = "character"
  )
)

#' @rdname PneumatronDatabase
#' @param data data.table, represents the data stored in a data.table.
#' @param file_path Character vector, represents the file path for the data file.
#' @param data_format Character vector, specifies the Pneumatron data format of the data (e.g., V2, V3, V4).
PneumatronDatabase <- function(data, file_path = NA_character_, data_format = NA_character_) {
  new("PneumatronDatabase", data = data, file_path = file_path, data_format = data_format)
}

setMethod("initialize", "PneumatronDatabase", function(.Object, data, file_path = NA_character_, data_format = NA_character_, ...) {
  # Ensure input data is a data.table
  if (!inherits(data, "data.table")) {
    if (is.data.frame(data)) {
      data <- data.table::as.data.table(data)
    } else {
      stop("Data must be a data.table, data.frame, or list")
    }
  }

  # Ensure datetime column is of POSIXct type
  if (!inherits(data[["datetime"]], "POSIXct")) {
    stop("Column datetime must be a POSIXct")
  }

  required_columns <- c("id", "measure", "log_line", "pressure", "datetime")
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("At least one required column is missing: ", paste(missing_cols, collapse = ", "))
  }

  .Object@data <- data
  .Object@file_path <- file_path
  .Object@data_format <- data_format

  return(.Object)
})

#' Get data from PneumatronDatabase object
#'
#' This method retrieves the data stored in a PneumatronDatabase object.
#'
#' @param x A PneumatronDatabase object.
#' @return A data.table containing the data stored in the PneumatronDatabase object.
#' @examples
#' \dontrun{
#' database <- open_pneumatron_database("path/to/your/datafile.csv")
#' data <- get_data(data)
#' }
#' @rdname get_data
#' @export
setGeneric("get_data", function(x) standardGeneric("get_data"))
#' @rdname get_data
#' @export
setMethod("get_data", "PneumatronDatabase", function(x) x@data)

