#' Class PneumatronData
#'
#' This class is designed for managing large datasets from Pneumatron devices efficiently.
#' It utilizes `data.table` for data manipulation, ensuring fast processing and easy handling of data.
#'
#' @importFrom methods setClass new
#' @importFrom data.table :=
#' @slot data data.table, represents the data stored in a data.table.
#' @slot file_path Character vector, represents the file path for the data file.
#' @slot data_format Character vector, specifies the Pneumatron data format of the data (e.g., V2, V3, V4).
#' @rdname PneumatronData
setClass(
  Class = "PneumatronData",
  slots = c(
    data = "data.table",
    file_path = "character",
    data_format = "character"
  )
)

#' @rdname PneumatronData
#' @param data data.table, represents the data stored in a data.table.
#' @param file_path Character vector, represents the file path for the data file.
#' @param data_format Character vector, specifies the Pneumatron data format of the data (e.g., V2, V3, V4).
PneumatronData <- function(data, file_path = NA_character_, data_format = NA_character_) {
  new("PneumatronData", data = data, file_path = file_path, data_format = data_format)
}

setMethod("initialize", "PneumatronData", function(.Object, data, file_path = NA_character_, data_format = NA_character_, ...) {
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

#' Get data from PneumatronData object
#'
#' This method retrieves the data stored in a PneumatronData object.
#'
#' @param x A PneumatronData object.
#' @return A data.table containing the data stored in the PneumatronData object.
#' @examples
#' \dontrun{
#' pneumatron_data <- PneumatronData(data = df)
#' data <- get_data(pneumatron_data)
#' }
#' @rdname get_data
#' @export
setGeneric("get_data", function(x) standardGeneric("get_data"))
#' @rdname get_data
#' @export
setMethod("get_data", "PneumatronData", function(x) x@data)

