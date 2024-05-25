#' Class GasDischarged
#'
#' This class extends PneumatronDatabase to store the calculated gas discharged data.
#'
#' @slot data data.table, represents the calculated gas discharge data stored in a data.table.
#' @slot file_path Character vector, represents the file path for the data file.
#' @slot data_format Character vector, specifies the Pneumatron data format of the data (e.g., V2, V3, V4).
#' @rdname GasDischarged
setClass(
  Class = "GasDischarged",
  contains = "PneumatronDatabase"
)

#' @rdname GasDischarged
#' @param data data.table, represents the calculated gas discharge data stored in a data.table.
#' @param file_path Character vector, represents the file path for the data file.
#' @param data_format Character vector, specifies the Pneumatron data format of the data (e.g., V2, V3, V4).
GasDischarged <- function(data, file_path = NA_character_, data_format = NA_character_) {
  new("GasDischarged", data = data, file_path = file_path, data_format = data_format)
}

setMethod("initialize", "GasDischarged", function(.Object, data, file_path = NA_character_, data_format = NA_character_, ...) {
  # Ensure input data is a data.table
  if (!inherits(data, "data.table")) {
    if (is.data.frame(data)) {
      data <- data.table::as.data.table(data)
    } else {
      stop("Data must be a data.table or data.frame")
    }
  }

  required_columns <- c("id", "measure", "datetime", "gd_ul")
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("At least one required column is missing: ", paste(missing_cols, collapse = ", "))
  }

  .Object@data <- data
  .Object@file_path <- file_path
  .Object@data_format <- data_format

  return(.Object)
})
