#' Read Data Formats for Pneumatron
#'
#' This collection of functions provides various methods for reading and processing
#' data from different versions of the Pneumatron data formats, each equipped with specific hardware configurations.
#' These functions support CSV file inputs and utilize the data.table package for data manipulation.
#'
#' @param file_path The path to the CSV file to be read.
#' @return A data.table containing the processed data.
#'
#' @details
#' - `read_format_v2`: Processes data from the grey Pneumatron equipped with an Arduino and a relative pressure sensor with ADS. It also has other environmental sensors.
#' - `read_format_v2_update`: Reads an updated version of format V2 focusing on essential data columns such as pressure, id, measure, and log_line.
#' - `read_format_v3_old`: Manages data from the white Pneumatron using an older COM-port reading script with an ESP8266.
#' - `read_format_v3`: Handles data from the white Pneumatron format V3 with an ESP8266 and enviromental sensors, implementing cleaning methods for corrupted files.
#' - `read_format_v4`: Designed for the newest Pneumatron format V4 with an ESP8266 and an absolute pressure sensor, also includes methods for cleaning corrupted files.
#'
#' Each function adapts to the specifics of the data format version it is designed to read, handling key data fields,
#' converting data types, and applying specific data cleaning procedures as necessary.
#' @rdname data-readers
read_format_v2 <- function(file_path) {
  # Avoid 'no visible binding for global variable ...' by initializing to NULL
  # https://github.com/Rdatatable/data.table/issues/850
  pressure <- group <- NULL

  # Some files had a rownames column
  preview <- data.table::fread(file_path, nrows = 1)
  col_index <- if (dim(preview)[2] == 19) 2:19 else 1:18

  data <- data.table::fread(file_path, blank.lines.skip = TRUE, select = col_index, col.names = c(
    "id", "ms", "temp1", "atm_pres1", "humid1", "temp2", "atm_pres2", "humid2",
    "seq", "measure", "log_line", "pressure", "pressure2", "co2", "voc",
    "co2_cozir", "light", "datetime"
  ))
  # Adjust pressure from relative to absolute values
  data[, pressure := 101.325 - pressure]
  data[, group := 0]
  return(data)
}

#' @rdname data-readers
read_format_v2_update <- function(file_path) {
  # Avoid 'no visible binding for global variable ...' by initializing to NULL
  # https://github.com/Rdatatable/data.table/issues/850
  pressure <- group <- temp1 <- NULL

  data <- data.table::fread(file_path, blank.lines.skip = TRUE, select = 1:6, col.names = c(
    "id", "seq", "measure", "log_line", "pressure", "datetime"
  ))
  # Adjust pressure from relative to absolute values
  data[, pressure := 101.325 - pressure]
  data[, group := 0]
  data[, temp1 := 0]
  return(data)
}

#' @rdname data-readers
read_format_v3_old <- function(file_path) {
  # Avoid 'no visible binding for global variable ...' by initializing to NULL
  # https://github.com/Rdatatable/data.table/issues/850
  datetime <- voltage <- raw <- group <- NULL

  data <- data.table::fread(file_path, blank.lines.skip = TRUE, header = FALSE, select = c(1, 2), col.names = c("raw", "datetime"))
  # Parse raw data into structured columns
  data[, c("id", "ms", "temp1", "pressure", "humid1", "temp2", "atm_pres2", "humid2", "seq", "measure", "log_line", "voltage") :=
    data.table::tstrsplit(raw, ",", fixed = TRUE)]
  # Convert strings to appropriate data types
  data[, datetime := lubridate::ymd_hms(datetime)]
  data[, voltage := as.numeric(voltage)]
  # Clean up raw column
  data[, raw := NULL]
  data[, group := 0]
  return(data[!is.na(datetime), ])
}

#' @rdname data-readers
read_format_v3 <- function(file_path) {
  # Avoid 'no visible binding for global variable ...' by initializing to NULL
  # https://github.com/Rdatatable/data.table/issues/850
  pressure <- group <- NULL

  allowed_regex <- "^(-?[0-9]+,){2}([0-9]+\\.[0-9]+,){6}([0-9]+,){3}[0-9]\\.[0-9]+,[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}"
  data <- tryCatch(
    {
      data.table::fread(file_path, blank.lines.skip = TRUE, select = 1:13, col.names = c(
        "id", "ms", "temp1", "pressure", "humid1", "temp2", "pressure2", "humid2",
        "seq", "measure", "log_line", "volt", "datetime"
      ))
    },
    warning = function(w) {
      if (grepl("Stopped early on line", w$message)) {
        message("Warning captured: ", w$message)
        message("Attempting to clean data...")
        lines <- clean_corrupted_data(file_path, allowed_regex)
        return(data.table::fread(file_path, blank.lines.skip = TRUE, select = 1:13, col.names = c(
          "id", "ms", "temp1", "pressure", "humid1", "temp2", "pressure2", "humid2",
          "seq", "measure", "log_line", "volt", "datetime"
        )))
      } else {
        message("Unhandled warning: ", w$message)
      }
    }
  )
  # Apply unit conversion to pressure
  data[, pressure := pressure / 10]
  data[, group := 0]
  return(data)
}

#' @rdname data-readers
read_format_v4 <- function(file_path) {
  allowed_regex <- "^(-?[0-9]+,){5}(([0-9]+\\.[0-9]+,){4}v3),[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}"
  data <- tryCatch(
    {
      data.table::fread(file_path, blank.lines.skip = TRUE, select = 1:11, col.names = c(
        "id", "group", "seq", "measure", "log_line", "humid", "pressure", "temp1",
        "volt", "version", "datetime"
      ))
    },
    warning = function(w) {
      if (grepl("Stopped early on line", w$message)) {
        message("Warning captured: ", w$message)
        message("Attempting to clean data...")
        lines <- clean_corrupted_data(file_path, allowed_regex)
        return(data.table::fread(text = lines, blank.lines.skip = TRUE, select = 1:11, col.names = c(
          "id", "group", "seq", "measure", "log_line", "humid", "pressure", "temp1",
          "volt", "version", "datetime"
        )))
      } else {
        message("Unhandled warning: ", w$message)
      }
    }
  )
  return(data)
}
