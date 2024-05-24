#' Open Pneumatron Data from File
#'
#' Reads Pneumatron measurement data from a CSV file using the specified data format.
#' Defaults to format V4 if no format is specified.
#'
#' @param file_path The path to the CSV file.
#' @param data_format The format of the data file ('V2', 'V2-update', 'V3', 'V3-old', 'V4').
#' Defaults to 'V4'.
#' @return An object of class PneumatronData.
#' @examples
#' \dontrun{
#' pneumatron_data <- open_pneumatron_data("path/to/your/datafile.csv", data_format = "V4")
#' }
#' @export
open_pneumatron_data <- function(file_path, data_format = "V4") {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string.")
  }
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  data <- switch(data_format,
    "V2" = read_format_v2(file_path),
    "V2-update" = read_format_v2_update(file_path),
    "V3" = read_format_v3(file_path),
    "V3-old" = read_format_v3_old(file_path),
    "V4" = read_format_v4(file_path),
    stop("Unsupported data format provided")
  )

  PneumatronData(data, file_path = file_path, data_format = data_format)
}

#' Clean Corrupted Data
#'
#' Reads a Pneumatron measurement data file and filters out lines that do not match a specified regular expression pattern.
#' This function is intended to clean corrupted data files by removing lines that are incorrectly formatted.
#'
#' @param file_path The path to the data file.
#' @param allowed_regex The regular expression pattern that valid lines must match.
#' @return A vector of strings, where each string is a line from the file that matches the allowed regular expression.
clean_corrupted_data <- function(file_path, allowed_regex) {
  if (!file.exists(file_path)) {
    stop("The specified file does not exist: ", file_path)
  }

  con <- file(file_path, "r")
  lines_from_file <- readLines(con)
  close(con)

  matched_lines <- grepl(allowed_regex, lines_from_file)

  if (sum(!matched_lines) > 0) {
    message("\nRemoved ", sum(!matched_lines), " line(s) with different format than expected")
  }

  return(lines_from_file[matched_lines])
}

#' Detect the Format of Pneumatron Data File
#'
#' Automatically detects the format of a Pneumatron data file by analyzing the header of the file.
#' The function reads only the first row to determine the number of columns and checks for specific headers
#' to infer the format version of the file.
#'
#' @param file_path The path to the CSV file whose format needs to be detected.
#' @return A character string indicating the detected data format ('V2', 'V2-update', 'V3', 'V3-old', 'V4').
#' If the format cannot be detected or is unsupported, the function will stop with an error message.
#' @examples
#' \dontrun{
#' format <- detect_data_format("path/to/your/datafile.csv")
#' }
#' @export
detect_data_format <- function(file_path) {
  # Some files had a rownames column
  file_content <- data.table::fread(file_path, nrows = 1)
  if (ncol(file_content) == 18) {
    return("V2")
  } else if (ncol(file_content) == 13) {
    return("V3")
  } else if (ncol(file_content) == 11) {
    return("V4")
  } else if (ncol(file_content) == 6) {
    return("V2-update")
  } else if (ncol(file_content) == 12) {
    return("V3-old")
  } else if (names(file_content)[1] == "V1" & ncol(file_content) == 19) {
    return("V2")
  } else {
    stop("Unsupported data format provided")
  }
}
