#' Calculate gas discharge from Pneumatron output
#'
#' This method calculates the gas discharge from the Pneumatron database.
#'
#' @param object A PneumatronDatabase object.
#' @param pressure_interval Interval to calculate pressure difference in seconds. Defauts 15s.
#' @param reservoir Tubing volume of the reservoir in mL.
#' @param p_atm Fixed atmospheric pressure in kPa, used if fixed_p_atm is TRUE.
#' @param fixed_p_atm Logical, if TRUE uses a fixed atmospheric pressure for calculations, otherwise uses pressure from data.
#' @param temp Fixed temperature in Celsius, used if fixed_temp is TRUE.
#' @param fixed_temp Logical, if TRUE uses a fixed temperature for calculations, otherwise uses temperature from data. Defauts is TRUE.
#' @param R Ideal gas law constant.
#' @return A GasDischarged object with calculated gas discharge values.
#' @examples
#' \dontrun{
#' database <- open_pneumatron_database("path/to/your/data.csv")
#' gas_discharged <- calculateGasDischarged(database)
#' }
#' @rdname calculate-gas-discharged
#' @export
setGeneric("calculateGasDischarged", function(object, pressure_interval = 15, reservoir = 2.6, p_atm = 101.3, fixed_p_atm = TRUE, temp = 25, fixed_temp = TRUE,  R = 8.3144621) {
  standardGeneric("calculateGasDischarged")
})

#' @rdname calculate-gas-discharged
#' @export
setMethod("calculateGasDischarged", "PneumatronDatabase", function(object, pressure_interval = 15, reservoir = 2.6, p_atm = 101.3, fixed_p_atm = TRUE, temp = 25, fixed_temp = TRUE,  R = 8.3144621) {
  # Prevent 'no visible binding for global variable ...' warnings by initializing to NULL
  # Reference: https://github.com/Rdatatable/data.table/issues/850
  p_atm_calc <- sensor_p_atm <- temp_calc <- sensor_temp <- pi <- pf <- temp1 <-
    pressure_diff <- gd_mol <- gd_ul <- n_mol <- NULL

  reservoir_volume <- reservoir * 10^-6
  data <- get_data(object)
  data_prepared <- prepare_data_for_gas_discharged(data, pressure_interval)

  if (fixed_p_atm) { # version == "Multiple", fixed_p_atm alway TRUE
    data_prepared[, p_atm_calc := p_atm]
  } else {
    data_prepared[, p_atm_calc := sensor_p_atm]
  }

  if (fixed_temp) {
    data_prepared[, temp_calc := temp + 273.15]
  } else {
    data_prepared[, temp_calc := sensor_temp + 273.15]
  }

  # perform gas discharge calculus
  data_prepared[, pressure_diff := abs(pi - pf)]
  data_prepared[, gd_mol := (pressure_diff * 100 * reservoir_volume) / (R * temp_calc)]
  data_prepared[, gd_ul := (gd_mol * R * temp_calc / (p_atm_calc * 100)) * 1e+09]
  data_prepared[, c := pressure_diff / (R * temp_calc)]
  data_prepared[, n_mol := (p_atm_calc * 1000 * reservoir_volume) / (R * temp_calc)]

  # remove calculus variables
  data_prepared[, temp_calc := NULL]
  data_prepared[, p_atm_calc := NULL]

  gas_discharged <- GasDischarged(data = data_prepared, file_path = object@file_path, data_format = object@data_format)

  return(gas_discharged)
})

#' Prepare data for gas discharged calculations
#'
#' This function is used internally for preparing data for gas discharge calculations. It is not exported for end user.
#' It prepares and formats the data by filtering and calculating necessary parameters for the discharge calculation.
#'
#' @param data Pneumatron output object, or a data frame with properly named columns.
#' @param pressure_interval Final pressure time in seconds.
#' @return A data table prepared for gas discharge calculations.
prepare_data_for_gas_discharged <- function(data, pressure_interval) {
  # Prevent 'no visible binding for global variable ...' warnings by initializing to NULL
  # Reference: https://github.com/Rdatatable/data.table/issues/850
  log_line_count <- .N <- id <- measure <- group <- . <- datetime <-
    log_line <- temp1 <- pressure <- NULL

  # filter gas discharged measurements with less than required pressure logs
  data <- data[!is.na(id)]
  data <- data[!is.na(measure)]
  data <- data[!is.na(group)]
  data <- data[!is.na(pressure)]
  data[, log_line_count := .N, by = list(id, measure, group)]
  data <- data[log_line_count >= pressure_interval*2]

  data_prepared <- data[, .(
    datetime = datetime[which(log_line == 1)],
    sensor_temp = temp1[which(log_line == 1)],
    sensor_p_atm = pressure[which(log_line == 1)],
    log_line_count = .N,
    initial_pressure_log_line = initial_pressure(log_line, pressure),
    # pi = pressure[which.min(abs(log_line - (initial_pressure(log_line, pressure) + 1)))], # test difference with and without +1
    pi = pressure[which.min(abs(log_line - (initial_pressure(log_line, pressure))))],
    pf = pressure[which.min(abs(log_line - (initial_pressure(log_line, pressure) + pressure_interval*2)))]
  ), by = list(id, measure, group)] # I used to have a grouping by day

  return(data_prepared)
}

#' Get initial pressure for gas discharge calculation
#'
#' Returns the position of the lowest pressure, which is the initial pressure, necessary for gas discharge calculations.
#' This function is used internally and is not exported for end users.
#'
#' @param log_line Vector of integers representing the log line of measured pressures.
#' @param pressure Vector of measured pressures from the Pneumatron in kPa.
#' @return Integer position of the lowest pressure in the sequence.
#' @details This function finds the minimum pressure point before a specified threshold in the log line sequence to define the initial pressure point for calculations.
initial_pressure <- function(log_line, pressure) {
  if (length(log_line) != length(pressure)) return(NA)
  if (length(log_line) < 10) return(NA)

  initial_p_log <- tryCatch({
    log_index <- which.min(pressure[1:7])
    log_line[log_index]
  },
  error = function(e) 3,
  warning = function(w) 3)

  return(initial_p_log)
}
