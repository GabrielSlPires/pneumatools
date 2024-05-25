#' Report Uptime for Pneumatron Devices
#'
#' A generic function to report the continuous uptime for each Pneumatron
#' device, considering measurements made at a specified interval (e.g., 15 min).
#' Any gap exceeding the interval is considered as downtime.
#'
#' @param object A PneumatronDatabase object.
#' @param lower_interval The expected lower interval between measurements in seconds.
#'   Defaults to 780 seconds (13 min).
#' @param upper_interval The expected upper interval between measurements in seconds.
#'   Defaults to 1020 seconds (17 min).
#' @return A data.table with the uptime information for each device.
#' @examples
#' \dontrun{
#' database <- open_pneumatron_database("path/to/your/datafile.csv")
#' uptime_summary <- reportUptime(database)
#' print(uptime_summary)
#' }
#' @rdname reportUptime
#' @export
setGeneric(
  "reportUptime",
  function(object, lower_interval = 780, upper_interval = 1020) {
    standardGeneric("reportUptime")
  }
)

#' @rdname reportUptime
#' @export
setMethod(
  "reportUptime", "PneumatronDatabase",
  function(object, lower_interval = 780, upper_interval = 1020) {
    # Prevent 'no visible binding for global variable ...' warnings by initializing to NULL
    # Reference: https://github.com/Rdatatable/data.table/issues/850
    . <- datetime <- .N <- id <- measure <- group <- start_time <- next_start_time <- time_between_measurements <- end_time <- interruption <- NULL

    summary_stats <- object@data[, .(
      start_time = min(datetime),
      end_time = max(datetime),
      pressure_log_count = .N
    ), by = .(id, measure, group)]

    summary_stats[, next_start_time := data.table::shift(start_time, type = "lead"), by = id]
    summary_stats[, time_between_measurements :=
      as.numeric(next_start_time) - as.numeric(start_time)]
    summary_stats[, interruption := data.table::rleid(
      data.table::between(time_between_measurements, lower_interval, upper_interval)
    ), by = id]

    uptime_summary <- summary_stats[, .(
      device_start_time = min(start_time),
      device_end_time = max(end_time),
      duration = sum(as.numeric(end_time - start_time)),
      n = .N
    ), by = .(id, interruption)]

    return(uptime_summary)
  }
)
