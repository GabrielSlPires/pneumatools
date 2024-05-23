#' Report Uptime for Pneumatron Devices
#'
#' A generic function to reports the continuous uptime for each Pneumatron
#' device, considering measurements made at a specified interval (e.g. 15min).
#' Any gap exceeding the interval is considered as downtime.
#'
#' @param object A PneumatronData object.
#' @param lower_interval The expected interval between measurements in seconds.
#'   Defaults to 780 seconds (13 min).
#' @param upper_interval The expected interval between measurements in seconds.
#'   Defaults to 1020 seconds (17 min).
#' @return A data.table with the uptime information for each device.
#' @examples
#' data <- open_pneumatron_data("path/to/your/datafile.csv")
#' uptime_summary <- reportUptime(pneumatron_data)
#' print(uptime_summary)
#' @export
setGeneric(
  "reportUptime",
  function(
      object,
      lower_interval = 750,
      upper_interval = 1050) {
    standardGeneric("reportUptime")
  }
)

#' @export
setMethod(
  "reportUptime", "PneumatronData",
  function(object, lower_interval = 750, upper_interval = 1050) {
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
