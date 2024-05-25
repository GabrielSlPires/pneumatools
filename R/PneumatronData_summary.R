#' Summary method for PneumatronDatabase
#'
#' Provides a summary of the PneumatronDatabase object, including the number of measurements,
#' start and end times for each Pneumatron device, and identifies measurements that need
#' to be checked or removed.
#'
#' @param object A PneumatronDatabase object.
#' @examples
#' \dontrun{
#' database <- open_pneumatron_database("path/to/your/datafile.csv", data_format = "V4")
#' summary(database)
#' }
#' @export
setMethod("summary", "PneumatronDatabase", function(object) {
  # Prevent 'no visible binding for global variable ...' warnings by initializing to NULL
  # Reference: https://github.com/Rdatatable/data.table/issues/850
  . <- datetime <- .N <- id <- measure <- group <- start_time <- next_start_time <- time_between_measurements <- end_time <- interruption <-
    measurement_duration <- pressure_log_count <- measurement_count <- good_measurement_count <- good_time_between_measurements_count <-
    device_start_time <- device_end_time <- bad_measurement_count <- log_duration <- bad_time_between_measurements_count <- NULL

  # Calculate the minimum and maximum datetime for each ID
  summary_stats <- object@data[, .(
    start_time = min(datetime),
    end_time = max(datetime),
    pressure_log_count = .N
  ), by = .(id, measure, group)]

  # Additional calculations
  summary_stats[, next_start_time := data.table::shift(start_time, type = "lead"), by = id]
  summary_stats[, measurement_duration := as.numeric(end_time) - as.numeric(start_time)]
  summary_stats[, time_between_measurements := as.numeric(next_start_time) - as.numeric(start_time)]
  summary_stats[, log_duration := measurement_duration / pressure_log_count]

  # Final summary to check measurement times
  final_summary <- summary_stats[pressure_log_count == 120, .(
    good_measurement_count = sum(data.table::between(measurement_duration, 50, 70)),
    good_time_between_measurements_count = sum(
      data.table::between(time_between_measurements, 750, 1050),
      na.rm = TRUE
    ),
    device_start_time = min(start_time),
    device_end_time = max(end_time),
    measurement_count = .N
  ), by = id]
  final_summary[, bad_measurement_count := measurement_count - good_measurement_count]
  final_summary[, bad_time_between_measurements_count := measurement_count - good_time_between_measurements_count]

  # Print the final summary (id, device start and end time, measurement_count)
  cat("Pneumatron devices summary:\n")
  final_summary_output <- final_summary[, .(
    id,
    device_start_time,
    device_end_time,
    measurement_count
  )]
  print(final_summary_output)

  # Print measurements that will be removed
  removed_measurements <- summary_stats[pressure_log_count != 120, .(
    id,
    count = .N
  ), by = id]
  if (nrow(removed_measurements) > 0) {
    cat("\nMeasurements with non-standard log pressure count:\n")
    for (i in 1:nrow(removed_measurements)) {
      cat("Pneumatron", removed_measurements$id[i], "has", removed_measurements$count[i], "removed measurements.\n")
    }
  }

  # Print measurements that need to be checked
  bad_measurements <- final_summary[bad_measurement_count > 0, .(
    id,
    count = bad_measurement_count
  )]
  if (nrow(bad_measurements) > 0) {
    cat("\nMeasurements that need to be checked:\n")
    for (i in 1:nrow(bad_measurements)) {
      cat("Pneumatron", bad_measurements$id[i], "has", bad_measurements$count[i], "measurements that need to be checked.\n")
    }
  }

  # Print pneumatron interruptions
  interruptions <- summary_stats[time_between_measurements > 2000, .(
    id,
    count = .N
  ), by = id]
  if (nrow(interruptions) > 0) {
    cat("\nInterruption Warnings:\n")
    for (i in 1:nrow(interruptions)) {
      cat("Pneumatron", interruptions$id[i], "was interrupted for more than 30 minutes for", interruptions$count[i], "time(s).\n")
    }
  }

  # cat("\nRun pneumatron_data_diagnose() for a more complete report.\n")
  invisible(summary_stats)
})
