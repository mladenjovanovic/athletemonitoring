new_athletemonitoring <- function(data,
                                  day_aggregate,
                                  NA_day,
                                  NA_session,
                                  acute,
                                  chronic,
                                  rolling_fill,
                                  estimators,
                                  long_format) {

  athletemonitoring_object <- list(
    data = data,
    day_aggregate = day_aggregate,
    NA_day = NA_day,
    NA_session = NA_session,
    acute = acute,
    chronic = chronic,
    rolling_fill = rolling_fill,
    estimators = estimators,
    long_format = long_format
  )

  class(athletemonitoring_object) <- "athletemonitoring"
  return(athletemonitoring_object)
}
