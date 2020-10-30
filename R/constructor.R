new_athletemonitoring <- function(type,
                                  data_wide,
                                  data_long,
                                  group_summary,
                                  day_aggregate,
                                  NA_session,
                                  NA_day,
                                  acute,
                                  chronic,
                                  rolling_fill,
                                  rolling_estimators,
                                  posthoc_estimators,
                                  group_summary_estimators) {
  athletemonitoring_object <- list(
    type = type,
    data_wide = data_wide,
    data_long = data_long,
    group_summary = group_summary,
    day_aggregate = day_aggregate,
    NA_session = NA_session,
    NA_day = NA_day,
    acute = acute,
    chronic = chronic,
    rolling_fill = rolling_fill,
    rolling_estimators = rolling_estimators,
    posthoc_estimators = posthoc_estimators,
    group_summary_estimators = group_summary_estimators
  )

  class(athletemonitoring_object) <- "athletemonitoring"
  return(athletemonitoring_object)
}
