prepare_numeric <- function(data,
                            athlete,
                            date,
                            variable,
                            value,
                            day_aggregate,
                            NA_session,
                            NA_day,
                            acute,
                            chronic,
                            rolling_fill,
                            rolling_estimators,
                            posthoc_estimators,
                            group_summary_estimators) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  estimator <- NULL
  missing_day <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  # Extract the data
  data <- data.frame(
    athlete = data[[athlete]],
    date = data[[date]],
    variable = data[[variable]],
    value = data[[value]]
  )

  # ------------------------------------
  data <- data %>%
    dplyr::group_by(athlete, date, variable) %>%

    # Fill in missing sessions
    dplyr::mutate(value = ifelse(is.na(value), NA_session, value)) %>%
    # Aggregate to day value
    dplyr::summarise(value = day_aggregate(value)) %>%

    # Get start and stop dates for every athlete and variable
    dplyr::group_by(athlete, variable) %>%
    dplyr::mutate(
      start_date = min(date),
      stop_date = max(date)
    )

  # Get the overall start and stop date to impute missing days
  start_date <- min(data$date)
  stop_date <- max(data$date)

  # Create a sequence of day
  if (is.numeric(data$date)) {
    date_seq <- seq(start_date, stop_date)
  } else {
    date_seq <- seq(start_date, stop_date, "days")
  }

  # Generate a continuous df so that there are no missing days for every athlete
  data <- merge(
    data,
    expand.grid(
      date = date_seq,
      athlete = unique(data$athlete),
      variable = unique(data$variable)
    ),
    all = TRUE
  )

  # Rolling function
  roll_func <- function(value, date) {
    # Acute
    acute_df <- data.frame(
      zoo::rollapply(
        value,
        FUN = rolling_estimators,
        width = acute,
        fill = rolling_fill,
        align = "right"
      )
    )

    # Check is amount of data in 'value' is smaller than rolling window
    # Since zoo::rollapply will not return names estimators
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (length(value) < acute) {
      acute_df <- rolling_estimators(rep(rolling_fill, length(value)))
      acute_df <- as.data.frame(t(acute_df))
    }

    colnames(acute_df) <- paste0("acute.", colnames(acute_df))

    # Chronic
    chronic_df <- data.frame(
      zoo::rollapply(
        value,
        FUN = rolling_estimators,
        width = chronic,
        fill = rolling_fill,
        align = "right"
      )
    )

    # Check is amount of data in 'value' is smaller than rolling window
    # Since zoo::rollapply will not return names estimators
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (length(value) < chronic) {
      chronic_df <- rolling_estimators(rep(rolling_fill, length(value)))
      chronic_df <- as.data.frame(t(chronic_df))
    }

    colnames(chronic_df) <- paste0("chronic.", colnames(chronic_df))

    # merge together
    data.frame(date = date, variable.value = value, acute_df, chronic_df)
  }

  data <- data %>%
    # fill in the individual/variable start and stop days and remove excess
    dplyr::group_by(athlete, variable) %>%

    # Arrange/Sort
    dplyr::arrange(date) %>%
    # Tag missing day
    dplyr::mutate(missing_day = is.na(start_date)) %>%

    tidyr::fill(start_date, stop_date, .direction = "up") %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    dplyr::select(-start_date, -stop_date) %>%

    # Fill in missing days
    dplyr::mutate(value = ifelse(missing_day, NA_day, value)) %>%
    dplyr::select(-missing_day) %>%

    # Generate rolling estimators
    dplyr::summarise(
      roll_func(value, date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(athlete, date, variable) %>%
    dplyr::arrange(athlete, date, variable)

  # Apply post-hoc estimators
  data <- posthoc_estimators(data)

  # Create long  version
  data_long <- tidyr::pivot_longer(
    data,
    cols = -(1:3),
    names_to = "estimator",
    values_to = "value"
  )

  # ===================================
  # Group summaries

  # Group summary aggregator
  group_func <- function(x) {
    group_df <- data.frame(t(group_summary_estimators(x)))

    colnames(group_df) <- paste0("group.", colnames(group_df))
    return(group_df)
  }

  group_summary <- data_long %>%
    dplyr::group_by(date, variable, estimator) %>%
    dplyr::summarise(
      group_func(value)) %>%
    dplyr::ungroup()

  return(
    new_athletemonitoring(
      type = "numeric",
      data_wide = data,
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
  )

}
