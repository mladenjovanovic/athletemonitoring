prepare_nominal <- function(data,
                            athlete,
                            date,
                            variable,
                            value,
                            day_aggregate,
                            NA_session,
                            NA_day,
                            acute,
                            chronic,
                            partial,
                            rolling_fill,
                            rolling_estimators,
                            posthoc_estimators,
                            group_summary_estimators,
                            align_all,
                            iter) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  entries <- NULL
  missing_day <- NULL
  missing_entry <- NULL
  level <- NULL
  proportion <- NULL
  estimator <- NULL
  acute.missing_entry <- NULL
  chronic.missing_entry <- NULL
  acute.missing_day <- NULL
  chronic.missing_day <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  if (iter) {
    message("Preparing data...")
  }

  # Check if factor, then save levels
  flag_value_factor <- FALSE
  if (is.factor(data[[value]])) {
    flag_value_factor <- TRUE
    levels_value_factor <- levels(data[[value]])
  }

  # Extract the data
  data <- data.frame(
    athlete = data[[athlete]],
    date = data[[date]],
    variable = data[[variable]],
    value = as.character(data[[value]]),
    stringsAsFactors = FALSE
  )

  # Fill missing entries
  data$missing_entry <- is.na(data$value)
  data$value[data$missing_entry] <- NA_session

  # ------------------------------------
  data <- data %>%
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

  # Fill missing_entry that has NA values now
  data$missing_entry <- ifelse(
    is.na(data$missing_entry),
    FALSE,
    data$missing_entry
  )

  data <- data %>%
    # fill in the individual/variable start and stop days and remove excess
    dplyr::group_by(athlete, variable) %>%
    # Arrange/Sort
    dplyr::arrange(date) %>%
    # Tag missing day
    dplyr::mutate(missing_day = is.na(start_date)) %>%
    tidyr::fill(start_date, stop_date, .direction = "up")

  if (align_all == FALSE) {
    data <- data %>%
      dplyr::filter(date >= start_date & date <= stop_date)
  }

  data <- data %>%
    dplyr::select(-start_date, -stop_date) %>%
    # Fill in missing days
    dplyr::mutate(value = ifelse(missing_day, NA_day, value))

  # Create wide version
  data <- data %>%
    dplyr::group_by(athlete, date, variable) %>%
    dplyr::mutate(session = dplyr::row_number()) %>%
    tidyr::pivot_wider(
      id_cols = c(
        "athlete", "date",
        "variable", "session",
        "missing_entry", "missing_day"
      ),
      names_from = "value"
    )

  # Remove NA to maintain the ratio between levels
  if (is.na(NA_day) | is.na(NA_session)) {
    data$`NA` <- NULL
  }

  data[-(1:6)] <- ifelse(is.na(data[-(1:6)]), 0, 1)

  # Aggregate on a day level
  data <- tidyr::pivot_longer(
    data,
    cols = -(1:6),
    names_to = "level"
  ) %>%
    dplyr::group_by(athlete, date, variable, level) %>%
    # Aggregate to day value
    dplyr::summarise(
      entries = dplyr::n(),
      missing_entry = sum(missing_entry),
      missing_day = sum(missing_day),
      value = day_aggregate(value)
    )

  # Rolling function
  # =================
  roll_func <- function(value, date) {
    # Acute
    acute_df <- data.frame(
      zoo::rollapply(
        value,
        FUN = rolling_estimators,
        width = acute,
        partial = partial,
        fill = rolling_fill,
        align = "right"
      )
    )

    # Check is amount of data in 'value' is smaller than rolling window
    # Since zoo::rollapply will not return names estimators
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (length(value) < acute & partial == FALSE) {
      acute_df <- rolling_estimators(rep(rolling_fill, length(value)))
      acute_df <- as.data.frame(t(acute_df))
    }

    # When there is only one estimator, the zoo::rollapply screws up
    # the name
    colnames(acute_df) <- paste0("acute.", names(rolling_estimators(1:100)))

    # Chronic
    chronic_df <- data.frame(
      zoo::rollapply(
        value,
        FUN = rolling_estimators,
        width = chronic,
        partial = partial,
        fill = rolling_fill,
        align = "right"
      )
    )

    # Check is amount of data in 'value' is smaller than rolling window
    # Since zoo::rollapply will not return names estimators
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (length(value) < chronic & partial == FALSE) {
      chronic_df <- rolling_estimators(rep(rolling_fill, length(value)))
      chronic_df <- as.data.frame(t(chronic_df))
    }

    # When there is only one estimator, the zoo::rollapply screws up
    # the name
    colnames(chronic_df) <- paste0("chronic.", names(rolling_estimators(1:100)))

    # merge together
    data.frame(date = date, variable.value = value, acute_df, chronic_df)
  }

  if (iter) {
    message("Rolling...")
  }

  data <- data %>%
    dplyr::group_by(athlete, variable, level) %>%
    # Arrange/Sort
    dplyr::arrange(date) %>%
    # Generate rolling estimators
    dplyr::mutate(roll_func(value, date))

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(-value) %>%
    dplyr::relocate(athlete, date, variable, level, entries, missing_entry, missing_day) %>%
    dplyr::arrange(athlete, date, variable, level)

  # Apply post-hoc estimators
  data <- posthoc_estimators(data)

  # Create long  version
  data_long <- tidyr::pivot_longer(
    data,
    cols = -(1:7),
    names_to = "estimator",
    values_to = "value"
  )

  # ===================================
  # Group summaries

  if (iter) {
    message("Group summaries...")
  }

  # Group summary aggregator
  group_func <- function(x) {
    group_df <- data.frame(t(group_summary_estimators(x)))

    colnames(group_df) <- paste0("group.", colnames(group_df))
    return(group_df)
  }

  group_summary <- data_long %>%
    dplyr::group_by(date, variable, level, estimator) %>%
    dplyr::summarise(
      group_func(value)
    ) %>%
    dplyr::ungroup()

  # Missing data aggregator

  if (iter) {
    message("Missing data summaries...")
  }

  missing_summary_athlete <- data %>%
    dplyr::group_by(athlete, date, variable) %>%
    dplyr::summarise(
      missing_entry = missing_entry[1],
      missing_day = missing_day[1]
    ) %>%
    dplyr::group_by(athlete, variable) %>%
    dplyr::mutate(
      acute.missing_entry = zoo::rollapply(
        missing_entry,
        FUN = sum,
        width = acute,
        partial = partial,
        fill = rolling_fill,
        align = "right"
      ),
      chronic.missing_entry = zoo::rollapply(
        missing_entry,
        FUN = sum,
        width = chronic,
        partial = partial,
        fill = rolling_fill,
        align = "right"
      ),
      acute.missing_day = zoo::rollapply(
        missing_day,
        FUN = sum,
        width = acute,
        partial = partial,
        fill = rolling_fill,
        align = "right"
      ),
      chronic.missing_day = zoo::rollapply(
        missing_day,
        FUN = sum,
        width = chronic,
        partial = partial,
        fill = rolling_fill,
        align = "right"
      )
    ) %>%
    dplyr::ungroup()

  missing_summary_group <- missing_summary_athlete %>%
    dplyr::group_by(date, variable) %>%
    dplyr::summarise(
      missing_entry = sum(missing_entry),
      missing_day = sum(missing_day),
      acute.missing_entry = sum(acute.missing_entry),
      chronic.missing_entry = sum(chronic.missing_entry),
      acute.missing_day = sum(acute.missing_day),
      chronic.missing_day = sum(chronic.missing_day)
    ) %>%
    dplyr::ungroup()

  # Return factor levels
  if (flag_value_factor) {
    data$level <- factor(
      data$level,
      levels = unique(c(levels_value_factor, unique(data$level)))
    )

    data_long$level <- factor(
      data_long$level,
      levels = unique(c(levels_value_factor, unique(data_long$level)))
    )

    group_summary$level <- factor(
      group_summary$level,
      levels = unique(c(levels_value_factor, unique(group_summary$level)))
    )
  }

  if (iter) {
    message("Done!")
  }

  return(
    new_athletemonitoring(
      type = "nominal",
      data_wide = data,
      data_long = data_long,
      group_summary = group_summary,
      missing_summary_athlete = missing_summary_athlete,
      missing_summary_group = missing_summary_group,
      day_aggregate = day_aggregate,
      NA_session = NA_session,
      NA_day = NA_day,
      acute = acute,
      chronic = chronic,
      partial = partial,
      rolling_fill = rolling_fill,
      rolling_estimators = rolling_estimators,
      posthoc_estimators = posthoc_estimators,
      group_summary_estimators = group_summary_estimators,
      align_all = align_all
    )
  )
}
