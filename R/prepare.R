prepare <- function(data,
                    athlete,
                    date,
                    variable,
                    value,
                    day_aggregate = function(x) {
                      sum(x, na.rm = TRUE)
                    },
                    NA_day = NA,
                    NA_session = NA,
                    acute = 7,
                    chronic = 28,
                    rolling_fill = NA,
                    estimators = function(x) {
                        c(
                        "mean" = mean(x, na.rm = TRUE),
                        "sd" = stats::sd(x, na.rm = TRUE),
                        "conf" = sum(!is.na(x)) / length(x)
                      )
                    },
                    long_format = FALSE) {

  # Extract the data
  data <- data.frame(
    athlete = data[[athlete]],
    date = data[[date]],
    variable = data[[variable]],
    value = data[[value]]
  )

  # ------------------------------------
  data <- data %>%
    dplyr::group_by(athlete, variable, date) %>%

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

  # Get the overal start and stop date to impute missing days
  start_date <- min(data$date)
  stop_date <- max(data$date)

  # Create a sequence of day
  date_seq <- seq(start_date, stop_date, "days")

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
      FUN = estimators,
      width = acute,
      fill = rolling_fill,
      align = "right"))

    colnames(acute_df) <- paste0("acute.", colnames(acute_df))

    # Chronic
    chronic_df <- data.frame(
      zoo::rollapply(
        value,
        FUN = estimators,
        width = chronic,
        fill = rolling_fill,
        align = "right"))

    colnames(chronic_df) <- paste0("chronic.", colnames(chronic_df))

    # merge together
    data.frame(date = date, variable.value = value, acute_df, chronic_df)
  }

  data <- data %>%
    # fill in the individual/variable start and stop days and remove excess
    dplyr::group_by(athlete, variable) %>%
    tidyr::fill(start_date, stop_date, .direction = "up") %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    dplyr::select(-start_date, -stop_date) %>%

    # Fill in missing days
    # create new column, so we can see where the data is missing afterwards in the column value
    dplyr::mutate(impute = ifelse(is.na(value), NA_day, value)) %>%

    # Generate rolling estimators
    arrange(date) %>%
    summarise(
      roll_func(value, date)
    ) %>%
    ungroup() %>%
    arrange(athlete, date, variable)

  # Convert to long if indicated
  if (long_format == TRUE) {
    data <- pivot_longer(
      data,
      cols = -(1:3),
      names_to = "estimator",
      values_to = "value")
  }

  return(
  new_athletemonitoring(
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
)
}
