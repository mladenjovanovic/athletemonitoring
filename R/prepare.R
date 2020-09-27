#' Prepare Athlete Monitoring Data
#'
#' @param data Data frame
#' @param athlete Name of the column in the \code{data} where the athlete id or name is located
#' @param date Name of the column in the \code{data} where the date is located. \code{date} has
#'     to be of \code{Date} class
#' @param variable Name of the column in the \code{data} where the variable name is located
#' @param value Name of the column in the \code{data} where the value of \code{variable} is located
#' @param day_aggregate Function for aggregating multiple day entries. Defaults is \code{sum}
#' @param NA_session What value should be imputed for missing values in \code{value}? Default is \code{NA}
#' @param NA_day What value should be imputed for missing days? Default is \code{NA}
#' @param acute Duration of the acute rolling window. Default is 7
#' @param chronic Duration of the chronic rolling window. Default is 7
#' @param rolling_fill Value used to fill start of the rolling windows. Default is \code{NA}
#' @param rolling_estimators Function providing rolling estimators. See Details
#' @param posthoc_estimators Function providing post-hoc estimators. See Details
#' @param group_summary_estimators Function providing group summary estimators. See Details
#'
#' @return Object of class \code{athletemonitoring}
#' @export
#'
#' @examples
#' data("monitoring")
#'
#' monitoring$Date <- as.Date(monitoring$Date, "%Y-%m-%d")
#'
#' prepared_data <- prepare(
#'   data = monitoring,
#'   athlete = "Full Name",
#'   date = "Date",
#'   variable = "Variable",
#'   value = "Value")
prepare <- function(data,
                    athlete,
                    date,
                    variable,
                    value,
                    day_aggregate = function(x) {
                      sum(x, na.rm = TRUE)
                    },
                    NA_session = NA,
                    NA_day = NA,
                    acute = 7,
                    chronic = 28,
                    rolling_fill = NA,
                    rolling_estimators = function(x) {
                      c(
                        "mean" = mean(x, na.rm = TRUE),
                        "sd" = stats::sd(x, na.rm = TRUE),
                        "cv" = stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
                        "conf" = sum(!is.na(x)) / length(x)
                      )
                    },
                    posthoc_estimators = function(data) {
                      return(data)
                    },
                    group_summary_estimators = function(x) {
                      c(
                        "median" = stats::median(x, na.rm = TRUE),
                        "lower" = stats::quantile(x, 0.25, na.rm = TRUE)[[1]],
                        "upper" = stats::quantile(x, 0.75, na.rm = TRUE)[[1]]
                      )
                    }) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  estimator <- NULL
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
        FUN = rolling_estimators,
        width = acute,
        fill = rolling_fill,
        align = "right"
      )
    )

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
    dplyr::mutate(value = ifelse(is.na(value), NA_day, value)) %>%

    # Generate rolling estimators
    dplyr::arrange(date) %>%
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
