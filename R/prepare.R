#' Prepare Athlete Monitoring Data
#'
#' @param data Data frame
#' @param athlete Name of the column in the \code{data} where the athlete id or name is located
#' @param date Name of the column in the \code{data} where the date is located. \code{date} has
#'     to be either \code{Date} or \code{numeric} class
#' @param variable Name of the column in the \code{data} where the variable name is located
#' @param value Name of the column in the \code{data} where the value of \code{variable} is located
#' @param day_aggregate Function for aggregating multiple day entries. Defaults is \code{sum}
#' @param NA_session What value should be imputed for missing values in \code{value}? Default is \code{NA}
#' @param NA_day What value should be imputed for missing days? Default is \code{NA}
#' @param acute Duration of the acute rolling window. Default is 7
#' @param chronic Duration of the chronic rolling window. Default is 28
#' @param rolling_fill Value used to fill start of the rolling windows. Default is \code{NA}
#' @param rolling_estimators Function providing rolling estimators. See Details
#' @param posthoc_estimators Function providing post-hoc estimators. See Details
#' @param group_summary_estimators Function providing group summary estimators. See Details
#'
#' @return Object of class \code{athletemonitoring}
#' @export
#'
#' @examples
#' # Load monitoring data set
#' data("monitoring")
#'
#' # Filter out only 'Training Load'
#' monitoring <- monitoring[monitoring$Variable == "Training Load", ]
#'
#' # Convert column to date format (or use numeric)
#' monitoring$Date <- as.Date(monitoring$Date, "%Y-%m-%d")
#'
#' # Run the athlete monitoring data preparation
#' prepared_data <- prepare(
#'   data = monitoring,
#'   athlete = "Full Name",
#'   date = "Date",
#'   variable = "Variable",
#'   value = "Value",
#'   acute = 7,
#'   chronic = 42,
#'
#'   # How should be missing entry treated?
#'   # What do we assume? Zero load? Let's keep NA
#'   NA_session =  NA,
#'
#'   # How should missing days (i.e. no entries) be treated?
#'   # Here we assume no training, hence zero
#'   NA_day = 0,
#'
#'   # How should be multiple day entries summarised?
#'   # With "load", it is a "sum", witho other metrics that
#'   # do not aggregate, it can me "mean"
#'   day_aggregate = function(x) {
#'     sum(x, na.rm = TRUE)
#'   },
#'
#'   # Rolling estimators for Acute and Chronic windows
#'   rolling_estimators = function(x) {
#'     c(
#'       "mean" = mean(x, na.rm = TRUE),
#'       "sd" = sd(x, na.rm = TRUE),
#'       "cv" = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
#'     )
#'   },
#'
#'   # Additional estimator post-rolling
#'   posthoc_estimators = function(data) {
#'     data$ACD <- data$acute.mean - data$chronic.mean
#'     data$ACR <- data$acute.mean / data$chronic.mean
#'     data$ES <- data$ACD / data$chronic.sd
#'
#'     # Make sure to return the data
#'     return(data)
#'   },
#'
#'   # Group summary estimators
#'   group_summary_estimators = function(x) {
#'     c(
#'       "median" = median(x, na.rm = TRUE),
#'       "lower" = quantile(x, 0.25, na.rm = TRUE)[[1]],
#'       "upper" = quantile(x, 0.75, na.rm = TRUE)[[1]]
#'     )
#'   }
#' )
#'
#' # Get summary
#' prepared_data
#' summary(prepared_data)
#'
#'
#' ## Plots
#'
#' # Table plot
#' # Produces formattable output with sparklines
#' plot(
#'   prepared_data,
#'   type = "table",
#'
#'   # Use to filter out estimators
#'   estimator_name = c("acute.mean", "chronic.mean", "ES", "chronic.sd", "chronic.cv"),
#'
#'   # Use to filter out athlete
#'   # athlete_name = NULL,
#'
#'   # Use to filter out variables
#'   #variable_name = NULL,
#'
#'   # Show last entries
#'   last_n = 42,
#'
#'   # Round numbers
#'   digits = 2
#' )
#'
#' # Bar plot
#' # To plot group average
#' plot(
#'   prepared_data,
#'   type = "bar")
#'
#' # To plot per athlete, use trellis argument
#' plot(
#'   prepared_data,
#'   type = "bar",
#'   trellis = TRUE)
#'
#' # To filter out athlete variable and add Acute and Chronic lines to the group average:
#' plot(
#'   prepared_data,
#'   type = "bar",
#'
#'   # To filter out athletes
#'   # athlete_name = NULL,
#'
#'   # To filter out variable
#'   # variable_name = NULL,
#'
#'   # Add acute mean
#'   acute_name = "acute.mean",
#'
#'   # Add chronic mean
#'   chronic_name = "chronic.mean",
#'
#'   # Plot last n entries/days
#'   last_n = 42)
#'
#' # If you want to plot for each athlete, use trellis=TRUE
#' plot(
#'   prepared_data,
#'   type = "bar",
#'   acute_name = "acute.mean",
#'   chronic_name = "chronic.mean",
#'   last_n = 42,
#'   trellis = TRUE)
#'
#' # Line plots
#' # These plots represent summary of the rollins estimators
#' plot(
#'   prepared_data,
#'   type = "line",
#'
#'   # To filter out athletes
#'   # athlete_name = NULL,
#'
#'   # To filter out variables
#'   # variable_name = NULL,
#'
#'   # To filter out estimators
#'   # estimator_name = NULL,
#'
#'   # Tell graph where the lower group estimator is
#'   # which is in this case 25%th percentile of the group
#'   group_lower_name = "group.lower",
#'
#'   # The name of the centrality estimator of the group
#'   group_central_name = "group.median",
#'
#'   # Tell graph where the upper group estimator is
#'   # which is in this case 75%th percentile of the group
#'   group_upper_name = "group.upper",
#'
#'   # Use trellis if you do not plot for a single individual
#'   trellis = TRUE)
#'
#' # Previous chart looks messy because it plot all athletes
#' # To avoid that, filter out only one athlete
#' plot(
#'   prepared_data,
#'   type = "line",
#'
#'  # To filter out athletes
#'   athlete_name = "Ann Whitaker",
#'
#'  group_lower_name = "group.lower",
#'  group_central_name = "group.median",
#'  group_upper_name = "group.upper",
#'   trellis = TRUE)
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

    # Check is ammount of data in 'value' is smaller than rolling window
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
    # Tag missing day
    dplyr::mutate(missing_day = is.na(start_date)) %>%

    tidyr::fill(start_date, stop_date, .direction = "up") %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    dplyr::select(-start_date, -stop_date) %>%

    # Fill in missing days
    dplyr::mutate(value = ifelse(missing_day, NA_day, value)) %>%
    dplyr::select(-missing_day) %>%

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

