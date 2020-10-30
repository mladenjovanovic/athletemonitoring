#' S3 method for plotting \code{\link{prepare}} results
#'
#' This function plots the selected estimator
#'
#' @param x Object of class \code{athletemonitoring}
#' @param type Type of the graph. Use "Bar" or "Line" (default). See Details
#' @param ... Extra arguments. See Details
#' @export
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
#'   NA_session = NA,
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
#'   # variable_name = NULL,
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
#'   type = "bar"
#' )
#'
#' # To plot per athlete, use trellis argument
#' plot(
#'   prepared_data,
#'   type = "bar",
#'   trellis = TRUE
#' )
#'
#' # To filter out athletem variable and add Acute and Chronic lines to the group average:
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
#'   last_n = 42
#' )
#'
#' # If you want to plot for each athlete, use trellis=TRUE
#' plot(
#'   prepared_data,
#'   type = "bar",
#'   acute_name = "acute.mean",
#'   chronic_name = "chronic.mean",
#'   last_n = 42,
#'   trellis = TRUE
#' )
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
#'   trellis = TRUE
#' )
#'
#' # Previous chart looks messy because it plot all athletes
#' # To avoid that, filter out only one athlete
#' plot(
#'   prepared_data,
#'   type = "line",
#'
#'   # To filter out athletes
#'   athlete_name = "Ann Whitaker",
#'
#'   group_lower_name = "group.lower",
#'   group_central_name = "group.median",
#'   group_upper_name = "group.upper",
#'   trellis = TRUE
#' )
plot.athletemonitoring <- function(x,
                                   type = "line",
                                   ...) {
  if (!(type %in% c("bar", "line", "table"))) {
    stop("Please use 'bar', 'line', or 'table' for the plot type", call. = FALSE)
  }

  if (type == "bar") {
    plot_athletemonitoring_bar(object = x, ...)
  } else if (type == "line") {
    plot_athletemonitoring_line(object = x, ...)
  } else {
    plot_athletemonitoring_table(object = x, ...)
  }
}


# ==================================================
plot_athletemonitoring_line <- function(object,
                                        athlete_name = NULL,
                                        variable_name = NULL,
                                        estimator_name = NULL,
                                        group_lower_name = NULL,
                                        group_central_name = NULL,
                                        group_upper_name = NULL,
                                        last_n = 42,
                                        trellis = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  athlete <- NULL
  variable <- NULL
  estimator <- NULL
  value <- NULL
  group_lower <- NULL
  group_central <- NULL
  group_upper <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- dplyr::left_join(
    object$data_long,
    object$group_summary,
    by = c("date", "variable", "estimator")
  )

  plot_data <- plot_data %>%
    # Filter last_n
    dplyr::group_by(athlete, variable) %>%
    dplyr::filter(date > max(date) - last_n) %>%
    dplyr::ungroup()

  # If provided, filter athlete
  if (!is.null(athlete_name)) {
    plot_data <- plot_data %>%
      dplyr::filter(athlete %in% athlete_name) %>%
      dplyr::mutate(athlete = factor(athlete, levels = athlete_name))
  }

  # If provided, filter variable
  if (!is.null(variable_name)) {
    plot_data <- plot_data %>%
      dplyr::filter(variable %in% variable_name) %>%
      dplyr::mutate(variable = factor(variable, levels = variable_name))
  }

  # If provided, filter estimator
  if (!is.null(estimator_name)) {
    plot_data <- plot_data %>%
      dplyr::filter(estimator %in% estimator_name) %>%
      dplyr::mutate(estimator = factor(estimator, levels = estimator_name))
  }

  # Add group information
  plot_data$group_lower <- plot_data$value # as.numeric(NA)
  plot_data$group_central <- plot_data$value # as.numeric(NA)
  plot_data$group_upper <- plot_data$value # as.numeric(NA)

  if (!is.null(group_lower_name)) plot_data$group_lower <- plot_data[[group_lower_name]]
  if (!is.null(group_central_name)) plot_data$group_central <- plot_data[[group_central_name]]
  if (!is.null(group_upper_name)) plot_data$group_upper <- plot_data[[group_upper_name]]

  # Main panel
  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = date, y = value, group = athlete)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = group_lower, ymax = group_upper), color = NA, fill = "grey", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = group_central), color = "white") +
    ggplot2::geom_line() +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL)

  # Add trellis
  if (trellis == TRUE) {
    gg <- gg +
      ggplot2::facet_wrap(~ variable + estimator, scales = "free_y")
  }

  return(gg)
}

# ==================================================
plot_athletemonitoring_bar <- function(object,
                                       athlete_name = NULL,
                                       variable_name = NULL,
                                       acute_name = NULL,
                                       chronic_name = NULL,
                                       last_n = 42,
                                       trellis = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  athlete <- NULL
  variable <- NULL
  variable.value <- NULL
  acute <- NULL
  chronic <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- object$data_wide

  plot_data <- plot_data %>%
    # Filter last_n
    dplyr::group_by(athlete, variable) %>%
    dplyr::filter(date > max(date) - last_n) %>%
    dplyr::ungroup()

  # If provided, filter athlete
  if (!is.null(athlete_name)) {
    plot_data <- plot_data %>%
      dplyr::filter(athlete %in% athlete_name) %>%
      dplyr::mutate(athlete = factor(athlete, levels = athlete_name))
  }

  # If provided, filter variable
  if (!is.null(variable_name)) {
    plot_data <- plot_data %>%
      dplyr::filter(variable %in% variable_name) %>%
      dplyr::mutate(variable = factor(variable, levels = variable_name))
  }

  # Add acute/chronic information
  plot_data$acute <- as.numeric(NA) # plot_data$variable.value
  plot_data$chronic <- as.numeric(NA) # plot_data$variable.value

  if (!is.null(acute_name)) plot_data$acute <- plot_data[[acute_name]]
  if (!is.null(chronic_name)) plot_data$chronic <- plot_data[[chronic_name]]

  # if athlete not provided, but no trellis, take the mean
  is_averaged <- FALSE
  if (is.null(athlete_name) & trellis == FALSE) {
    message("Plotting average across athletes. Please select athlete or use `trellis=TRUE`")
    plot_data <- plot_data %>%
      dplyr::group_by(date, variable) %>%
      dplyr::summarise(
        variable.value = mean(variable.value, na.rm = TRUE),
        acute = mean(acute, na.rm = TRUE),
        chronic = mean(chronic, na.rm = TRUE)
      )
    is_averaged <- TRUE
  }

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = date, y = variable.value, group = variable)
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = acute), color = "black") +
    ggplot2::geom_line(ggplot2::aes(y = chronic), color = "black", linetype = "dashed") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL)

  # If averaged and more than one variable, then add trellis
  if (is.null(variable_name) & is_averaged == TRUE) {
    gg <- gg +
      ggplot2::facet_wrap(~variable, scales = "free_y")
  }

  # Add trellis
  if (trellis == TRUE) {
    gg <- gg +
      ggplot2::facet_wrap(~ athlete + variable, scales = "free_y")
  }

  return(gg)
}


# ===========================================================
plot_athletemonitoring_table <- function(object,
                                         athlete_name = NULL,
                                         variable_name = NULL,
                                         estimator_name = NULL,
                                         last_n = 42,
                                         digits = 2) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  athlete <- NULL
  variable <- NULL
  estimator <- NULL
  value <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  # Check if packages are available
  if (!requireNamespace("sparkline", quietly = TRUE)) {
    stop("Package \"sparkline\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("formattable", quietly = TRUE)) {
    stop("Package \"formattable\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package \"htmlwidgets\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  plot_data <- object$data_long

  plot_data <- plot_data %>%
    # Filter last_n
    dplyr::group_by(athlete, variable) %>%
    dplyr::filter(date > max(date) - last_n) %>%
    dplyr::ungroup()

  # If provided, filter athlete
  if (!is.null(athlete_name)) {
    plot_data <- plot_data %>%
      dplyr::filter(athlete %in% athlete_name) %>%
      dplyr::mutate(athlete = factor(athlete, levels = athlete_name))
  }

  # If provided, filter variable
  if (!is.null(variable_name)) {
    plot_data <- plot_data %>%
      dplyr::filter(variable %in% variable_name) %>%
      dplyr::mutate(variable = factor(variable, levels = variable_name))
  }

  # If provided, filter estimator
  if (!is.null(estimator_name)) {
    # Make sure to add "variable.value"
    if (!("variable.value" %in% estimator_name)) {
      estimator_name <- c("variable.value", estimator_name)
    }

    estimator_name <- factor(estimator_name)
    estimator_name <- stats::relevel(estimator_name, ref = "variable.value")
    estimator_name <- levels(estimator_name)

    plot_data <- plot_data %>%
      dplyr::filter(estimator %in% estimator_name) %>%
      dplyr::mutate(estimator = factor(estimator, levels = estimator_name))
  } else {
    # Put variable.value as first
    plot_data <- plot_data %>%
      dplyr::mutate(
        estimator = factor(estimator),
        estimator = stats::relevel(estimator, ref = "variable.value")
      )
  }

  # Round values
  plot_data$value <- round(plot_data$value, digits)

  # Historical data
  historical_data <- plot_data %>%
    dplyr::group_by(athlete, variable, estimator) %>%
    dplyr::summarize(
      historical = as.character(htmltools::as.tags(
        sparkline::sparkline(value,
          type = ifelse(estimator[1] == "variable.value", "bar", "line"),
          barColor = "darkblue",
          lineColor = "black",
          fillColor = "lightgrey",
          lineWidth = 3,
          spotRadius = 3
        )
      ))
    ) %>%
    dplyr::ungroup()

  historical_data <- tidyr::pivot_wider(
    historical_data,
    names_from = "estimator",
    values_from = "historical",
    names_prefix = "historical."
  )

  plot_data <- tidyr::pivot_wider(
    dplyr::arrange(plot_data, athlete, variable, estimator),
    names_from = "estimator",
    values_from = "value",
    names_prefix = ""
  ) %>%
    # Filter out latest values
    dplyr::group_by(athlete, variable) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  # Merge together

  plot_data <- dplyr::left_join(plot_data, historical_data, by = c("athlete", "variable"))

  # Re arrange columns
  n_estimators <- ncol(historical_data) - 2
  re_arranged_cols <- numeric()
  for (i in seq(1, n_estimators)) {
    re_arranged_cols <- c(re_arranged_cols, i + n_estimators, i)
  }

  plot_data <- plot_data[c(1:3, re_arranged_cols + 3)]

  # Create table
  out <- formattable::formattable(
    plot_data,
    list(
      # area(col = seq(5, ncol(plot_data), by = 2)) ~ color_tile("#DeF7E9", "#71CA97")
    )
  )

  out <- formattable::as.htmlwidget(out)
  out$dependencies <- c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))

  return(out)
}
