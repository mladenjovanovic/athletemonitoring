#' S3 method for plotting \code{\link{prepare}} results
#'
#' This function plots the selected estimator
#'
#' @param x Object of class \code{athletemonitoring}
#' @param type Type of the graph. Use "Bar" or "Line" (default). See Details
#' @param ... Extra arguments. See Details
#' @export
#' @examples
#'     1+1
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
    plot_data <- plot_data %>%
      dplyr::filter(estimator %in% estimator_name) %>%
      dplyr::mutate(estimator = factor(estimator, levels = estimator_name))
  } else {
    # Remove variable.value
    plot_data <- plot_data %>%
      dplyr::filter(estimator != "variable.value") %>%
      dplyr::mutate(estimator = factor(estimator))
  }

  # Round values
  plot_data$value <- round(plot_data$value, digits)

  # Historical data
  historical_data <- plot_data %>%
    dplyr::group_by(athlete, variable, estimator) %>%
    dplyr::summarize(
      historical = as.character(htmltools::as.tags(
        sparkline::sparkline(value,
          type = "line",
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
      #area(col = seq(5, ncol(plot_data), by = 2)) ~ color_tile("#DeF7E9", "#71CA97")
    )
  )

  out <- formattable::as.htmlwidget(out)
  out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))

  return(out)
}
