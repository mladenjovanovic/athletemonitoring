#' S3 method for printing \code{\link{prepare}} results
#'
#' This function prints short summary of the \code{athletemonitoring} object
#'
#' @param x Object of class \code{athletemonitoring}
#' @param ... Extra arguments. Not used
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
print.athletemonitoring <- function(x, ...) {
  if (x$type == "nominal") {
    cat("Athlete monitoring nominal data with the following characteristics:\n\n")
  } else {
    cat("Athlete monitoring numeric data with the following characteristics:\n\n")
  }

  cat(length(unique(x$data_wide$athlete)), "athletes:\n")
  cat(paste(unique(x$data_wide$athlete), collapse = ", "), "\n\n")
  cat(length(unique(x$data_wide$date)), "days:\n")
  cat("From", min(x$data_wide$date), "to", max(x$data_wide$date), "\n\n")
  cat(length(unique(x$data_wide$variable)), "variables:\n")
  cat(paste(unique(x$data_wide$variable), collapse = ", "), "\n\n")

  if (x$type == "nominal") {
    cat(length(unique(x$data_wide$level)), "levels:\n")
    cat(paste(levels(x$data_wide$level), collapse = ", "), "\n\n")
  }

  cat(length(unique(x$data_long$estimator)), "estimators:\n")
  cat(paste(unique(x$data_long$estimator), collapse = ", "))
}
