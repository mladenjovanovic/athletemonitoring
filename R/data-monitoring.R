#' Monitoring Data
#'
#' Simulated athlete monitoring data involving 10 athletes, monitored across
#' a year using morning \code{Wellness} rating (every few days; in arbitrary units (AU)),
#' and \code{Training Load} after every session (10 sessions a week; in AU).
#'
#' @format A data frame with 6200 rows and 5 variables:
#' \describe{
#'   \item{Full Name}{Name of the athlete}
#'   \item{Date}{Date in the YYYY-MM-DD format}
#'   \item{Time}{Time of the day in HH:MM 24h format}
#'   \item{Variable}{Name of the variable measured (Wellness or Training Load)}
#'   \item{Value}{Numeric value}
#' }
#' @usage
#' data(monitoring)
"monitoring"
