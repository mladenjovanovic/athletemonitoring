% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-asrbody.R
\docType{data}
\name{asrbody}
\alias{asrbody}
\title{AthleteSR Body Diagram polygons}
\format{
A data frame with 3391 rows and 6 variables:
\describe{
\item{View}{Body diagram view. Levels are 'Front' and 'Back'}
\item{Side}{Body diagram side. Levels are 'Left' and 'Right'}
\item{Part}{Name of the body diagram part. Contains 26 distincts parts}
\item{Area}{Name of the body diagram area. Represents \code{Veiw} +
\code{Side} + \code{Part}. Contains 74 distinct areas}
\item{x}{x coordinate of the polygon}
\item{y}{y coordinate of the polygon}
}
}
\usage{
data(asrbody)
}
\description{
Originally created by Jose Fernandez, this dataset contains AthleteSR
body diagram definition used for plotting.
}
\keyword{datasets}
