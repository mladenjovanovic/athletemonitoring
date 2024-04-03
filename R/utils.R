my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm = TRUE), NA)
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm = TRUE), NA)
