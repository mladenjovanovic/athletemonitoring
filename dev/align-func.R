library(tidyverse)

df <- tribble(
  ~athlete, ~day, ~variable, ~value,
  "A01", 1, "V01", "Low",
  #"A01", 2, "V01", "Low",
  "A01", 3, "V01", NA,
  "A01", 4, "V02", "Low",
  "A02", 0, "V01", "Medium",
  "A03", 5, "V03", "High"
)

prepared_data <- prepare(
  data = df,
  athlete = "athlete",
  date = "day",
  variable = "variable",
  value = "value",
  acute = 7, NA_session = "NA session", NA_day = "NA Day",
  chronic = 42, partial = TRUE,
  extend = "none", extend_fill = "Extended"
)

prepared_data
x <- prepared_data$data_wide
x <- summary(prepared_data)


