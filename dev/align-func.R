library(tidyverse)

df <- tribble(
  ~athlete, ~day, ~variable, ~value,
  "A01", 1, "V01", "10",
  "A01", 2, "V01", "12",
  "A01", 3, "V01", "13",
  "A01", 4, "V02", "0",
  "A02", 0, "V01", "15",
  "A03", 5, "V03", "17"
)

prepared_data <- prepare(
  data = df,
  athlete = "athlete",
  date = "day",
  variable = "variable",
  value = "value",
  acute = 7,
  chronic = 42,
  extend = "both"
)

x <- prepared_data$data_wide
summary(prepared_data)
