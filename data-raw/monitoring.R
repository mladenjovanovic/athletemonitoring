## code to prepare `monitoring` dataset

require(tidyverse)
require(lubridate)

set.seed(1667)

athletes <- tribble(
  ~`First Name`, ~`Last Name`, ~gain, ~freq, ~offset, ~bias, ~variance, ~easyLoad, ~mediumLoad, ~hardLoad,
  "John", "Doe", 1.5, 0.75, 0, 3, 2, 100, 300, 500,
  "Peter", "Jackson", 1.5, 2, 100, 6, 3, 200, 500, 700,
  "Michael", "Peterson", 3, 2, 200, 6, 1, 50, 150, 500,
  "Ann", "Whitaker", 4, 0.25, 300, 1, 4, 150, 350, 600,
  "Susan", "Kane", 1.5, 0.25, 400, 6, 6, 30, 90, 180,

  "Mike", "Smith", 3, 0.25, 0, 6, 1, 250, 500, 800,
  "Stuart", "Rogan", 1, 0.75, 100, 3, 3, 80, 160, 320,
  "Alan", "McDonald", 1, 5, 200, 3, 1, 100, 200, 300,
  "Eve", "Black", 3, 5, 300, 6, 6, 200, 400, 600,
  "Frank", "West", 1, 0.25, 400, 3, 6, 100, 200, 400
)

athletes <- athletes %>%
  mutate(`Full Name` = paste(`First Name`, `Last Name`))

# =====================================================
# Generate wellness metric

wellness <- expand_grid(
  athletes,
  tibble(index = seq(1, 100, by = 1))
) %>%
  group_by(`Full Name`) %>%
  mutate(day = sample(1:365, n())) %>%
  ungroup()

wellness <- wellness %>%
  mutate(
    Wellness = gain * (sin(freq * (2 * pi) / 365 * (day + offset)) / 2) + 0.5,
    Wellness = Wellness + bias + rnorm(length(Wellness), mean = 0, sd = variance / 5),
    Wellness = Wellness * 10,
    Wellness = round(Wellness, 2),
    `Date` = dmy("01-01-2020") + day,
    `Time` = hm("8:00")
  )

# Format to long
wellness <- wellness %>%
  select(`Full Name`, `Date`, `Time`, `Wellness`) %>%
  pivot_longer(cols = "Wellness", names_to = "Variable", values_to = "Value")

# ====================================================
# Create training load
training_load <- tribble(
  ~Day, ~`9:00`, ~`16:00`,
  1, "Easy", "Medium",
  2, "Medium", "Hard",
  3, "Easy", NA,
  4, "Medium", "Easy",
  5, "Easy", "Hard",
  6, "Easy", NA,
  7, NA, NA
)

training_load <- pivot_longer(training_load, cols = c("9:00", "16:00"), names_to = "Time", values_to = "Training Type")

training_load <- expand_grid(
  training_load,
  Week = 1:52
)

training_load <- expand_grid(
  training_load,
  athletes
) %>%
  mutate(
    `Training Load` = ifelse(`Training Type` == "Easy", runif(n(), 0, easyLoad),
      ifelse(`Training Type` == "Medium", runif(n(), easyLoad, mediumLoad),
        ifelse(`Training Type` == "Hard", runif(n(), mediumLoad, hardLoad), NA)
      )
    ),
    `Date` = dmy("01-01-2020") + Day + ((Week - 1) * 7)
  ) %>%
  select(`Full Name`, `Date`, `Time`, `Training Load`) %>%
  mutate(
    `Training Load` = round(`Training Load`, 0),
    Time = hm(Time)
  ) %>%
  na.omit()

# Convert to long format
training_load <- training_load %>%
  pivot_longer(cols = "Training Load", names_to = "Variable", values_to = "Value")

# ================================================
# Combine datasets

monitoring <- rbind(
  wellness,
  training_load
) %>%
  arrange(`Full Name`, `Date`, `Time`, `Variable`) %>%
  mutate(
    Time = sprintf("%02d:%02d", hour(Time), minute(Time)),
    Date = sprintf("%02d-%02d-%02d", year(Date), month(Date), day(Date))
  )

usethis::use_data(monitoring, overwrite = TRUE)
