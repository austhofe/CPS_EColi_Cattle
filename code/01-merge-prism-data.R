#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(reticulate)


#### FUNCTIONS ####

#### Loop through and merge weather data files
library(dplyr)

# Directory containing the files
data_dir <- "C:/Users/barrette/Documents/Work/CFPS Grant 2023_funded/CPS_EColi_Cattle/data/weather/MARC"

# List CSV files that start with "Data-"
csv_files <- list.files(
  path = data_dir,
  pattern = "^Data-.*\\.csv$",
  full.names = TRUE
)

# Define column names (same for all files)
col_names <- c(
  "Date",
  "Temperature",
  "Dewpoint",
  "WindSpeed",
  "SolarRadiation",
  "NoShade_HSI",
  "Shade_HSI",
  "Barn_HSI"
)

# Read and merge all files
weather_data <- bind_rows(
  lapply(csv_files, function(file) {
    read.csv(
      file,
      header = FALSE,
      col.names = col_names,
      stringsAsFactors = FALSE
    )
  })
)

# Edit Date field to data/time
weather_data <- weather_data %>%
  mutate(
    Date = as.POSIXct(
      Date,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "America/Chicago"  # change if needed
    )
  )

weather_data <- weather_data %>%
  mutate(
    year  = format(Date, "%Y"),
    month = format(Date, "%m"),
    day   = format(Date, "%d")
  )

weather_data <- weather_data %>%
  mutate(
    across(
      c(Temperature, Dewpoint, WindSpeed, SolarRadiation),
      ~ na_if(.x, -32768)
    ),
    across(
      c(NoShade_HSI, Shade_HSI, Barn_HSI),
      ~ na_if(.x, "Normal")
    ),
    across(
      c(NoShade_HSI, Shade_HSI, Barn_HSI, year, month, day),
      ~ as.numeric(.x)
    )
  )

#daily weather dataset
daily_weather <- weather_data %>%
  mutate(Date_day = as.Date(Date)) %>%
  group_by(Date_day) %>%
  summarise(
    across(
      c(
        Temperature,
        Dewpoint,
        WindSpeed,
        SolarRadiation,
        NoShade_HSI,
        Shade_HSI,
        Barn_HSI
      ),
      list(
        min  = ~min(.x, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE),
        mean = ~mean(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# weekly weather_data
#uses monday as the start of the week
# If you want Sunday-start weeks (common in U.S. reporting): mutate(week = floor_date(Date, unit = "week", week_start = 7))
#If you want ISO weeks: mutate(iso_year = isoyear(Date), iso_week = isoweek(Date))
weekly_weather <- weather_data %>%
  mutate(week = floor_date(Date, unit = "week")) %>%
  group_by(week) %>%
  summarise(
    across(
      c(
        Temperature,
        Dewpoint,
        WindSpeed,
        SolarRadiation,
        NoShade_HSI,
        Shade_HSI,
        Barn_HSI
      ),
      list(
        min  = ~min(.x, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE),
        mean = ~mean(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
