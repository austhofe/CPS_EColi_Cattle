################################################################################
# SCRIPT: Merge PRISM and MARC-provided data
# PURPOSE: Download daily PRISM data via ACIS API and calculate 
#          Standardized Precipitation Index (SPI) using the SCI package.
#         Then load and merge in data provided from Jim for the US MARC HSI.
# AUTHOR: M. Crimmins/Gemini AI & Erika Austhof
# DATE: 2026-03-24
# UPDATED: 2026-06-08
# Run in R 4.6.0
################################################################################

#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(reticulate)
library(httr)
library(jsonlite)
library(SCI)


# --- FUNCTION 1: Download PRISM Data from ACIS ---
get_prism_data <- function(lat, lon, start_date, end_date) {
  url <- "https://data.rcc-acis.org/GridData"
  
  payload <- list(
    loc = paste(lon, lat, sep = ","),
    sdate = start_date,
    edate = end_date,
    grid = "21", 
    elems = list(
      list(name = "pcpn", interval = "dly"),
      list(name = "maxt", interval = "dly"),
      list(name = "mint", interval = "dly"),
      list(name = "avgt", interval = "dly")
    )
  )
  
  response <- POST(url, 
                   body = payload, 
                   encode = "json", 
                   add_headers(`Content-Type` = "application/json"))
  
  if (status_code(response) != 200) {
    stop("API request failed with status: ", status_code(response))
  }
  
  raw_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  df <- as.data.frame(raw_data$data)
  
  colnames(df) <- c("date", "precip_in", "tmax_f", "tmin_f", "tavg_f")
  
  df$date <- as.Date(df$date)
  df$precip_in <- as.numeric(as.character(df$precip_in))
  df$tmax_f    <- as.numeric(as.character(df$tmax_f))
  df$tmin_f    <- as.numeric(as.character(df$tmin_f))
  df$tavg_f    <- as.numeric(as.character(df$tavg_f))
  
  return(df)
}

# --- FUNCTION 2: Calculate Multiple SPI Timescales ---
calculate_multi_spi <- function(daily_df, timescales = c(3, 6, 12)) {
  # 1. Aggregate to monthly totals
  monthly_data <- daily_df %>%
    mutate(month_yr = floor_date(date, "month")) %>%
    group_by(month_yr) %>%
    summarise(precip_in = sum(precip_in, na.rm = TRUE)) %>%
    arrange(month_yr)
  
  start_mon <- month(min(monthly_data$month_yr))
  precip_vec <- monthly_data$precip_in
  
  # 2. Loop through each timescale and calculate SPI
  for (ts in timescales) {
    column_name <- paste0("spi_", ts)
    
    # Fit
    spi_para <- fitSCI(
      x = precip_vec, 
      first.mon = start_mon, 
      distr = "gamma", 
      time.scale = ts, 
      p0 = TRUE
    )
    
    # Transform
    spi_values <- transformSCI(
      x = precip_vec, 
      first.mon = start_mon, 
      obj = spi_para
    )
    
    # Add to dataframe
    monthly_data[[column_name]] <- as.numeric(spi_values)
  }
  
  return(monthly_data)
}

# --- BATCH EXECUTION ---

# 1. Define your parameters
start_yr <- "1981-01-01"
end_yr   <- "2025-12-31"
scales   <- c(1, 3, 6, 12)
locations_df <- data.frame(lat = c(40.5239),
                           lon = c(-98.1331),
                           id = c("US_MARC"))
# 2. Process all locations
results_list <- pmap(list(locations_df$lat, locations_df$lon, locations_df$id), 
                     function(lt, ln, id) {
                       
                       message(paste("Processing ID:", id))
                       
                       tryCatch({
                         # Download daily data (Full range for SPI)
                         daily_raw <- get_prism_data(lat = lt, lon = ln, 
                                                     start_date = start_yr, 
                                                     end_date = end_yr)
                         
                         # Calculate SPI
                         spi_results <- calculate_multi_spi(daily_raw, timescales = scales)
                         spi_results <- spi_results %>% mutate(id = id)
                         
                         # Subset daily data for 2024-2025 and add id
                         daily_subset <- daily_raw %>%
                           filter(date >= as.Date("2024-01-01") & date <= as.Date("2025-12-31")) %>%
                           mutate(id = id) %>%
                           dplyr::select(id, date, precip_in, tmin_f, tmax_f, tavg_f)
                         
                         # Return BOTH dataframes inside a list
                         return(list(spi = spi_results, daily = daily_subset))
                         
                       }, error = function(e) {
                         message(paste("Error with ID", id, ":", e$message))
                         return(NULL)
                       })
                     })

# 3. Combine into master dataframes
# Extract the 'spi' and 'daily' lists separately and bind them 
final_spi_df <- bind_rows(map(results_list, "spi")) 
final_spi_df <- final_spi_df %>%
  rename(precip_month = precip_in) %>%
  mutate(month = month(month_yr),
         year = year(month_yr)) %>%
  dplyr::select(-id, -month_yr)

final_daily_df <- bind_rows(map(results_list, "daily"))
final_daily_df <- final_daily_df %>%
  rename(precip_day = precip_in) %>%
  dplyr::select(-id)

# View results
print("SPI Dataframe Head:")
head(final_spi_df)

print("Daily Dataframe Head:")
head(final_daily_df)

#### export data if needed
# write.csv(final_daily_df, "data\\prism\\prism_daily.csv", row.names = T)
# write.csv(final_spi_df, "data\\prism\\spi_monthly.csv", row.names = T)


#### MARC weather code

#### Loop through and merge weather data files
# Directory containing the files
data_dir <- "C:/Users/barrette/Documents/Work/CFPS Grant 2023_Cooper_funded/CPS_EColi_Cattle/data/weather/MARC"

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
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
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
