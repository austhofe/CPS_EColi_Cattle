#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(reticulate)


#### FUNCTIONS ####

#### Load Data ####

daily_weather <- daily_weather %>%
  mutate(month = month(date),
         year = year(date))
         
sampling_periods <- read_excel("data/marc_sampling.xlsx")
sampling_periods <- sampling_periods %>%
  mutate(date = as_date(date))

sampling <- read_excel("data/CPS_Nebraska_Year1_Sampling_clean.xlsx")
sampling <- sampling %>%
  mutate(Sampling_Day = as_date(Sampling_Day))

final_df <- daily_weather %>% 
  left_join(sampling, by=c("date"="Sampling_Day")) %>%
  left_join(sampling_periods, by=c("date"="date")) %>%
  left_join(wind_data, by=c("date"="Date")) %>%
  left_join(final_daily_df, by="date") %>%
  left_join(final_spi_df, by=c("month", "year"))
