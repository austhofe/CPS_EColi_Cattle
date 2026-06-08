library(openair)
library(nasapower)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

### Use the nasapower package to get wind data for the lat and long for the US MARC
NEM_data <- get_power(
  community = "ag",
  lonlat = c(-98.1331, 40.5239),
  pars = c("WS10M", "WD10M"),
  dates = c("2025-01-01", "2025-12-31"),
  temporal_api = "daily")

## extract only the needed fields for the wind rose
wind_data <- NEM_data %>%
  select(YYYYMMDD, WS10M, WD10M) %>%
  rename(Date = YYYYMMDD,
         Wind_Speed = WS10M,
         wind_dir = WD10M)

#edit column names so that the openair package can read it
Wind_rose <- NEM_data[,8:9]
colnames(Wind_rose)<- c("ws", "wd")

# convert wind speed to MPH
windrose1 <- NEM_data %>%
  mutate(mph = WS10M*2.237)

# filter for an additional wind rose showing only the extreme levels of wind (over 20 MPH)
windrose_ext <- NEM_data %>%
  mutate(mph = WS10M*2.237) %>%
  filter(mph>20)

# windrose for all data
windRose(windrose1,
         ws = "mph",
         wd = "WD10M",
         #ws2 = NA,
         #wd2 = NA,
         paddle = FALSE, 
         breaks = c(1,5,10,15,20,25),
         calm.thresh = 5,
         labs(dictionary = c( wd = "mph")),
         cols = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f"))
               #col= "YlOrBr")

# windrose for extreme wind speeds
windRose(windrose_ext,
         ws = "mph",
         wd = "WD10M",
         #ws2 = NA,
         #wd2 = NA,
         paddle = FALSE, 
         breaks = c(20,25,30,35,40,45,50),
         calm.thresh = 5,
         cols = c("#ff7f2f", "#8f1402", "#411900"))
