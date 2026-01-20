#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(reticulate)


#### FUNCTIONS ####
# PRISM function 
read.prism.csv.and.add.filename <- function(filepath){
  read_csv(filepath, skip = 10) %>%
    mutate(filepath=filepath)
}

# Drought Function
read.drought.csv.and.add.filename <- function(filepath){
  read_csv(filepath, skip = 3) %>%
    mutate(filepath=filepath)
}

#### Import census tract population center data from US Census 2020 ####
co_CT_info <- read_excel("data/references/Census Tracts 2020 Population Centers.xlsx")





#### IMPORT DAILY PRISM Data ####
ct_folders <- c("CT_1", "CT_2", "CT_3")

all_prism_data <- map_dfr(ct_folders, function(ct) {
  
  raw_prism_files <- data_frame(
    filename = list.files(paste0("data/prism/", ct, "/"))
  )
  
  raw_prism_file_paths <- raw_prism_files %>%
    mutate(filepath = paste0("data/prism/", ct, "/", filename))
  
  raw_prism_file_paths %>%
    rowwise() %>%
    do(., read.prism.csv.and.add.filename(file = .$filepath)) %>%
    ungroup() %>%
    mutate(ct_folder = ct)
})

#Quality control check for dataframe
all_prism_data %>%
  head(5)


#### MERGE PRISM DATA WITH CT INFORMATION ####
#convert CTCode to numeric
co_CT_info$CTCode <- as.numeric(co_CT_info$CTCode)

# Merge in county information
CT_prism_data <- all_prism_data %>% 
  left_join(co_CT_info, join_by("Name" == "CTCode"))

#Quality control check for dataframe
CT_prism_data %>%
  head(5)


#### EXTRACT DATE INFO FOR MERGING ####
prism_df <- CT_prism_data %>%
  mutate(
    year  = year(Date),
    month = month(Date),
    day   = day(Date)
  )

#Quality control check for dataframe
prism_df %>%
  head(5)
summary(prism_df)
