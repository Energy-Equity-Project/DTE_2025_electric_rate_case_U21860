
# Libraries
library(tidyverse)
library(janitor)

# Occupied housing units are usually what people refer to as households
occupied_housing_units <- read.csv("outputs/occupied_housing_units.csv", colClasses = "character") %>%
  clean_names() %>%
  rename(hh_count = b25002_002e) %>%
  mutate(GEOID = paste0(state, county, tract)) %>%
  mutate(hh_count = as.numeric(hh_count)) %>%
  select(-c(name, state, county, tract))

write.csv(
  occupied_housing_units,
  "outputs/occupied_housing_2023_cleaned.csv",
  row.names = FALSE
)
