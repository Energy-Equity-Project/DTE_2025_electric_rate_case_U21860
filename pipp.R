
# Libraries
library(tidyverse)
library(sf)
library(lubridate)


# GIS LAYERS====================================================================

# DTE service territory
dte_service_territory <- st_read("../../Data/Electric_Retail_Service_Territories/Electric_Retail_Service_Territories.shp") %>%
  filter(NAME == "DTE ELECTRIC COMPANY")

# US census geography
mi_tracts <- st_read("../../Data/GIS/US_Census/Census Tract Shapefiles/2020/tl_2020_26_tract/tl_2020_26_tract.shp")

st_crs(dte_service_territory)
st_crs(mi_tracts)

dte_service_territory <- st_transform(dte_service_territory, crs = 4269)

# DOE LEAD
doe_files <- list.files("../../Data/DOE/01242025/CSV", full.names = TRUE)
doe_lead <- data.frame()
for (i in 1:length(doe_files)) {
  curr_doe <- read.csv(doe_files[i])
  doe_lead <- doe_lead %>%
    bind_rows(curr_doe)
}

mi_tracts <- st_make_valid(mi_tracts)
dte_service_territory <- st_make_valid(dte_service_territory)

# Find all the census tracts within DTE service territory
dte_tracts <- st_intersection(mi_tracts, dte_service_territory) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  select(GEOID) %>%
  distinct()

dte_lead <- dte_tracts %>%
  as.data.frame() %>%
  select(-geometry) %>%
  left_join(
    doe_lead,
    by = c("GEOID"="gi")
  )

# US Census ACS data============================================================
median_household_income <- read.csv("outputs/median_household_income_2023.csv", colClasses = "character") %>%
  mutate(GEOID = as.numeric(paste0(state, county, tract))) %>%
  select(-c(NAME, state, county, tract)) %>%
  rename(hh_med_income = S1901_C01_012E) %>%
  mutate(hh_med_income= as.numeric(hh_med_income))

household_counts <- read.csv("outputs/total_number_of_households_2023.csv", colClasses = "character") %>%
  mutate(GEOID = as.numeric(paste0(state, county, tract))) %>%
  select(-c(NAME, state, county, tract)) %>%
  rename(hh_count = S1101_C01_001E) %>%
  mutate(hh_count= as.numeric(hh_count))

# DTE data======================================================================
total_revenue <- read.csv("outputs/dte_total_revenue_2019_2024.csv")

total_revenue_2024 <- total_revenue %>%
  mutate(year = year(date)) %>%
  filter(year == 2024) %>%
  summarize(revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(revenue)

# Analysis======================================================================

# Calculating the proportion of total energy consumption by census tract
dte_doe_consumption <- dte_lead %>%
  filter(state == "MI") %>%
  mutate(total_energy_costs = yr_cost * h_count) %>%
  group_by(GEOID) %>%
  summarize(lead_energy_costs = sum(total_energy_costs, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = lead_energy_costs / sum(lead_energy_costs, na.rm = TRUE))

# Broadcast total DTE revenue across service territory using DOE LEAD proportions
dte_doe_consumption <- dte_doe_consumption %>%
  mutate(dte_energy_costs = prop * total_revenue_2024) %>%
  # adding number of households
  left_join(
    household_counts,
    by = c("GEOID")
  ) %>%
  # adding median income
  left_join(
    median_household_income,
    by = c("GEOID")
  ) %>%
  mutate(avg_annual_hh_dte_costs = dte_energy_costs / hh_count) %>%
  mutate(burden = avg_annual_hh_dte_costs / hh_med_income)
