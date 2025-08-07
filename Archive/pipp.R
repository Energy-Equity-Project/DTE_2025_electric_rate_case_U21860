
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

# MiEJScreen====================================================================
miejscreen <- read.csv("outputs/miejscreen.csv")

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
  mutate(total_elec_costs = yr_cost_e * h_count) %>%
  group_by(GEOID) %>%
  summarize(lead_elec_costs = sum(total_elec_costs, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = lead_elec_costs / sum(lead_elec_costs, na.rm = TRUE))

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
  mutate(avg_annual_hh_dte_elec_costs = dte_energy_costs / hh_count) %>%
  mutate(burden_e = avg_annual_hh_dte_elec_costs / hh_med_income) %>%
  # Adding MiEJScreen Scores and Percentiles
  left_join(
    miejscreen %>%
      select(GEOID, MiEJScore, MiEJScorePL),
    by = c("GEOID")
  ) %>%
  # Adding low income percents
  left_join(
    fpl %>%
      filter(fpl_cat == "low-income") %>%
      select(GEOID, percent),
    by = c("GEOID")
  )

# Number of tracts above 1.75%
dte_doe_consumption %>%
  filter(burden_e > 0.0175) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 3%
dte_doe_consumption %>%
  filter(burden_e > 0.03) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 3.22%
dte_doe_consumption %>%
  filter(burden_e > 0.0322) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 3.5%
dte_doe_consumption %>%
  filter(burden_e > 0.035) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 4%
dte_doe_consumption %>%
  filter(burden_e > 0.04) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 4.83%
dte_doe_consumption %>%
  filter(burden_e > 0.0483) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 4.83%
tmp <- dte_doe_consumption %>%
  filter(burden_e > 0.0483,
         MiEJScorePL > 80,
         percent > 65) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 5%
dte_doe_consumption %>%
  filter(burden_e > 0.05) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 6%
dte_doe_consumption %>%
  filter(burden_e > 0.06) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 7.5%
dte_doe_consumption %>%
  filter(burden_e > 0.075) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 75th percentile electric burden
dte_doe_consumption %>%
  mutate(percentile = percent_rank(burden_e)) %>%
  filter(percentile > 0.75) %>%
  filter(burden_e > 0.0175) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Number of tracts above 75th percentile electric burden
dte_doe_consumption %>%
  mutate(percentile = percent_rank(burden_e)) %>%
  filter(percentile > 0.75) %>%
  filter(burden_e > 0.03) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

dte_doe_consumption %>%
  mutate(percentile = percent_rank(burden_e)) %>%
  filter(percentile > 0.75) %>%
  filter(burden_e > 0.035) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

dte_doe_consumption %>%
  mutate(percentile = percent_rank(burden_e)) %>%
  filter(percentile > 0.75) %>%
  filter(burden_e > 0.05) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

