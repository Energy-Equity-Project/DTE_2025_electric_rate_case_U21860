
# Libraries
library(tidyverse)
library(janitor)
library(sf)

# Reading in GIS data===========================================================

# DTE service territory
dte_service_territory <- st_read("../../Data/Electric_Retail_Service_Territories/Electric_Retail_Service_Territories.shp") %>%
  filter(NAME == "DTE ELECTRIC COMPANY")

# US census geography
mi_tracts <- st_read("../../Data/GIS/US_Census/tl_2023_26_tract/tl_2023_26_tract.shp")
mi_tracts <- mi_tracts %>%
  mutate(GEOID = as.numeric(GEOID))

st_crs(dte_service_territory)
st_crs(mi_tracts)

dte_service_territory <- st_transform(dte_service_territory, crs = 4269)

mi_tracts <- st_make_valid(mi_tracts)
dte_service_territory <- st_make_valid(dte_service_territory)

# Find all the census tracts within DTE service territory
dte_tracts <- st_intersection(mi_tracts, dte_service_territory) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  select(GEOID) %>%
  distinct()

# DOE LEAD 2025 updated data====================================================

# MI DOE LEAD data
mi_lead_raw <- read.csv("../../Data/DOE/08062025/Data Unzipped/MI-2022-LEAD-data/MI FPL Census Tracts 2022.csv")

# Cleaning MI DOE LEAD data
mi_lead_clean <- mi_lead_raw %>%
  clean_names() %>%
  rename(GEOID = fip) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  # Finding the average energy costs per housing unit
  mutate(elep = elep_units / units,
         gasp = gasp_units / units,
         fulp = fulp_units / units,
         hincp = hincp_units / units) %>%
  select(GEOID, fpl150, units, elep_units, gasp_units, fulp_units, elep, gasp, fulp, hincp) %>%
  # Summarizing energy costs by census tract and federal poverty level
  group_by(GEOID, fpl150) %>%
  summarize(
    hincp = weighted.mean(hincp, units, na.rm = TRUE),
    elep = weighted.mean(elep, units, na.rm = TRUE),
    gasp = weighted.mean(gasp, units, na.rm = TRUE),
    fulp = weighted.mean(fulp, units, na.rm = TRUE),
    units = sum(units, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Calculating total energy costs and 
  mutate(total_cost = elep + gasp + fulp) %>%
  mutate(burden = total_cost / hincp) %>%
  mutate(burden_e = elep / hincp)

write.csv(
  mi_lead_clean,
  "outputs/mi_lead_clean.csv",
  row.names = FALSE
)

# Data checks===================================================================
occupied_housing_units <- read.csv("outputs/occupied_housing_2023_cleaned.csv")

# 82 tracts not included in DOE LEAD data
# These only represent 36 occupied housing units (households)
not_included <- unique(dte_tracts$GEOID)[!(unique(dte_tracts$GEOID) %in% unique(mi_lead_clean$GEOID))]

occupied_housing_units %>%
  filter(GEOID %in% not_included) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE))

dte_tracts <- dte_tracts %>%
  filter(GEOID %in% unique(mi_lead_clean$GEOID))

# Finding DOE LEAD data for the DTE service territory
dte_lead <- mi_lead_clean %>%
  filter(GEOID %in% unique(dte_tracts$GEOID))

write.csv(
  dte_lead,
  "outputs/dte_lead.csv",
  row.names = FALSE
)


# A few preliminary data summaries
dte_lead %>%
  summarize(total_elep = sum(elep * units, na.rm = TRUE))

dte_lead %>%
  summarize(mean_elep = weighted.mean(elep, units, na.rm = TRUE))

dte_lead %>%
  summarize(units = sum(units, na.rm = TRUE))

# Example pipp for total energy burden of 6%
dte_lead %>%
  mutate(target_cost = hincp * 0.06) %>%
  mutate(affordability_gap = total_cost - target_cost) %>%
  filter(burden > 0.06) %>%
  summarize(affordability_gap = sum(affordability_gap * units, na.rm = TRUE))

# Example PIPP for electric only energy burden of 6%x
dte_lead %>%
  mutate(target_cost = hincp * 0.0322) %>%
  mutate(affordability_gap = elep - target_cost) %>%
  filter(burden_e > 0.0322) %>%
  summarize(affordability_gap = sum(affordability_gap * units, na.rm = TRUE))

# Example PIPP after summarizing by GEOID (losing FPL)
dte_lead %>%
  group_by(GEOID) %>%
  summarize(hincp = weighted.mean(hincp, units, na.rm = TRUE),
            total_cost = weighted.mean(total_cost, units, na.rm =TRUE),
            units = sum(units, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(burden = total_cost / hincp) %>%
  mutate(target_cost = hincp * 0.06) %>%
  mutate(affordability_gap = total_cost - target_cost) %>%
  filter(burden > 0.06) %>%
  summarize(affordability_gap = sum(affordability_gap * units, na.rm = TRUE))

dte_lead %>%
  filter(GEOID == 26125166400) %>%
  summarize(hincp = weighted.mean(hincp, units, na.rm = TRUE),
            total_cost = weighted.mean(total_cost, units, na.rm =TRUE),
            units = sum(units, na.rm = TRUE)) %>%
  mutate(burden = total_cost / hincp)
