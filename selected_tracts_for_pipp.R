# Libraries
library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(tidycensus)
library(ggspatial)
library(janitor)
library(tigris)


# GIS LAYERS====================================================================

# MI places
mi_places <- places(state = "MI")

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

reliability_metrics <- read.csv("outputs/dte_reliability_metrics_2023_2024.csv") %>%
  mutate(caidi_percentile = percent_rank(avg_caidi),
         saidi_percentile = percent_rank(avg_saidi))

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

li_non_li_hh <- read.csv("outputs/li_vs_non-li_percents_2023.csv")

race <- read.csv("outputs/bipoc_percent.csv")

# MiEJScreen====================================================================
miejscreen <- read.csv("outputs/miejscreen.csv")

# Climate Vulnerability Index===================================================

climate_vulnerability_index <- read_excel("../../Data/Climate Vulnerability Index/Master CVI Dataset - Oct 2023.xlsx", sheet = "Domain CVI Values") %>%
  clean_names() %>%
  mutate(baseline_all_percentile = percent_rank(baseline_all))

# DTE data======================================================================
# FIXIT: is this just revenue from electric?
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
    li_non_li_hh %>%
      filter(fpl_cat == "low-income") %>%
      select(GEOID, li_percent=percent),
    by = c("GEOID")
  ) %>%
  # Adding % BIPOC
  left_join(
    race %>%
      select(GEOID, bipoc_percent),
    by = c("GEOID")
  ) %>%
  # Adding climate vulnerability
  left_join(
    climate_vulnerability_index %>%
      mutate(GEOID = as.numeric(fips_code)) %>%
      select(GEOID, climate_community_baseline = baseline_all_percentile),
    by = c("GEOID")
  ) %>%
  # Adding Avg CAIDI per census tract for 2024
  left_join(
    reliability_metrics %>%
      select(GEOID, saidi_percentile),
    by = c("GEOID")
  )

# Selecting tracts for community-wide PIPP======================================

# Selection criteria includes:
  # Estimated electric burden of 4.83 (1.5x the 3.22% found by taking share of electric from total bills in DOE LEAD)
  # MiEJScreen score percentile of above 80th percentile
  # Percent low income families above 65%
community_wide_pipp <- dte_doe_consumption %>%
  filter(burden_e > 0.0483,
         MiEJScorePL > 80,
         li_percent > 65) %>%
  mutate(county_id = substr(as.character(GEOID), 3, 5)) %>%
  left_join(
    fips_codes %>%
      filter(state == "MI") %>%
      select(county_code, county),
    by = c("county_id"="county_code")
  ) %>%
  # Calculating affordability gap
  mutate(affordable_dte_cost = 0.0322 * hh_med_income) %>%
  mutate(affordability_gap_per_hh = avg_annual_hh_dte_elec_costs - affordable_dte_cost) %>%
  mutate(total_affordability_gap = affordability_gap_per_hh * hh_count) %>%
  mutate(burden_e = burden_e * 100,
         bipoc_percent = bipoc_percent / 100,
         li_percent = li_percent / 100,
         MiEJScorePL = MiEJScorePL / 100) %>%
  select(GEOID,
         county,
         hh_count,
         avg_annual_hh_dte_elec_costs,
         hh_med_income,
         burden_e,
         MiEJScorePL,
         li_percent,
         bipoc_percent,
         climate_community_baseline,
         saidi_percentile,
         affordability_gap_per_hh,
         total_affordability_gap) %>%
  arrange(desc(burden_e))

write.csv(
  community_wide_pipp,
  "outputs/community_wide_pipp.csv",
  row.names = FALSE
)

# Map of Community-wide PIPP enrollment tracts
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(
    data = mi_tracts %>%
      filter(GEOID %in% unique(community_wide_pipp$GEOID)),
    fill = "red",
    color = "black",
    alpha = 0.5
  ) +
  geom_sf(
    data = mi_places %>%
      filter(NAME == "Detroit"),
      # mutate(INTPTLAT = as.numeric(INTPTLAT),
      #        INTPTLON = as.numeric(INTPTLON)) %>%
      # filter(INTPTLAT > 42.2 & INTPTLAT < 42.5 &
      #          INTPTLON > -83.3 & INTPTLON < -82.9),
    color = "#002e55",
    linewidth = 1,
    alpha = 0
  ) +
  geom_sf_text(
    data = mi_places %>%
      filter(NAME == "Detroit"),
    aes(label = NAME),
    color = "black",
    size = 5
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggsave(
  "outputs/community_wide_pipp_locations.png",
  units = "in",
  height = 5,
  width = 7
)
