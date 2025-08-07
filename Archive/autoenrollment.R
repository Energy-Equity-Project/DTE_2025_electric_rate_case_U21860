
# Libraries
library(tidyverse)
library(janitor)

# Reading in MiEJScreen data from 07/29/2024
miejscreen <- read.csv("outputs/miejscreen.csv")

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

# Calculating the proportion of total energy consumption by census tract
doe_consumption <- doe_lead %>%
  filter(state == "MI") %>%
  mutate(total_energy_costs = yr_cost * h_count) %>%
  group_by(gi) %>%
  summarize(energy_costs = sum(total_energy_costs, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = energy_costs / sum(energy_costs, na.rm = TRUE))

mi_tracts <- st_make_valid(mi_tracts)
dte_service_territory <- st_make_valid(dte_service_territory)

# Find all the census tracts within DTE service territory
dte_tracts <- st_intersection(mi_tracts, dte_service_territory) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  select(GEOID) %>%
  distinct() %>%
  left_join(
    miejscreen %>%
      select(GEOID, MiEJScore, MiEJScorePL),
    by = c("GEOID")
  )

# Checking to see how many census tracts do not have MiEJScreen scores
# Lack of MiEJScreen scores occur because of a difference in Census tract geography year
dte_tracts %>%
  filter(is.na(MiEJScore)) %>%
  nrow()

# Tracking which census tracks do not appear across multiple datasets
length(unique(dte_tracts$GEOID)[!(unique(dte_tracts$GEOID) %in% unique(mi_tracts$GEOID))])
length(unique(miejscreen$GEOID)[!(unique(miejscreen$GEOID) %in% unique(mi_tracts$GEOID))])
length(unique(mi_tracts$GEOID)[!(unique(mi_tracts$GEOID) %in% unique(miejscreen$GEOID))])
length(unique(dte_tracts$GEOID)[!(unique(dte_tracts$GEOID) %in% unique(miejscreen$GEOID))])

# 80th percentile cutoff based on MiEJScreen score
dte_tracts %>%
  filter(MiEJScorePL > 80) %>%
  nrow()

dte_tracts %>%
  filter(MiEJScorePL > 90) %>%
  nrow()

dte_tracts %>%
  filter(MiEJScorePL > 95) %>%
  nrow()

# Distribute DTE energy consumption across all census tracts within DTE service territory
# Use distribution of energy consumption as provided by DOE LEAD

