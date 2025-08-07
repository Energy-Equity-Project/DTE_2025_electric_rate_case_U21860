
library(tidyverse)
library(janitor)

mi_tracts <- mi_tracts %>%
  mutate(GEOID = as.numeric(GEOID))

# DTE service territory
dte_service_territory <- st_read("../../Data/Electric_Retail_Service_Territories/Electric_Retail_Service_Territories.shp") %>%
  filter(NAME == "DTE ELECTRIC COMPANY")

# US census geography
mi_tracts <- st_read("../../Data/GIS/US_Census/tl_2023_26_tract/tl_2023_26_tract.shp")

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






mi_lead_raw <- read.csv("../../Data/DOE/08062025/Data Unzipped/MI-2022-LEAD-data/MI FPL Census Tracts 2022.csv")



mi_lead_clean <- mi_lead_raw %>%
  clean_names() %>%
  rename(GEOID = fip) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  mutate(elep = elep_units / units,
         gasp = gasp_units / units,
         fulp = fulp_units / units,
         hincp = hincp_units / units) %>%
  select(GEOID, fpl150, units, elep_units, gasp_units, fulp_units, elep, gasp, fulp, hincp) %>%
  group_by(GEOID, fpl150) %>%
  summarize(
    hincp = weighted.mean(hincp, units, na.rm = TRUE),
    elep = weighted.mean(elep, units, na.rm = TRUE),
    gasp = weighted.mean(gasp, units, na.rm = TRUE),
    fulp = weighted.mean(fulp, units, na.rm = TRUE),
    units = sum(units, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(total_cost = elep + gasp + fulp) %>%
  mutate(burden = total_cost / hincp) %>%
  mutate(burden_e = elep / hincp)

write.csv(
  mi_lead_clean,
  "outputs/mi_lead_clean.csv",
  row.names = FALSE
)

# 82 tracts not included in DOE LEAD data
# These only represent 36 occupied housing units (households)
not_included <- unique(dte_tracts$GEOID)[!(unique(dte_tracts$GEOID) %in% unique(mi_lead_clean$GEOID))]

occupied_housing_units %>%
  filter(GEOID %in% not_included) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE))

dte_tracts <- dte_tracts %>%
  filter(GEOID %in% unique(mi_lead_clean$GEOID))

dte_lead <- mi_lead_clean %>%
  filter(GEOID %in% unique(dte_tracts$GEOID))

write.csv(
  dte_lead,
  "outputs/dte_lead.csv",
  row.names = FALSE
)

dte_lead %>%
  summarize(total_elep = sum(elep * units, na.rm = TRUE))

dte_lead %>%
  summarize(mean_elep = weighted.mean(elep, units, na.rm = TRUE))

dte_lead %>%
  summarize(units = sum(units, na.rm = TRUE))



