
# Libraries
library(tidyverse)
library(janitor)
library(sf)

# Directory structure
outdir <- "outputs"

# DTE service territory
dte_service_territory <- st_read("../../Data/Electric_Retail_Service_Territories/Electric_Retail_Service_Territories.shp") %>%
  filter(NAME == "DTE ELECTRIC COMPANY")

# US census geography
mi_tracts <- st_read("../../Data/GIS/US_Census/Census Tract Shapefiles/2022/tl_2022_26_tract/tl_2022_26_tract.shp")

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
  mutate(GEOID = as.numeric(GEOID))

# Find average distribution of burden across income levels in dte census tracts
energy_type_summary <- doe_lead %>%
  filter(gi %in% unique(dte_tracts$GEOID)) %>%
  group_by(fpl) %>%
  summarize(
    mean_elec_burden = weighted.mean(burden_e, h_count, na.rm = TRUE),
    mean_gas_burden = weighted.mean(burden_g, h_count, na.rm = TRUE),
    mean_other_burden = weighted.mean(burden_o, h_count, na.rm = TRUE),
    mean_elec_cost = weighted.mean(yr_cost_e, h_count, na.rm = TRUE),
    mean_gas_cost = weighted.mean(yr_cost_g, h_count, na.rm = TRUE),
    mean_other_cost = weighted.mean(yr_cost_o, h_count, na.rm = TRUE)
  ) %>%
  ungroup()

write.csv(
  energy_type_summary,
  file.path(outdir, "energy_type_summary.csv"),
  row.names = FALSE
)

li_non_li_costs <- doe_lead %>%
  filter(gi %in% unique(dte_tracts$GEOID)) %>%
  mutate(fpl = case_when(
    fpl %in% c("0-100", "100-150", "150-200") ~ "0-200",
    TRUE ~ "200+"
  )) %>%
  group_by(fpl) %>%
  summarize(
    mean_elec_burden = weighted.mean(burden_e, h_count, na.rm = TRUE),
    mean_gas_burden = weighted.mean(burden_g, h_count, na.rm = TRUE),
    mean_other_burden = weighted.mean(burden_o, h_count, na.rm = TRUE),
    mean_elec_cost = weighted.mean(yr_cost_e, h_count, na.rm = TRUE),
    mean_gas_cost = weighted.mean(yr_cost_g, h_count, na.rm = TRUE),
    mean_other_cost = weighted.mean(yr_cost_o, h_count, na.rm = TRUE)
  ) %>%
  ungroup()

write.csv(
  li_non_li_costs,
  file.path(outdir, "li_vs_non-li_energy_costs.csv"),
  row.names = FALSE
)

# Calculate for all energy burden reductions using a PIPP
elec_prop_cost <- 0.5368
affordable_burden <- 0.06
affordable_elec_burden <- affordable_burden * elec_prop_cost
affordable_gas_burden <- affordable_burden * (1 - elec_prop_cost)

pipp <- doe_lead %>%
  filter(gi %in% unique(dte_tracts$GEOID)) %>%
  mutate(fpl = case_when(
    fpl %in% c("0-100", "100-150", "150-200") ~ "0-200",
    TRUE ~ "200+"
  )) %>%
  mutate(affordable_elec_cost = income * affordable_elec_burden,
         affordable_gas_cost = yr_cost * affordable_gas_burden) %>%
  mutate(elec_gap = yr_cost_e - affordable_elec_cost,
         gas_gap = (yr_cost - yr_cost_e) - affordable_gas_cost) %>%
  filter(elec_gap > 0 &
           gas_gap > 0) %>%
  group_by(fpl) %>%
  summarize(mean_elec_gap = weighted.mean(elec_gap, h_count, na.rm = TRUE),
            mean_gas_cap = weighted.mean(gas_gap, h_count, na.rm = TRUE)) %>%
  ungroup()

# Federal Poverty Level US Census 2022
fpl_hh <- read.csv("data/ACSDT5Y2022.B17026_2024-09-26T151853/ACSDT5Y2022.B17026-Data.csv") %>%
  clean_names() %>%
  select("geo_id" | ends_with("e")) %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  mutate(geoid = as.numeric(substr(geography, 10, 20))) %>%
  select(-c(geography, geographic_area_name)) %>%
  select(-estimate_total) %>%
  pivot_longer(-geoid, names_to = "fpl", values_to = "hh_count") %>%
  mutate(hh_count = as.numeric(hh_count)) %>%
  mutate(fpl = case_when(
    fpl == "estimate_total_under_50" ~ "0-100",
    fpl == "estimate_total_50_to_74" ~ "0-100",
    fpl == "estimate_total_75_to_99" ~ "0-100",
    fpl == "estimate_total_1_00_to_1_24" ~ "100-150",
    fpl == "estimate_total_1_25_to_1_49" ~ "100-150",
    fpl == "estimate_total_1_50_to_1_74" ~ "150-200",
    fpl == "estimate_total_1_75_to_1_84" ~ "150-200",
    fpl == "estimate_total_1_85_to_1_99" ~ "150-200",
    fpl == "estimate_total_1_85_to_1_99" ~ "150-200",
    fpl == "estimate_total_2_00_to_2_99" ~ "200-400",
    fpl == "estimate_total_3_00_to_3_99" ~ "200-400",
    fpl == "estimate_total_4_00_to_4_99" ~ "400+",
    fpl == "estimate_total_5_00_and_over" ~ "400+",
    TRUE ~ fpl
  )) %>%
  group_by(geoid, fpl) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fpl = factor(fpl, levels = c(
    "0-100", "100-150", "150-200", "200-400", "400+"
  )))

write.csv(
  fpl_hh %>%
    group_by(fpl) %>%
    summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(hh_percent = 100 * (hh_count / sum(hh_count))),
  file.path(outdir, "fpl_hh_summary.csv"),
  row.names = FALSE
)

fpl_hh %>%
  group_by(fpl) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(hh_percent = 100 * (hh_count / sum(hh_count)))
 