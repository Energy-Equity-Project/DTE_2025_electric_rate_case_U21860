

# Libraries
library(tidyverse)
library(matrixStats)
library(sf)
library(tidycensus)
library(ggspatial)
library(janitor)
library(tigris)
library(ggpubr)

# Climate vulnerability index
climate_vulnerability_index <- read_excel("../../Data/Climate Vulnerability Index/Master CVI Dataset - Oct 2023.xlsx", sheet = "Domain CVI Values") %>%
  clean_names() %>%
  mutate(baseline_all_percentile = percent_rank(baseline_all))

# MiEJScreen scores
miejscreen <- read.csv("outputs/miejscreen.csv")

# US census race data
bipoc <- read.csv("outputs/bipoc_percent.csv")

# DTE reliability metrics
reliability_metrics <- read.csv("outputs/dte_reliability_metrics_2023_2024.csv") %>%
  mutate(caidi_percentile = percent_rank(avg_caidi),
         saidi_percentile = percent_rank(avg_saidi))

# DTE revenue and customers disagregated by DOE LEAD props
dte_df <- read.csv("outputs/dte_df.csv")


# Calculating percentage of low income customers in DTE tracts
li_percent <- dte_df %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(GEOID, income_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(GEOID) %>%
  mutate(percent = 100 * (dte_customers / sum(dte_customers, na.rm = TRUE))) %>%
  ungroup() %>%
  filter(income_cat == "low income") %>%
  select(-c(income_cat, dte_customers)) %>%
  rename(low_income_percent = percent)


autoenroll_tracts <- dte_df %>%
  group_by(GEOID) %>%
  summarize(
    # med_burden_e = weighted.mean(avg_hh_dte_energy_costs/hincp, dte_customers, na.rm = TRUE),
    median_income = weightedMedian(hincp, dte_customers, na.rm = TRUE),
    median_elec_costs = weightedMedian(avg_hh_dte_energy_costs, dte_customers, na.rm = TRUE),
    dte_customers = sum(dte_customers, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(med_burden_e = median_elec_costs/median_income) %>%
  # Adding MiEJScreen Scores and Percentiles
  left_join(
    miejscreen %>%
      select(GEOID, MiEJScorePL),
    by = c("GEOID")
  ) %>%
  # Adding percent of dte low income customers
  left_join(
    li_percent %>%
      mutate(low_income_percent = low_income_percent / 100),
    by = c("GEOID")
  ) %>%
  # Adding BIPOC percent
  left_join(
    bipoc %>%
      select(GEOID, bipoc_percent) %>%
      mutate(bipoc_percent = bipoc_percent / 100),
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

affordable_elec_burden <- 0.036
elec_burden_threshold <- affordable_elec_burden * 1.5

selected_autoenroll_tracts <- autoenroll_tracts %>%
  filter(med_burden_e > elec_burden_threshold,
         MiEJScorePL > 80,
         low_income_percent > 0.65) %>%
  mutate(county_id = substr(as.character(GEOID), 3, 5)) %>%
  left_join(
    fips_codes %>%
      filter(state == "MI") %>%
      select(county_code, county),
    by = c("county_id"="county_code")
  ) %>%
  mutate(target_elec_costs = median_income * affordable_elec_burden) %>%
  mutate(hh_affordability_gap = median_elec_costs - target_elec_costs) %>%
  mutate(monthly_benefit = hh_affordability_gap / 12) %>%
  mutate(discount_percent = hh_affordability_gap / median_elec_costs) %>%
  arrange(desc(med_burden_e)) %>%
  mutate(cumulative_affordability_gap = hh_affordability_gap * dte_customers) %>%
  mutate(pipp_offset = low_income_percent * cumulative_affordability_gap) %>%
  select(GEOID, county, dte_customers, median_elec_costs, median_income, med_burden_e, MiEJScorePL, low_income_percent, bipoc_percent, saidi_percentile, climate_community_baseline, hh_affordability_gap, monthly_benefit, discount_percent, cumulative_affordability_gap, pipp_offset)


write.csv(
  selected_autoenroll_tracts,
  "outputs/selected_autoenroll_tracts.csv",
  row.names = FALSE
)

selected_autoenroll_tracts %>%
  summarize(li_customers = sum(low_income_percent * dte_customers),
            med_income = weightedMedian(median_income, dte_customers, na.rm = TRUE),
            dte_customers = sum(dte_customers, na.rm = TRUE)
            )

selected_autoenroll_tracts_strict_saidi <- selected_autoenroll_tracts %>%
  filter(saidi_percentile > 0.65)

write.csv(
  selected_autoenroll_tracts_strict_saidi,
  "outputs/selected_autoenroll_tracts_strict_saidi.csv",
  row.names = FALSE
)

# autoenroll where the target is 2.4%
selected_autoenroll_tracts_2_4 <- autoenroll_tracts %>%
  filter(med_burden_e > elec_burden_threshold,
         MiEJScorePL > 80,
         low_income_percent > 0.65) %>%
  mutate(county_id = substr(as.character(GEOID), 3, 5)) %>%
  left_join(
    fips_codes %>%
      filter(state == "MI") %>%
      select(county_code, county),
    by = c("county_id"="county_code")
  ) %>%
  mutate(target_elec_costs = median_income * 0.024) %>%
  mutate(hh_affordability_gap = median_elec_costs - target_elec_costs) %>%
  mutate(monthly_benefit = hh_affordability_gap / 12) %>%
  mutate(discount_percent = hh_affordability_gap / median_elec_costs) %>%
  arrange(desc(med_burden_e)) %>%
  mutate(cumulative_affordability_gap = hh_affordability_gap * dte_customers) %>%
  mutate(pipp_offset = low_income_percent * cumulative_affordability_gap) %>%
  select(GEOID, county, dte_customers, median_elec_costs, median_income, med_burden_e, MiEJScorePL, low_income_percent, bipoc_percent, saidi_percentile, climate_community_baseline, hh_affordability_gap, monthly_benefit, discount_percent, cumulative_affordability_gap, pipp_offset)

write.csv(
  selected_autoenroll_tracts_2_4,
  "outputs/selected_autoenroll_tracts_2_4.csv",
  row.names = FALSE
)

selected_autoenroll_tracts_strict_saidi_2_4 <- selected_autoenroll_tracts_2_4 %>%
  filter(saidi_percentile > 0.65)

write.csv(
  selected_autoenroll_tracts_strict_saidi_2_4,
  "outputs/selected_autoenroll_tracts_strict_saidi_2_4.csv",
  row.names = FALSE
)



selected_autoenroll_tracts %>%
  summarize(cumulative_affordability_gap = sum(cumulative_affordability_gap, na.rm = TRUE))

# MI places
mi_places <- places(state = "MI")
# US census geography
mi_tracts <- st_read("../../Data/GIS/US_Census/Census Tract Shapefiles/2020/tl_2020_26_tract/tl_2020_26_tract.shp")

wayne_geoids <- selected_autoenroll_tracts %>%
  filter(county == "Wayne County") %>%
  pull(GEOID)

oakland_geoids <- selected_autoenroll_tracts %>%
  filter(county == "Oakland County") %>%
  pull(GEOID)

# Map of Community-wide PIPP enrollment tracts
detroit_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(
    data = mi_tracts %>%
      mutate(GEOID = as.numeric(GEOID)) %>%
      filter(GEOID %in% wayne_geoids),
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
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

pontiac_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(
    data = mi_tracts %>%
      mutate(GEOID = as.numeric(GEOID)) %>%
      filter(GEOID %in% oakland_geoids),
    fill = "red",
    color = "black",
    alpha = 0.5
  ) +
  geom_sf(
    data = mi_places %>%
      filter(NAME == "Pontiac"),
    color = "#002e55",
    linewidth = 1,
    alpha = 0
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggarrange(
  detroit_map,
  pontiac_map,
  nrow = 1
)

