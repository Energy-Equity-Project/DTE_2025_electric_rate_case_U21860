
library(tidyverse)
library(janitor)
library(lubridate)

# Total sales data
total_kwh <- read.csv("outputs/total_sales_kwh.csv")
total_customer_counts <- read.csv("outputs/total_customer_counts.csv")
total_revenue <- read.csv("outputs/dte_total_revenue_2019_2024.csv")

# Low sales data
li_kwh <- read.csv("outputs/li_kwh.csv")
li_customer_counts <- read.csv("outputs/li_customer_counts.csv")
li_revenue <- read.csv("outputs/li_revenues.csv")


# Remove customers from that would be double counted across supplemental rate plans
supplemental_rate_plans <- c("D1.1", "D1.9", "D2", "D5", "D9")

dte_customers_counts <- total_customer_counts %>%
  filter(!(rate_plan %in% supplemental_rate_plans)) %>%
  group_by(rate_plan) %>%
  summarize(mean_total_customers = mean(num_customers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rate_plan = tolower(gsub("\\.", "_", rate_plan))) %>%
  left_join(
    li_customer_counts %>%
      mutate(year = year(date)) %>%
      filter(year == 2024) %>%
      select(-year) %>%
      group_by(rate_plan) %>%
      summarize(mean_li_customers = mean(customer_count, na.rm = TRUE)) %>%
      ungroup(),
    by = c("rate_plan")
  ) %>%
  replace_na(list(mean_li_customers = 0)) %>%
  mutate(mean_non_li_customers = mean_total_customers - mean_li_customers)

dte_customers_counts %>%
  summarize(
    total_customers = sum(mean_total_customers, na.rm = TRUE),
    total_li_customers = sum(mean_li_customers, na.rm = TRUE)
  ) %>%
  mutate(total_non_li_customers = total_customers - total_li_customers)

# Compare to what US census says


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

# US CENSUS Household counts
household_counts <- read.csv("outputs/total_number_of_households_2023.csv", colClasses = "character") %>%
  mutate(GEOID = as.numeric(paste0(state, county, tract))) %>%
  select(-c(NAME, state, county, tract)) %>%
  rename(hh_count = S1101_C01_001E) %>%
  mutate(hh_count= as.numeric(hh_count))

li_non_li_hh <- read.csv("outputs/li_vs_non-li_percents_2023.csv")

dte_hh_estimates <- li_non_li_hh %>%
  filter(GEOID %in% unique(dte_tracts$GEOID)) %>%
  group_by(fpl_cat) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = 100 * (hh_count / sum(hh_count, na.rm = TRUE)))
