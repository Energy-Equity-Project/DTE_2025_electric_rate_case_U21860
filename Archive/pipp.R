# Libraries
library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(tidycensus)
library(ggspatial)
library(janitor)
library(tigris)

# DOE LEAD data=================================================================

mi_lead <- read.csv("outputs/mi_lead_clean.csv")
dte_lead <- read.csv("outputs/dte_lead.csv")

# US Census ACS data============================================================
avg_household_income <- read.csv("outputs/median_and_mean_household_income_2023.csv", colClasses = "character") %>%
  mutate(GEOID = as.numeric(paste0(state, county, tract))) %>%
  select(-c(NAME, state, county, tract)) %>%
  rename(hh_med_income = S1901_C01_012E,
         hh_mean_income = S1901_C01_013E) %>%
  mutate(hh_med_income = as.numeric(hh_med_income),
         hh_mean_income = as.numeric(hh_mean_income))

occupied_housing_units <- read.csv("outputs/occupied_housing_2023_cleaned.csv")

li_non_li_hh <- read.csv("outputs/li_vs_non-li_percents_2023.csv")

race <- read.csv("outputs/bipoc_percent.csv")

# MiEJScreen====================================================================
miejscreen <- read.csv("outputs/miejscreen.csv")

# Climate Vulnerability Index===================================================

climate_vulnerability_index <- read_excel("../../Data/Climate Vulnerability Index/Master CVI Dataset - Oct 2023.xlsx", sheet = "Domain CVI Values") %>%
  clean_names() %>%
  mutate(baseline_all_percentile = percent_rank(baseline_all))

# DTE data======================================================================

reliability_metrics <- read.csv("outputs/dte_reliability_metrics_2023_2024.csv") %>%
  mutate(caidi_percentile = percent_rank(avg_caidi),
         saidi_percentile = percent_rank(avg_saidi))

total_revenue <- read.csv("outputs/dte_total_revenue_2019_2024.csv")

total_revenue_2024 <- total_revenue %>%
  mutate(year = year(date)) %>%
  filter(year == 2024) %>%
  summarize(revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(revenue)

total_customer_counts <- read.csv("outputs/total_customer_counts.csv")
li_customer_counts <- read.csv("outputs/li_customer_counts.csv")

# Remove customers from that would be double counted across supplemental rate plans
supplemental_rate_plans <- c("D1.1", "D1.9", "D2", "D5", "D9")

# Calculating customer count by income from DTE data
dte_customers_counts <- total_customer_counts %>%
  filter(!(rate_plan %in% supplemental_rate_plans)) %>%
  filter(!is.na(num_customers)) %>%
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

dte_total_customer_count <- dte_customers_counts %>%
  summarize(total_customers = sum(mean_total_customers, na.rm = TRUE)) %>%
  pull(total_customers)

# US census estimates for households within DTE service territory
dte_tracts_customers <- occupied_housing_units %>%
  filter(GEOID %in% unique(dte_lead$GEOID)) %>%
  mutate(prop_hh = hh_count / sum(hh_count, na.rm = TRUE)) %>%
  rename(census_hh_count = hh_count) %>%
  mutate(dte_customers = dte_total_customer_count * prop_hh) %>%
  left_join(
    li_non_li_hh %>%
      filter(fpl_cat == "low-income") %>%
      select(GEOID, li_percent = percent),
    by = c("GEOID")
  ) %>%
  mutate(dte_li_customers = dte_customers * (li_percent / 100)) %>%
  mutate(dte_non_li_customers = dte_customers - dte_li_customers)

# Analysis======================================================================

# Calculating DTE LEAD proportion of bills that are due to electric consumption alone
dte_lead %>%
  group_by(GEOID) %>%
  summarize(
    elep = weighted.mean(elep, units, na.rm = TRUE),
    gasp = weighted.mean(gasp, units, na.rm = TRUE),
    fulp = weighted.mean(fulp, units, na.rm = TRUE),
    units = sum(units, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(total_elec_costs = elep * units,
         total_costs = (elep + gasp + fulp) * units) %>%
  mutate(prop_elec = total_elec_costs / total_costs) %>%
  summarize(prop_elec = mean(prop_elec, na.rm = TRUE))

# Calculating the proportion of total energy consumption by census tract
dte_doe_consumption <- dte_lead %>%
  group_by(GEOID) %>%
  summarize(
    elep = weighted.mean(elep, units, na.rm = TRUE),
    gasp = weighted.mean(gasp, units, na.rm = TRUE),
    fulp = weighted.mean(fulp, units, na.rm = TRUE),
    units = sum(units, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(total_elec_costs = elep * units) %>%
  group_by(GEOID) %>%
  summarize(lead_elec_costs = sum(total_elec_costs, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_lead = lead_elec_costs / sum(lead_elec_costs, na.rm = TRUE))

# Broadcast total DTE revenue across service territory using DOE LEAD proportions
dte_doe_consumption <- dte_doe_consumption %>%
  mutate(dte_energy_costs = prop_lead * total_revenue_2024) %>%
  # adding number of households
  left_join(
    dte_tracts_customers %>%
      select(
        GEOID,
        total_customers = dte_customers,
        li_customers = dte_li_customers,
        non_li_customers = dte_non_li_customers
      ),
    by = c("GEOID")
  ) %>%
  # adding median income
  left_join(
    avg_household_income,
    by = c("GEOID")
  ) %>%
  mutate(avg_annual_hh_dte_elec_costs = dte_energy_costs / total_customers) %>%
  mutate(burden_e = avg_annual_hh_dte_elec_costs / hh_mean_income)

write.csv(
  dte_doe_consumption,
  "outputs/dte_doe_consumption.csv",
  row.names = FALSE
)

dte_lead %>%
  group_by(GEOID) %>%
  summarize(
    elep = weighted.mean(elep, units, na.rm = TRUE),
    gasp = weighted.mean(gasp, units, na.rm = TRUE),
    fulp = weighted.mean(fulp, units, na.rm = TRUE),
    units = sum(units, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(total_cost = elep + gasp + fulp) %>%
  mutate(prop_elec = elep / total_cost) %>%
  summarize(
    elep = weighted.mean(elep, units, na.rm = TRUE),
    total_cost = weighted.mean(total_cost, units, na.rm = TRUE),
    prop_elec = weighted.mean(prop_elec, units, na.rm = TRUE)
  )

pipp <- dte_doe_consumption %>%
  mutate(hh_med_income = case_when(
    hh_med_income < 0 ~ hh_mean_income,
    TRUE ~ hh_med_income
  )) %>%
  mutate(target_burden_e = 0.0327) %>%
  mutate(target_dte_hh_elec_costs = hh_med_income * target_burden_e) %>%
  mutate(affordability_gap = avg_annual_hh_dte_elec_costs - target_dte_hh_elec_costs) %>%
  filter(burden_e > 0.0327)

write.csv(
  dte_doe_consumption,
  "outputs/pipp.csv",
  row.names = FALSE
)

pipp %>%
  summarize(affordability_gap = sum(affordability_gap * total_customers, na.rm = TRUE))

pipp %>%
  summarize(affordability_gap = sum(affordability_gap * li_customers, na.rm = TRUE))

pipp %>%
  summarize(affordability_gap = sum(affordability_gap * non_li_customers, na.rm = TRUE))

pipp %>%
  summarize(total_customers = sum(total_customers, na.rm = TRUE))

pipp %>%
  summarize(li_customers = sum(li_customers, na.rm = TRUE))

pipp %>%
  summarize(non_li_customers = sum(non_li_customers, na.rm = TRUE))

# Data summaries around unaffordability


