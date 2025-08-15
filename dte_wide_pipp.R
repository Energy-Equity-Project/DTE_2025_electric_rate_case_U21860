
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
  mutate(rate_plan = tolower(gsub("\\.", "_", rate_plan)))

dte_total_customer_count <- dte_customers_counts %>%
  summarize(mean_total_customers = sum(mean_total_customers, na.rm = TRUE)) %>%
  pull(mean_total_customers)

# How much of utility bills are due to electric consumption: 0.5993347
dte_lead %>%
  # LI customers
  filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(elep = weighted.mean(elep, units, na.rm = TRUE),
            total_cost = weighted.mean(total_cost, units, na.rm = TRUE)) %>%
  mutate(elep / total_cost)

dte_lead %>%
  filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(
    elep = weighted.mean(elep, units, na.rm = TRUE),
    total_cost = weighted.mean(total_cost, units, na.rm = TRUE)
  )


dte_df <- dte_lead %>%
  mutate(total_electric_costs = elep * units) %>%
  mutate(prop_lead = total_electric_costs / sum(total_electric_costs, na.rm = TRUE)) %>%
  mutate(dte_energy_costs = prop_lead * total_revenue_2024) %>%
  mutate(prop_lead_units = units / sum(units, na.rm = TRUE)) %>%
  mutate(dte_customers = prop_lead_units * dte_total_customer_count) %>%
  mutate(avg_hh_dte_energy_costs = dte_energy_costs / dte_customers) %>%
  mutate(dte_burden_e = avg_hh_dte_energy_costs / hincp)

write.csv(
  dte_df,
  "outputs/dte_df.csv",
  row.names = FALSE
)

dte_pipp <- dte_df %>%
  mutate(
    target_burden_e_6 = 0.06 * 0.5993347,
    target_burden_e_10 = 0.1 * 0.5993347
  ) %>%
  mutate(
    target_avg_hh_elec_cost_6 = hincp * target_burden_e_6,
    target_avg_hh_elec_cost_10 = hincp * target_burden_e_10,
  ) %>%
  mutate(
    affordability_gap_6 = avg_hh_dte_energy_costs - target_avg_hh_elec_cost_6,
    affordability_gap_10 = avg_hh_dte_energy_costs - target_avg_hh_elec_cost_10
  )

dte_df %>%
  filter(!(fpl150 %in% c("0-100%", "100-150%", "150-200%"))) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))

dte_df %>%
  # filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))


# DTE wide PIPP affordability gap for 6% total energy burden====================

# Total affordability gap
dte_pipp %>%
  filter(affordability_gap_6 > 0) %>%
  summarize(affordability_gap_6 = sum(affordability_gap_6 * dte_customers, na.rm = TRUE))

# Affordability gap for LI customers only
dte_pipp %>%
  filter(affordability_gap_6 > 0) %>%
  filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(affordability_gap_6 = sum(affordability_gap_6 * dte_customers, na.rm = TRUE))

# total number of DTE customers served by PIPP
dte_pipp %>%
  filter(affordability_gap_6 > 0) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))

# LI customers served by PIPP
dte_pipp %>%
  filter(affordability_gap_6 > 0) %>%
  filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))


# DTE wide PIPP affordability gap for 10% total energy burden===================

# Total affordability gap
dte_pipp %>%
  filter(affordability_gap_10 > 0) %>%
  summarize(affordability_gap_10 = sum(affordability_gap_6 * dte_customers, na.rm = TRUE))

# Affordability gap from LI customers only
dte_pipp %>%
  filter(affordability_gap_10 > 0) %>%
  filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(affordability_gap_10 = sum(affordability_gap_6 * dte_customers, na.rm = TRUE))

# total number of DTE customers served by PIPP
dte_pipp %>%
  filter(affordability_gap_10 > 0) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))

# LI customers served by PIPP
dte_pipp %>%
  filter(affordability_gap_10 > 0) %>%
  filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))

  