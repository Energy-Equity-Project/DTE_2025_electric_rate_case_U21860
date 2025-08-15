
# Libraries
library(tidyverse)
library(janitor)
library(matrixStats)

dte_lead <- read.csv("outputs/dte_lead.csv")

dte_df <- read.csv("outputs/dte_df.csv")

# Summarizing number of LI customers in DTE
dte_df %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = 100 * (dte_customers / sum(dte_customers, na.rm = TRUE)))

# Number of customers that have unaffordable electric burdens
dte_df %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  group_by(affordable_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  filter(affordable_cat == "unaffordable") %>%
  group_by(income_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat, affordable_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()

# 2025 rate increase of 4.65%
dte_df %>%
  mutate(avg_hh_dte_energy_costs_2025 = avg_hh_dte_energy_costs * 1.0465) %>%
  mutate(dte_burden_e_2025 = avg_hh_dte_energy_costs_2025 / hincp) %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e_2025 > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  group_by(affordable_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2025 = avg_hh_dte_energy_costs * 1.0465) %>%
  mutate(dte_burden_e_2025 = avg_hh_dte_energy_costs_2025 / hincp) %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e_2025 > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  filter(affordable_cat == "unaffordable") %>%
  group_by(income_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2025 = avg_hh_dte_energy_costs * 1.0465) %>%
  mutate(dte_burden_e_2025 = avg_hh_dte_energy_costs_2025 / hincp) %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e_2025 > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat, affordable_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()


# 2026 rate increase of 11.1% (on top of 4.65% rate increase)
dte_df %>%
  mutate(avg_hh_dte_energy_costs_2026 = avg_hh_dte_energy_costs * (1.0465 * 1.111)) %>%
  mutate(dte_burden_e_2026 = avg_hh_dte_energy_costs_2026 / hincp) %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e_2026 > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  group_by(affordable_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2026 = avg_hh_dte_energy_costs * (1.0465 * 1.111)) %>%
  mutate(dte_burden_e_2026 = avg_hh_dte_energy_costs_2026 / hincp) %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e_2026 > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  filter(affordable_cat == "unaffordable") %>%
  group_by(income_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2026 = avg_hh_dte_energy_costs * (1.0465 * 1.111)) %>%
  mutate(dte_burden_e_2026 = avg_hh_dte_energy_costs_2026 / hincp) %>%
  mutate(
    affordable_cat = case_when(
      dte_burden_e_2026 > 0.06 * 0.5993347 ~ "unaffordable",
      TRUE ~ "affordable"
    )
  ) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat, affordable_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup()


# Electric only burdens=========================================================

# 2024
dte_df %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat) %>%
  summarize(med_hh_burden_e = weightedMedian(dte_burden_e, dte_customers, na.rm = TRUE),
            med_hh_dte_elec_costs = weightedMedian(avg_hh_dte_energy_costs, dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  summarize(
    med_hh_burden_e = weightedMedian(dte_burden_e, dte_customers, na.rm = TRUE),
    med_hh_dte_elec_costs = weightedMedian(avg_hh_dte_energy_costs, dte_customers, na.rm = TRUE),
    mean_hh_burden_e = weighted.mean(dte_burden_e, dte_customers, na.rm = TRUE),
    mean_hh_dte_elec_costs = weighted.mean(avg_hh_dte_energy_costs, dte_customers, na.rm = TRUE)
  ) %>%
  ungroup()

# 2024
dte_df %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat) %>%
  summarize(mean_hh_burden_e = weighted.mean(dte_burden_e, dte_customers, na.rm = TRUE),
            mean_hh_dte_elec_costs = weighted.mean(avg_hh_dte_energy_costs, dte_customers, na.rm = TRUE)) %>%
  ungroup()

# 2025

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2025 = avg_hh_dte_energy_costs * 1.0465) %>%
  mutate(dte_burden_e_2025 = avg_hh_dte_energy_costs_2025 / hincp) %>%
  summarize(
    med_hh_burden_e = weightedMedian(dte_burden_e_2025, dte_customers, na.rm = TRUE),
    med_hh_dte_elec_costs = weightedMedian(avg_hh_dte_energy_costs_2025, dte_customers, na.rm = TRUE),
    mean_hh_burden_e = weighted.mean(dte_burden_e_2025, dte_customers, na.rm = TRUE),
    mean_hh_dte_elec_costs = weighted.mean(avg_hh_dte_energy_costs_2025, dte_customers, na.rm = TRUE)
  ) %>%
  ungroup()


dte_df %>%
  mutate(avg_hh_dte_energy_costs_2025 = avg_hh_dte_energy_costs * 1.0465) %>%
  mutate(dte_burden_e_2025 = avg_hh_dte_energy_costs_2025 / hincp) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat) %>%
  summarize(med_hh_burden_e = weightedMedian(dte_burden_e_2025, dte_customers, na.rm = TRUE),
            med_hh_dte_elec_costs = weightedMedian(avg_hh_dte_energy_costs_2025, dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2025 = avg_hh_dte_energy_costs * 1.0465) %>%
  mutate(dte_burden_e_2025 = avg_hh_dte_energy_costs_2025 / hincp) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat) %>%
  summarize(mean_hh_burden_e = weighted.mean(dte_burden_e_2025, dte_customers, na.rm = TRUE),
            mean_hh_dte_elec_costs = weighted.mean(avg_hh_dte_energy_costs_2025, dte_customers, na.rm = TRUE)) %>%
  ungroup()

# 2026
dte_df %>%
  mutate(avg_hh_dte_energy_costs_2026 = avg_hh_dte_energy_costs * (1.0465 * 1.111)) %>%
  mutate(dte_burden_e_2026 = avg_hh_dte_energy_costs_2026 / hincp) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat) %>%
  summarize(med_hh_burden_e = weightedMedian(dte_burden_e_2026, dte_customers, na.rm = TRUE),
            med_hh_dte_elec_costs = weightedMedian(avg_hh_dte_energy_costs_2026, dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2026 = avg_hh_dte_energy_costs * (1.0465 * 1.111)) %>%
  mutate(dte_burden_e_2026 = avg_hh_dte_energy_costs_2026 / hincp) %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(income_cat) %>%
  summarize(mean_hh_burden_e = weighted.mean(dte_burden_e_2026, dte_customers, na.rm = TRUE),
            mean_hh_dte_elec_costs = weighted.mean(avg_hh_dte_energy_costs_2026, dte_customers, na.rm = TRUE)) %>%
  ungroup()

dte_df %>%
  mutate(avg_hh_dte_energy_costs_2026 = avg_hh_dte_energy_costs * (1.0465 * 1.111)) %>%
  mutate(dte_burden_e_2026 = avg_hh_dte_energy_costs_2026 / hincp) %>%
  summarize(mean_hh_burden_e = weighted.mean(dte_burden_e_2026, dte_customers, na.rm = TRUE),
            mean_hh_dte_elec_costs = weighted.mean(avg_hh_dte_energy_costs_2026, dte_customers, na.rm = TRUE)) %>%
  ungroup()


dte_df %>%
  mutate(avg_hh_dte_energy_costs_2026 = avg_hh_dte_energy_costs * (1.0465 * 1.111)) %>%
  mutate(dte_burden_e_2026 = avg_hh_dte_energy_costs_2026 / hincp) %>%
  summarize(
    med_hh_burden_e = weightedMedian(dte_burden_e_2026, dte_customers, na.rm = TRUE),
    med_hh_dte_elec_costs = weightedMedian(avg_hh_dte_energy_costs_2026, dte_customers, na.rm = TRUE),
    mean_hh_burden_e = weighted.mean(dte_burden_e_2026, dte_customers, na.rm = TRUE),
    mean_hh_dte_elec_costs = weighted.mean(avg_hh_dte_energy_costs_2026, dte_customers, na.rm = TRUE)
  ) %>%
  ungroup()


