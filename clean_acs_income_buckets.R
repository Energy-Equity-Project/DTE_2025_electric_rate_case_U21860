
# Libraries
library(tidyverse)
library(janitor)

income_buckets_raw <- read.csv("outputs/income_buckets_2023.csv")

income_buckets_clean <- income_buckets_raw %>%
  select(GEOID = GEO_ID, ends_with("E")) %>%
  select(
    GEOID,
    S1901_C01_001E, # all households
    S1901_C01_002E, # 0 - 10k
    S1901_C01_003E, # 10- 15k
    S1901_C01_004E, # 15 - 25k
    S1901_C01_005E, # 25 - 35k
    S1901_C01_006E, # 35 - 50k
    S1901_C01_007E, # 50 - 75k
    S1901_C01_008E, # 75 - 100k
    S1901_C01_009E, # 100 - 150k
    S1901_C01_010E, # 150 - 200k
    S1901_C01_011E # 200k+
  ) %>%
  pivot_longer(
    -c(GEOID, S1901_C01_001E), names_to = "income_range", values_to = "percent_hh"
  ) %>%
  rename(total_hh = S1901_C01_001E) %>%
  mutate(hh_count = total_hh * (percent_hh / 100)) %>%
  mutate(GEOID = as.numeric(gsub("1400000US", "", GEOID)))


write.csv(
  income_buckets_clean,
  "outputs/income_buckets_clean_2023.csv",
  row.names = FALSE
)

dte_lead <- read.csv("outputs/dte_lead.csv")

# for an income of 50116========================================================

interested_range <- 75000 - 50000
interval_in_range <- 50116 - 50000
percent_within_range <- interval_in_range / interested_range

total_customer_counts <- read.csv("outputs/total_customer_counts.csv")

# Remove customers from that would be double counted across supplemental rate plans
supplemental_rate_plans <- c("D1.1", "D1.9", "D2", "D5", "D9")

dte_customers_summary <- total_customer_counts %>%
  filter(!(rate_plan %in% supplemental_rate_plans)) %>%
  filter(!is.na(num_customers)) %>%
  group_by(rate_plan) %>%
  summarize(mean_total_customers = mean(num_customers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rate_plan = tolower(gsub("\\.", "_", rate_plan)))

dte_total_customers <- dte_customers_summary %>%
  summarize(mean_total_customers = sum(mean_total_customers, na.rm = TRUE)) %>%
  pull(mean_total_customers)

# only include DTE tracts
dte_income_buckets <- income_buckets_clean %>%
  filter(GEOID %in% unique(dte_lead$GEOID)) %>%
  mutate(prop_acs_hh = hh_count / sum(hh_count, na.rm = TRUE)) %>%
  mutate(dte_customers = prop_acs_hh * dte_total_customers)

tmp <- dte_income_buckets %>%
  filter(income_range %in% c("S1901_C01_002E", "S1901_C01_003E", "S1901_C01_004E", "S1901_C01_005E", "S1901_C01_006E", "S1901_C01_007E")) %>%
  mutate(
    dte_customers = case_when(
      income_range == "S1901_C01_007E" ~ dte_customers * percent_within_range,
      TRUE ~ dte_customers
    )
  )

tmp %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))

dte_customers_summary %>%
  summarize(mean_total_customers = sum(mean_total_customers, na.rm =TRUE))

# for an income of 40,583=======================================================
interested_range <- 50000 - 35000
interval_in_range <- 40583 - 35000
percent_within_range <- interval_in_range / interested_range

total_customer_counts <- read.csv("outputs/total_customer_counts.csv")

# Remove customers from that would be double counted across supplemental rate plans
supplemental_rate_plans <- c("D1.1", "D1.9", "D2", "D5", "D9")

dte_customers_summary <- total_customer_counts %>%
  filter(!(rate_plan %in% supplemental_rate_plans)) %>%
  filter(!is.na(num_customers)) %>%
  group_by(rate_plan) %>%
  summarize(mean_total_customers = mean(num_customers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rate_plan = tolower(gsub("\\.", "_", rate_plan)))

dte_total_customers <- dte_customers_summary %>%
  summarize(mean_total_customers = sum(mean_total_customers, na.rm = TRUE)) %>%
  pull(mean_total_customers)

# only include DTE tracts
dte_income_buckets <- income_buckets_clean %>%
  filter(GEOID %in% unique(dte_lead$GEOID)) %>%
  mutate(prop_acs_hh = hh_count / sum(hh_count, na.rm = TRUE)) %>%
  mutate(dte_customers = prop_acs_hh * dte_total_customers)

tmp <- dte_income_buckets %>%
  filter(income_range %in% c("S1901_C01_002E", "S1901_C01_003E", "S1901_C01_004E", "S1901_C01_005E", "S1901_C01_006E")) %>%
  mutate(
    dte_customers = case_when(
      income_range == "S1901_C01_006E" ~ dte_customers * percent_within_range,
      TRUE ~ dte_customers
    )
  )

tmp %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE))

dte_customers_summary %>%
  summarize(mean_total_customers = sum(mean_total_customers, na.rm =TRUE))

  

