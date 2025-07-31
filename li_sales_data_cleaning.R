
# Libraries=====================================================================
library(tidyverse)
library(readxl)
library(janitor)

# DTE provided data=============================================================

# U-21860 DAAODE-3.11-13-01 Low Income Monthly Sales, Revenue and Counts by Rate Schedule
li_sales <- read_excel("../../Data/DTE/DTE Electric Rate case 2025/U-21860 DAAODE-3.11-13-01 Low Income Monthly Sales, Revenue and Counts by Rate Schedule.xlsx")

li_sales <- li_sales[7:55, 1:ncol(li_sales)] %>%
  rename(date = "...1") %>%
  mutate(date = as.integer(date)) %>%
  mutate(date = as.character(date)) %>%
  mutate(date = paste0(date, "01")) %>%
  mutate(date = as.Date.character(date, format="%Y%m%d"))

# Low income revenue by rate schedule
li_revenues <- li_sales[1:nrow(li_sales), 1:15] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename(date = na) %>%
  pivot_longer(-c(date), names_to = "rate_plan", values_to = "revenue") %>%
  mutate(revenue = as.numeric(revenue))

write.csv(
  li_revenues,
  "outputs/li_revenues.csv",
  row.names = FALSE
)

# Billed sales (kWh) to low income customers by rate schedule
li_kwh <- li_sales[1:nrow(li_sales), c(1, 16:29)] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename(date = na) %>%
  pivot_longer(-c(date), names_to = "rate_plan", values_to = "kWh") %>%
  mutate(kWh = as.numeric(kWh))

write.csv(
  li_kwh,
  "outputs/li_kwh.csv",
  row.names = FALSE
)

# Counts of low income customers by rate schedule
li_customer_counts <- li_sales[1:nrow(li_sales), c(1, 30:43)] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename(date = na) %>%
  pivot_longer(-c(date), names_to = "rate_plan", values_to = "customer_count") %>%
  mutate(customer_count = as.numeric(customer_count))

write.csv(
  li_customer_counts,
  "outputs/li_customer_counts.csv",
  row.names = FALSE
)

# Average bill by customer
li_avg_bill_per_customer <- li_revenues %>%
  left_join(
    li_customer_counts,
    by = c("date", "rate_plan")
  ) %>%
  mutate(monthly_bill = revenue / customer_count)

avg_bill_per_customer %>%
  filter(rate_plan == "d1_11" |
           rate_plan == "d1") %>%
  filter(date != "2023-03-01") %>%
  ggplot(aes(x = date, y = monthly_bill, color = rate_plan)) +
  geom_line() +
  theme_bw()
