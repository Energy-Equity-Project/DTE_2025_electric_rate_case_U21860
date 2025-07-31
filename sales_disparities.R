
# Libraries
library(tidyverse)
library(janitor)
library(lubridate)

# Compare total revenue to low-income revenue

# Total sales data
total_kwh <- read.csv("outputs/total_sales_kwh.csv")
total_customer_counts <- read.csv("outputs/total_customer_counts.csv")
total_revenue <- read.csv("outputs/dte_total_revenue_2019_2024.csv")

# Low sales data
li_kwh <- read.csv("outputs/li_kwh.csv")
li_customer_counts <- read.csv("outputs/li_customer_counts.csv")
li_revenue <- read.csv("outputs/li_revenues.csv")

# Non low-income
non_li_customer_counts <- total_customer_counts %>%
  mutate(rate_plan = tolower(rate_plan)) %>%
  mutate(rate_plan = gsub("\\.", "_", rate_plan)) %>%
  rename(total_customers = num_customers) %>%
  left_join(
    li_customer_counts %>%
      rename(li_customers = customer_count),
    by = c("rate_plan", "date")
  ) %>%
  mutate(non_li_customers = total_customers - li_customers)

non_li_revenue <- total_revenue %>%
  mutate(year = year(date)) %>%
  filter(year == 2024) %>%
  mutate(rate_plan = tolower(rate_plan)) %>%
  mutate(rate_plan = gsub("\\.", "_", rate_plan)) %>%
  rename(all_customers_revenue = revenue) %>%
  left_join(
    li_revenue %>%
      rename(li_customers_revenue = revenue),
    by = c("date", "rate_plan")
  ) %>%
  mutate(non_li_customers_revenue = all_customers_revenue - li_customers_revenue)

# Low income revenue as a percent of total revenue
revenue <- non_li_revenue %>%
  mutate(percent_li_revenue = 100 * (li_customers_revenue / all_customers_revenue)) %>%
  filter(rate_plan != "d1_6")

# Average li revenue as a percent of all DTE revenue is ~3.4%
revenue %>%
  summarize(percent_li_revenue = mean(percent_li_revenue, na.rm = TRUE))

avg_customer_bills <- non_li_revenue %>%
  select(date, rate_plan, li_customers_revenue, non_li_customers_revenue) %>%
  left_join(
    non_li_customer_counts %>%
      select(date, rate_plan, li_customers, non_li_customers),
    by = c("date", "rate_plan")
  ) %>%
  mutate(
    avg_li_bill = li_customers_revenue / li_customers,
    avg_non_li_bill = non_li_customers_revenue / non_li_customers
  )

non_li_rates <- total_kwh %>%
  mutate(year = year(date)) %>%
  filter(year == 2024) %>%
  mutate(rate_plan = tolower(rate_plan)) %>%
  mutate(rate_plan = gsub("\\.", "_", rate_plan)) %>%
  rename(total_sales_kWh = sales_kWh) %>%
  left_join(
    li_kwh %>%
      rename(li_sales_kWh = kWh),
    by = c("date", "rate_plan")
  ) %>%
  mutate(non_li_kWh = total_sales_kWh - li_sales_kWh) %>%
  left_join(
    non_li_customer_counts,
    by = c("date", "rate_plan")
  ) %>%
  left_join(
    non_li_revenue %>%
      select(rate_plan, date, li_customers_revenue, non_li_customers_revenue),
    by = c("date", "rate_plan")
  ) %>%
  mutate(li_rate = (li_customers_revenue / li_sales_kWh),
         non_li_rate = (non_li_customers_revenue / non_li_kWh)) %>%
  mutate(rate_difference = non_li_rate - li_rate)

non_li_rates %>%
  filter(rate_plan == "d1_11") %>%
  summarize(mean_diff = mean(rate_difference, na.rm = TRUE))

# Average percent kWH sold to li is ~3.4% of total kWh sold
non_li_rates %>%
  filter(rate_plan != "d1_6") %>%
  mutate(percent_li_kwh = 100 * (li_sales_kWh / total_sales_kWh)) %>%
  summarize(avg_percent = mean(percent_li_kwh, na.rm = TRUE))

