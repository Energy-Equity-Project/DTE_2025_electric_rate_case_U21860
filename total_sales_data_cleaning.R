
# Libraries
library(tidyverse)
library(readxl)
library(janitor)

# Directory structure
outdir <- "outputs"

total_revenue <- read_excel("../../Data/DTE/DTE Electric Rate case 2025/U-21860 DAAODE-3.9-01 Total Residential Revenue 2019 - 2024.xlsx") %>%
  clean_names() %>%
  select(-x74)

# Total DTE revenue
total_revenue <- total_revenue[1:17, 1:ncol(total_revenue)] %>%
  rename(rate_plan = x1) %>%
  pivot_longer(-c(rate_plan), names_to = "date", values_to = "revenue") %>%
  mutate(date = gsub("x", "", date)) %>%
  mutate(date = gsub("_", "/", date)) %>%
  mutate(date = paste0(date, "/01")) %>%
  mutate(date = as.Date.character(date, format = "%Y/%m/%d"))

write.csv(
  total_revenue,
  file.path(outdir, "dte_total_revenue_2019_2024.csv"),
  row.names = FALSE
)

total_sales_and_customers <- read_excel("../../Data/DTE/DTE Electric Rate case 2025/U-21860 DAAODE-3.7-8-01 Total Residential Count and Sales 2024.xlsx")


# Total number of DTE kWh sold
total_sales_kwh <- total_sales_and_customers[c(1, 3:18), c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename(rate_plan = residential) %>%
  pivot_longer(-rate_plan, names_to = "date", values_to = "sales_kWh") %>%
  mutate(date = gsub("x", "", date)) %>%
  mutate(date = gsub("_", "-", date)) %>%
  mutate(date = paste0(date, "-01")) %>%
  mutate(date = as.Date.character(date, format = "%Y-%m-%d"))

write.csv(
  total_sales_kwh,
  "outputs/total_sales_kwh.csv",
  row.names = FALSE
)


# Total number of DTE customers
total_customer_counts <- total_sales_and_customers[c(1, 24:39), c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename(rate_plan = residential) %>%
  pivot_longer(-c(rate_plan), names_to = "date", values_to = "num_customers") %>%
  mutate(date = gsub("x", "", date)) %>%
  mutate(date = gsub("_", "-", date)) %>%
  mutate(date = paste0(date, "-01")) %>%
  mutate(date = as.Date.character(date, format = "%Y-%m-%d"),
         num_customers = as.numeric(num_customers))

write.csv(
  total_customer_counts,
  "outputs/total_customer_counts.csv",
  row.names = FALSE
)

