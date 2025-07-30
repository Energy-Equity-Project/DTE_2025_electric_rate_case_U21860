
# Libraries
library(tidyverse)
library(readxl)
library(janitor)

# Directory structure
outdir <- "outputs"

total_sales <- read_excel("../../Data/DTE/DTE Electric Rate case 2025/U-21860 DAAODE-3.9-01 Total Residential Revenue 2019 - 2024.xlsx") %>%
  clean_names() %>%
  select(-x74)

total_sales <- total_sales[1:17, 1:ncol(total_sales)] %>%
  rename(rate_plan = x1) %>%
  pivot_longer(-c(rate_plan), names_to = "date", values_to = "revenue") %>%
  mutate(date = gsub("x", "", date)) %>%
  mutate(date = gsub("_", "/", date)) %>%
  mutate(date = paste0(date, "/01")) %>%
  mutate(date = as.Date.character(date, format = "%Y/%m/%d"))

write.csv(
  total_sales,
  file.path(outdir, "dte_total_revenue_2019_2024.csv"),
  row.names = FALSE
)

