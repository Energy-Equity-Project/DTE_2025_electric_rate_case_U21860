
# Libraries=====================================================================
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# DTE provided data

# U-21860 DAAODE-3.10 Electric Disconnects by Month
shutoffs_raw <- read.excel("../../Data/DTE/DTE Electric Rate case 2025/")
shutoffs_raw <- read_excel("../../Data/DTE/DTE Electric Rate case 2025/U-21860 DAAODE-3.10 Electric Disconnects by Month.xlsx") %>%
  row_to_names(row_number = 7) %>%
  clean_names()

shutoffs <- shutoffs_raw[c(2,3,6), 1:(ncol(shutoffs_raw)-1)] %>%
  pivot_longer(-electric_disconnects, names_to = "month") %>%
  mutate(year = 2019) %>%
  bind_rows(
    shutoffs_raw[c(10,11,14), 1:(ncol(shutoffs_raw)-1)] %>%
      pivot_longer(-electric_disconnects, names_to = "month") %>%
      mutate(year = 2020)
  ) %>%
  bind_rows(
    shutoffs_raw[c(18,19,22), 1:(ncol(shutoffs_raw)-1)] %>%
      pivot_longer(-electric_disconnects, names_to = "month") %>%
      mutate(year = 2021)
  ) %>%
  bind_rows(
    shutoffs_raw[c(26,27,30), 1:(ncol(shutoffs_raw)-1)] %>%
      pivot_longer(-electric_disconnects, names_to = "month") %>%
      mutate(year = 2022)
  ) %>%
  bind_rows(
    shutoffs_raw[c(34,35,38), 1:(ncol(shutoffs_raw)-1)] %>%
      pivot_longer(-electric_disconnects, names_to = "month") %>%
      mutate(year = 2023)
  ) %>%
  bind_rows(
    shutoffs_raw[c(42,43,46), 1:(ncol(shutoffs_raw)-1)] %>%
      pivot_longer(-electric_disconnects, names_to = "month") %>%
      mutate(year = 2024)
  ) %>%
  mutate(date = paste0(year, "-", month, "-01")) %>%
  mutate(date = ymd(date)) %>%
  select(-c(month, year)) %>%
  rename(num_shutoffs = value) %>%
  mutate(num_shutoffs = as.numeric(num_shutoffs)) %>%
  mutate(electric_disconnects = case_when(
    electric_disconnects == "Field Disconnects" ~ "Field Disconnects (meter)",
    TRUE ~ electric_disconnects
  ))

shutoffs %>%
  group_by(date) %>%
  summarize(num_shutoffs = sum(num_shutoffs, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = num_shutoffs)) +
  geom_area(fill = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  labs(
    x = "",
    y = "DTE number of shutoffs",
    caption = 
  )

tmp <- shutoffs %>%
  group_by(date) %>%
  summarize(total_shutoffs = sum(num_shutoffs, na.rm = TRUE)) %>%
  ungroup()

