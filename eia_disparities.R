
# Libraries
library(tidyverse)
library(janitor)
library(ggrepel)

eia <- read.csv("../../Cleaned Data/EIA/Form_861/form_861_utility_sales.csv")

tmp <- eia %>%
  filter(utility_name == "DTE Electric Company") %>%
  filter(data_year == 2015 |
           data_year == 2019 |
           data_year == 2020 |
           data_year == 2023) %>%
  select(data_year, customer_base, rate_c_per_kWh) %>%
  pivot_wider(names_from = data_year, names_prefix = "year_", values_from = rate_c_per_kWh) %>%
  mutate(rate_difference_23_15 = year_2023 - year_2015) %>%
  mutate(rate_percent_diff_23_15 = rate_difference_23_15 / year_2015) %>%
  mutate(rate_difference_23_20 = year_2023 - year_2020) %>%
  mutate(rate_percent_diff_23_20 = rate_difference_23_20 / year_2020) %>%
  mutate(rate_difference_19_15 = year_2019 - year_2015) %>%
  mutate(rate_percent_diff_19_15 = rate_difference_19_15 / year_2015)

eia %>%
  mutate(customer_base = str_to_sentence(customer_base)) %>%
  mutate(customer_base = factor(customer_base, levels = c("Residential", "Industrial", "Commercial"))) %>%
  filter(utility_name == "DTE Electric Company") %>%
  ggplot(aes(x = data_year, y = rate_c_per_kWh, color = customer_base)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("darkblue", "darkred", "orange")) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  scale_x_continuous(limits = c(2015, 2023), breaks = seq(2015, 2023, 1)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(family = "Inter", color = "grey50", size = 10, hjust = 0, vjust = 0)
  ) +
  labs(
    x = "",
    y = "DTE rate (cents per kWh)",
    color = "Customer base",
    caption = "Data source:\nEIA Form 861, 2023"
  )

# Rate disparities across utilities
eia_sales <- read_excel("../../Data/EIA/861/Form 861 - unzipped/Sales_Ult_Cust_2023.xlsx")
residential <- eia_sales[1:nrow(eia_sales), 1:12] %>%
  row_to_names(row_number = 2) %>%
  clean_names() %>%
  mutate(
    thousand_dollars = as.numeric(thousand_dollars),
    megawatthours = as.numeric(megawatthours),
    count = as.numeric(count)
  ) %>%
  mutate(
    sales_usd = thousand_dollars * 1000,
    sales_kwh = megawatthours * 1000
  )

industrial <- eia_sales[1:nrow(eia_sales), c(1:9, 16:18)] %>%
  row_to_names(row_number = 2) %>%
  clean_names() %>%
  mutate(
    thousand_dollars = as.numeric(thousand_dollars),
    megawatthours = as.numeric(megawatthours),
    count = as.numeric(count)
  ) %>%
  mutate(
    sales_usd = thousand_dollars * 1000,
    sales_kwh = megawatthours * 1000
  )
  
residential_customers <- residential %>%
  filter(ownership != "Behind the Meter") %>%
  filter(part != "C") %>%
  group_by(utility_name) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

residential_rates <- residential %>%
  filter(ownership != "Behind the Meter") %>%
  group_by(utility_name) %>%
  summarize(
    sales_usd = sum(sales_usd, na.rm = TRUE),
    sales_kwh = sum(sales_kwh, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(rate = (sales_usd * 100) / sales_kwh)

industrial_customers <- industrial %>%
  filter(ownership != "Behind the Meter") %>%
  filter(part != "C") %>%
  group_by(utility_name) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

industrial_rates <- industrial %>%
  filter(ownership != "Behind the Meter") %>%
  group_by(utility_name) %>%
  summarize(
    sales_usd = sum(sales_usd, na.rm = TRUE),
    sales_kwh = sum(sales_kwh, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(rate = (sales_usd * 100) / sales_kwh)

residential_df <- residential_customers %>%
  full_join(
    residential_rates,
    by = c("utility_name")
  )

industrial_df <- industrial_customers %>%
  full_join(
    industrial_rates,
    by = c("utility_name")
  )

comparing_disparities <- residential_df %>%
  select(utility_name, residential_rate = rate) %>%
  full_join(
    industrial_df %>%
      select(utility_name, industrial_rate = rate),
    by = c("utility_name")
  ) %>%
  mutate(difference = residential_rate - industrial_rate) %>%
  mutate(ratio = residential_rate / industrial_rate)


comparing_disparities %>%
  filter(!is.na(residential_rate) & !is.na(industrial_rate)) %>%
  left_join(
    residential %>%
      select(utility_name, state),
    by = c("utility_name")
  ) %>%
  filter(state == "MI") %>%
  ggplot(aes(x = industrial_rate, y = residential_rate, label = utility_name)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(0, 30)) +
  theme_bw()


comparing_disparities %>%
  filter(!is.na(residential_rate) & !is.na(industrial_rate)) %>%
  left_join(
    residential %>%
      select(utility_name, state) %>%
      distinct(),
    by = c("utility_name")
  ) %>%
  filter(state == "MI") %>%
  arrange(ratio) %>%
  mutate(utility_name = factor(utility_name, levels = unique(utility_name))) %>%
  mutate(
    name_signal = case_when(
      utility_name == "DTE Electric Company" ~ "DTE",
      TRUE ~ "other"
    )
  ) %>%
  ggplot(aes(x = ratio, y = utility_name, fill = name_signal)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkred", "#909090")) +
  scale_x_continuous(limits = c(0, 3.7), breaks = seq(0, 3.8, 0.2)) +
  theme_bw() +
  theme(
    plot.caption = element_text(family = "Inter", color = "grey50", size = 10, hjust = 1, vjust = 0),
    legend.position = "none"
  ) +
  labs(
    x = "Ratio of residential rates to industrial rates",
    y = "",
    caption = "Data source: EIA form 861, 2023"
  )

comparing_disparities %>%
  filter(!is.na(residential_rate) & !is.na(industrial_rate)) %>%
  left_join(
    residential %>%
      select(utility_name, state) %>%
      distinct(),
    by = c("utility_name")
  )


state_comparison <- residential_rates %>%
  select(utility_name, res_rate = rate) %>%
  left_join(
    residential_customers %>%
      select(utility_name, res_count = count),
    by = c("utility_name")
  ) %>%
  left_join(
    residential %>%
      select(utility_name, state) %>%
      distinct(),
    by = c("utility_name")
  ) %>%
  group_by(state) %>%
  summarize(mean_res_rate = weighted.mean(res_rate, res_count, na.rm = TRUE)) %>%
  ungroup() %>%
  full_join(
    industrial_rates %>%
      select(utility_name, ind_rate = rate) %>%
      left_join(
        industrial_customers %>%
          select(utility_name, ind_count = count),
        by = c("utility_name")
      ) %>%
      left_join(
        industrial %>%
          select(utility_name, state) %>%
          distinct(),
        by = c("utility_name")
      ) %>%
      group_by(state) %>%
      summarize(mean_ind_rate = weighted.mean(ind_rate, ind_count, na.rm = TRUE)) %>%
      ungroup(),
    by = c("state")
  ) %>%
  mutate(ratio = mean_res_rate / mean_ind_rate)

state_comparison %>%
  mutate(
    name_signal = case_when(
      state == "MI" ~ "MI",
      TRUE ~ "other"
    )
  ) %>%
  ggplot(aes(x = mean_ind_rate, mean_res_rate, color = name_signal)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0, 36)) +
  scale_y_continuous(limits = c(0, 44)) +
  theme_bw()

