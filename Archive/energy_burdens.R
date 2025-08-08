dte_consumption_estimates <- read.csv("outputs/dte_doe_consumption_estimates.csv")

dte_consumption_estimates %>%
  filter(li_percent > 50) %>%
  summarize(med_burden_e = weightedMedian(burden_e, hh_count, na.rm = TRUE))

dte_consumption_estimates %>%
  mutate(affordability_metric = case_when(
    burden_e > 0.0322 ~ "unaffordable",
    burden_e <= 0.0322 ~ "affordable",
    TRUE ~ "error"
  )) %>%
  group_by(affordability_metric) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = 100 * (hh_count / sum(hh_count, na.rm = TRUE)))


dte_consumption_estimates %>%
  mutate(affordability_metric = case_when(
    burden_e > 0.0322 ~ "unaffordable",
    burden_e <= 0.0322 ~ "affordable",
    TRUE ~ "error"
  )) %>%
  mutate(majority_li = case_when(
    li_percent > 50 ~ "majority li",
    li_percent <= 50 ~ "majority non-li",
    TRUE ~ "error"
  )) %>%
  group_by(majority_li, affordability_metric) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(majority_li) %>%
  mutate(percent = 100 * (hh_count / sum(hh_count, na.rm = TRUE))) %>%
  ungroup()

dte_consumption_estimates %>%
  mutate(majority_li = case_when(
    li_percent > 50 ~ "majority li",
    li_percent <= 50 ~ "majority non-li",
    TRUE ~ "error"
  )) %>%
  group_by(majority_li) %>%
  summarize(med_burden_e = weightedMedian(burden_e, hh_count)) %>%
  ungroup()

# Trends in electric burdens by LI percentage
# Note: LI means less than 200% FPL
dte_consumption_estimates %>%
  ggplot(aes(x = li_percent, y = burden_e * 100)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  theme_bw() +
  theme(
    plot.caption = element_text(family = "Inter", color = "grey50", size = 10, hjust = 0, vjust = 0)
  ) +
  # Add yellow rectangle background for annotation
  annotate("rect", 
           xmin = 0, xmax = 44,
           ymin = 9,
           ymax = 10,
           fill = "#FFEB99", alpha = 0.7) +
  # Add custom annotation text
  annotate("text", x = 1, y = 9.5, 
           label = "Correlation Coefficient: 0.736",
           hjust = 0, vjust = 0.5, family = "Inter", size = 5, fontface = "bold", lineheight = 0.5) +
  labs(
    x = "Percent of census tract with households below 200% FPL",
    y = "Electric-only burden within census tract",
    caption = "Data sources:\nUS Census, American Community Survey - B17026, 2023\nDOE, Low Income Energy Affordability Tool, 2024\nU-21860 DAAODE-3.9-01 Total Residential Revenue 2019 - 2024"
  )

ggsave(
  "outputs/li_percent_vs_burden_e.png"
  # units = "in",
  # height = 4,
  # width = 5
)

cor(
  dte_consumption_estimates$li_percent,
  dte_consumption_estimates$burden_e,
  use = "complete.obs",
  method = "pearson"
)

# 1.300958% median electric burden
dte_consumption_estimates %>%
  filter(bipoc_percent <= 10) %>%
  summarize(med_burden_e = weightedMedian(burden_e, hh_count, na.rm = TRUE))

# Majority White census tracts max electric burden: 1.051863%
dte_consumption_estimates %>%
  filter(bipoc_percent < 50) %>%
  summarize(max_burden_e = max(burden_e, na.rm = TRUE))

# Majority White census tracts
dte_consumption_estimates %>%
  mutate(race_cat = case_when(
    bipoc_percent < 50 ~ "Majority White",
    bipoc_percent >= 50 ~ "Majority BIPOC",
    TRUE ~ "error"
  )) %>%
  group_by(race_cat) %>%
  summarize(med_burden_e = weightedMedian(burden_e, hh_count, na.rm = TRUE)) %>%
  ungroup()

dte_consumption_estimates %>%
  mutate(race_cat = case_when(
    bipoc_percent < 50 ~ "Majority White",
    bipoc_percent >= 50 ~ "Majority BIPOC",
    TRUE ~ "error"
  )) %>%
  mutate(affordability_metric = case_when(
    burden_e > 0.0322 ~ "unaffordable",
    burden_e <= 0.0322 ~ "affordable",
    TRUE ~ "error"
  )) %>%
  group_by(race_cat, affordability_metric) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(race_cat) %>%
  mutate(percent = 100 * (hh_count / sum(hh_count, na.rm = TRUE))) %>%
  ungroup()

# 3.682921% median electric burden
dte_consumption_estimates %>%
  filter(bipoc_percent > 90) %>%
  summarize(med_burden_e = weightedMedian(burden_e, hh_count, na.rm = TRUE))

dte_consumption_estimates %>%
  filter(bipoc_percent < 50) %>%
  summarize(max_burden_e = max(burden_e, na.rm = TRUE))

dte_consumption_estimates %>%
  filter(bipoc_percent > 50,
         burden_e > 0.053) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE),
            n = n())

# Majority White census tracts max electric burden: 9.460135%
dte_consumption_estimates %>%
  filter(bipoc_percent >= 0.5) %>%
  summarize(max_burden_e = max(burden_e, na.rm = TRUE))

dte_consumption_estimates %>%
  filter(bipoc_percent >= 0.5) %>%
  mutate(burden_cat = case_when(
    burden_e > 0.055 ~ "extreme",
    burden_e <= 0.055 ~ "non-extreme",
    TRUE ~ "error"
  )) %>%
  group_by(burden_cat) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = 100 * (hh_count / sum(hh_count, na.rm = TRUE)))

dte_consumption_estimates %>%
  ggplot(aes(x = bipoc_percent, y = burden_e * 100)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 5.293659, linetype = "dashed") +
  # Add custom annotation text
  annotate("text", x = 0, y = 6, 
           label = "Max median energy burden\n(majority white): 5.3%",
           hjust = 0, vjust = 0.5, family = "Inter", size = 3, fontface = "bold", lineheight = 1) +
  theme_bw() +
  theme(
    plot.caption = element_text(family = "Inter", color = "grey50", size = 10, hjust = 0, vjust = 0)
  ) +
  # Add red rectangle background for tracts that majority BIPOC only experience these burdens
  annotate("rect", 
           xmin = 50, xmax = 101,
           ymin = 5.293659,
           ymax = 10,
           fill = "red", alpha = 0.3) +
  # Add custom annotation text
  annotate("text", x = 50, y = 10.5, 
           label = "Median energy burdens\nonly experienced by BIPOC tracts",
           hjust = 0, vjust = 0.5, family = "Inter", size = 3, fontface = "bold", lineheight = 1) +
  # Add yellow rectangle background for annotation
  annotate("rect", 
           xmin = 0, xmax = 25,
           ymin = 8.5,
           ymax = 10.5,
           fill = "#FFEB99", alpha = 0.7) +
  # Add custom annotation text
  annotate("text", x = 1, y = 9.5, 
           label = "Correlation\nCoefficient:\n0.600",
           hjust = 0, vjust = 0.5, family = "Inter", size = 4, fontface = "bold", lineheight = 0.75) +
  labs(
    x = "% of the Population identified as BIPOC",
    y = "Electric-only burden within census tract",
    caption = "Data sources:\nUS Census, American Community Survey - B01001, 2023\nDOE, Low Income Energy Affordability Tool, 2024\nU-21860 DAAODE-3.9-01 Total Residential Revenue 2019 - 2024"
  )

cor(
  dte_consumption_estimates$bipoc_percent,
  dte_consumption_estimates$burden_e,
  use = "complete.obs",
  method = "pearson"
)