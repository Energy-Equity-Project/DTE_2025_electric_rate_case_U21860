
# Libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(matrixStats)

# Add the Inter font (you need to do this once)
font_add_google("Inter", "Inter")
font_add_google("Bitter", "Bitter")


dte_df <- read.csv("outputs/dte_df.csv")

bipoc <- read.csv("outputs/bipoc_percent.csv")

li_percent <- dte_df %>%
  mutate(
    income_cat = case_when(
      fpl150 %in% c("0-100%", "100-150%", "150-200%") ~ "low income",
      TRUE ~ "non-low income"
    )
  ) %>%
  group_by(GEOID, income_cat) %>%
  summarize(dte_customers = sum(dte_customers, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(GEOID) %>%
  mutate(percent = 100 * (dte_customers / sum(dte_customers, na.rm = TRUE))) %>%
  ungroup() %>%
  filter(income_cat == "low income") %>%
  select(-c(income_cat, dte_customers)) %>%
  rename(low_income_percent = percent)

demographic_burdens <- dte_df %>%
  group_by(GEOID) %>%
  summarize(
    med_elec_burden = weightedMedian(dte_burden_e, dte_customers, na.rm = TRUE),
    dte_customers = sum(dte_customers, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(med_elec_burden = 100 * med_elec_burden) %>%
  left_join(
    li_percent,
    by = c("GEOID")
  ) %>%
  left_join(
    bipoc %>%
      select(GEOID, bipoc_percent),
    by = c("GEOID")
  )

cor(
  demographic_burdens$med_elec_burden,
  demographic_burdens$low_income_percent,
  method = "pearson",use = "complete"
)

demographic_burdens %>%
  ggplot(aes(x = low_income_percent, y = med_elec_burden)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  geom_hline(yintercept = 3.6, linetype = "dashed", size = 1, color = "red") +
  # Add custom annotation text
  annotate("text", x = 0, y = 4.5, 
           label = "Affordable electric\nburden: 3.6%",
           hjust = 0, vjust = 0.5, family = "Inter", size = 3.6, fontface = "bold", lineheight = 1) +
  scale_y_continuous(limits = c(0, 18.5), breaks = seq(0, 18, 2)) +
  theme_bw() +
  theme(
    plot.caption = element_text(family = "Inter", color = "grey50", size = 10, hjust = 0, vjust = 0)
  ) +
 # Add yellow rectangle background for annotation
 annotate("rect", 
          xmin = 0, xmax = 49,
          ymin = 16.5,
          ymax = 17.5,
          fill = "#FFEB99", alpha = 0.7) +
 # Add custom annotation text
 annotate("text", x = 1, y = 17, 
          label = "Correlation Coefficient: 0.838",
          hjust = 0, vjust = 0.5, family = "Inter", size = 5, fontface = "bold", lineheight = 0.5) +
  labs(
    x = "% of low income customers in tract",
    y = "Median electric burden within tract",
    caption = "Data sources:\nDOE, Low Income Energy Affordability Tool, 2024\nU-21860 DAAODE-3.9-01 Total Residential Revenue 2019 - 2024\nU-21860 DAAODE-3.7-8-01 Total Residential Count and Sales 2024"
  )
 
cor(
  demographic_burdens$med_elec_burden,
  demographic_burdens$bipoc_percent,
  method = "pearson",
  use = "complete"
)

demographic_burdens %>%
  ggplot(aes(x = bipoc_percent, y = med_elec_burden)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 3.6, linetype = "dashed", size = 1, color = "red") +
  # Add custom annotation text
  annotate("text", x = -2, y = 4.5, 
           label = "Affordable electric\nburden: 3.6%", angle = 90,
           hjust = 0, vjust = 0.5, family = "Inter", size = 3.6, fontface = "bold", lineheight = 1) +
  geom_smooth() +
  scale_y_continuous(limits = c(0, 18.5), breaks = seq(0, 18, 2)) +
  # Add yellow rectangle background for annotation
  annotate("rect", 
           xmin = 0, xmax = 50,
           ymin = 16.5,
           ymax = 17.5,
           fill = "#FFEB99", alpha = 0.7) +
  # Add custom annotation text
  annotate("text", x = 1, y = 17, 
           label = "Correlation Coefficient: 0.682",
           hjust = 0, vjust = 0.5, family = "Inter", size = 5, fontface = "bold", lineheight = 0.5) +
  theme_bw() +
  theme(
    plot.caption = element_text(family = "Inter", color = "grey50", size = 10, hjust = 0, vjust = 0)
  ) +
  labs(
    x = "% of BIPOC pop. in tract",
    y = "Median electric burden within tract",
    caption = "Data sources:\nUS Census American Community Survey, 2023, B01001_001, B01001A_001\nDOE, Low Income Energy Affordability Tool, 2024\nU-21860 DAAODE-3.9-01 Total Residential Revenue 2019 - 2024\nU-21860 DAAODE-3.7-8-01 Total Residential Count and Sales 2024"
  )

demographic_burdens %>%
  mutate(
    race_cat = case_when(
      bipoc_percent < 50 ~ "white alone",
      bipoc_percent >= 50 ~ "BIPOC",
      TRUE ~ "error"
    )
  ) %>%
  mutate(
    burden_cat = case_when(
      med_elec_burden > 3.6 ~ "unaffordable",
      med_elec_burden <= 3.6 ~ "affordable",
      TRUE ~ "error"
    )
  ) %>%
  group_by(race_cat, burden_cat) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(race_cat) %>%
  mutate(percent = 100 * (n / sum(n))) %>%
  ungroup()
  
  
demographic_burdens %>%
  mutate(
    race_cat = case_when(
      bipoc_percent < 50 ~ "white alone",
      bipoc_percent >= 50 ~ "BIPOC",
      TRUE ~ "error"
    )
  ) %>%
  group_by(race_cat) %>%
  summarize(med_elec_burden = weightedMedian(med_elec_burden, dte_customers, na.rm = TRUE)) %>%
  ungroup()

demographic_burdens %>%
  filter(bipoc_percent < 50) %>%
  summarize(max_burden = max(med_elec_burden, na.rm = TRUE))

demographic_burdens %>%
  filter(bipoc_percent >= 50 &
           med_elec_burden > 9.03) %>%
  nrow()

# Stats needed
dte_df %>%
  filter(fpl150 %in% c("0-100%", "100-150%", "150-200%")) %>%
  summarize(med_elec_burden = weightedMedian(avg_hh_dte_energy_costs, dte_customers, na.rm = TRUE),
            med_income = weightedMedian())
  


