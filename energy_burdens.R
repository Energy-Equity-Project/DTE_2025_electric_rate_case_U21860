
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
    med_elec_burden = weightedMedian(dte_burden_e, dte_customers, na.rm = TRUE)
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
  geom_hline(yintercept = 3.5, linetype = "dashed", size = 1, color = "red") +
  # Add custom annotation text
  annotate("text", x = 0, y = 4.5, 
           label = "Affordable electric\nburden: 3.5%",
           hjust = 0, vjust = 0.5, family = "Inter", size = 3.5, fontface = "bold", lineheight = 1) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  theme_bw() +
  theme(
    plot.caption = element_text(family = "Inter", color = "grey50", size = 10, hjust = 0, vjust = 0)
  ) +
 # Add yellow rectangle background for annotation
 annotate("rect", 
          xmin = 0, xmax = 47,
          ymin = 18.5,
          ymax = 19.5,
          fill = "#FFEB99", alpha = 0.7) +
 # Add custom annotation text
 annotate("text", x = 1, y = 19, 
          label = "Correlation Coefficient: 0.804",
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
  geom_hline(yintercept = 3.5, linetype = "dashed", size = 1, color = "red") +
  # Add custom annotation text
  annotate("text", x = -2, y = 4.5, 
           label = "Affordable electric\nburden: 3.5%", angle = 90,
           hjust = 0, vjust = 0.5, family = "Inter", size = 3.5, fontface = "bold", lineheight = 1) +
  geom_smooth() +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  # Add yellow rectangle background for annotation
  annotate("rect", 
           xmin = 0, xmax = 47,
           ymin = 18.5,
           ymax = 19.5,
           fill = "#FFEB99", alpha = 0.7) +
  # Add custom annotation text
  annotate("text", x = 1, y = 19, 
           label = "Correlation Coefficient: 0.643",
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
  filter(bipoc_percent < 50) %>%
  summarize(max_burden = max(med_elec_burden, na.rm = TRUE))

demographic_burdens %>%
  filter(bipoc_percent >= 50 &
           med_elec_burden > 9.03) %>%
  nrow()


