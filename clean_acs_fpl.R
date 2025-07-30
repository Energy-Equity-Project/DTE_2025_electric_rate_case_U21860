
# Libraries
library(tidyverse)
library(readxl)
library(janitor)

fpl <- read.csv("outputs/fpl_2023.csv") %>%
  select(GEOID = GEO_ID, ends_with("E")) %>%
  select(-state) %>%
  pivot_longer(-c(NAME, GEOID, B17026_001E), names_to = "fpl_cat", values_to = "hh_count") %>%
  rename(total = B17026_001E) %>%
  mutate(
    fpl_cat = case_when(
      fpl_cat %in% c("B17026_002E", "B17026_003E", "B17026_004E", "B17026_005E", "B17026_006E", "B17026_007E", "B17026_008E", "B17026_009E") ~ "low-income",
      fpl_cat %in% c("B17026_010E", "B17026_011E", "B17026_012E", "B17026_013E") ~ "non low-income",
      TRUE ~ "error"
    )
  ) %>%
  group_by(GEOID, fpl_cat) %>%
  summarize(hh_count = sum(hh_count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(GEOID) %>%
  mutate(percent = 100 * (hh_count / sum(hh_count, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(GEOID = as.numeric(gsub("1400000US", "", GEOID)))

write.csv(
  fpl,
  "outputs/li_vs_non-li_percents_2023.csv",
  row.names = FALSE
)
