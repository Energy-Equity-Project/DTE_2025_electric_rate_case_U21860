
# Libraries
library(tidyverse)
library(readxl)
library(janitor)


dte_reliability_files <- c(
  list.files("../../Data/MPSC/Reliability Data/2024/DTE", full.names = TRUE),
  list.files("../../Data/MPSC/Reliability Data/2023/DTE", full.names = TRUE)
)


dte_reliability <- data.frame()

for (dte_reliability_fp in dte_reliability_files) {
  
  print(dte_reliability_fp)
  
  curr_reliability <- read_excel(dte_reliability_fp, sheet = "Census Tract")
  # Getting month date for data
  curr_date <- excel_numeric_to_date(as.numeric(curr_reliability[3,2]))
  # Structuring dataframe to capture census tract level data
  curr_reliability <- curr_reliability[10:nrow(curr_reliability)-1,] %>%
    row_to_names(1) %>%
    clean_names() %>%
    rename(geoid = census_tract_number) %>%
    filter(!is.na(geoid)) %>%
    mutate(date = curr_date)
  
  dte_reliability <- dte_reliability %>% bind_rows(curr_reliability)
}

reliability_metrics <- dte_reliability %>%
  mutate(saidi = as.numeric(system_average_interruption_duration_index),
         saifi = as.numeric(system_average_interruption_frequency_index)) %>%
  group_by(geoid) %>%
  summarize(avg_saidi = mean(saidi, na.rm = TRUE),
            avg_saifi = mean(saifi, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avg_saidi = avg_saidi / 60) %>%
  mutate(avg_caidi = avg_saidi / avg_saifi) %>%
  select(GEOID=geoid, avg_saidi, avg_saifi, avg_caidi)

write.csv(
  reliability_metrics,
  "outputs/dte_reliability_metrics_2023_2024.csv",
  row.names = FALSE
)
