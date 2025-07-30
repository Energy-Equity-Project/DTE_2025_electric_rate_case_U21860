
# Libraries
library(tidyverse)
library(janitor)

# Estimating % BIPOC 
# B01001_001E: total population
# B01001A_001E: White Alone
race <- read.csv("outputs/race_2023.csv", colClasses = "character") %>%
  clean_names() %>%
  rename(total_pop = b01001_001e,
         white_alone_pop = b01001a_001e) %>%
  mutate(total_pop = as.numeric(total_pop),
         white_alone_pop = as.numeric(white_alone_pop)) %>%
  mutate(bipoc_pop = total_pop - white_alone_pop) %>%
  mutate(bipoc_percent = 100 * (bipoc_pop / total_pop)) %>%
  mutate(GEOID = paste0(state, county, tract)) %>%
  select(GEOID, total_pop, white_alone_pop, bipoc_pop, bipoc_percent)

write.csv(
  race,
  "outputs/bipoc_percent.csv",
  row.names = FALSE
)
