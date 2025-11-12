
library(ggspatial)

# Note: run the autoenrollment.R script first up until the section that
# creates the selected autoenrolled tracts

# Zipcodes
zcta <- st_read("../../Data/DTE/DTE EWR/tl_2025_us_zcta520/tl_2025_us_zcta520.shp")
tracts <- st_read("../../Data/GIS/US_Census/Census Tract Shapefiles/2024/tl_2024_26_tract/tl_2024_26_tract.shp") %>%
  mutate(GEOID = as.numeric(GEOID))

tracts <- st_transform(tracts, st_crs(zcta))

# DTE proposed tracts===========================================================
dte_proposed_tracts <- read_excel(
  "../../Data/DTE/DTE EWR/tracts_requested.xlsx",
  sheet = "dte_tracts"
)

dte_proposed_tracts <- autoenroll_tracts %>%
  filter(GEOID %in% unique(dte_proposed_tracts$GEOID))

# Finding the zipcodes the dte proposed tracts are located in
dte_proposed_zipcodes <- zcta %>%
  st_join(
    tracts %>%
      filter(GEOID %in% unique(dte_proposed_tracts$GEOID)),
    join = st_intersects
  ) %>%
  filter(!is.na(GEOID))

dte_proposed_tracts <- dte_proposed_zipcodes %>%
  as.data.frame() %>%
  select(GEOID20, GEOID) %>%
  distinct() %>%
  group_by(GEOID) %>%
  summarise(zipcodes = paste(sort(unique(GEOID20)), collapse = ", ")) %>%
  ungroup() %>%
  left_join(
    autoenroll_tracts,
    by = c("GEOID")
  )

write.csv(
  dte_proposed_tracts,
  "dte_ewr/dte_proposed_tracts.csv",
  row.names = FALSE
)

dzcta %>%
  filter(GEOID20 %in% unique(dte_proposed_zipcodes$GEOID20)) %>%
  ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +  # Add basemap
  geom_sf(aes(fill = GEOID20), color = NA, alpha = 0.5) +
  scale_fill_viridis_d(option = "turbo") +
  geom_sf(data = tracts %>%
            filter(GEOID %in% unique(dte_proposed_tracts$GEOID)), alpha = 0.5, fill = "white", color = "black", linewidth = 0.5) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "right"
  ) +
  labs(
    fill = "Zipcodes",
    caption = "*U.S. Census Bureau, ZCTA and Census tracts"
  )

ggsave(
  "dte_ewr/dte_proposed_tracts.png",
  dpi = 600,
  height = 4,
  width = 6
)
  

# Checking that all census tracts are available via the US Census tract map
unique(dte_proposed_tracts$GEOID)[!(unique(dte_proposed_tracts$GEOID)  %in% unique(tracts$GEOID))]

write.csv(
  dte_proposed_tracts,
  "dte_ewr/dte_proposed_tracts.csv",
  row.names = FALSE
)

# CWAP 33=================================================================
cwap_33 <- read_excel(
  "../../Data/DTE/DTE EWR/tracts_requested.xlsx",
  sheet = "eep_cwap_33"
)

cwap_33 <- autoenroll_tracts %>%
  filter(GEOID %in% unique(cwap_33$GEOID))

cwap_33_zipcodes <- zcta %>%
  st_join(
    tracts %>%
      filter(GEOID %in% unique(cwap_33$GEOID)),
    join = st_intersects
  ) %>%
  filter(!is.na(GEOID))

cwap_33 <- cwap_33_zipcodes %>%
  as.data.frame() %>%
  select(GEOID20, GEOID) %>%
  distinct() %>%
  group_by(GEOID) %>%
  summarise(zipcodes = paste(sort(unique(GEOID20)), collapse = ", ")) %>%
  ungroup() %>%
  left_join(
    autoenroll_tracts,
    by = c("GEOID")
  )

write.csv(
  cwap_33,
  "dte_ewr/cwap_33.csv",
  row.names = FALSE
)

zcta %>%
  filter(GEOID20 %in% unique(cwap_33_zipcodes$GEOID20)) %>%
  ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +  # Add basemap
  geom_sf(aes(fill = GEOID20), color = NA, alpha = 0.5) +
  scale_fill_viridis_d(option = "turbo") +
  geom_sf(data = tracts %>%
            filter(GEOID %in% unique(cwap_33$GEOID)), alpha = 0.5, fill = "white", color = "black", linewidth = 0.5) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "right"
  ) +
  labs(
    fill = "Zipcodes",
    caption = "*U.S. Census Bureau, ZCTA and Census tracts"
  )

ggsave(
  "dte_ewr/cwap_33.png",
  dpi = 600,
  height = 8,
  width = 6
)

# CWAP 8=================================================================

cwap_8 <- read_excel(
  "../../Data/DTE/DTE EWR/tracts_requested.xlsx",
  sheet = "eep_cwap_8"
)

cwap_8 <- autoenroll_tracts %>%
  filter(GEOID %in% unique(cwap_8$GEOID))

cwap_8_zipcodes <- zcta %>%
  st_join(
    tracts %>%
      filter(GEOID %in% unique(cwap_8$GEOID)),
    join = st_intersects
  ) %>%
  filter(!is.na(GEOID))

cwap_8 <- cwap_8_zipcodes %>%
  as.data.frame() %>%
  select(GEOID20, GEOID) %>%
  distinct() %>%
  group_by(GEOID) %>%
  summarise(zipcodes = paste(sort(unique(GEOID20)), collapse = ", ")) %>%
  ungroup() %>%
  left_join(
    autoenroll_tracts,
    by = c("GEOID")
  )

write.csv(
  cwap_8,
  "dte_ewr/cwap_8.csv",
  row.names = FALSE
)

zcta %>%
  filter(GEOID20 %in% unique(cwap_8_zipcodes$GEOID20)) %>%
  ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +  # Add basemap
  geom_sf(aes(fill = GEOID20), color = NA, alpha = 0.5) +
  scale_fill_viridis_d(option = "turbo") +
  geom_sf(data = tracts %>%
            filter(GEOID %in% unique(cwap_8$GEOID)), alpha = 0.5, fill = "white", color = "black", linewidth = 0.5) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "right"
  ) +
  labs(
    fill = "Zipcodes",
    caption = "*U.S. Census Bureau, ZCTA and Census tracts"
  )

ggsave(
  "dte_ewr/cwap_8.png",
  dpi = 600,
  height = 4,
  width = 6
)


# Census tract map========================================================
tracts %>%
  filter(GEOID %in% unique(c(cwap_8$GEOID, cwap_33$GEOID, dte_proposed_tracts$GEOID))) %>%
  mutate(tract_cat = case_when(
    GEOID %in% cwap_8$GEOID ~ "EEP CWAP-8",
    GEOID %in% cwap_33$GEOID ~ "EEP CWAP-33",
    GEOID %in% dte_proposed_tracts$GEOID ~ "DTE proposed",
    TRUE ~ "error"
  )) %>%
  filter(tract_cat != "error") %>%
  ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(aes(fill = tract_cat), alpha = 0.75, color = NA) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    fill = "Census Tracts\nSelected"
  )

ggsave(
  "dte_ewr/tracts_selected.png",
  dpi = 600,
  height = 8,
  width = 6
)

