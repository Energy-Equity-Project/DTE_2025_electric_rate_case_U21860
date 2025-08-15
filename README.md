## Data Collection
- get_us_census_data.ipynb: Requests the US Census ACS API for ACS 5-year, 2023 data.
- miejscreen_cleaning.ipynb: Requests MiEJScreen publicly available data.

## Data Cleaning and Preprocessing
- clean_acs_income_buckets.R: Cleans and summarizes US Census ACS income range data and finds number of DTE customers below $50,116 and below $40,583.
- clean_acs_race.R: Cleans and summarizes US Census ACS population counts by race.
- clean_dte_reliability.R: Cleans and summarizes DTE reliability metrics (specifically SAIDI, SAIFI, CAIDI), requested under docket U-21122.
- clean_doe_lead_2022.R: Cleans and summarizes the most recently updated DOE LEAD data (updated on August 1st, 2025) and finds all census tracts within the DTE electric service territory.
- li_sales_data_cleaning.R: Cleaning and summarizing DTE provided data on low income customer counts, sales and revenue.
- shutoff_data_cleaning.R: Initial data cleaning and summary of DTE provided shutoff data.
- total_sales_data_cleaning.R: Cleaning and summarizing DTE provided data on All customer counts, sales and revenue.

## Data Analysis
- dte_wide_pipp.R: Develops estimates for a DTE-wide PIPP program, isolating FPL groups wihtin census tracts and finding the number of LI and non-LI customers serviced by the PIPP.
- autoenrollment.R: Analysis for a Community-Wide PIPP, that would automatically provide a credit to DTE customers within census tracts that are "disadvantaged", specifically the top 33 and top 8 tracts.
- eia_disparities.R: Understanding the disparities in rates and rate increases across customers bases (residential vs industrial). Comparison done across MI utilities and between MI and the rest of the country.
- energy_burdens.R: Analyzing trends in electric burdens and affordability across DTE customers. Understanding disparities between race and income groups.
- impacts_of_rate_increases.R: Understanding the impacts of DTE rate increases on electric affordability from 2024 to 2026, specifically looking at highlighting disparities across income groups.

## Data sources
- [DOE Low Income Affordability Tool (2024)](https://data.openei.org/submissions/6219)
Ma, O., & Vimont, A. (2024). Low-Income Energy Affordability Data - LEAD Tool - 2022 Update. [Data set]. Open Energy Data Initiative (OEDI). U.S. Department of Energy, Office of Energy Efficiency and Renewable Energy. https://doi.org/10.25984/2504170
- [EIA, Form 861, 2023](https://www.eia.gov/electricity/data/eia861/) - Revenue, Sales and Customer counts by utility
- [DTE Electric Service Territory Map](https://data-michiganpsc.hub.arcgis.com/datasets/0e5d879b789d496f96d7dba37259b9bd_16/about)
- [MiEJScreen](https://egle.maps.arcgis.com/apps/webappviewer/index.html?id=b100011f137945138a52a35ec6d8676f) - MiEJScreen score percentiles, developed by the Michigan Department of Environment, Great Lakes and Energy (EGLE).
- [US Climate Vulnerability Index](https://map.climatevulnerabilityindex.org/map/baseline/usa?mapBoundaries=Tract&mapFilter=0&reportBoundaries=Tract&geoContext=State) - Community baseline scores which highlight “the long-standing inequities shaping resilience to climate impacts” - developed by Environmental Defense Fund (EDF), Texas A&M University and Darkhorse Analytics
- US Census, American Community Survey, 2023
  - Population by race, B01001
  - Number of households by income range, S1901
  
- All other datasets were provided by DTE as part of their testimony or discovery