# Andrew Boslett
# University of Rochester Medical Center/FLX AI
# Email: andrew_boslett@urmc.rochester.edu
# Permanent email: andrew.boslett@gmail.com

rm(list = ls())

# Goal: Import all data from the 1-download-all-data.R and combine into a single data frame.

# Set up main database -----------------------
# Notes: Including treatment indicators, treatment timing, drilling exposure.

# Import oil and gas spud data

well_counts <- readRDS('shale-varying/Scratch/County_Well_Counts_by_Year_2000_2017.rds')

# Import Bartik-timing

shale_timing <- readRDS('shale-varying/Data/Bartik/Shale_Play_Development_Timing.rds')

# Import county mapping to shale

distance_dummies <- readRDS('shale-varying/Scratch/County_to_Shale_Distance_File_Dummy.rds')

# Get county key dataframe
# Note: Use county shapefile USCB

county_shp <- readOGR(dsn = 'shale-varying/Data/GIS',
                      layer = 'tl_2020_us_county_prj',
                      verbose = TRUE) %>% 
  as.data.frame() %>%
  mutate(FID = row_number() - 1)

county_shp %<>% dplyr::select(county_fips_code = GEOID) %>% unique()

county_shp <- bind_rows(replicate(18, county_shp, simplify = FALSE))

county_shp %<>% group_by(county_fips_code) %>%
  mutate(year = 1999 + row_number()) %>%
  ungroup()

# Join well counts to the key DF

county_shp %<>% left_join(well_counts, by = c('county_fips_code', 'year' = 'Spud Year'))

rm(well_counts)

county_shp %<>% mutate_at(vars(D, H, hd_wells),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))

# Define county-level exposure to shale-plays over time
# Note: We'll make the distance table into a long data-frame. We'll then join the timing data from Bartik et al. We'll extract our the first
# year if a county overlays more than one productive shale play (e.g., Utica + Marcellus).

distance_dummies %<>% gather(shale_play, presence, -county_fips_code) %>%
  left_join(shale_timing, by = c('shale_play'))

distance_dummies %<>% filter(presence == 1 & !is.na(presence) == TRUE) # Filter out only shale play counties

distance_dummies %<>% filter(shale_play != 'non_active_shale') # Drops about 300 county-play connections

distance_dummies %<>% mutate(first_frac_year = case_when(
  shale_play %in% c('New Albany', 'Antrim') ~ 2010,
  !(shale_play %in% c('New Albany', 'Antrim')) ~ first_frac_year
)) # Give Antrim and New Albany 2010

# Filter out first year of exposure by Bartik et al. if multiple shale plays are in area

distance_dummies %<>% arrange(county_fips_code, first_frac_year) %>%
  group_by(county_fips_code) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Merge county-level exposure to shale plays over time

county_shp %<>% left_join(distance_dummies, by = c('county_fips_code'))

rm(list = setdiff(ls(), c('county_shp', 'shale_timing')))

county_shp %<>% mutate(shale_county = ifelse(!is.na(shale_play) == TRUE, 1, 0),
                       post_shale = ifelse(year >= first_frac_year & !is.na(first_frac_year) == TRUE, 1, 0))

county_shp %<>% arrange(county_fips_code, year)

# Merge with other databases -----------------------------

# Year-County data

for(fff in c('lau_1995_2016', 'SEER_Population_Data_2000_2018', 'SAIPE_2000_2019', 'LAPI_2000_2019',
             'Zillow_ZHVI_SFR_1996_2021', 'QCEW_County_Data_2000_2018', 'OASDI_Data_2004_2017')) {
  
  temp <- readRDS(paste0('shale-varying/Scratch/', fff, '.rds'))
  
  # Join by year and county
  
  county_shp %<>% left_join(temp, by = c('county_fips_code', 'year'))
  
  rm(temp)
  
}

# County data

for(fff in c('USDA_ERS_Rural_Urban_Classification', 'County_Area_Sq_Miles')) {
  
  temp <- readRDS(paste0('shale-varying/Scratch/', fff, '.rds'))
  
  # Join by year and county
  
  county_shp %<>% left_join(temp, by = c('county_fips_code'))
  
  rm(temp)
  
}

# Add explanatory variables -----------------------

county_shp %<>% mutate(non_white_pop_percentage = (black + other) / (black + other + white),
                       total_pop = black + other + white,
                       interaction_term = shale_county * post_shale,
                       state_fips_code = str_sub(county_fips_code, end = 2)) %>%
  group_by(state_fips_code) %>%
  mutate(shale_state = max(shale_county)) %>%
  ungroup()

# Rename variables

names(county_shp) %<>% str_replace_all(pattern = ' ', replacement = '_')

# Add ZHVI (SFR) as logged variable

county_shp %<>% mutate(zhvi_sfr_log = log(zhvi_sfr))

# Add population, total and density

county_shp %<>% mutate(population = black + white + other,
                       population_ln = log(population),
                       population_density = population / sq_miles)

# Make disability_income a numeric variable and created interpolated/logged versions of them

county_shp$disability_income %<>% as.numeric()

county_shp %<>% arrange(county_fips_code, year) %>%
  group_by(county_fips_code) %>%
  mutate(disability_income_imputed = na.approx(disability_income, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(disability_income_imputed_log = log(disability_income_imputed))

# Add border-county indicator --------------------------

county_adjacency <- readRDS('shale-varying/Scratch/County_Adjacency_Neighbor_Exposure_to_Shale.rds')

county_shp %<>% left_join(county_adjacency, by = c('county_fips_code'))

county_shp %<>% mutate(shale_border_county = ifelse(shale_county == 0 & any_shale_neighbor == 1, 1, 0))

# Drop Puerto Rico and other non-states -----------------------

county_shp %<>% filter(as.numeric(state_fips_code) <= 56)

# Define employment in other industries outcomes -------------------------------------

# Generate employment shares in manufacturing, mining, etc.,

county_shp %<>% mutate_at(vars(annual_average_employment_construction, annual_average_employment_manufacturing, annual_average_employment_natural_resources,
                               annual_average_employment_service_providing),
                          funs(percent = . / annual_average_employment_all))

# Add alternative timing based on # of wells drilled in county ---------------------------
# County
# Notes: We will estimate the year when a county had experienced, on a cumulative basis, 10 horizontal/directional wells drilled.

county_shp %<>% arrange(county_fips_code, year) %>%
  group_by(county_fips_code) %>%
  mutate(hd_wells_cumulative = cumsum(hd_wells)) %>%
  ungroup() %>%
  mutate(hd_wells_cumulative_10 = ifelse(hd_wells_cumulative >= 10, 1, 0)) %>%
  group_by(county_fips_code) %>%
  mutate(hd_wells_cumulative_10 = cummax(hd_wells_cumulative_10)) %>%
  ungroup()

# (2) State
# Notes: We will do the same as above but at the state-level. We will use 100 horizontal/directional wells.

county_shp %>% group_by(year, state_fips_code) %>%
  group_by(state_fips_code, year) %>%
  summarise(hd_wells = sum(hd_wells, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(state_fips_code, year) %>%
  group_by(state_fips_code) %>%
  mutate(hd_wells_cumulative_state = cumsum(hd_wells)) %>%
  ungroup() %>%
  mutate(hd_wells_cumulative_state_250 = ifelse(hd_wells_cumulative_state >= 250, 1, 0)) %>%
  group_by(state_fips_code) %>%
  mutate(hd_wells_cumulative_state_250 = cummax(hd_wells_cumulative_state_250)) %>%
  ungroup() %>%
  dplyr::select(state_fips_code, year, hd_wells_cumulative_state_250) -> state_counts

county_shp %<>% left_join(state_counts, by = c('state_fips_code', 'year'))

rm(state_counts)

# Save as analysis database --------------------------

county_shp %>% saveRDS('shale-varying/Scratch/Analysis_Database.rds')
