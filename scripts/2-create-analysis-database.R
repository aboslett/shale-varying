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

for(fff in c('lau_1995_2016', 'SEER_Population_Data_2000_2018', 'SAIPE_2000_2019',
             'Zillow_ZHVI_SFR_1996_2021', 'QCEW_County_Data_2000_2018')) {
  
  temp <- readRDS(paste0('shale-varying/Scratch/', fff, '.rds'))
  
  # Join by year and county
  
  county_shp %<>% left_join(temp, by = c('county_fips_code', 'year'))
  
  rm(temp)
  
}

# County data

for(fff in c('USDA_ERS_Rural_Urban_Classification')) {
  
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

# Save as analysis database --------------------------

county_shp %>% saveRDS('shale-varying/Scratch/Analysis_Database.rds')
