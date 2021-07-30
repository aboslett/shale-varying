# Andrew Boslett
# University of Rochester Medical Center/FLX AI
# Email: andrew_boslett@urmc.rochester.edu
# Permanent email: andrew.boslett@gmail.com

rm(list = ls())

# Goal: Import some data for the analyses. 

# SAIPE Data --------------------------------
# Note: Small Area Income and Poverty Estimates (SAIPE) Program. Set up loop to download and save
# year-specific Excel files of Saipe data.

# Add SAIPE folder

dir.create('shale-varying/Data/SAIPE', showWarnings = FALSE)

for(fff in 2000:2019) {

  # Get data file type
  # Note: Pre-2003 data is saved as a .dat
  
  file_type <- ifelse(fff >= 2003, '.xls', '.dat')
  
  # Get index for download
  
  ggg <- as.character(fff - 2000)
  ggg <- ifelse(str_length(as.character(ggg)) == 1, 
                paste0('0', as.character(ggg)), as.character(ggg))
  
  # Download file from URL. Save to Data folder.
  
  download.file(paste0('https://www2.census.gov/programs-surveys/saipe/datasets/',
                       as.character(fff), 
                       '/',
                       as.character(fff),
                       '-state-and-county/est', 
                       as.character(ggg), 
                       'all', 
                       file_type), 
                destfile = paste0('shale-varying/Data/SAIPE/saipe_', as.character(fff), file_type),
                mode = 'wb')
  
  # Print index
  
  print(fff)
  
  rm(fff, file_type)
  
}

# Clean file

saipe <- data.frame()

for(fff in 2000:2019) {
  
  # Import file

  if(fff >= 2003 & fff <= 2005) {
    
    temp <- read_excel(paste0('shale-varying/Data/SAIPE/saipe_', as.character(fff), '.xls'),
                       col_names = TRUE,
                       skip = 1)
  
  } else if(fff >= 2006 & fff <= 2012) {
    
    temp <- read_excel(paste0('shale-varying/Data/SAIPE/saipe_', as.character(fff), '.xls'),
                       col_names = TRUE,
                       skip = 2)
    
  } else if(fff >= 2013 & fff <= 2019) {
    
    temp <- read_excel(paste0('shale-varying/Data/SAIPE/saipe_', as.character(fff), '.xls'),
                       col_names = TRUE,
                       skip = 3)
    
  } else {
    
    temp <- read_delim(paste0('shale-varying/Data/SAIPE/saipe_', as.character(fff), '.dat'), 
                       skip = 1, delim = ' ',
                       col_names = c('State FIPS', 'County FIPS', 'a1', 'a2', 'a3', 'Poverty Percent All Ages', 'a4', 'a5', 
                                      'a6', 'a7', 'x8', 'Poverty Percent Under Age 18',
                                     'a8', 'a9', 'a10', 'a11', 
                                     'a12', 'Poverty Percent Ages 5-17',
                                     'a13', 'a14', 'Median Household Income',
                                     'a15', 'a16', 'a17', 'a18', 'a19',
                                     'Poverty Percent Ages 0-4', 
                                     'a15', 'a16',
                                     'Name', 'Postal', 'Hi'))

    
  }
  
  # Rename FIPS code variable names to match earlier vintages
  
  names(temp) %<>% str_replace_all(pattern = ' FIPS Code', replacement = 'FIPS') %>%
    str_replace_all(pattern = 'StateFIPS', replacement = 'State FIPS') %>%
    str_replace_all(pattern = 'CountyFIPS', replacement = 'County FIPS') %>%
    str_replace_all(pattern = 'Poverty Percent, All Ages', replacement = 'Poverty Percent All Ages')
  
  # Add year
  
  temp %<>% mutate(year = fff)
  
  # Select relevant variables
  
  temp %<>% dplyr::select(`State FIPS`, `County FIPS`,
                          contains('Poverty Percent All Ages'), 
                          contains('Median'),
                          year)
  
  temp %<>% mutate_at(vars(contains('Poverty')),
                      funs(as.numeric(.)))
  
  temp %<>% mutate_at(vars(contains('Poverty')),
                      funs(round(., 4)))
  
  temp %<>% mutate_at(vars(-contains('Poverty')),
                      funs(as.character(.)))
  
  # Bind to saipe data.frame
  
  saipe %<>% bind_rows(temp)
  
  # Remove file
  
  rm(temp, fff)
  
}

# Save to scratch

temp %>% saveRDS('shale-varying/Scratch/SAIPE_2000_2019.rds')

# Local Area Unemployment Statistics ----------------------------------
# Note: Use mapping function to download data from all vintages.

# Add SAIPE folder

dir.create('shale-varying/Data/LAUS', showWarnings = FALSE)

# Download data

years <- data.frame(fff = 0:18)
years %<>% mutate(fff = ifelse(str_length(as.character(fff)) == 1, paste0('0', as.character(fff)), as.character(fff)))

years <- years %>% pull()

urls   <- paste0("https://www.bls.gov/lau/laucnty",
                 years, ".txt")

files  <- paste('shale-varying/Data/LAUS/', basename(urls), sep = "/")

map2(urls, files, function(urls, files)
 if(!file.exists(files)) download.file(urls, files))

# Loop import and clean

lau <- data.frame()

for(fff in 0:18) {
  
  fff <- ifelse(str_length(as.character(fff)) == 1, paste0('0', as.character(fff)), as.character(fff))
  
  # Read in file 
  
  temp <- read_fwf(paste0('shale-varying/Data/LAUS/laucnty', fff, '.txt'),
                   skip = 12, fwf_widths(c(17, 7, 7, 50, 12, 10, 14, 10, 6)))
  
  # Add fips code
  
  temp %<>% mutate(county_fips_code = paste0(X2, X3))
  
  temp %<>% select(county_fips_code, unemployment_rate = X9)
  
  temp %<>% mutate(year = 2000 + as.numeric(fff))
  
  temp %<>% arrange(county_fips_code, year)
  
  lau %<>% bind_rows(temp)
  
  rm(temp)
  
}

# Save as DTA and RDS files

lau %>% saveRDS(file = paste0('shale-varying/Data/LAUS/lau_1995_2016.rds'))

# QCEW ----------------------------------

# Create directory

dir.create('shale-varying/Data/QCEW')

# Download data

for(fff in 2000:2018) {
  
  # Download file
  
  temp <- tempfile()
  
  download.file(paste0('https://data.bls.gov/cew/data/files/', as.character(fff), 
                       '/xls/', as.character(fff), '_all_county_high_level.zip'),
                paste0('shale-varying/Data/QCEW/qcew_', as.character(fff), '.zip'))
  
}

# Extract all files in folders in QCEW folder

zipped_folders <- list.files(path = "shale-varying/Data/QCEW", pattern = "*.zip")

for(fff in zipped_folders) {
  
  unzip(paste0('shale-varying/Data/QCEW/', fff), exdir = 'shale-varying/Data/QCEW')
  
}

# Extract all yearly files (county data)
# Notes: Use most generalized file.

qcew <- data.frame()

for(fff in 0:18) {
  
  fff <- ifelse(str_length(as.character(fff)) == 1, paste0('0', as.character(fff)), as.character(fff))
  
  # Read Excel file
  
  temp <- read_excel(paste0('shale-varying/Data/QCEW/allhlcn', fff, '.xlsx'),
                     sheet = 'US_St_Cn_MSA', col_names = TRUE)
  
  # Keep county data
  
  temp %<>% filter(`Area Type` %in% c('County'))
  
  # Filter only private industries
  
  temp %<>% filter(Ownership %in% c('Private'))
  
  # Keep only variables of interest
  
  temp %<>% dplyr::select(county_fipes_code = 1, Year, NAICS, Industry, `Annual Average Employment`, `Annual Total Wages`, 
                          `Annual Average Establishment Count`, `Annual Average Pay`)
  
  # Rename data frame columns
  
  names(temp) %<>% str_to_lower() %>% str_replace_all(pattern = ' ', replacement = '_')
  
  # Attach data to QCEW data frame
  
  qcew %<>% bind_rows(temp)
  
  # Print and remove files
  
  print(fff)
  
  rm(temp, fff)
  
}

# Save as RDS file (county data)

qcew %>% saveRDS('shale-varying/Scratch/QCEW_County_Data_2000_2018.rds')

# QCEW (State-wide, refined industry -------------------------------------
# Note: These data are different in that they provide estimates of employment/income by state/county with very refined
# NAICS codes.

dir.create('shale-varying/Data/QCEW_Refined')

# Download data
# Note: From this link. https://data.bls.gov/cew/data/files/[year]/csv/[year]_annual_by_area.zip

for(fff in 2000:2018) {
  
  # Download file
  
  temp <- tempfile()
  
  download.file(paste0('https://data.bls.gov/cew/data/files/', as.character(fff), 
                       '/csv/', as.character(fff), '_annual_by_area.zip'),
                paste0('shale-varying/Data/QCEW_Refined/qcew_refined_', as.character(fff), '.zip'))
  
}

# Extract all files in folders in QCEW folder

zipped_folders <- list.files(path = "shale-varying/Data/QCEW_Refined", pattern = "*.zip")

for(fff in zipped_folders) {
  
  unzip(paste0('shale-varying/Data/QCEW_Refined/', fff), exdir = 'shale-varying/Data/QCEW_Refined')
  
}

# Delete zipped folders
# Note: Can keep it if you prefer, but the size of the files is pretty large.

file.remove(paste0('shale-varying/Data/QCEW_Refined/', zipped_folders))

rm(zipped_folders)

# # Extract all yearly files (state data)
# Notes: Use most generalized file.

qcew_state <- data.frame()

for(fff in 2000:2018) {
  
  # Grab statewide files
  
  temp_files <- list.files(path = paste0('shale-varying/Data/QCEW_Refined/', as.character(fff),
                                         '.annual.by_area'),
                           pattern = 'Statewide\\.csv')
  
  # Add data frame for merging
  
  temp_qcew <- data.frame()
  
  # Set up loop for reading/cleaning
  
  for(ggg in temp_files) {
    
    # Read CSV file
    
    temp <- read_csv(paste0('shale-varying/Data/QCEW_Refined/', as.character(fff),
                            '.annual.by_area/', ggg), col_names = TRUE)
    
    # Filter NAICS codes for all + oil and gas extraction
    
    temp$industry_code %<>% as.character()
    
    temp %<>% filter(industry_code %in% c('10', '211') & own_title %in% c('Private'))
  
    
    # Select relevant variables
    
    temp %<>% dplyr::select(area_fips, area_title, year, industry_code,
                            annual_avg_emplvl, total_annual_wages, avg_annual_pay) 
    
    # Make all variables character
    
    temp %<>% mutate_at(vars(area_fips, area_title, industry_code),
                        funs(as.character(.)))
    
    # Attach data to QCEW data
    
    temp_qcew %<>% bind_rows(temp)
   
    # Remove files
    
    rm(temp, ggg)
    
  }

  # Bind to larger data frame
  
  qcew_state %<>% bind_rows(temp_qcew)
  
  # Print progress
  
  print(fff)
  
  # Remove files
  
  rm(temp_qcew, fff)
  
}

# Clean bound data

qcew_state %<>% rename(state_fips_code = area_fips,
                       state_name = area_title) %>%
  mutate(state_fips_code = str_sub(state_fips_code, end = 2),
         state_name = str_extract(state_name, pattern = '.*(?=(\\s\\-\\-))')) %>%
  rename(employment = annual_avg_emplvl,
         wages = total_annual_wages,
         average_wages = avg_annual_pay) %>%
  mutate(industry_code = ifelse(industry_code == '10', 'total', 'og')) %>%
  setDT() %>%
  dcast(state_fips_code + state_name + year ~ industry_code, value.var = c('employment', 'wages', 'average_wages'))

qcew_state %<>% mutate(
  employment_prop_og = employment_og / employment_total,
  wages_prop_og = wages_og / wages_total
)

# Save as RDS file (state data)

qcew_state %>% saveRDS('shale-varying/Scratch/QCEW_State_Data_2000_2018.rds')

# Oil and gas production data ---------------------------------
# Note: County-level data from 2000 onward. These data are from work performed by Elaine Hill's lab. I reference
# an internal Box folder, for now, but am asking Elaine Hill to upload the file to her website for easy downloading.
# In the data gathering, the team built off of the USDA's Economic Research Service's county-level oil and gas production
# data, which is only updated as of 2012. The team followed Jeremy Weber's notes in the update.

dir.create('shale-varying/Data/ERS')

# Import file from internal Box folder
# Note: Asked Elaine for push to website. Waiting to hear update. It appears that some years are missing (e.g., Pennsylvania < 2010).
# I will also import the USDA ERS version.

oil_and_gas_production_data <- read.dta('ShaleGas/OGProduction/Stata/Dta/Totals/USA_Final_v2_old.dta')

# Download and import ERS version

download.file('https://www.ers.usda.gov/webdocs/DataFiles/48668/oilgascounty.xls?v=1127.6',
              'shale-varying/Data/ERS/oilgascounty.xls',
              mode = 'wb')

oil_and_gas_production_data_ers <- read_excel('shale-varying/Data/ERS/oilgascounty.xls',
                                              sheet = 'oilgascounty', col_names = TRUE)

# Clean data (ERS)

# Select variables

oil_and_gas_production_data_ers %<>% dplyr::select(county_fips_code = geoid,
                                                   county_name = County_Name,
                                                   state_abbreviation = Stabr,
                                                   starts_with('oil20'),
                                                   starts_with('gas20'))

# Wide-to-long transformation

oil_and_gas_production_data_ers %<>% gather(variable, value, -county_fips_code, -county_name, -state_abbreviation) %>%
  mutate(year = as.numeric(str_sub(variable, start = 4)),
         energy = str_sub(variable, end = 3)) %>%
  dcast(county_fips_code + county_name + state_abbreviation + year ~ energy, value.var = c('value'))

# Clean data (Hill Lab)

# Fill NAs with interpolation
# Note: Linear interpolation.

oil_and_gas_production_data %>% filter(is.na(oil_production)) %>% nrow() # 2%

oil_and_gas_production_data %<>% group_by(county, year) %>%
                                 mutate_at(vars(contains('production')), 
                                           funs(na.approx(., na.rm = FALSE))) %>%
                                 ungroup()

# Filter 2000+

oil_and_gas_production_data %<>% filter(year >= 2000)

# Drop county FIPS code-year combinations that are in the ERS data

oil_and_gas_production_data %<>% filter(!(fips_code %in% oil_and_gas_production_data_ers$county_fips_code & 
                                            year %in% oil_and_gas_production_data_ers$year))

# Rename oil and gas production data for joining with ERS data

oil_and_gas_production_data %<>% rename(county_fips_code = fips_code,
                                        county_name = county, oil = oil_production, gas = gas_production)

# Bind data together

oil_and_gas_production_data %<>% bind_rows(oil_and_gas_production_data_ers)

rm(oil_and_gas_production_data_ers)

# Check number of distinct county FIPS code and year (duplicates)

oil_and_gas_production_data %<>% group_by(county_fips_code, year) %>%
  filter(row_number() == 1) %>%
  ungroup() # 17 or so dropped

# Save file as RDS

oil_and_gas_production_data %>% saveRDS('shale-varying/Scratch/County_Oil_and_Gas_Production_2000_2017.rds')

# Interlude: Does pre-2010 data line up?

# temp_ers <- oil_and_gas_production_data_ers %>% filter(year <= 2010) %>%
#   rename(gas_ers = gas, oil_ers = oil)
# 
# temp_hill <- oil_and_gas_production_data %>% filter(year <= 2010) %>%
#   rename(gas_hill = gas_production, oil_hill = oil_production)
# 
# temp <- temp_hill %>% left_join(temp_ers, by = c('year', 'fips_code' = 'county_fips_code'))
# 
# rm(temp_ers, temp_hill)
# 
# temp %>% mutate(exact_gas = ifelse(gas_hill == gas_ers, 1, 0),
#                 exact_oil = ifelse(oil_hill == oil_ers, 1, 0)) %>%
#   summarise_at(vars(contains('exact')),
#                funs(mean(., na.rm = TRUE)))
# 
# temp %>% filter(year == 2005) %>%
#   group_by(state_abbreviation) %>%
#   summarise_at(vars(gas_hill, gas_ers),
#                funs(sum(., na.rm = TRUE))) %>% ungroup() %>%
#   mutate(diff = (gas_hill - gas_ers) / gas_ers) -> hiya

# SEER Population data ----------------------------------
# Note: From the CDC SEER website, not NBER.

# Download file

temp <- tempfile()
download.file("https://data.nber.org/seer-pop/uswbo19agesadj.csv.zip",temp)
recent_pop <- read_csv(unz(temp, "uswbo19agesadj.csv"))
unlink(temp)

names(recent_pop) <-  c('year', 'state', 'state_fips', 'county_fips_code',
                        'registry', 'race', 'origin', 'sex', 'age', 
                        'population')

# Calculate population by race, year, and county FIPS code

recent_pop %<>% group_by(county_fips_code, year, race) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  mutate(race = case_when(
    race == 1 ~ 'white',
    race == 2 ~ 'black',
    race == 3 ~ 'other'
  )) %>%
  dcast(county_fips_code + year ~ race, value.var = c('population'))

# Filter post-2000

recent_pop %<>% filter(year >= 2000)

# Save as RDS file 

recent_pop %>% saveRDS('shale-varying/Scratch/SEER_Population_Data_2000_2018.rds')

# SOI Tax Income data ---------------------------------
# Notes: See Feyrer et al. (2017) (and associated comment from James & Smith). These data are based on residence
# of taxpayer, rather than address of employer.

# Create directory

dir.create('shale-varying/Data/SOI')

# Download data (2011-2018)

for(fff in 00:18) {
  
  if(fff >= 11) {
    
    # Define file type
    # Note: Changes to xlsx in 2017 
    
    file_type <- ifelse(fff < 17, 'xls', 'xlsx')
    
    # Download file
    
    download.file(url = paste0('https://www.irs.gov/pub/irs-soi/', as.character(fff), 'incyall.', file_type),
                  destfile = paste0('shale-varying/Data/SOI/', as.character(fff), 'incyall.', file_type), mode = 'wb')
  
  } else if(fff <= 9) {
    
    download.file(url = paste0('https://www.irs.gov/pub/irs-soi/', as.character(2000 + fff), 'countyincome.zip'),
                  paste0('shale-varying/Data/SOI/', as.character(2000 + fff), 'countyincome.zip'))
    
    
  } else {
    
    download.file(url = paste0('https://www.irs.gov/pub/irs-soi/', as.character(2000 + fff), 'countydata.zip'),
                  paste0('shale-varying/Data/SOI/', as.character(2000 + fff), 'countydata.zip'))
    
  }
  
}

# Extract all zip files in folders in SOI folder

zipped_folders <- list.files(path = "shale-varying/Data/SOI", pattern = "*.zip")

for(fff in zipped_folders) {
  
  unzip(paste0('shale-varying/Data/SOI/', fff), exdir = 'shale-varying/Data/SOI')
  
}
# Shale play timing --------------------------
# Note: I don't buy this timing...yet...but I am going to bring it in anyway as a starting point for defining when
# shale development happened in a particular play. This is from Bartik et al. (2019) in the American Economic Review (Applied).
# Some of these years don't resonate with me but some serious shoe-leather went into their work (see their Appendix). We'll have to check
# this out and square with some production/drilling data + employment shares in O&G Extraction.

# Create directory

dir.create('shale-varying/Data/Bartik')

# Import by hand
# Note: Will get around to OCRing this image for fun.

shale_timing <- data.frame(shale_play = c('Woodford-Anadarko', 'Marcellus', 'Utica', 
                           'Woodford-Ardmore', 'Fayetteville', 'Woodford-Arkoma',
                           'Niobrara-Denver', 'Barnett', 'Niobrara-Greater Green River',
                           'Permian, all plays', 'Niobrara-Powder River', 'Haynesville',
                           'Eagle Ford', 'Bakken'),
                           shale_basin = c('Anadarko', 'Appalachian', 'Appalachian',
                                           'Ardmore', 'Arkoma', 'Arkoma', 'Denver',
                                           'Fort Worth', 'Greater Green River', 'Permian',
                                           'Powder River', 'TX-LA-MS-Salt', 'Western Gulf',
                                           'Williston Basin'),
                           first_frac_year = c(2008, 2008, 2012, 2007, 2005, 2006, 
                                               2010, 2001, 2012, 2005, 2010, 2008,
                                               2009, 2007))

# Save as RDS file

shale_timing %>% saveRDS('shale-varying/Data/Bartik/Shale_Play_Development_Timing.rds')

# Mapping shale plays to counties -----------------------------------
# Notes: In this part of the code, I download shapefile data from the U.S. Energy Information Administration on locations of
# shale plays in the U.S. I also download county shapefile data from the U.S. Census Bureau. I then use a Python script (1-spatial-data-gathering.py)
# to estimate the distance from each county in the U.S. to the near(est) shale plays.

# Create GIS directory

dir.create('shale-varying/Data/GIS')

# Download shapefiles

# EIA shapefile data: https://www.eia.gov/maps/map_data/TightOil_ShaleGas_Plays_Lower48_EIA.zip

download.file('https://www.eia.gov/maps/map_data/TightOil_ShaleGas_Plays_Lower48_EIA.zip',
              'shale-varying/Data/GIS/TightOil_ShaleGas_Plays_Lower48_EIA.zip')

unzip(paste0('shale-varying/Data/GIS/TightOil_ShaleGas_Plays_Lower48_EIA.zip'), exdir = 'shale-varying/Data/GIS')
  
# U.S. CB county-shapefile data: 

download.file('https://www2.census.gov/geo/tiger/TIGER2020/COUNTY/tl_2020_us_county.zip',
              'shale-varying/Data/GIS/tl_2020_us_county.zip')

unzip(paste0('shale-varying/Data/GIS/tl_2020_us_county.zip'), exdir = 'shale-varying/Data/GIS')

# Import distance between two shapefiles

temp_distance <- read_csv('shale-varying/Scratch/USCB_County_to_USEIA_Shale_Play_50Miles.csv')

# Import shapefiles

county_shp <- readOGR(dsn = 'shale-varying/Data/GIS',
                      layer = 'tl_2020_us_county',
                      verbose = TRUE) %>% as.data.frame() %>%
  mutate(FID = row_number() - 1)

shale_shp <- readOGR(dsn = 'shale-varying/Data/GIS',
                     layer = 'ShalePlays_US_EIA_Sep2019',
                     verbose = TRUE) %>% as.data.frame() %>%
  mutate(FID = row_number() - 1)

# Clean the shapefiles

county_shp %<>% dplyr::select(FID, county_fips_code = GEOID)

shale_shp %<>% dplyr::select(FID, contains('Shale'), shale_basin = Basin)

# Clean the distance file

temp_distance %<>% dplyr::select(contains('FID'), NEAR_DIST)

# Join features to distance table

temp_distance %<>% left_join(county_shp, by = c('NEAR_FID' = 'FID')) %>%
                   left_join(shale_shp, by = c('IN_FID' = 'FID')) %>%
  dplyr::select(-contains('FID'))

# Import classifications from Bartik paper
# Note: See code above. This will 
# Create distance matrix

shale_timing <- readRDS('shale-varying/Data/Bartik/Shale_Play_Development_Timing.rds')

# Map shale plays from shapefile to shale play name from Bartik

temp_distance %>% dplyr::select(Shale_play) %>%
  unique() %>% 
  filter(!Shale_play %in% shale_timing$shale_play)

shale_plays_eia <- shale_shp %>% dplyr::select(shale_play = Shale_play,
                                               shale_basin) %>%
  unique()

shale_play_bartik <- shale_timing %>% dplyr::select(shale_play, shale_basin)

stringdist_join(shale_plays_eia, shale_play_bartik, 
                by = "shale_play",
                mode = "full",
                ignore_case = TRUE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") -> string_matches

names(string_matches) %<>% str_replace_all(pattern = '\\.x', replacement = '_eia') %>%
  str_replace_all(pattern = '\\.y', replacement = '_bartik')

string_matches %<>% arrange(dist) %>%
  group_by(shale_play_eia) %>%
  filter(row_number() <= 3) %>%
  ungroup()

  group_by(name.x) %>%
  slice_min(order_by = dist, n = 1)