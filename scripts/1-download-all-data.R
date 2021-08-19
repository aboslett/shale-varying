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

  if(fff >= 2003 & fff <= 2004) {
    
    temp <- read_excel(paste0('shale-varying/Data/SAIPE/saipe_', as.character(fff), '.xls'),
                       col_names = TRUE,
                       skip = 1)
  
  } else if(fff >= 2005 & fff <= 2012) {
    
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
  
  temp %<>% mutate_at(vars(contains('Poverty'), contains('Median')),
                      funs(as.numeric(.)))
  
  temp %<>% mutate_at(vars(contains('Poverty')),
                      funs(round(., 4)))
  
  temp %<>% mutate_at(vars(-contains('Poverty'), -contains('Median')),
                      funs(as.character(.)))
  
  # Bind to saipe data.frame
  
  saipe %<>% bind_rows(temp)
  
  # Remove file
  
  rm(temp, fff)
  
}

# Clean file

saipe %<>% mutate_at(vars(contains('FIPS')), 
                     funs(as.character(.)))

saipe %<>% mutate_at(vars(contains('FIPS')), 
                     funs(str_trim(.)))

saipe %<>% mutate(`State FIPS` = ifelse(str_length(`State FIPS`) == 1, paste0('0', `State FIPS`), `State FIPS`),
                  `County FIPS` = case_when(
                    str_length(`County FIPS`) == 1 ~ paste0('00', `County FIPS`),
                    str_length(`County FIPS`) == 2 ~ paste0('0', `County FIPS`),
                    str_length(`County FIPS`) == 3 ~ `County FIPS`
                  ),
                  county_fips_code = paste0(`State FIPS`, `County FIPS`),
                  year = as.numeric(year)) %>%
  dplyr::select(county_fips_code, year, `Median Household Income`, contains('Poverty'))

saipe %<>% arrange(county_fips_code, year)

# Save as RDS file

saipe %>% saveRDS('shale-varying/Scratch/SAIPE_2000_2019.rds')

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

# DrillingInfo data ---------------------------------
# Notes: These data are not publicly available. FracTracker provides a nice directory of state-level databases from 
# government agencies. One could go down that route with the goal of having fully-replicable work.

di_wells <- read_csv('ShaleGas/Drillinginfo/Nationwide/National_HD_Wells_10252017.csv')

# Clean data

names(di_wells)

# Keep relevant data

di_wells %<>% dplyr::select(API14, `County/Parish`,
                            `Production Type`, `True Vertical Depth`, 
                            `Drill Type`, `Spud Date`, 
                            contains('Latitude'), contains('Longitude'),
                            -contains('Bottom'))

# County name is a mess (county name + (state abbreviation)). Can we use lat/long to assign wells to counties?

di_wells %>% filter(is.na(`Surface Hole Latitude (WGS84)`) == TRUE) %>% nrow() / nrow(di_wells) # 0.12% of wells

temp_plot <- ggplot(data = di_wells,
                    aes(x = `Surface Hole Latitude (WGS84)`)) + 
  theme_classic() + geom_histogram(binwidth = 1, color = 'black', fill = 'grey80')

temp_plot 

rm(temp_plot)

temp_plot <- ggplot(data = di_wells,
                    aes(x = `Surface Hole Longitude (WGS84)`)) + 
  theme_classic() + geom_histogram(binwidth = 1, color = 'black', fill = 'grey80')

temp_plot

rm(temp_plot)

# Keep if latitude and longitude are non-missing

di_wells %<>% filter(!is.na(`Surface Hole Longitude (WGS84)`) == TRUE & 
                       !is.na(`Surface Hole Latitude (WGS84)`))

# What types of wells are we dealing with here?

di_wells %>% group_by(`Production Type`) %>% summarise(n = n()) %>%
  ungroup()

di_wells %<>% filter(`Production Type` %in% c('OIL', 'OIL & GAS', 'GAS', 'UNKNOWN'))

di_wells %>% group_by(`Drill Type`) %>% summarise(n = n()) %>% 
  ungroup() # D + H is good.

# Can't be sure that the well latitude/longitudes were measured reasonably well, but we do know that there weren't
# weird coordinate definition issues (e.g., using negative values instead of positive for latitude, using projected coordinate
# systems instead of geographic coordinate systems). Let's use ArcGIS to assign wells to counties. Let's convert this file
# into a shapefile and assign each well to its overlying county. This will probably give us the same exact answer as the count-by-attribute
# but this is arguably easier.

di_wells %<>% as.data.frame()

di_wells %<>% select(latitude = `Surface Hole Latitude (WGS84)`,
                     longitude = `Surface Hole Longitude (WGS84)`,
                     `Drill Type`,
                     `Spud Date`) %>%
  mutate(unique_id = row_number())

# Create a spatial points object

sp::SpatialPointsDataFrame(coords = select(di_wells, longitude, latitude),
                           proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '), 
                           data = di_wells) -> di_wells_sp

# Reproject to albers equal area conic

di_wells_sp %<>% spTransform(CRSobj = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

# Write as a shapefile

writeOGR(obj = di_wells_sp, 
         dsn = paste0('shale-varying/Data/GIS'), 
         layer = 'National_HD_Wells', 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

rm(di_wells_sp)

# Read in distance file 
# Notes: From 1-gather...

temp_distance <- read_csv('shale-varying/Scratch/USCB_County_to_DI_HD_Wells_5Miles.csv')

temp_distance %<>% dplyr::select(contains('FID'), NEAR_DIST)

# Read county shapefile

county_shp <- readOGR(dsn = 'shale-varying/Data/GIS',
                      layer = 'tl_2020_us_county_prj',
                      verbose = TRUE) %>% as.data.frame() %>%
  mutate(FID = row_number() - 1) %>% dplyr::select(FID, county_fips_code = GEOID)

# Join shapefile data to distance file

di_wells %<>% mutate(FID = row_number() - 1)

# Join distance file with attributes

temp_distance %<>% left_join(county_shp, by = c('IN_FID' = 'FID')) %>%
  left_join(di_wells, by = c('NEAR_FID' = 'FID'))

# Filter out to only the well-to-county matches
# Note: We only want to keep those matches where the well is within the county.

temp_distance %<>% filter(NEAR_DIST == 0)

n_distinct(temp_distance$NEAR_FID) == nrow(temp_distance) # Each well only appears once 

# Drop wells that don't have a spud date

temp_distance %>% filter(!is.na(`Spud Date`) == TRUE) %>% nrow()

temp_distance %<>% filter(!is.na(`Spud Date`) == TRUE)

# Keep post-2000

temp_distance %<>% mutate(`Spud Date` = lubridate::mdy(`Spud Date`),
                          `Spud Year` = lubridate::year(`Spud Date`))

temp_distance %<>% filter(`Spud Year` >= 2000)

# Calculate # of wells by county-year

temp_distance %>% group_by(county_fips_code, `Spud Year`, `Drill Type`) %>%
  summarise(n_n = n()) %>%
  ungroup() %>%
  dcast(county_fips_code + `Spud Year` ~ `Drill Type`, value.var = c('n_n')) -> well_counts
  
well_counts %<>% mutate_at(vars(D, H),
                           funs(ifelse(is.na(.) == TRUE, 0, .))) %>%
  mutate(hd_wells = D + H)

# Save as RDS file 

well_counts %>% saveRDS('shale-varying/Scratch/County_Well_Counts_by_Year_2000_2017.rds')

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
                      layer = 'tl_2020_us_county_prj',
                      verbose = TRUE) %>% as.data.frame() %>%
  mutate(FID = row_number() - 1)

shale_shp <- readOGR(dsn = 'shale-varying/Data/GIS',
                     layer = 'ShalePlays_US_EIA_Sep2019_prj',
                     verbose = TRUE) %>% as.data.frame() %>%
  mutate(FID = row_number() - 1)

# Clean the shapefiles

county_shp %<>% dplyr::select(FID, county_fips_code = GEOID)

shale_shp %<>% dplyr::select(FID, contains('Shale'), shale_basin = Basin)

shale_shp %<>% rename(shale_play = Shale_play)

# Clean the distance file

temp_distance %<>% dplyr::select(contains('FID'), NEAR_DIST)

# Join features to distance table

temp_distance %<>% left_join(shale_shp, by = c('NEAR_FID' = 'FID')) %>%
                   left_join(county_shp, by = c('IN_FID' = 'FID')) %>%
  dplyr::select(-contains('FID'))

names(temp_distance) %<>% str_to_lower()

# Import classifications from Bartik paper
# Note: See code above. This will 
# Create distance matrix

shale_timing <- readRDS('shale-varying/Data/Bartik/Shale_Play_Development_Timing.rds')

# Map shale plays from shapefile to shale play name from Bartik

# Rename some plays in the EIA file by hand and extract unique plays

rename_shales <- function(df) {
  
  df %<>% mutate(shale_play = ifelse(shale_basin == 'Permian', 'Permian, all plays', shale_play),
                 shale_play = ifelse(shale_basin == 'Powder River', 'Niobrara-Powder River', shale_play),
                 shale_play = ifelse(shale_basin == 'Williston', 'Bakken', shale_play),
                 shale_play = ifelse(shale_basin == 'Greater Green River', 'Niobrara-Greater Green River', shale_play),
                 shale_play = ifelse(str_detect(shale_basin, pattern = 'Denver') | str_detect(shale_play, pattern = 'Denver'), 'Niobrara-Denver', shale_play),
                 shale_play = ifelse(str_detect(shale_play, 'Woodford$') == TRUE, paste0(shale_play, '-', shale_basin), shale_play))
  
  df$shale_play %<>% str_replace_all(pattern = 'Caney', 'Arkoma') %>%
    str_replace_all(pattern = '\\-Bossier', replacement = '')
  
  return(df)
  
}

shale_shp <- rename_shales(shale_shp)
temp_distance <- rename_shales(temp_distance)

shale_timing %>% filter(!shale_play %in% shale_shp$shale_play) %>% nrow() == 0

# Bartik et al. (2019) used the Modern Shale Gas Development in the U.S. - An Update paper to define
# which shale plays they looked at. As you'll notice from the script, there are a number of plays that
# have seen only limited activity. However, there were a few shale plays that were active beyond a frontier
# state that were not included in their sample:
# (1) Antrim Shale (Michigan)
# (2) New Albany Shale (Illinois)
# We will keep these shales in our database, plus those that are in their paper, moving forward. We'll label all other
# shales as non-active shales in our database.

temp_distance %<>% mutate(shale_play = case_when(
  shale_play %in% shale_timing$shale_play | shale_play %in% c('New Albany', 'Antrim') ~ shale_play,
  !(shale_play %in% shale_timing$shale_play | shale_play %in% c('New Albany', 'Antrim')) ~ 'non_active_shale'
))

# Reshape-wide

temp_distance %<>% dplyr::select(county_fips_code, shale_play, near_dist) %>%
  arrange(county_fips_code, shale_play, near_dist) %>%
  group_by(county_fips_code, shale_play) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  dcast(county_fips_code ~ shale_play, value.var = c('near_dist'))

# Generate binary-version of distance table

temp_distance %<>% mutate_at(vars(Antrim:`Woodford-Arkoma`),
                             funs(ifelse(is.na(.) == TRUE, 50000, .))) %>%
  mutate_at(vars(Antrim:`Woodford-Arkoma`),
            funs(ifelse(. == 0, 1, 0))) -> distance_dummies

# Save files as RDS files

temp_distance %>% saveRDS('shale-varying/Scratch/County_to_Shale_Distance_File_50mi.rds')
distance_dummies %>% saveRDS('shale-varying/Scratch/County_to_Shale_Distance_File_Dummy.rds')
