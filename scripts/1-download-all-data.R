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

# Extract all yearly files 
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

# Save as RDS file 

qcew %>% saveRDS('shale-varying/Scratch/QCEW_Data_2000_2018.rds')

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
                                                   starts_with('oil20'),
                                                   starts_with('gas20'))

# Wide-to-long transformation

oil_and_gas_production_data_ers %<>% gather(variable, value, -county_fips_code, -county_name) %>%
  mutate(year = as.numeric(str_sub(variable, start = 4)),
         energy = str_sub(variable, end = 3)) %>%
  dcast(county_fips_code + county_name + year ~ energy, value.var = c('value'))

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
                                        county_name = county, oil = oil_production, gas = gas_production) %>%
  dplyr::select(-state_abbreviation)

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

# County Business Patterns -----------------------------
# Notes: Abboud and Betz (2021) use these data in their evaluation of shale development. I probably won't use these data
# but we'll see.

# Create directory

dir.create('shale-varying/Data/CBP')

# Download and unzip data

for(fff in 2000:2018) {
  
  ggg <- ifelse(str_length(as.character(fff - 2000)) == 1, paste0('0', as.character(fff - 2000)), as.character(fff - 2000))
  
  # Download file
  
  temp <- tempfile()
  
  download.file(paste0('https://www2.census.gov/programs-surveys/cbp/datasets/', as.character(fff), 
                       '/cbp', as.character(ggg), 'co.zip'),
                paste0('shale-varying/Data/CBP/cbp_', as.character(fff), '.zip'))
  
  # Extract contents into folder
  
  unzip(paste0('shale-varying/Data/CBP/cbp_', as.character(fff), '.zip'), exdir = 'shale-varying/Data/CBP')
  
}

# Import and clean files

cbp_data <- data.frame()

cbp_files <- list.files(path = 'shale-varying/Data/CBP', pattern = '*.txt')

for(fff in cbp_files) {
  
    temp <- read_delim(file = paste0('shale-varying/Data/CBP/', fff),
                       delim = ',', col_names = TRUE)
    
    # Make all variable names lowercase (for 15-18)
    
    names(temp) %<>% str_to_lower()
    
    # Select variables of interest and add county FIPS code
    
    temp %<>% mutate(county_fips_code = paste0(fipstate, fipscty),
                     year = 2000 + as.numeric(str_sub(fff, start = 4, end = 5))) %>%
      dplyr::select(county_fips_code, year, contains('nai'),
                    emp, ap, est)
    
    # Grab two-digit NAICS and oil and gas-extract NAICS (211)
    
    temp$naics %<>% str_replace_all(pattern = '[:punct:]',
                                    replacement = '') %>%
      str_trim()
    
    temp %<>% filter(naics == '' | str_length(naics) == 2 | naics == '211')
    
    # Long-to-wide
    
    temp %<>% mutate(naics = paste0('naics_', naics)) %>%
      setDT() %>%
      dcast(county_fips_code + year ~ naics, value.var = c('emp', 'ap', 'est'))
    
    # Bind to data frame
    
    cbp_data %<>% bind_rows(temp)
    
    # Remove files and print progress
    
    print(fff)
    
    rm(temp, fff)
    
}

# Arrange data

cbp_data %<>% arrange(county_fips_code, year)

# Save data as RDS

cbp_data %>% saveRDS('shale-varying/Scratch/CBP_Data_2000_2018.rds')

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