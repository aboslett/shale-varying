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

# Save as DTA and RDS files --------------

lau %>% saveRDS(file = paste0('shale-varying/Data/LAUS/lau_1995_2016.rds'))
