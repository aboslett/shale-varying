# Andrew Boslett
# University of Rochester Medical Center/FLX AI
# Email: andrew_boslett@urmc.rochester.edu
# Permanent email: andrew.boslett@gmail.com

rm(list = ls())

# Goal: Explore data. Visualize what's going on viz a viz income in areas with vs. without shale development. What's the general
# trajectory of development across states? Is there differential timing in exposure to drilling?

# Visualize oil and gas production across time by state  ------------------------

# Import OG production data
# Note: Using a combination of USDA ERS oil and gas production data, assembled by Jeremy Weber,
# and data updated through 2017-2018 from Elaine Hill's lab. 

oil_and_gas_production_data <- readRDS('shale-varying/Scratch/County_Oil_and_Gas_Production_2000_2017.rds')

# Clean data

oil_and_gas_production_data$county_fips_code %<>% str_trim()

oil_and_gas_production_data %<>% filter(!is.na(county_fips_code) | county_fips_code == '')

oil_and_gas_production_data %<>% arrange(county_fips_code, year)

oil_and_gas_production_data %<>% filter(year <= 2011) # Focus on ERS-based data (allows us to avoid data gathering differences)

# (1) State-by-year plot ----------------------------------
# Note: Use logs.

temp_summary <- oil_and_gas_production_data %>% group_by(state_abbreviation, year) %>%
  summarise_at(vars(oil, gas),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(oil, gas),
            funs(. + 1)) %>%
  mutate_at(vars(oil, gas),
            funs(log = log(.)))

# Plot

temp_plot <- ggplot(data = subset(temp_summary,
                                  state_abbreviation %in% c('PA', 'WV', 'OH', 'TX', 'OK', 'CO', 'LA', 'ND',
                                                            'AR', 'NM')),
                    aes(x = year, y = oil_log, colour = state_abbreviation, group = state_abbreviation)) + 
  theme_classic() + geom_line() + labs(x = 'Year', y = 'Oil production (in logs)') +
  theme(legend.title = element_blank())

temp_plot

ggsave('shale-varying/Figures/Figure_X_Oil_Production_in_Logs_by_State.jpg')

# Explore treatment timing by county/state ---------------------------
# Notes: It seems like a number of papers use U.S. BEA State/County Personal Income, but I think QCEW data
# for oil and gas extraction-related employment could be used, too.

rm(list = ls())

qcew <- readRDS('shale-varying/Scratch/QCEW_State_Data_2000_2018.rds')

# Plot trends in proportions

qcew_plot <- ggplot(data = qcew,
                    aes(x = year, y = employment_prop_og, group = state_name)) + 
  theme_classic() + labs(x = 'Year', y = '% of private employment in O&G extraction',
                         title = 'Trends in % of private employment in O&G extraction',
                         subtitle = 'NAICS Code: 211',
                         caption = 'Source: Estimates based on QCEW data.') + 
  geom_line(color = 'grey90') + theme(legend.title = element_blank())

qcew_plot

# Interesting trends: For a number of states, the proportion increases through 2010-2014 below dropping substantially post-2014. 

ggsave('shale-varying/Figures/Figure_X_Proportion_of_Private_Employment_OG_Extraction.jpg')

# Filter on initial reliance

qcew %>% arrange(year, -employment_prop_og) %>%
  group_by(year) %>%
  mutate(ranking = row_number()) %>%
  ungroup() %>%
  filter(year == 2000) %>%
  arrange(-employment_prop_og) %>%
  filter(row_number() <= 10) -> temp_high

qcew_plot <- ggplot(data = subset(qcew, state_name %in% temp_high$state_name),
                    aes(x = year, y = employment_prop_og, 
                        colour = state_name, group = state_name)) + 
  theme_classic() + labs(x = 'Year', y = '% of private employment in O&G extraction',
                         title = 'Trends in % of private employment in O&G extraction',
                         subtitle = 'NAICS Code: 211',
                         caption = 'Source: Estimates based on QCEW data.') + 
  geom_line() + theme(legend.title = element_blank())

qcew_plot

ggsave('shale-varying/Figures/Figure_X_Proportion_of_Private_Employment_OG_Extraction.jpg')

# Compare Bartik et al. timing with shale production numbers by state ---------------------------

rm(list = ls())

# Import oil and gas spud data

well_counts <- readRDS('shale-varying/Scratch/County_Well_Counts_by_Year_2000_2017.rds')

# Import Bartik-timing

shale_timing <- readRDS('shale-varying/Data/Bartik/Shale_Play_Development_Timing.rds')

# Import county mapping to shale

distance_dummies <- readRDS('shale-varying/Scratch/County_to_Shale_Distance_File_Dummy.rds')

# Steps in joining
# (1) Get county key dataframe
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

# (2) Join well counts to the key DF

county_shp %<>% left_join(well_counts, by = c('county_fips_code', 'year' = 'Spud Year'))

rm(well_counts)

county_shp %<>% mutate_at(vars(D, H, hd_wells),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))

# (3) Define county-level exposure to shale-plays over time
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

# (4) Merge county-level exposure to shale plays over time

county_shp %<>% left_join(distance_dummies, by = c('county_fips_code'))

rm(list = setdiff(ls(), c('county_shp', 'shale_timing')))

county_shp %<>% mutate(shale_county = ifelse(!is.na(shale_play) == TRUE, 1, 0),
                       post_shale = ifelse(year >= first_frac_year & !is.na(first_frac_year) == TRUE, 1, 0))

county_shp %<>% arrange(county_fips_code, year)

# (6) Merge with other explanatory data

rural_urban <- readRDS('shale-varying/Scratch/USDA_ERS_Rural_Urban_Classification.rds')

county_shp %<>% left_join(rural_urban, by = c('county_fips_code'))

rm(rural_urban)

lau <- readRDS(file = paste0('shale-varying/Data/LAUS/lau_1995_2016.rds'))

county_shp %<>% left_join(lau, by = c('county_fips_code', 'year'))

rm(lau)

recent_pop <- readRDS('shale-varying/Scratch/SEER_Population_Data_2000_2018.rds')

recent_pop %<>% mutate(non_white_pop_percentage = (black + other) / (black + other + white),
                       total_pop = black + other + white)

county_shp %<>% left_join(recent_pop, by = c('county_fips_code', 'year'))

rm(recent_pop)

# (5) Plot development over time by shale play

for(fff in unique(shale_timing$shale_play)) {
  
  # Calculate # of wells by year
  
  temp <- county_shp %>% filter(shale_play == fff) %>%
    group_by(year) %>%
    summarise_at(vars(D, H, hd_wells),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  # Grab first year of development (Bartik)
  
  first_year <- shale_timing %>% filter(shale_play == fff) %>% dplyr::select(first_frac_year) %>% pull()
  
  # Plot data and save
  
  temp_plot <- ggplot(data = temp,
                      aes(x = year, y = hd_wells)) + 
    theme_classic() + labs(x = 'Year', y = '# of horizontal/directional wells',
                           title = 'Annual shale development by active play in the U.S.',
                           subtitle = fff) + 
    geom_point(color = 'dodgerblue', size = 3) + geom_line(color = 'black') + 
    geom_vline(xintercept = first_year)
  
  temp_plot
  
  ggsave(paste0('shale-varying/Figures/Figure_X_Shale_Development_in_', fff, '_Play.jpg'))
  
  # Timestamp and remove files
  
  print(fff)
  
  rm(temp, temp_plot, first_year, fff)
    
}

# Regressions -----------------------------------

# Merge with SAIPE data

saipe <- readRDS('shale-varying/Scratch/SAIPE_2000_2019.rds')

county_shp %<>% left_join(saipe, by = c('county_fips_code', 'year'))

# Merge with Zillow SFR data

zillow_sfr <- readRDS('shale-varying/Scratch/Zillow_ZHVI_SFR_1996_2021.rds')

county_shp %<>% left_join(zillow_sfr, by = c('county_fips_code', 'year'))

rm(zillow_sfr)

# Add interaction term

county_shp %<>% mutate(interaction_term = shale_county * post_shale)

county_shp %<>% mutate(state_fips_code = str_sub(county_fips_code, end = 2)) %>%
  group_by(state_fips_code) %>%
  mutate(shale_state = max(shale_county)) %>%
  ungroup()

# Rename variables

names(county_shp) %<>% str_replace_all(pattern = ' ', replacement = '_')

# Benchmark difference-in-difference models

all_results <- data.frame() 

# (1) Shale counties + standard FE

temp_model <- felm(data = subset(county_shp, shale_state == 1),
                   Median_Household_Income ~ interaction_term + log(total_pop) + non_white_pop_percentage | 
                     county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
  mutate_at(vars(-contains('term')),
            funs(round(., 3))) %>%
  filter(term == 'interaction_term') %>%
  mutate(model = 'Income, Shale States, State + Year FE')

all_results %<>% bind_rows(temp_model)

temp_model <- felm(data = subset(county_shp, shale_state == 1),
                    Poverty_Percent_All_Ages ~ interaction_term + log(total_pop) + non_white_pop_percentage | 
                     county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
  mutate_at(vars(-contains('term')),
            funs(round(., 3))) %>%
  filter(term == 'interaction_term') %>%
  mutate(model = 'Poverty, Shale States, State + Year FE')

all_results %<>% bind_rows(temp_model)

temp_model <- felm(data = subset(county_shp, shale_state == 1),
                   unemployment_rate ~ interaction_term + log(total_pop) + non_white_pop_percentage | 
                     county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
  mutate_at(vars(-contains('term')),
            funs(round(., 3))) %>%
  filter(term == 'interaction_term') %>%
  mutate(model = 'Unemployment rate, Shale States, State + Year FE')

all_results %<>% bind_rows(temp_model)

temp_model <- felm(data = subset(county_shp, shale_state == 1),
                   zhvi_sfr ~ interaction_term + log(total_pop) + non_white_pop_percentage | 
                     county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
  mutate_at(vars(-contains('term')),
            funs(round(., 3))) %>%
  filter(term == 'interaction_term') %>%
  mutate(model = 'Zillow SFR, Shale States, State + Year FE')

all_results %<>% bind_rows(temp_model)

# (2) Same as above, by metro vs. non-metro
# Note: Based on USDA-ERS Rural-Urban Continuum data.

metro_reg <- function(df, metro_class) {
  
  metro_class_string <- ifelse(metro_class == 1, 'Metro', 'Non-Metro')
  
  temp_models <- data.frame()
  
  temp_model <- felm(data = subset(df, shale_state == 1 & metro == metro_class),
                    Median_Household_Income ~ interaction_term  + log(total_pop) + non_white_pop_percentage | 
                      county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
    mutate_at(vars(-contains('term')),
              funs(round(., 3))) %>%
    filter(term == 'interaction_term') %>%
    mutate(model = paste0('Income, Shale States, State + Year FE, ', metro_class_string, ' Counties'))
  
  temp_models %<>% bind_rows(temp_model)
  
  temp_model <- felm(data = subset(df, shale_state == 1 & metro == metro_class),
                     Poverty_Percent_All_Ages ~ interaction_term  + log(total_pop) + non_white_pop_percentage | 
                       county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
    mutate_at(vars(-contains('term')),
              funs(round(., 3))) %>%
    filter(term == 'interaction_term') %>%
    mutate(model = paste0('Poverty, Shale States, State + Year FE, ', metro_class_string, ' Counties'))
  
  temp_models %<>% bind_rows(temp_model)
  
  temp_model <- felm(data = subset(df, shale_state == 1 & metro == metro_class),
                     unemployment_rate ~ interaction_term  + log(total_pop) + non_white_pop_percentage | 
                       county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
    mutate_at(vars(-contains('term')),
              funs(round(., 3))) %>%
    filter(term == 'interaction_term') %>%
    mutate(model = paste0('Unemployment rate, Shale States, State + Year FE, ', metro_class_string, ' Counties'))
  
   temp_models %<>% bind_rows(temp_model)
   
   temp_model <- felm(data = subset(df, shale_state == 1 & metro == metro_class),
                      zhvi_sfr ~ interaction_term  + log(total_pop) + non_white_pop_percentage | 
                        county_fips_code + as.character(year) | 0 | county_fips_code) %>% tidy() %>%
     mutate_at(vars(-contains('term')),
               funs(round(., 3))) %>%
     filter(term == 'interaction_term') %>%
     mutate(model = paste0('Zillow SFR, Shale States, State + Year FE, ', metro_class_string, ' Counties'))
   
   temp_models %<>% bind_rows(temp_model)
   
  return(temp_models)
  
}

temp_model <- metro_reg(county_shp, 1)

all_results %<>% bind_rows(temp_model)

temp_model <- metro_reg(county_shp, 0)

all_results %<>% bind_rows(temp_model)

rm(temp_model, metro_reg)

# Bacon Decomposition -------------------------------

# See if results are the same
# Note: Focus on non-metros for now.

# Bacon-Decomposition

df_bacon <- bacon(Median_Household_Income ~ interaction_term,
                  data = subset(county_shp, shale_state == 1 & metro == 0),
                  id_var = "county_fips_code",
                  time_var = "year")

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

# Two-way fixed effects

fit_tw <- lm(Median_Household_Income ~ interaction_term + factor(county_fips_code) + factor(year),
             data = subset(county_shp, shale_state == 1 & metro == 0))

print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))

# Plot weights and estimates

ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point() + theme_classic()

# Note: Much of the variation comes from 2008 and 2010 compared to never-treated. This makes sense, given the timing of
# drilling. Both of these estimates are negative, despite overall positive relationship between drilling and income.

# Bacon-Decomposition (ZHVI SFR) -------------------------------------------

# Impute Zillow SFR

county_shp %<>% mutate(zhvi_sfr_imputed = zhvi_sfr) %>%
  arrange(county_fips_code, year) %>%
  group_by(county_fips_code) %>%
  mutate(zhvi_sfr_imputed = na.approx(zhvi_sfr_imputed, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(zhvi_sfr_missing = ifelse(is.na(zhvi_sfr) == TRUE, 1, 0)) %>%
  group_by(county_fips_code) %>%
  mutate(zhvi_sfr_ever_missing = max(zhvi_sfr_missing))
  
df_bacon <- bacon(zhvi_sfr_imputed ~ interaction_term,
                  data = subset(county_shp, shale_state == 1 & metro == 0 & zhvi_sfr_ever_missing == 0),
                  id_var = "county_fips_code",
                  time_var = "year")

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

# Plot weights and estimates

ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point() + theme_classic()

# Use the Callaway & Sant'Anna approach 
# Notes: Will have to set up to make work our data frame work with the function from the did package.
# Good resources (and source of underlying code): https://causalinf.substack.com/p/callaway-and-santanna-dd-estimator

# Setup

county_shp %<>% mutate(treatment_year = ifelse(interaction_term == 1, year, NA_real_)) %>%
  group_by(county_fips_code) %>%
  mutate(treatment_year = min(treatment_year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(treatment_year = ifelse(is.na(treatment_year) == TRUE | is.infinite(treatment_year) == TRUE, 0, treatment_year))

county_shp %<>% mutate(no_shale = ifelse(shale_county == 0, 1, 0))

county_shp %<>% mutate(county_fips_code_num = as.numeric(county_fips_code))

# Run the model (Median Household Income)

atts <- att_gt(yname = "Median_Household_Income", # LHS variable
               tname = "year", # time variable
               idname = "county_fips_code_num", # id variable
               gname = "treatment_year", # first treatment period variable
               data = county_shp, # data
               xformla = NULL, # no covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "no_shale", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "county_fips_code_num", # cluster level
               panel = TRUE)

# Aggregate ATT

agg_effects <- aggte(atts, type = "group")

summary(agg_effects)

# Group-time ATTs

summary(atts)

# Plot group-time ATTs

ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients

ggdid(agg_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in median household income',
       title = 'Average effect of shale development on median household income',
       subtitle = 'Time-varying effects by length of exposure')  -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_Median_Household_Income_by_Length_of_Exposure')


# Notes: Not perfect pre-trends but not bad either. Very interesting.

# Run the model (Unemployment Rate)

atts <- att_gt(yname = "unemployment_rate", # LHS variable
               tname = "year", # time variable
               idname = "county_fips_code_num", # id variable
               gname = "treatment_year", # first treatment period variable
               data = county_shp, # data
               xformla = NULL, # no covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "no_shale", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "county_fips_code_num", # cluster level
               panel = TRUE)

# Aggregate ATT

agg_effects <- aggte(atts, type = "group")

summary(agg_effects)

# Group-time ATTs

summary(atts)

# Plot group-time ATTs

ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients

ggdid(agg_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in unemployment rate (%)',
       title = 'Average effect of shale development on the unemployment rate',
       subtitle = 'Time-varying effects by length of exposure')  -> unemployment_plot

unemployment_plot

ggsave('shale-varying/Figures/Figure_X_Unemployment_Rate_by_Length_of_Exposure.jpg')

