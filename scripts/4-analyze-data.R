# Andrew Boslett
# University of Rochester Medical Center/FLX AI
# Email: andrew_boslett@urmc.rochester.edu
# Permanent email: andrew.boslett@gmail.com

rm(list = ls())

# Goal: Set up and execute an analysis plan for this work.
# Notes: I have done some preliminary exploration of the models in 2-explore-data.R. But here, I put together an
# analysis plan that focuses on heterogenous treatment effects of shale development on employment and income over time
# and by characteristics.

# Notes ------------------------------
# Years of data: 2000-2018
# Observation set: All counties in states with shale coverage
# Treatment variables:
# (1) Shale county: Binary variable indicating presence of shale underneath county
# (2) Post-drilling: State-level binary variable indicating whether hydraulic fracturing-related drilling has occurred by given year

# Create folder in Scratch folder for results ----------------------------

dir.create('shale-varying/Scratch/Results', showWarnings = TRUE)

# Import data -------------------------------
# Notes: Created from 2-create-analysis-database.R.

county_shp <- readRDS('shale-varying/Scratch/Analysis_Database.rds')

# Show trends in development over time by state -------------------------

# By shale play

for(fff in unique(county_shp$shale_play)) {
  
  # Calculate # of wells by year
  
  temp <- county_shp %>% filter(shale_play == fff) %>%
    group_by(year) %>%
    summarise_at(vars(D, H, hd_wells),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  # Grab first year of development (Bartik)
  
  first_year <- county_shp %>% filter(shale_play == fff) %>% dplyr::select(first_frac_year) %>% unique() %>% pull()
  
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

# Sample of major shale plays

temp <- county_shp %>% filter(shale_play %in% c('Marcellus', 'Barnett', 'Bakken', 'Eagle Ford', 'Fayetteville')) %>%
  group_by(shale_play, year) %>%
  summarise_at(vars(D, H, hd_wells),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

first_year <- county_shp %>% dplyr::select(shale_play, first_frac_year) %>% unique() %>%
  filter(shale_play %in% temp$shale_play) %>% mutate(indicator = 1)

temp_first <- temp %>% left_join(first_year, by = c('shale_play', 'year' = 'first_frac_year')) %>%
  filter(!is.na(indicator))

temp_plot <- ggplot() + 
  theme_classic() + labs(x = 'Year', y = '# of horizontal/directional wells', title = 'Annual shale development in the U.S.',
                         subtitle = 'A sample of major shale plays, 2000-2018') + 
  geom_line(data = temp,
            aes(x = year, y = hd_wells, colour = shale_play)) + 
  geom_point(data = temp,
             aes(x = year, y = hd_wells, colour = shale_play)) + 
  geom_point(data = temp_first, aes(x = year, y = hd_wells, colour = shale_play),
             size = 4, shape = 19) + 
  theme(legend.title = element_blank()) + theme(legend.position = c(0.1, 0.8)) + 
  scale_color_manual(values = c(c('#543005', '#bf812d', '#c7eae5', '#35978f', '#003c30')))

temp_plot

ggsave('shale-varying/Figures/Figure_X_Shale_Development_by_Play.jpg')

# Summary statistics -----------------------------
# Notes: Outcome & control variables, by pre vs. post-shale boom and shale presence indicator, 
# with timing based at the state-level (based on Bartik et al. in AER). We choose 2012 as the post-shale year
# because it's the first year when all shale booms have turned on.

county_shp %>% filter(shale_state == 1) %>%
  filter(year %in% c(2000, 2012)) %>%
  mutate(`Post-Boom` = ifelse(year == 2000, 'Pre-Boom', 'Post-Boom'),
         shale_county = ifelse(shale_county == 1, 'Shale', 'Non-Shale')) %>%
  group_by(`Post-Boom`, shale_county) %>%
  summarise_at(vars(hd_wells, unemployment_rate, Median_Household_Income, Poverty_Percent_All_Ages, zhvi_sfr,
                    contains('labor_share')),
               funs(mean(., na.rm = TRUE),
                    sd(., na.rm = TRUE))) %>%
  ungroup() %>%
  gather(variable, value, -`Post-Boom`, -shale_county) %>%
  mutate(statistic = ifelse(str_detect(string = variable, pattern = 'mean') == TRUE, 'mean', 'sd'),
         variable = str_replace_all(string = variable, pattern = '\\_(mean|sd)$', replacement = '')) %>%
  dcast(variable ~ shale_county + `Post-Boom` + statistic, value.var = c('value')) %>%
  mutate_at(vars(contains('mean'), contains('sd')),
            funs(round(., 2))) %>%
  mutate(variable = case_when(
    variable == 'hd_wells' ~ '# of horizontal wells',
    variable == 'Median_Household_Income' ~ 'Median household income',
    variable == 'unemployment_rate' ~ 'Unemployment rate',
    variable == 'Poverty_Percent_All_Ages' ~ 'All-ages poverty rate',
    variable == 'zhvi_sfr' ~ 'Zillow ZHVI (SFR)'
  )) %>%
  dplyr::select(Variable = variable, contains('Pre'), contains('Post')) %>%
  write_excel_csv('shale-varying/Scratch/Results/Table_1_Summary_Statistics.csv')


# Set up analysis database for CS package --------------------------

county_shp %<>% mutate(treatment_year = ifelse(interaction_term == 1, year, NA_real_)) %>%
  group_by(county_fips_code) %>%
  mutate(treatment_year = min(treatment_year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(treatment_year = ifelse(is.na(treatment_year) == TRUE | is.infinite(treatment_year) == TRUE, 0, treatment_year))

# Note: All years are measured in event time relative to the first treatment year for an observation.

county_shp %<>% mutate(no_shale = ifelse(shale_county == 0, 1, 0))

county_shp %<>% mutate(county_fips_code_num = as.numeric(county_fips_code))

# Figure 1: Dynamics of income & shale development ---------------------------
# Notes: Per Paredes et al. (2015), the BEA LAPI includes wage and non-wage income, the latter of which
# is relevant to natural gas and oil leasing.

# Run the model (Median Household Income)

atts <- att_gt(yname = "personal_income_per_capita", # LHS variable
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

# Event-study graph

agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients

ggdid(agg_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI)',
       subtitle = 'Time-varying effects by length of exposure')  -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Length_of_Exposure.jpg')


# Figure 2: Dynamics of employment & shale development ------------------------

employment_atts <- att_gt(yname = "unemployment_rate", # LHS variable
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

employment_effects <- aggte(employment_atts, type = "group")

summary(employment_effects)

# Set up event study

employment_effects_es <- aggte(employment_atts, type = "dynamic")

# Plot event-study coefficients

ggdid(employment_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in unemployment rate (%)',
       title = 'Average effect of shale development on the unemployment rate',
       subtitle = 'Time-varying effects by length of exposure')  -> unemployment_plot

unemployment_plot

ggsave('shale-varying/Figures/Figure_X_Unemployment_Rate_by_Length_of_Exposure.jpg')

# Notes: Huge declines in unemployment, post-shale. Pre-trends aren't a concern. The effect seems persistent,
# though we have to remember that the 10-years post-treatment are essentially Barnett and Permian Basin, the latter of which really exploded
# in that time period.

# Figure 3: Dynamics of Zillow SFR & shale development ------------------------

zhvi_atts <- att_gt(yname = "zhvi_sfr_log", # LHS variable
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

zhvi_effects <- aggte(zhvi_atts, type = "group")

summary(zhvi_effects)

# Set up event study

zhvi_effects_es <- aggte(zhvi_atts, type = "dynamic")

# Plot event-study coefficients

ggdid(zhvi_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in ZHVI (SFR)',
       title = 'Average effect of shale development on the ZHVI (SFR)',
       subtitle = 'Time-varying effects by length of exposure')  -> zhvi_plot

zhvi_plot

ggsave('shale-varying/Figures/Figure_X_ZHVI_SFR_by_Length_of_Exposure.jpg')

# Notes: Major pre-trends.

# Robustness checks -------------------------------------
# (1) Drop neighboring counties to reduce spillover --------------------------

temp <- county_shp %>% filter(shale_border_county == 0)

for(variable in c('Median_Household_Income', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'Median_Household_Income', 'Change in median household income', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'Median_Household_Income', 'Average effect of shale development on median household income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'Median_Household_Income', 
                        'Figure_X_Median_Household_Income_by_Length_of_Exposure_NoSpl',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_NoSpl')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, # LHS variable
                 tname = "year", # time variable
                 idname = "county_fips_code_num", # id variable
                 gname = "treatment_year", # first treatment period variable
                 data = temp, # data
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
  
  # Event-study graph
  
  agg_effects_es <- aggte(atts, type = "dynamic")
  summary(agg_effects_es)
  
  # Plot event-study coefficients
  
  ggdid(agg_effects_es) + 
    theme(legend.title = element_blank()) + 
    geom_hline(color = 'grey70', yintercept = 0) + 
    theme_classic() + 
    labs(x = 'Years before/after treatment', y = y_label,
         title = temp_title,
         subtitle = 'Time-varying effects by length of exposure')  -> income_plot
  
  income_plot
  
  ggsave(paste0('shale-varying/Figures/', figure_name, '.jpg'))
  
}

# (2) Drop Antrim and New Albany -----------------------------

temp <- county_shp %>% filter(!shale_play %in% c('New Albany', 'Antrim'))

for(variable in c('Median_Household_Income', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'Median_Household_Income', 'Change in median household income', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'Median_Household_Income', 'Average effect of shale development on median household income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'Median_Household_Income', 
                        'Figure_X_Median_Household_Income_by_Length_of_Exposure_Rstrd',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_Rstrd')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, # LHS variable
                 tname = "year", # time variable
                 idname = "county_fips_code_num", # id variable
                 gname = "treatment_year", # first treatment period variable
                 data = temp, # data
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
  
  # Event-study graph
  
  agg_effects_es <- aggte(atts, type = "dynamic")
  summary(agg_effects_es)
  
  # Plot event-study coefficients
  
  ggdid(agg_effects_es) + 
    theme(legend.title = element_blank()) + 
    geom_hline(color = 'grey70', yintercept = 0) + 
    theme_classic() + 
    labs(x = 'Years before/after treatment', y = y_label,
         title = temp_title,
         subtitle = 'Time-varying effects by length of exposure')  -> income_plot
  
  income_plot
  
  ggsave(paste0('shale-varying/Figures/', figure_name, '.jpg'))
  
}

# (3) Change timing to be based on # of wells in a year by state 
# (4) Change timing to be based on # of wells in a year by county
# (5) 
# Other outcomes --------------------------------
# (1) Poverty % ------------------------------

poverty_atts <- att_gt(yname = "Poverty_Percent_All_Ages", # LHS variable
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

poverty_effects <- aggte(poverty_atts, type = "group")

summary(poverty_effects)

# Set up event study

poverty_effects_es <- aggte(poverty_atts, type = "dynamic")

# Plot event-study coefficients

ggdid(poverty_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in all-ages povery rate (%)',
       title = 'Average effect of shale development on the poverty rate (all-ages)',
       subtitle = 'Time-varying effects by length of exposure')  -> poverty_plot

unemployment_plot

ggsave('shale-varying/Figures/Figure_X_Poverty_Rate_by_Length_of_Exposure.jpg')

# (2) Social security payments for disabilities ----------------------------------

# Keep only 2004-2017

temp <- county_shp %>% filter(year >= 2004 & year <= 2017) %>%
  filter(!is.na(disability_income_imputed_log) & !is.infinite(disability_income_imputed_log))

disability_atts <- att_gt(yname = "disability_income_imputed_log", # LHS variable
                       tname = "year", # time variable
                       idname = "county_fips_code_num", # id variable
                       gname = "treatment_year", # first treatment period variable
                       data = temp, # data
                       xformla = NULL, # no covariates
                       est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                       control_group = "no_shale", # set the comparison group which is either "nevertreated" or "notyettreated" 
                       bstrap = TRUE, # if TRUE compute bootstrapped SE
                       biters = 1000, # number of bootstrap iterations
                       print_details = FALSE, # if TRUE, print detailed results
                       clustervars = "county_fips_code_num", # cluster level
                       panel = TRUE)

# Aggregate ATT

disability_effects <- aggte(disability_atts, type = "group")

summary(disability_effects)

# Set up event study

disability_effects_es <- aggte(disability_atts, type = "dynamic")

# Plot event-study coefficients

ggdid(disability_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in disability payments (in logs)',
       title = 'Average effect of shale development on level of disability payments to workers',
       subtitle = 'Time-varying effects by length of exposure')  -> disability_plot

disability_plot

ggsave('shale-varying/Figures/Figure_X_Disability_Payments_by_Length_of_Exposure.jpg')

# (3) Income/Employment in non-mining sectors ----------------------------------
# Notes: Here we are thinking about long-term resource "curse"-related work.

# Employment shares by industry

for(industry in c('construction', 'manufacturing', 'natural_resources', 'service_providing')) {
  
  # Make data long and filter on chosen variable
  
  temp <- county_shp %>% gather(variable, value, -county_fips_code, -county_fips_code_num, -year, -treatment_year, -no_shale) %>%
    filter(variable == paste0("annual_average_employment_", industry, "_percent")) %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value) & !is.nan(value) & !is.infinite(value))
  
  # Run model
  
  atts <- att_gt(yname = "value", # LHS variable
                 tname = "year", # time variable
                 idname = "county_fips_code_num", # id variable
                 gname = "treatment_year", # first treatment period variable
                 data = temp, # data
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
  
  # Event-study graph
  
  agg_effects_es <- aggte(atts, type = "dynamic")
  summary(agg_effects_es)
  
  # Plot event-study coefficients
  
  ggdid(agg_effects_es) + 
    theme(legend.title = element_blank()) + 
    geom_hline(color = 'grey70', yintercept = 0) + 
    theme_classic() + 
    labs(x = 'Years before/after treatment', y = paste0('% of employment: ', str_to_title(industry)),
         title = paste0('Average effect of shale development on % of employment in ', str_to_title(industry)),
         subtitle = 'Time-varying effects by length of exposure')  -> temp_plot
  
  temp_plot
  
  ggsave(paste0('shale-varying/Figures/Figure_X_', 'Employment_in_Industry_', str_to_title(industry), '_by_Length_of_Exposure.jpg'))
  
  print(industry)
  
  # Remove files
  
  rm(temp_plot, industry, agg_effects_es, atts, temp)
  
}

# (4) Alternative measure of income from SAIPE -------------------------
# Notes: Instead of the per capital personal income from BEA (which includes wage and non-wage income),
# we here use median household income from the SAIPE

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

# Event-study graph

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

ggsave('shale-varying/Figures/Figure_X_Median_Household_Income_by_Length_of_Exposure.jpg')

# Heterogeneity of treatment effects by characteristics -----------------------------
# Notes: There is recent work on the use of causal forests in panel frameworks to understand treatment effect
# heterogeneity. I think we can also use the individual TEs estimated from the methods outlined in Callaway/Sant'Anna
# to extract 
# (1) Income
# (2) Employment
# (3) Zillow SFR
