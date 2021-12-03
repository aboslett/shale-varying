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
  mutate_at(vars(hd_wells, unemployment_rate, Median_Household_Income, 
                 personal_income_per_capita, Poverty_Percent_All_Ages, zhvi_sfr,
                 ends_with('_percent'), population, population_density),
            funs(ifelse(is.nan(.) == TRUE | is.infinite(.) == TRUE, NA_real_, .))) %>%
  group_by(`Post-Boom`, shale_county) %>%
  summarise_at(vars(hd_wells, unemployment_rate, Median_Household_Income, 
                    personal_income_per_capita, Poverty_Percent_All_Ages, zhvi_sfr,
                    ends_with('_percent'), population, population_density),
               funs(mean(., na.rm = TRUE),
                    sd(., na.rm = TRUE))) %>%
  ungroup() %>%
  gather(variable, value, -`Post-Boom`, -shale_county) %>%
  mutate(statistic = ifelse(str_detect(string = variable, pattern = 'mean') == TRUE, 'mean', 'sd'),
         variable = str_replace_all(string = variable, pattern = '\\_(mean|sd)$', replacement = '')) %>%
  dcast(variable ~ shale_county + `Post-Boom` + statistic, value.var = c('value')) %>%
  mutate(variable = case_when(
    variable == 'hd_wells' ~ '# of horizontal wells',
    variable == 'personal_income_per_capita' ~ 'Personal income, per capita',
    variable == 'Median_Household_Income' ~ 'Median household income',
    variable == 'unemployment_rate' ~ 'Unemployment rate',
    variable == 'Poverty_Percent_All_Ages' ~ 'All-ages poverty rate',
    variable == 'zhvi_sfr' ~ 'Zillow ZHVI (SFR)',
    variable == 'population' ~ 'Population',
    variable == 'population_density' ~ 'Population density (population/square miles)',
    variable == 'annual_average_employment_construction_percent' ~ '% of employment in construction',
    variable == 'annual_average_employment_manufacturing_percent' ~ '% of employment in manufacturing',
    variable == 'annual_average_employment_natural_resources_percent' ~ '% of employment in natural resources',
    variable == 'annual_average_employment_service_providing_percent' ~ '% of employment in service-providing sectors'
  )) %>%
  mutate(
    `Normalized Difference, 2000` = (`Non-Shale_Pre-Boom_mean` - `Shale_Pre-Boom_mean`) / 
      sqrt((`Non-Shale_Pre-Boom_sd` ^ 2) + (`Shale_Pre-Boom_sd` ^ 2)),
    `Normalized Difference, 2012` = (`Non-Shale_Post-Boom_mean` - `Shale_Post-Boom_mean`) / 
      sqrt((`Non-Shale_Post-Boom_sd` ^ 2) + (`Shale_Post-Boom_sd` ^ 2)),
  ) %>%
  mutate_at(vars(contains('mean'), contains('sd')),
            funs(round(., 2))) %>%
  dplyr::select(Variable = variable, contains('Pre'), contains('Post'), contains('Norm'),
                -contains('sd')) %>%
  write_excel_csv('shale-varying/Scratch/Results/Table_1_Summary_Statistics.csv')

# Show densities of drilling activity in 2012 --------------------------------

# Generate drilling plot and export

county_shp %<>% mutate(shale_county_character = ifelse(shale_county == 1,
                                                       'Shale', 'Non-Shale'))

temp_plot <- ggplot(data = subset(county_shp, year == 2012 & shale_state == 1),
                    aes(x = log(hd_wells + 1), fill = shale_county_character)) + 
  theme_classic() + 
  labs(x = '# of horizontal wells drilled in 2012',
       y = 'Density') + 
  geom_density(alpha = 0.75) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.8, 0.8))

temp_plot

ggsave('shale-varying/Figures/Figure_X_Drilling_Activity_2012.jpg')

rm(temp_plot)

# Set up analysis database for CS package --------------------------

county_shp %<>% mutate(treatment_year = ifelse(interaction_term == 1, year, NA_real_)) %>%
  group_by(county_fips_code) %>%
  mutate(treatment_year = min(treatment_year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(treatment_year = ifelse(is.na(treatment_year) == TRUE | is.infinite(treatment_year) == TRUE, 0, treatment_year))

# Note: All years are measured in event time relative to the first treatment year for an observation.

county_shp %<>% mutate(no_shale = ifelse(shale_county == 0, 1, 0))

county_shp %<>% mutate(county_fips_code_num = as.numeric(county_fips_code))

# Add alternative timing by state/county 

county_shp %<>% mutate(treatment_year_state = ifelse(hd_wells_cumulative_state_250 == 1, year, NA_real_),
                       treatment_year_county = ifelse(hd_wells_cumulative_10 == 1, year, NA_real_)) %>%
  group_by(county_fips_code) %>%
  mutate(treatment_year_state = min(treatment_year_state, na.rm = TRUE),
         treatment_year_county = min(treatment_year_county, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at(vars(treatment_year_state, treatment_year_county),
            funs(ifelse(is.na(.) == TRUE | is.infinite(.) == TRUE, 0, .)))

# Set up function for tidying results ------------------------------------

tidy.AGGTEobj<- function(x, ...) {
  if(x$type == "dynamic"){
    out <- data.frame(
      type          = x$type,
      term = paste0('ATT(', x$egt, ")"),
      event.time= x$egt,
      estimate  = x$att.egt,
      std.error = x$se.egt,
      conf.low  = x$att.egt - x$crit.val.egt * x$se.egt,
      conf.high = x$att.egt + x$crit.val.egt * x$se.egt,
      point.conf.low  = x$att.egt - stats::qnorm(1 - x$DIDparams$alp/2) * x$se.egt,
      point.conf.high = x$att.egt + stats::qnorm(1 - x$DIDparams$alp/2) * x$se.egt)
  }
  if(x$type == "group"){
    out <- data.frame(
      type     = x$type,
      term = c(paste0('ATT(Average)'), paste0('ATT(', x$egt, ")")),
      group    = c('Average', x$egt),
      estimate  = c(x$overall.att, x$att.egt),
      std.error = c(x$overall.se, x$se.egt),
      conf.low  = c(x$overall.att - stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se, x$att.egt - x$crit.val.egt * x$se.egt),
      conf.high = c(x$overall.att + stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se, x$att.egt + x$crit.val.egt * x$se.egt),
      point.conf.low  = c(x$overall.att - stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se, x$att.egt - stats::qnorm(1 - x$DIDparams$alp/2) * x$se.egt),
      point.conf.high = c(x$overall.att + stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se,x$att.egt + stats::qnorm(1 - x$DIDparams$alp/2) * x$se.egt))
  }
  
  if(x$type == "calendar"){
    out <- data.frame(
      type      = x$type,
      time      = x$egt,
      term = paste0('ATT(', x$egt, ")"),
      estimate  = x$att.egt,
      std.error = x$se.egt,
      conf.low  = x$att.egt - x$crit.val.egt * x$se.egt,
      conf.high = x$att.egt + x$crit.val.egt * x$se.egt,
      point.conf.low  = x$att.egt - stats::qnorm(1 - x$DIDparams$alp/2) * x$se.egt,
      point.conf.high = x$att.egt + stats::qnorm(1 - x$DIDparams$alp/2) * x$se.egt)
  }
  
  if(x$type == "simple"){
    out <- data.frame(
      type      = x$type,
      estimate  = x$overall.att,
      std.error = x$overall.se,
      conf.low  = x$overall.se - stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se,
      conf.high = x$overall.se + stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se,
      point.conf.low  = x$overall.se - stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se,
      point.conf.high = x$overall.se + stats::qnorm(1 - x$DIDparams$alp/2) * x$overall.se)
  }
  
  out
}

# Set up data-frames for all results -----------------------------

simple_aggregation <- data.frame()
simple_aggregation_other_vars <- data.frame()

# Figure 1: Dynamics of income & shale development ---------------------------
# Notes: Per Paredes et al. (2015), the BEA LAPI includes wage and non-wage income, the latter of which
# is relevant to natural gas and oil leasing.

# Run the model (per capita personal income)

atts <- att_gt(yname = "personal_income_per_capita", 
               tname = "year", 
               idname = "county_fips_code_num", 
               gname = "treatment_year", 
               data = county_shp, 
               xformla = NULL, 
               est_method = "dr", 
               control_group = "nevertreated", 
               bstrap = TRUE, 
               biters = 1000, 
               print_details = FALSE, 
               clustervars = "county_fips_code_num", 
               panel = TRUE)

# Simple aggregations of ATT

agg_effects <- aggte(atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'personal_income_per_capita',
         adjusted_model = 'unconditioned') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation %<>% bind_rows(agg_effects)

rm(agg_effects)

# Aggregate ATT

agg_effects <- aggte(atts, type = "group")

summary(agg_effects)

# Set up calendar

agg.ct <- aggte(atts, type = "calendar")
summary(agg.ct)

# Calendar-based plot

ggdid(agg.ct) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Year', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI)',
       subtitle = 'Time-varying effects by length of exposure') + 
  theme(legend.position="none") + 
  scale_color_manual(values = c('dodgerblue')) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Calendar.jpg')

# Event-study graph

agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients

ggdid(agg_effects_es) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI)',
       subtitle = 'Time-varying effects by calendar time') + 
  theme(legend.title = element_blank()) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Length_of_Exposure.jpg')

# Figure 2: Dynamics of employment & shale development ------------------------

employment_atts <- att_gt(yname = "unemployment_rate", 
                           tname = "year", 
                           idname = "county_fips_code_num", 
                           gname = "treatment_year", 
                           data = county_shp, 
                           xformla = NULL, 
                           est_method = "dr", 
                           control_group = 'nevertreated', 
                           bstrap = TRUE, 
                           biters = 1000, 
                           print_details = FALSE, 
                           clustervars = "county_fips_code_num", 
                           panel = TRUE)

# Simple aggregation

agg_effects <- aggte(employment_atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'unemployment_rate',
         adjusted_model = 'unconditioned') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation %<>% bind_rows(agg_effects)

rm(agg_effects)

# Aggregate ATT

employment_effects <- aggte(employment_atts, type = "group")

summary(employment_effects)

# Set up calendar

agg.ct <- aggte(employment_atts, type = "calendar")
summary(agg.ct)

# Calendar-based plot

ggdid(agg.ct) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Year', y = 'Change in unemployment rate (%)',
       title = 'Average effect of shale development on the unemployment rate',
       subtitle = 'Time-varying effects by calendar time') + 
  theme(legend.position="none") + 
  scale_color_manual(values = c('dodgerblue')) -> unemployment_plot

unemployment_plot

ggsave('shale-varying/Figures/Figure_X_Unemployment_Rate_by_Calendar.jpg')

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

# Figure 3: Dynamics of income & shale development ---------------------------
# Notes: Use natural logs.

county_shp %<>% mutate(personal_income_per_capita_ln = log(personal_income_per_capita))

# Run the model (per capita personal income)

atts <- att_gt(yname = "personal_income_per_capita_ln", 
               tname = "year", 
               idname = "county_fips_code_num", 
               gname = "treatment_year", 
               data = county_shp, 
               xformla = NULL, 
               est_method = "dr", 
               control_group = "nevertreated", 
               bstrap = TRUE, 
               biters = 1000, 
               print_details = FALSE, 
               clustervars = "county_fips_code_num", 
               panel = TRUE)

# Simple aggregations of ATT

agg_effects <- aggte(atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'personal_income_per_capita',
         adjusted_model = 'unconditioned, natural logs') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation %<>% bind_rows(agg_effects)

rm(agg_effects)

# Aggregate ATT

agg_effects <- aggte(atts, type = "group")

summary(agg_effects)

# Set up calendar

agg.ct <- aggte(atts, type = "calendar")
summary(agg.ct)

# Calendar-based plot

ggdid(agg.ct) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Year', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI) in natural logs',
       subtitle = 'Time-varying effects by length of exposure') + 
  theme(legend.position="none") + 
  scale_color_manual(values = c('dodgerblue')) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Calendar_ln.jpg')

# Event-study graph

agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients

ggdid(agg_effects_es) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI) in natural logs',
       subtitle = 'Time-varying effects by calendar time') + 
  theme(legend.title = element_blank()) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Length_of_Exposure_ln.jpg')


# Figure 4: Dynamics of income & shale development, conditioned ----------------------------

temp <- county_shp %>% filter(!is.na(population))

# Run the model (per capita personal income)

atts <- att_gt(yname = "personal_income_per_capita", 
               tname = "year", 
               idname = "county_fips_code_num", 
               gname = "treatment_year", 
               data = temp, 
               xformla = ~population + population_density + 
                 annual_average_employment_construction_percent + annual_average_employment_manufacturing_percent + 
                 annual_average_employment_natural_resources_percent + annual_average_employment_service_providing_percent, # plus covariates
               est_method = "dr", 
               control_group = 'nevertreated', 
               bstrap = TRUE, 
               biters = 1000, 
               print_details = FALSE, 
               clustervars = "county_fips_code_num", 
               panel = TRUE)

# Simple aggregations of ATT

agg_effects <- aggte(atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'personal_income_per_capita',
         adjusted_model = 'Conditioned') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation %<>% bind_rows(agg_effects)

rm(agg_effects)

# Aggregate ATT

agg_effects <- aggte(atts, type = "group")

summary(agg_effects)

# Set up calendar

agg.ct <- aggte(atts, type = "calendar")
summary(agg.ct)

# Calendar-based plot

ggdid(agg.ct) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Year', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI)',
       subtitle = 'Time-varying effects by length of exposure, conditoned with control vars.') + 
  theme(legend.position="none") + 
  scale_color_manual(values = c('dodgerblue')) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Calendar_Cdnt.jpg')

# Event-study graph

agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients

ggdid(agg_effects_es) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI)',
       subtitle = 'Time-varying effects by calendar time, conditioned with control vars.') + 
  theme(legend.title = element_blank()) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Length_of_Exposure_Cdnt.jpg')

# Figure 5: Dynamics of employment & shale development, conditioned ------------------------

# Only those with population data

temp <- county_shp %>% filter(!is.na(population))

# Run model

employment_atts <- att_gt(yname = "unemployment_rate", 
                          tname = "year", 
                          idname = "county_fips_code_num", 
                          gname = "treatment_year", 
                          data = temp, 
                          xformla = ~population + population_density + 
                            annual_average_employment_construction_percent + annual_average_employment_manufacturing_percent + 
                            annual_average_employment_natural_resources_percent + annual_average_employment_service_providing_percent, # plus covariates
                          est_method = "dr", 
                          control_group = 'nevertreated', 
                          bstrap = TRUE, 
                          biters = 1000, 
                          print_details = FALSE, 
                          clustervars = "county_fips_code_num", 
                          panel = TRUE)

# Simple aggregation

agg_effects <- aggte(employment_atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'unemployment_rate',
         adjusted_model = 'Conditioned') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation %<>% bind_rows(agg_effects)

rm(agg_effects)

# Aggregate ATT

employment_effects <- aggte(employment_atts, type = "group")

summary(employment_effects)

# Set up calendar

agg.ct <- aggte(employment_atts, type = "calendar")
summary(agg.ct)

# Calendar-based plot

ggdid(agg.ct) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Year', y = 'Change in unemployment rate (%)',
       title = 'Average effect of shale development on the unemployment rate',
       subtitle = 'Time-varying effects by calendar time, conditioned with control variables') + 
  theme(legend.position="none") + 
  scale_color_manual(values = c('dodgerblue')) -> unemployment_plot

unemployment_plot

ggsave('shale-varying/Figures/Figure_X_Unemployment_Rate_by_Calendar_Cdnt.jpg')

# Set up event study

employment_effects_es <- aggte(employment_atts, type = "dynamic")

# Plot event-study coefficients

ggdid(employment_effects_es) + 
  theme(legend.title = element_blank()) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in unemployment rate (%)',
       title = 'Average effect of shale development on the unemployment rate',
       subtitle = 'Time-varying effects by length of exposure, conditioned with control variables')  -> unemployment_plot

unemployment_plot

ggsave('shale-varying/Figures/Figure_X_Unemployment_Rate_by_Length_of_Exposure_Cdnt.jpg')

# Figure 6: Dynamics of income & shale development ---------------------------
# Notes: Use natural logs.

temp <- county_shp %>% filter(!is.na(population))

# Run the model (per capita personal income)

atts <- att_gt(yname = "personal_income_per_capita_ln", 
               tname = "year", 
               idname = "county_fips_code_num", 
               gname = "treatment_year", 
               data = temp, 
               xformla = ~population + population_density + 
                 annual_average_employment_construction_percent + annual_average_employment_manufacturing_percent + 
                 annual_average_employment_natural_resources_percent + annual_average_employment_service_providing_percent, # plus covariates
               est_method = "dr", 
               control_group = "nevertreated", 
               bstrap = TRUE, 
               biters = 1000, 
               print_details = FALSE, 
               clustervars = "county_fips_code_num", 
               panel = TRUE)

# Simple aggregations of ATT

agg_effects <- aggte(atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'personal_income_per_capita',
         adjusted_model = 'conditioned, natural logs') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation %<>% bind_rows(agg_effects)

rm(agg_effects)

# Aggregate ATT

agg_effects <- aggte(atts, type = "group")

summary(agg_effects)

# Set up calendar

agg.ct <- aggte(atts, type = "calendar")
summary(agg.ct)

# Calendar-based plot

ggdid(agg.ct) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Year', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI) in natural logs',
       subtitle = 'Time-varying effects by length of exposure') + 
  theme(legend.position="none") + 
  scale_color_manual(values = c('dodgerblue')) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Calendar_ln_c.jpg')

# Event-study graph

agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients

ggdid(agg_effects_es) + 
  geom_hline(color = 'grey70', yintercept = 0) + 
  theme_classic() + 
  labs(x = 'Years before/after treatment', y = 'Change in per capita income (LAPI)',
       title = 'Average effect of shale development on per capita income (LAPI) in natural logs',
       subtitle = 'Time-varying effects by calendar time') + 
  theme(legend.title = element_blank()) -> income_plot

income_plot

ggsave('shale-varying/Figures/Figure_X_LAPI_Income_by_Length_of_Exposure_ln_c.jpg')

# Robustness checks -------------------------------------
# (1) Drop neighboring counties to reduce spillover --------------------------

temp <- county_shp %>% filter(shale_border_county == 0)

for(variable in c('personal_income_per_capita', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'personal_income_per_capita', 'Change in per capita income (LAPI)', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'personal_income_per_capita', 'Average effect of shale development on per capita income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'personal_income_per_capita', 
                        'Figure_X_LAPI_Income_by_Length_of_Exposure_NoSpl',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_NoSpl')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year", 
                 data = temp, 
                 xformla = NULL, 
                 est_method = "dr", 
                 control_group = 'nevertreated', 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Robustness check table (simple aggregation)
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = y_label,
           robustness = 'Spillover',
           adjusted_model = 'unconditioned') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
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

for(variable in c('personal_income_per_capita', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'personal_income_per_capita', 'Change in per capita income (LAPI)', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'personal_income_per_capita', 'Average effect of shale development on per capita income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'personal_income_per_capita', 
                        'Figure_X_LAPI_Income_by_Length_of_Exposure_Rstrd',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_Rstrd')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year", 
                 data = temp, 
                 xformla = NULL, 
                 est_method = "dr", 
                 control_group = 'nevertreated', 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Robustness check table (simple aggregation)
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = y_label,
           robustness = 'Drop Antrim + New Albany',
           adjusted_model = 'unconditioned') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
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

# (3) Rural counties only --------------------------------

temp <- county_shp %>% filter(str_detect(string = description, pattern = 'on[:punct:]*metro'))

for(variable in c('personal_income_per_capita', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'personal_income_per_capita', 'Change in per capita income (LAPI)', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'personal_income_per_capita', 'Average effect of shale development on per capita income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'personal_income_per_capita', 
                        'Figure_X_LAPI_Income_by_Length_of_Exposure_Rural',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_Rural')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year", 
                 data = temp, 
                 xformla = NULL, 
                 est_method = "dr", 
                 control_group = 'nevertreated', 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Robustness check table (simple aggregation)
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = y_label,
           robustness = 'Only Rural Counties',
           adjusted_model = 'unconditioned') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
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

# (4) Use of both never-treated + not-yet-treated -------------------------
# Note: See Callaway and Sant'Anna (2020). So far, we have used both never-treated and not-yet-treated
# observations as controls (in line with Assumptions 4+5 of their paper, as I under stand them). In these runs,
# we only use never-treated observations (see Assumption 4 - essentially, we are assuming that treated observations
# and never-treated observations would follow the same relative path in outcomes, conditioned on control variables,
# in the absence of treatment.

temp <- county_shp %>% filter(shale_border_county == 0)

for(variable in c('personal_income_per_capita', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'personal_income_per_capita', 'Change in per capita income (LAPI)', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'personal_income_per_capita', 'Average effect of shale development on per capita income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'personal_income_per_capita', 
                        'Figure_X_LAPI_Income_by_Length_of_Exposure_NevTr',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_NevTr')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year", 
                 data = temp, 
                 xformla = NULL, 
                 est_method = "dr", 
                 control_group = 'notyettreated', 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Robustness check table (simple aggregation)
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = y_label,
           robustness = 'Controls = Never Treated + Not-yet-Treated',
           adjusted_model = 'unconditioned') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
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

# (5) Alt. initial dates: Change timing to be based on # of wells in a year by state ---------------------------

for(variable in c('personal_income_per_capita', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'personal_income_per_capita', 'Change in per capita income (LAPI)', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'personal_income_per_capita', 'Average effect of shale development on per capita income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'personal_income_per_capita', 
                        'Figure_X_LAPI_Income_by_Length_of_Exposure_AltTS',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_AltTS')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year_state", 
                 data = county_shp, 
                 xformla = NULL, 
                 est_method = "dr", 
                 control_group = 'nevertreated', 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Robustness check table (simple aggregation)
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = y_label,
           robustness = 'Alt. Timing, State',
           adjusted_model = 'unconditioned') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
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

# (6) Alt. initial dates: Change timing to be based on # of wells in a year by county --------------------------

for(variable in c('personal_income_per_capita', 'unemployment_rate')) {
  
  # Get other local variables for naming in figures
  
  y_label <- ifelse(variable == 'personal_income_per_capita', 'Change in per capita income (LAPI)', 'Change in unemployment rate (%)')
  temp_title <- ifelse(variable == 'personal_income_per_capita', 'Average effect of shale development on per capita income',
                       'Average effect of shale development on the unemployment rate')
  figure_name <- ifelse(variable == 'personal_income_per_capita', 
                        'Figure_X_LAPI_Income_by_Length_of_Exposure_AltTC',
                        'Figure_X_Unemployment_Rate_by_Length_of_Exposure_AltTC')
  
  # Run the model and export results
  
  atts <- att_gt(yname = variable, 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year_county", 
                 data = county_shp, 
                 xformla = NULL, 
                 est_method = "dr", 
                 control_group = 'nevertreated', 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Robustness check table (simple aggregation)
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = y_label,
           robustness = 'Alt. Timing, County',
           adjusted_model = 'unconditioned') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
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

# Other outcomes --------------------------------
# (1) Poverty % ------------------------------

poverty_atts <- att_gt(yname = "Poverty_Percent_All_Ages", 
                          tname = "year", 
                          idname = "county_fips_code_num", 
                          gname = "treatment_year", 
                          data = county_shp, 
                          xformla = NULL, 
                          est_method = "dr", 
                          control_group = 'nevertreated', 
                          bstrap = TRUE, 
                          biters = 1000, 
                          print_details = FALSE, 
                          clustervars = "county_fips_code_num", 
                          panel = TRUE)

# Robustness check table (simple aggregation)

agg_effects <- aggte(poverty_atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'Poverty_Percent_All_Ages') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation_other_vars %<>% bind_rows(agg_effects)

rm(agg_effects)

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

disability_atts <- att_gt(yname = "disability_income_imputed_log", 
                       tname = "year", 
                       idname = "county_fips_code_num", 
                       gname = "treatment_year", 
                       data = temp, 
                       xformla = NULL, 
                       est_method = "dr", 
                       control_group = 'nevertreated', 
                       bstrap = TRUE, 
                       biters = 1000, 
                       print_details = FALSE, 
                       clustervars = "county_fips_code_num", 
                       panel = TRUE)

# Robustness check table (simple aggregation)

agg_effects <- aggte(disability_atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'disability_income_imputed_log') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation_other_vars %<>% bind_rows(agg_effects)

rm(agg_effects)

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
  
  atts <- att_gt(yname = "value", 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year", 
                 data = temp, 
                 xformla = NULL, 
                 est_method = "dr", 
                 control_group = 'nevertreated', 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Robustness check table (simple aggregation)
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = 'Percent of employment in ', industry, ' sector') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation_other_vars %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
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

atts <- att_gt(yname = "Median_Household_Income", 
               tname = "year", 
               idname = "county_fips_code_num", 
               gname = "treatment_year", 
               data = county_shp, 
               xformla = NULL, 
               est_method = "dr", 
               control_group = 'nevertreated', 
               bstrap = TRUE, 
               biters = 1000, 
               print_details = FALSE, 
               clustervars = "county_fips_code_num", 
               panel = TRUE)

# Robustness check table (simple aggregation)

agg_effects <- aggte(atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'Median Household Income (SAIPE)') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation_other_vars %<>% bind_rows(agg_effects)

rm(agg_effects)

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

# (5) Zillow SFR --------------------------------------

zhvi_atts <- att_gt(yname = "zhvi_sfr_log", 
                    tname = "year", 
                    idname = "county_fips_code_num", 
                    gname = "treatment_year", 
                    data = county_shp, 
                    xformla = NULL, 
                    est_method = "dr", 
                    control_group = 'nevertreated', 
                    bstrap = TRUE, 
                    biters = 1000, 
                    print_details = FALSE, 
                    clustervars = "county_fips_code_num", 
                    panel = TRUE)

# Robustness check table (simple aggregation)

agg_effects <- aggte(zhvi_atts, type = 'simple')

summary(agg_effects)

agg_effects %<>% tidy.AGGTEobj() %>%
  dplyr::select(estimate, std.error) %>%
  mutate(outcome_variable = 'ZHVI for single-family residences') %>%
  mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
         confidence_interval_high_95 = estimate + (1.96 * std.error))

simple_aggregation_other_vars %<>% bind_rows(agg_effects)

rm(agg_effects)

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

# (6) Levels of employment & establishments by sector --------------------------

for(variable_name in c('annual_average_employment_all', 'annual_average_employment_construction', 
                       'annual_average_employment_manufacturing', 'annual_average_employment_natural_resources',
                       'annual_average_employment_service_providing')) {
  
  
  # Filter out counties with population data
  
  temp <- county_shp %>% filter(!is.na(population))
  
  # Run the model (per capita personal income)
  
  atts <- att_gt(yname = variable_name, 
                 tname = "year", 
                 idname = "county_fips_code_num", 
                 gname = "treatment_year", 
                 data = temp, 
                 xformla = ~population + population_density, # plus covariates
                 est_method = "dr", 
                 control_group = "nevertreated", 
                 bstrap = TRUE, 
                 biters = 1000, 
                 print_details = FALSE, 
                 clustervars = "county_fips_code_num", 
                 panel = TRUE)
  
  # Simple aggregations of ATT
  
  agg_effects <- aggte(atts, type = 'simple')
  
  summary(agg_effects)
  
  agg_effects %<>% tidy.AGGTEobj() %>%
    dplyr::select(estimate, std.error) %>%
    mutate(outcome_variable = variable_name,
           adjusted_model = 'conditioned') %>%
    mutate(confidence_interval_low_95 = estimate - (1.96 * std.error),
           confidence_interval_high_95 = estimate + (1.96 * std.error))
  
  simple_aggregation %<>% bind_rows(agg_effects)
  
  rm(agg_effects)
  
  # Aggregate ATT
  
  agg_effects <- aggte(atts, type = "group")
  
  summary(agg_effects)
  
  # Set up calendar
  
  agg.ct <- aggte(atts, type = "calendar")
  summary(agg.ct)
  
  # Calendar-based plot
  
  ggg <- variable_name %>% str_replace_all(pattern = '\\_', replacement = ' ') %>%
    str_to_title()
  
  ggdid(agg.ct) + 
    geom_hline(color = 'grey70', yintercept = 0) + 
    theme_classic() + 
    labs(x = 'Year', y = ggg,) + 
    theme(legend.position="none") + 
    scale_color_manual(values = c('dodgerblue')) -> income_plot
  
  income_plot
  
  ggsave(paste0('shale-varying/Figures/Figure_X_', variable_name, '_by_C_c.jpg'))
  
  # Event-study graph
  
  agg_effects_es <- aggte(atts, type = "dynamic")
  summary(agg_effects_es)
  
  # Plot event-study coefficients
  
  ggdid(agg_effects_es) + 
    geom_hline(color = 'grey70', yintercept = 0) + 
    theme_classic() + 
    labs(x = 'Years before/after treatment', y = ggg) + 
    theme(legend.title = element_blank()) -> income_plot
  
  income_plot
  
  ggsave(paste0('shale-varying/Figures/Figure_X_', variable_name, '_by_L_Exposure_c.jpg'))
  
}


# Extra analyses -----------------------------
# Proportion of wells in non-metro counties

county_shp %>% filter(year >= 2000 & year <= 2017) %>%
  summarise(hd_wells = sum(hd_wells, na.rm = TRUE)) %>% pull() -> all_wells

county_shp %>% filter(year >= 2000 & year <= 2017) %>%
  filter(metro == 0) %>%
  summarise(hd_wells = sum(hd_wells, na.rm = TRUE)) %>% pull() -> rural_wells

rural_wells / all_wells


