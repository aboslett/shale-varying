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

