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

# Import data -------------------------------
# Notes: Created from 2-create-analysis-database.R.

# Show trends in development over time by state -------------------------
# Summary statistics -----------------------------
# Notes: Outcome & control variables, by pre vs. post-shale boom, as indicated at the state-level (based on Bartik et al. in AER).

# Figure 1: Dynamics of income & shale development ----------------------------
# Figure 2: Dynamics of employment & shale development ------------------------
# Figure 3: Dynamics of Zillow SFR & shale development ------------------------
# Robustness checks -------------------------------------
# (1) Drop neighboring counties to reduce spillover
# (2) Drop Antrim and New Albany shales
# (3) Change timing to be based on # of wells in a year by state 
# (4) Change timing to be based on # of wells in a year by county
# (5) 
# Other outcomes --------------------------------
# Poverty %
# Social security payments for disabilities
# Rental rates
# Placebo tests ---------------------------
# Shift timing back three years
# Heterogeneity of treatment effects by characteristics -----------------------------
# Notes: There is recent work on the use of causal forests in panel frameworks to understand treatment effect
# heterogeneity. I think we can also use the individual TEs estimated from the methods outlined in Callaway/Sant'Anna
# to extract 
# (1) Income
# (2) Employment
# (3) Zillow SFR
