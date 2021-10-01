# Andrew Boslett
# University of Rochester Medical Center/FLX AI
# Email: andrew_boslett@urmc.rochester.edu
# Permanent email: andrew.boslett@gmail.com

rm(list = ls())

# Goal: Set out a to-do list for an October 2021 sprint. -----------------------

# List of issues -------------------------------------

october_sprint <- data.frame(
  epic = c('Paper-writing', 'Paper-writing', 'Paper-writing', 'Paper-writing',
           'Analysis', 'Analysis', 'Analysis', 'Analysis',
           'Data management'),
  issue = c('Write out conceptual framework', 'Write out clear description of CS (2021) in empirical methods',
            'Outline purpose of paper in introduction section', 'Start building skeleton of results section',
            'Push aggregate estimates into table for export into Excel', 'Build out models with explanatory variables',
            'Set up skeleton to evaluate heterogeneous treatment effects using double-ML', 
            'Push out aggregate estimates by early vs. late treatment counties into Excel',
            'Add early-years worth of OASDI data')
)

# Add progress indicator

october_sprint %<>% mutate(progress = 'To-do')

# Update progress -----------------------------

october_sprint %<>% mutate(progress = case_when(
  str_detect(issue, pattern = '2021|introduction|Excel') == TRUE ~ 'In progress',
  str_detect(issue, pattern = '2021|introduction|Excel') == FALSE ~ progress
))