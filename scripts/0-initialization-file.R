# Andrew Boslett
# University of Rochester Medical Center/FLX AI
# Email: andrew_boslett@urmc.rochester.edu
# Permanent email: andrew.boslett@gmail.com

# Set options

options(scipen = 999)

# Load packages
# Note: I believe that all of these packages are used in this code (or were used in various iterations of it, some
# of which may be deprecated).

library(RCurl)
library(ggalt)
library(tidyverse)
library(foreign)
library(readr)
library(readxl)
library(data.table)
library(statar)
library(sp)
library(rgdal)
library(stringr)
library(fuzzyjoin)
library(rlang)
library(dplyr)
library(magrittr)
library(randomForest)
library(gbm)
library(boot)
library(caret)
library(xgboost)
library(broom)
library(ggrepel)
library(glmnet)
library(zoo)
library(maps)
library(tidycensus)
library(vcd)
library(fuzzyjoin)
library(gganimate)
library(gifski)

# One directories of interest here:
# (1) My personal directory, which connects my Box folder on my external hard-drive.

# Set directory
# Note: I set my working directory to the root of my research folder (which is synced in Box due to
# organizational preferences). 

setwd("C:/Users/aboslett/Box")

# Set folder structure in your research directory.
# Note: I create a project folder for data, scratch (e.g., intermediate files), and folders
# for documents and figures created in support of the paper.

dir.create('shale-varying', showWarnings = TRUE)

for(fff in c('Data', 'Scratch', 'Documents', 'Figures', 'Tables')) {
  
  dir.create(paste0('shale-varying/', fff), showWarnings = TRUE)
  
}
