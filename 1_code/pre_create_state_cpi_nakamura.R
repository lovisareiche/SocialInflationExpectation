# Purpose: Create time series of US commuting zones inflation data

# Inputs: 
#     statecpi_beta.csv
#     fips_state.xlsx
#     geo_match.csv

# Outputs: 
#     cpi_cz2000-timeseries_nakamura.csv

# Date: 04/08/2022
# Steps:
#     1. State inflation time series
#     2. Match commuting zones to inflation

# This code makes use of the Nakamura data on US states inflation
# Hazell, J., J. Herre  ̃no, E. Nakamura, and J. Steinsson (2021): “The Slope of the Phillips Curve:
# Evidence from U.S. States,” Quarterly Journal of Economics, forthcoming
# https://eml.berkeley.edu/~enakamura/papers/StateCPIData_readme.pdf


rm(list=ls())
NAME <- 'pre_create_state_cpi_nakamura' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'SocialInflationExpectations'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!


## -------
## Imports
## -------
### All the library imports go here

library(tidyverse)
library(lubridate)
library(readxl)

## --------
## Settings
## --------
### Any settings go here


setwd(file.path(PROJECT_DIR, PROJECT))


#### FILL IN THIS LINE BEFORE RUNNING ####

# Geographic IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.geo <- file.path('empirical', '0_data', 'external','US','geo_match.csv')

# State IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.stateid <- file.path('empirical', '0_data', 'external','US','fips_state.xlsx')

# Regional CPI data
# https://eml.berkeley.edu/~enakamura/papers/statecpi_beta.csv
dir.cpi <- file.path('empirical', '0_data', 'external','US','statecpi_beta.csv')



## ----------------------------------
## Set  up pipeline folder if missing
## ----------------------------------
### The code below will automatically create a pipeline folder for this code file if it does not exist.

if (dir.exists(file.path('empirical', '2_pipeline'))){
  pipeline <- file.path('empirical', '2_pipeline', NAME)
} else {
  pipeline <- file.path('2_pipeline', NAME)
}

if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
    dir.create(file.path(pipeline, folder))
  }
}


#########################################
##### 1. State inflation timeseries #####
#########################################


dat <- data.frame(lapply(read_csv(dir.cpi),rep,rep(3,4699)))
  
# ridiculously inefficient piece to add month  
  for (i in 1:length(dat$quarter))
  {
    if (i ==1)
    {
      dat$month[i] = 01
    }
    else if (i ==2)
    {
      dat$month[i] = 02
    }
    else if (i ==3)
    {
      dat$month[i] = 03
    }
    else if (i ==4)
    {
      dat$month[i] = 04
    }
    else if (i ==5)
    {
      dat$month[i] = 05
    }
    else if (i ==6)
    {
      dat$month[i] = 06
    }
    else if (i ==7)
    {
      dat$month[i] = 07
    }
    else if (i ==8)
    {
      dat$month[i] = 08
    }
    else if (i ==9)
    {
      dat$month[i] = 09
    }
    else if (i ==10)
    {
      dat$month[i] = 10
    }
    else if (i ==11)
    {
      dat$month[i] = 11
    }
    else if (i ==12)
    {
      dat$month[i] = 12
    }
    
    else if (dat$quarter[i] ==1 & dat$year[i] != dat$year[i-1])
    {
      dat$month[i] = 01
    }
    else if (dat$quarter[i] ==1 & dat$year[i] != dat$year[i-2])
    {
      dat$month[i] = 02
    }
    else if (dat$quarter[i] ==1 & dat$year[i] != dat$year[i-3])
    {
      dat$month[i] = 03
    }
    else if (dat$quarter[i] ==2 & dat$year[i] != dat$year[i-4])
    {
      dat$month[i] = 04
    }
    else if (dat$quarter[i] ==2 & dat$year[i] != dat$year[i-5])
    {
      dat$month[i] = 05
    }
    else if (dat$quarter[i] ==2 & dat$year[i] != dat$year[i-6])
    {
      dat$month[i] = 06
    }
    else if (dat$quarter[i] ==3 & dat$year[i] != dat$year[i-7])
    {
      dat$month[i] = 07
    }
    else if (dat$quarter[i] ==3 & dat$year[i] != dat$year[i-8])
    {
      dat$month[i] = 08
    }
    else if (dat$quarter[i] ==3 & dat$year[i] != dat$year[i-9])
    {
      dat$month[i] = 09
    }
    else if (dat$quarter[i] ==4 & dat$year[i] != dat$year[i-10])
    {
      dat$month[i] = 10
    }
    else if (dat$quarter[i] ==4 & dat$year[i] != dat$year[i-11])
    {
      dat$month[i] = 11
    }
    else if (dat$quarter[i] ==4 & dat$year[i] != dat$year[i-12])
    {
      dat$month[i] = 12
    }
  }

dat <- dat %>%
  mutate(month = str_pad(as.character(month), 2, "left", "0"))



#################################################
##### 2. Match commuting zones to inflation #####
#################################################


# 2.1 Match state to fips number using dat_geo
##############################################

# Read in table of FIPS, 2000 cz 
dat_geo <- read_csv2(dir.geo) %>%
  # select only fips and 2000 commuting zones
  select(fips = FIPS, cz2000 = "Commuting Zone ID, 2000") %>%
  # obtain state fips id
  mutate(id = substr(fips, 1, 2)) %>% 
  # make car string with zero in front
  mutate(id = str_pad(as.character(id), 2, "left", "0")) %>% 
  select(id, cz2000) %>%
  arrange(cz2000,id) %>%
  unique

# 2.2 Match id number to state name
###################################

dat_state <- read_xlsx(dir.stateid) %>%
  # make car string with zero in front
  mutate(id = str_pad(as.character(STATE), 2, "left", "0")) %>% 
  select(state = STATE_NAME, id)

# 2.3 Compile time series of inflation for each commuting zone
##############################################################

cpi <- dat %>%
  inner_join(dat_state) %>%
  left_join(dat_geo) %>%
  arrange(cz2000,id)
  
  # Create date string (new line because cannoot get argument in with oterwise)
cpi <- mutate(cpi,date = with(cpi,paste(year,month,sep="-"))) %>%
  mutate(date = parse_date(date,"%Y-%m")) %>%
  select(cz2000,date,pi_nt,pi_t,pi,state) %>% # can leave state in to check
  # Take mean as one commuting zone can be in several states
  group_by(cz2000,date) %>%
  mutate(pi_mean = mean(pi)) %>%
  mutate(pi_t_mean = mean(pi_t)) %>%
  mutate(pi_nt_mean = mean(pi_nt)) %>%  
  ungroup %>%
  select(cz2000,date,pi_mean,pi_t_mean,pi_nt_mean) %>%
  unique %>%
  arrange(cz2000,date)

# save  
write_csv(cpi, file.path(pipeline, 'out', 'cpi_cz2000-timeseries_nakamura.csv'))

