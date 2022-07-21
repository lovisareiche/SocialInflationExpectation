# Purpose: Build the regression table for the time series analyses
#           and make maps of all time series measures
# Inputs: 
#     _intermediate/inflexp_date_cz.csv
#     _intermediate/sci_weighted_inflation.csv
#     _intermediate/dist_weighted_inflation.csv
#     _intermediate/covariates.csv
# Outputs: 
#     _intermediate/time_series_regress_dat.csv
# Date: 21/07/202
# Steps:
#     1. Prep all data sources
#     2. Join all data sources

library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(pracma)
library(plm)

# Throughout we will analyze changes in inflation expectations at a monthly level


####################################
##### 1. Prep all data sources #####
####################################

# Read in baseline inflation expectations data
dat_inflex <- read_csv("../SocialInflationExpectation/_intermediate/inflexp_date_cz.csv")

# Make the change in inflation expectations from the last period.
inflex_change <- dat_inflex %>% 
  mutate(cz2000 = as.numeric(cz2000)) %>% 
  group_by(cz2000) %>%
  mutate(chg_median = inflexp_median - lag(inflexp_median)) %>% 
  ungroup

# Read in SCI-weighted inflation (Social Proximity to Inflation)
# Built in a1_county_data_collect.R
sci_weighted_inflation_control <- read_csv("../SocialInflationExpectation/_intermediate/sci_weighted_inflation_control.csv")
sci_weighted_inflation <- read_csv("../SocialInflationExpectation/_intermediate/sci_weighted_inflation.csv")

# Read in distance-weighted inflation (Physical Proximity to Inflation)
# Built in a1_county_data_collect.R
dist_weighted_inflation <- read_csv("../SocialInflationExpectation/_intermediate/dist_weighted_inflation.csv")


# Join the weighted cases measures
# CHOOSE IF WANT TO WORK WITH CONTROL OR NOT
weighted_inflation_dat <- dist_weighted_inflation %>% 
  left_join(sci_weighted_inflation_control, by=c("cz1"="user_loc", "date"="date"))

# Make the changes in weighted cases
weighted_inflex_change <- weighted_inflation_dat %>%
  mutate(cz1 = as.numeric(cz1)) %>% 
  group_by(cz1) %>% 
  # Change SWI/D
  mutate(change_swi = sci_weighted_inflation - lag(sci_weighted_inflation)) %>% 
  # Change DWI/D
  mutate(change_dwi = dist_weighted_inflation - lag(dist_weighted_inflation)) %>% 
ungroup

##### Chose covariates you want to control for ##########

dat_covariates <- read_csv("../SocialInflationExpectation/_intermediate/covariates.csv") %>% 
  select(cz2000, med_hhinc2016, popdensity2010) 

U = uniq(dat_covariates$cz2000)
a1 = accumarray(U$n,dat_covariates$med_hhinc2016, func = mean)
a2 = accumarray(U$n,dat_covariates$popdensity2010, func = mean)

dat_covariates <- tibble(cz2000 = as.character(U$b), popdensity2010 = a2, med_hhinc2016 = a1)


#######################################
##### 2. Combine all data sources #####
#######################################

regress_dat <- inflex_change %>% 
  inner_join(weighted_inflex_change, by=c("cz2000"="cz1", "date"="date")) %>% 
  # This starts us at inflation expectation change from 2013-07 to 2020-11
  # Merge with covariates
  mutate(cz2000 = as.character(cz2000)) %>%
  left_join(dat_covariates, by=c("cz2000"="cz2000")) %>%
  group_by(cz2000) %>% 
  arrange(cz2000, date) %>% 
  # Make the lagged versions of our variables
  # First, in differences
  mutate(l1_chg_swi = lag(change_swi),
         l2_chg_swi = lag(change_swi, 2),
         l1_chg_dwi = lag(change_dwi),
         l2_chg_dwi = lag(change_dwi, 2),
         l1_chg_median = lag(chg_median),
         l2_chg_median = lag(chg_median, 2)) %>% 
  # Then, in levels
  mutate(l1_swi = lag(sci_weighted_inflation),
         l2_swi = lag(sci_weighted_inflation, 2),
         l1_dwi = lag(dist_weighted_inflation),
         l2_dwi = lag(dist_weighted_inflation, 2),
         l1_median = lag(inflexp_median),
         l2_median = lag(inflexp_median, 2))

write_csv(regress_dat, "../SocialInflationExpectation/_intermediate/time_series_regress_dat.csv")


###############################
##### 3. Panel regression #####
###############################


# Set individual commuting zones as factors
regress_dat$cz2000 <- as.factor(regress_dat$cz2000)

# estimate the fixed effects regression with plm()
inflex_fe_mod <- plm(inflexp_median ~ sci_weighted_inflation + dist_weighted_inflation, 
                    data = regress_dat,
                    index = c("cz2000", "date"), 
                    model = "within")
coeftest(inflex_fe_mod)


# Points to Note:
# - covariates are time invariant (not in reality but in data we have) and thus not included in fixed effects regression
# - can control for friends having same experience in same cz by excluding same cz from measure
# - some cz have only very few people answering the inflation expectations survey so the medians may be driven by very few people

plot(regress_dat$inflexp_median,regress_dat$sci_weighted_inflation)
text(paste("Correlation:", round(cor(regress_dat$inflexp_median,regress_dat$sci_weighted_inflation), 2)), x = -50, y = 70)

