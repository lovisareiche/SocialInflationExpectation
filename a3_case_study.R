# Purpose: Case study analysis
#
# Inputs: 
#     _intermediate/inflexp_date_cz.csv
#     _intermediate/sci_cz_cz.tsv
#     _intermediate/cpi_cz2000-timeseries.csv
# Outputs: 
# Date: 26/07/22
# Steps:
#     1. Read in data
#     2. Find case studies
#     3. 3. Match inflation expectations to sci with cases


library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(pracma)
library(plm)
library(lmtest)


###########################
##### 1. Read in data #####
###########################

# SCI
# Built in a1_county_data_collect
sci <- read_tsv("../SocialInflationExpectation/_intermediate/sci_cz_cz.tsv")
sci_control <- read_tsv("../SocialInflationExpectation/_intermediate/sci_cz_cz_control.tsv")

# CPI
# Built in b1_create_state_cpi
cpi <- read_csv("../SocialInflationExpectation/_intermediate/cpi_cz2000-timeseries.csv")

# Inflex
# Built in a1_county_data_collect
inflex <- read_csv("../SocialInflationExpectation/_intermediate/inflexp_date_cz.csv")


################################
##### 2. Find case studies #####
################################

# cpi with dates we have expectations for
cpi_used <- cpi[cpi$Date <= max(inflex$date),]


# 2.1 Date-CZ pairs with highest cpi in levels
##############################################

cpi_max1 <- cpi_used[cpi_used$cpi_mean == max(cpi_used$cpi_mean),]


# 2.2 Date-CZ pairs with the highest cpi month-on-month change
##############################################################

cpi_max2 <- cpi_used[cpi_used$cpi_mom_mean == max(cpi_used$cpi_mom_mean),]


#############################################################
##### 3. Match inflation expectations to sci with cases #####
#############################################################


# 3.1 calculate changes in expectations in all regions
######################################################

inflex_sub1 <- inflex %>%
  group_by(cz2000) %>%
  # change of current period versus kast period
  mutate(chg_inflex_med = inflexp_median - dplyr::lag(inflexp_median)) %>%
  # only use period identified in case
  filter(date == unique(cpi_max1$Date))

# same procedure for alternative case
inflex_sub2 <- inflex %>%
  group_by(cz2000) %>%
  mutate(chg_inflex_med = inflexp_median - dplyr::lag(inflexp_median)) %>%
  filter(date == unique(cpi_max2$Date))


# 3.2 Match sci to expectations
#################################################################

dat <- sci %>%
  # inflation expectations in the user's location
  inner_join(inflex_sub1, by = c("user_loc" = "cz2000")) %>%
  # share of connections with cpi experience
  group_by(user_loc) %>%
  summarise(share_high = sum(share_sci[fr_loc %in% cpi_max1$cz2000]), chg_inflex_med) %>%
  ungroup %>%
  unique
  


