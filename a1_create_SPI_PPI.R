# Purpose: Make two"weighted" measures, 
#           Social Proximity to Inflation and Physical Proximity to Inflation
#
# Inputs: 
#     _intermediate/covariates.csv
#     _intermediate/sci.tsv
#     _intermediate/outwardness.csv
#     _intermediate/inflexp_date_cz.csv
#     _input/dist.xlsx
#
# Outputs: 
#     _intermediate/sci_weighted_inflation.csv
#     _intermediate/sci_weighted_inflation_control.csv
#
# Date: 18/09/2022
# written by: Lovisa Reiche
#
# Steps:
#     1. Specify data location
#     2. Create Social Proximity to Inflation
#     3. Create Physical Proximity to Inflation


rm(list = ls())
library(tidyverse)
library(readxl) # to read excel files
#library(lubridate)
#library(pracma)

########################
##### Make Choices #####
########################

# Do you want to look at mean or median computation?
c <- "median"

# Do you want to look at US counties or EU countries?
l <- "US"

# Which survey?
s <- "FRBNY"


####################################
##### 1. Specify data location #####
####################################


#### FILL IN THESE LINEs BEFORE RUNNING ####

# SCI 
# created in pre file
dir.sci <- paste("../SocialInflationExpectation/_intermediate/sci_",l,".tsv",sep="")
    
# Distance
# created in pre file
dir.dist <- paste("../SocialInflationExpectation/_intermediate/dist_",l,".csv",sep="")

# Covariates
# created in pre file
dir.covariates <- paste("../SocialInflationExpectation/_intermediate/covariates_",l,".csv",sep="")
      
# Inflation
# created in pre file
dir.inflexp <- paste("../SocialInflationExpectation/_intermediate/inflexp_",l,".csv",sep="")

# Geographic IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.geo <- paste("../SocialInflationExpectation/_input/",l,"/cz00_eqv_v1.csv",sep="")
    
# Population
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-total.html#par_textimage_70769902
dir.pop <- paste("../SocialInflationExpectation/_input/",l,"/pop.xlsx",sep="")


############################################
##### 3. Create Proximity to Inflation #####
############################################

# if run it from here
rm(list = ls())

# Geographic IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.geo <- "../SocialInflationExpectation/_input/cz00_eqv_v1.csv"
# Distance
# https://data.nber.org/data/county-distance-database.html
dir.dist <- "../SocialInflationExpectation/_input/sf12010countydistancemiles.csv"
# Inflation
# Hazell et al 2020
dir.cpi <- "../SocialInflationExpectation/_intermediate/cpi_cz2000-timeseries_nakamura.csv"


dat_inflex_agg2 <- read_csv("../SocialInflationExpectation/_intermediate/inflexp_date_cz_2.csv")
dat_sci_final <- read_tsv("../SocialInflationExpectation/_intermediate/sci_cz_cz.tsv")
dat_cpi <- read_csv(dir.cpi) %>%
  select(cz2000,date,pi_mean)

dat_geo <- read_csv2(dir.geo) %>%
  # select only fips and 2000 commuting zones
  select(fips = FIPS, cz2000 = "Commuting Zone ID, 2000", cz1990 = "Commuting Zone ID, 1990")


# 3.1 Social Proximity (by SCI)
################################


curr_dat_mean <- dat_sci_final %>%
  # Join in the Inflation data for foreign cz
  inner_join(dat_inflex_agg2, by=c("fr_loc"="cz2000")) %>%
  rename(inflexp_median_fr = inflexp_median, inflexp_mean_fr = inflexp_mean, obs_fr = obs) %>%
  # Join actual inflation data for foreign cz'
  inner_join(dat_cpi, by=c("fr_loc"="cz2000", "date")) %>%
  rename(pi_mean_fr = pi_mean) %>%
  # Join inflation exp for local cz by date
  inner_join(dat_inflex_agg2, by=c("user_loc"="cz2000","date")) %>%
  rename(inflexp_median_user = inflexp_median, inflexp_mean_user = inflexp_mean, obs_user = obs) %>%
  # Collapse and make the final weighted measure
  group_by(user_loc, date) %>% 
  # compute spi using differences
  mutate(SPI1 = sum((inflexp_mean_fr)*share_sci)) %>%
  mutate(SPI2 = sum((inflexp_mean_fr-inflexp_mean_user)*share_sci)) %>%
  summarise(SPI1, SPI2, SPI3 = sum((pi_mean_fr)*share_sci)) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(curr_dat_mean, "../SocialInflationExpectation/_intermediate/SPI_mean.csv")

curr_dat_median <- dat_sci_final %>%
  # Join in the Inflation data for foreign cz
  inner_join(dat_inflex_agg2, by=c("fr_loc"="cz2000")) %>%
  rename(inflexp_median_fr = inflexp_median, inflexp_mean_fr = inflexp_mean, obs_fr = obs) %>%
  # Join actual inflation data for foreign cz'
  inner_join(dat_cpi, by=c("fr_loc"="cz2000", "date")) %>%
  rename(pi_mean_fr = pi_mean) %>%
  # Join inflation exp for local cz by date
  inner_join(dat_inflex_agg2, by=c("user_loc"="cz2000","date")) %>%
  rename(inflexp_median_user = inflexp_median, inflexp_mean_user = inflexp_mean, obs_user = obs) %>%
  # Collapse and make the final weighted measure
  group_by(user_loc, date) %>% 
  # compute spi using differences
  mutate(SPI1 = sum((inflexp_median_fr)*share_sci)) %>%
  mutate(SPI2 = sum((inflexp_median_fr-inflexp_median_user)*share_sci)) %>%
  summarise(SPI1, SPI2, SPI3 = sum((pi_mean_fr)*share_sci)) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(curr_dat_median, "../SocialInflationExpectation/_intermediate/SPI_median.csv")


# 3.2 Physiscal Proximity (by distance)
################################



curr_dat_median <- dat_dist_final %>%
  summarise(cz1 = as.double(cz1), cz2 = as.double(cz2), mi_to_cz) %>%
  # Join in the Inflation data
  inner_join(dat_inflex_agg2, by=c("cz2"="cz2000")) %>% 
  rename(inflexp_median_fr = inflexp_median, inflexp_mean_fr = inflexp_mean, obs_fr = obs) %>%
  # Join actual inflation data for foreign cz'
  inner_join(dat_cpi, by=c("cz2"="cz2000", "date")) %>%
  rename(pi_mean_fr = pi_mean) %>%
  # Join inflation exp for local cz by date
  inner_join(dat_inflex_agg2, by=c("cz1"="cz2000","date")) %>%
  rename(inflexp_median_user = inflexp_median, inflexp_mean_user = inflexp_mean, obs_user = obs) %>%
  
  # Collapse and make the final weighted measure
  group_by(cz1, date) %>% 
  mutate(PPI1 = sum((inflexp_median_fr)/(1+mi_to_cz))) %>%
  mutate(PPI2 = sum((inflexp_median_fr-inflexp_median_user)/(1+mi_to_cz))) %>%
  summarise(PPI1, PPI2, PPI3 = sum((pi_mean_fr)/(1+mi_to_cz))) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(curr_dat_median, "../SocialInflationExpectation/_intermediate/PPI_median.csv")


curr_dat_mean <- dat_dist_final %>%
  summarise(cz1 = as.double(cz1), cz2 = as.double(cz2), mi_to_cz) %>%
  # Join in the Inflation data
  inner_join(dat_inflex_agg2, by=c("cz2"="cz2000")) %>% 
  rename(inflexp_median_fr = inflexp_median, inflexp_mean_fr = inflexp_mean, obs_fr = obs) %>%
  # Join actual inflation data for foreign cz'
  inner_join(dat_cpi, by=c("cz2"="cz2000", "date")) %>%
  rename(pi_mean_fr = pi_mean) %>%
  # Join inflation exp for local cz by date
  inner_join(dat_inflex_agg2, by=c("cz1"="cz2000","date")) %>%
  rename(inflexp_median_user = inflexp_median, inflexp_mean_user = inflexp_mean, obs_user = obs) %>%
  
  # Collapse and make the final weighted measure
  group_by(cz1, date) %>% 
  mutate(PPI1 = sum((inflexp_mean_fr)/(1+mi_to_cz))) %>%
  mutate(PPI2 = sum((inflexp_mean_fr-inflexp_mean_user)/(1+mi_to_cz))) %>%
  summarise(PPI1, PPI2, PPI3 = sum((pi_mean_fr)/(1+mi_to_cz))) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(curr_dat_mean, "../SocialInflationExpectation/_intermediate/PPI_mean.csv")

