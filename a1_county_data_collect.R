# Purpose: Make two"weighted" measures, 
#           Social Proximity to Inflation and Physical Proximity to Inflation

# Inputs: 
#     _input/county_county.tsv
#     _input/cty_covariates.csv
#     _input/sf12010countydistancemiles.csv
#     _input/FRBNY-SCE-Public-Microdata-Complete.csv
#     _input/cz00_eqv_v1.csv
# Outputs: 
#     _intermediate/covariates.csv
#     _intermediate/sci_cz_cz.tsv
#     _intermediate/sci_cz_cz_control.tsv
#     _intermediate/inflexp_date_cz.csv
#     _intermediate/sci_weighted_inflation.csv
#     _intermediate/sci_weighted_inflation_control.csv

# Date: 16/07/2022
# written by: Lovisa Reiche

# Steps:
#     1. Specify data location
#     2. Prep the datasets
#     3. Create Proximity to Inflation (social and physical)


rm(list = ls())
library(tidyverse)
library(lubridate)
library(pracma)


####################################
##### 1. Specify data location #####
####################################


#### FILL IN THESE LINEs BEFORE RUNNING ####

# SCI 
# https://data.humdata.org/dataset/social-connectedness-index'
dir.county_county_sci <- "../SocialInflationExpectation/_input/county_county.tsv"

# Distance
# https://data.nber.org/data/county-distance-database.html
dir.county_county_dist <- "../SocialInflationExpectation/_input/sf12010countydistancemiles.csv"

# Covariates
# https://opportunityinsights.org/data/?geographic_level=102&topic=0&paper_id=0#resource-listing
dir.covariates <- "../SocialInflationExpectation/_input/cty_covariates.csv"
# Note: use 2010 FIPS and 1990 commuting zones

# Inflation
#  Survey of Consumer Expectations, © 2013-21 Federal Reserve Bank of New York (FRBNY)
dir.inflexp <- "../SocialInflationExpectation/_input/FRBNY-SCE-Public-Microdata-Complete.csv"
# Note: use 2000 commuting zones

# Geographic IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.geo <- "../SocialInflationExpectation/_input/cz00_eqv_v1.csv"

# Population
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-total.html#par_textimage_70769902
dir.pop <- "../SocialInflationExpectation/_input/co-est2021-alldata.csv"



################################
##### 2. Prep the datasets #####
################################

# 2.1 need to convert FIPS to 2000 commuting zones
################################

# Read in table of FIPS, 2000 cz and 1990 cz
dat_geo <- read_csv2(dir.geo) %>%
  # select only fips and 2000 commuting zones
  select(fips = FIPS, cz2000 = "Commuting Zone ID, 2000", cz1990 = "Commuting Zone ID, 1990")

# Read in population size in each county to aggregate population weighted
dat_pop <- read_csv(dir.pop) %>%
  # create fips index for counties by combining state and county code
  unite("fips",STATE:COUNTY, sep = "") %>%
  select(fips, pop = POPESTIMATE2021)

# Read in Covariates, goal: assign 2000 cz to all areas in covariates
dat_covariates <- read_csv(dir.covariates) %>%
  rename(cz1990 = cz) %>% # rename for clarity
  mutate(cz1990 = str_pad(as.character(cz1990), 5, "left", "0")) %>% # shape into same format
  # left_join with geo data to use cz2000 instead of cz1990
  left_join(dat_geo,by = "cz1990")

write_csv(dat_covariates,"../SocialInflationExpectation/_intermediate/covariates.csv")

# Read in SCI, goal: convert fips to cz2000 (more coarse)
dat_sci <- read_tsv(dir.county_county_sci) %>%
  # inner_join with geo to get cz2000 for each foreign fips (removes na)
  inner_join(dat_geo,by = c("fr_loc"="fips")) %>%
  rename(fr_loc_cz = cz2000) %>%
  subset(select = c(user_loc,fr_loc,scaled_sci,fr_loc_cz)) %>%
  inner_join(dat_geo,by = c("user_loc"="fips")) %>%
  rename(user_loc_cz = cz2000) %>%
  subset(select = c(user_loc,fr_loc,scaled_sci,fr_loc_cz,user_loc_cz)) %>%
  # join with population data
  inner_join(dat_pop, by = c("user_loc"="fips")) %>%
  rename(user_loc_pop = pop) %>%
  inner_join(dat_pop, by = c("fr_loc"="fips")) %>%
  rename(fr_loc_pop = pop) 

# Functions for weighting

# type 3: use weight of each county in aggregation to commuting zone
#---------------------------------------------------------------------------------------------------
weighting_type3 <- function(dat){
# Collapse by combining all from one location
for (i in unique(dat$user_loc_cz)) {
  
  print(i)
  
  # create subset for cz2000 i
  us_subset <- filter(dat,user_loc_cz == i)
  
  # 1. Compute population share of each foreign county in its commuting zone
  
  # split it by user loc counties to make sure we weigh all
  for (ii in unique(us_subset$user_loc)){
    us_subset_sub <- us_subset[us_subset$user_loc == ii,] %>%
      # we want to weigh counties in each foreign commuting zone
      group_by(fr_loc_cz) %>%
      # sum population in this commuting zone
      mutate(total_pop = sum(fr_loc_pop)) %>%
      ungroup %>%
      # calculate population share in each foreign county when aggregating sci
      mutate(weight_fr = fr_loc_pop/total_pop)
  
    if (ii == unique(us_subset$user_loc)[1]){
      S <- us_subset_sub
    } else {
      S <- rbind(S,us_subset_sub)
    }
  }
  
  # 2. Compute population share of each user county in the commuting zone
  
  S <- S %>%
    # weight of county within user commuting zone
    mutate(weight_user = user_loc_pop/sum(unique(user_loc_pop))) %>%
    
    # 3. Compute the weighted SCI
    
    mutate(weighted_sci = scaled_sci * weight_fr * weight_user)

  
  # index of each county that occurs within this cz2000
  U = uniq(S$fr_loc_cz)
  
  # acummulate sci
  a = accumarray(U$n,S$weighted_sci)
  
  # collapse all user locs into one
  if (i==dat_sci$user_loc_cz[1]){
    dat_final <- tibble(user_loc = rep(i,times=length(a)), fr_loc = as.character(U$b), sci = a)
  } else {
    dat_final <- rbind(dat_final,tibble(user_loc = as.character(rep(i,times=length(a))), fr_loc = as.character(U$b), sci = a))
  }
}
}
#---------------------------------------------------------------------------------------------------

# type 2: weigh strength of connection by population in the two counties combined
#---------------------------------------------------------------------------------------------------
weighting_type2 <- function(dat){
  # Collapse by combining all from one location
  for (i in unique(dat$user_loc_cz)) {
    
    print(i)
    
    # create subset for cz2000 i
    us_subset <- filter(dat,user_loc_cz == i) %>%
      mutate(weighted_sci = scaled_sci/(user_loc_pop + fr_loc_pop))
  
    # index of each county that occurs within this cz2000
    U = uniq(us_subset$fr_loc_cz)
    
    # acummulate sci
    a = accumarray(U$n,us_subset$weighted_sci)
    
    # collapse all user locs into one
    if (i==dat_sci$user_loc_cz[1]){
      dat_final <- tibble(user_loc = rep(i,times=length(a)), fr_loc = as.character(U$b), sci = a)
    } else {
      dat_final <- rbind(dat_final,tibble(user_loc = as.character(rep(i,times=length(a))), fr_loc = as.character(U$b), sci = a))
    }
  }
  dat_final
}
#---------------------------------------------------------------------------------------------------

# type 1: No weighting
#---------------------------------------------------------------------------------------------------
no_weighting <- function(dat){
  # Collapse by combining all from one location
  for (i in unique(dat$user_loc_cz)) {
    
    print(i)
    
    # create subset for cz2000 i
    us_subset <- filter(dat,user_loc_cz == i)
    
    # index of each county that occurs within this cz2000
    U = uniq(us_subset$fr_loc_cz)
    
    # acummulate sci
    a = accumarray(U$n,us_subset$scaled_sci)
    
    # collapse all user locs into one
    if (i==dat_sci$user_loc_cz[1]){
      dat_final <- tibble(user_loc = rep(i,times=length(a)), fr_loc = as.character(U$b), sci = a)
    } else {
      dat_final <- rbind(dat_final,tibble(user_loc = as.character(rep(i,times=length(a))), fr_loc = as.character(U$b), sci = a))
    }
  }
  dat_final
}
#---------------------------------------------------------------------------------------------------

# Choose which weighting is to be used
dat_sci_final_3 <- weighting_type3(dat_sci)
dat_sci_final_2 <- weighting_type2(dat_sci)
dat_sci_final_1 <- no_weighting(dat_sci)


# Get the share of total SCI from each county
dat_sci_final <- dat_sci_final %>%
  group_by(user_loc) %>%
  mutate(total_sci = sum(sci)) %>%
  mutate(share_sci = sci/total_sci) %>%
  ungroup

outwardness_dat <- dat_sci_final %>%
  summarise(outwardness = (1-share_sci[user_loc==fr_loc]), user_loc = unique(user_loc))


write_tsv(dat_sci_final,"../SocialInflationExpectation/_intermediate/sci_cz_cz.tsv")
write_csv(outwardness_dat,"../SocialInflationExpectation/_intermediate/outwardness.csv")


# 2.2 aggregate inflation expectations on cz2000 level
################################

# Read in Inflation expectations micro-data
dat_inflex <- read_csv(dir.inflexp) %>%
  rename(cz2000 = "_COMMUTING_ZONE", inflexp = "Q8v2part2") %>% # rename as name leads to errors
  subset(select = c(date,userid,inflexp,cz2000)) %>%
  mutate(date = as.character(date)) %>%
  mutate(date = parse_date(date, "%Y%m")) %>%
  group_by(cz2000) %>% 
  arrange(cz2000, date) %>% # for overview
  ungroup

# count obervations in each cz date pair
dat_inflex_count <- aggregate(dat_inflex$userid, by=subset(dat_inflex, select = c(date,cz2000)), FUN = length) %>%
  rename(obs = x)

# calculate average expectations in each cz date pair
dat_inflex_agg <- aggregate(dat_inflex$inflexp, by=subset(dat_inflex, select = c(date,cz2000)), FUN = mean, na.action = na.rm) %>%
  rename(inflexp_mean = x) %>%
  inner_join(dat_inflex_count) %>%
  filter(!is.na(inflexp_mean))

dat_inflex_agg2 <- aggregate(dat_inflex$inflexp, by=subset(dat_inflex, select = c(date,cz2000)), FUN = median, na.action = na.rm) %>%
  rename(inflexp_median = x) %>%
  inner_join(dat_inflex_agg) %>%
  filter(!is.na(inflexp_median)) %>%
  filter(obs>=3) %>%
  mutate(cz2000 = as.character(cz2000))


write_csv(dat_inflex_agg2,"../SocialInflationExpectation/_intermediate/inflexp_date_cz_2.csv")



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
dir.county_county_dist <- "../SocialInflationExpectation/_input/sf12010countydistancemiles.csv"
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

# Read in data for county distances
county_county_dist <- read_csv(dir.county_county_dist) %>%
  inner_join(dat_geo,by = c("county1"="fips")) %>%
  rename(cz1 = cz2000) %>%
  subset(select = c(cz1,mi_to_county,county2)) %>%
  inner_join(dat_geo,by = c("county2"="fips")) %>%
  rename(cz2 = cz2000) %>%
  subset(select = c(cz1,mi_to_county,cz2))


# Since distances are given for counties we need to convert to commuting zones
# as distance we choose the average distance between cz1 and cz2
for (i in unique(county_county_dist$cz1)) {
  
  print(i)
  
  us_subset <- filter(county_county_dist, cz1 == i)
  U = uniq(us_subset$cz2)
  a = accumarray(U$n,us_subset$mi_to_county, func = mean)
  
  if (i==county_county_dist$cz1[1]){
    dat_dist_final <- tibble(cz1 = rep(i,times=length(a)), cz2 = as.character(U$b), mi_to_cz = a)
  } else {
    dat_dist_final <- rbind(dat_dist_final,tibble(cz1 = rep(i,times=length(a)), cz2 = as.character(U$b), mi_to_cz = a))
  }
  
}


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

