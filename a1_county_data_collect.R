
# Purpose: Make two"weighted" measures, 
#           Social Proximity to Inflation and Physical Proximity to Inflation (not yet achieved)

# Inputs: 
#     _input/county_county.tsv
#     _input/cty_covariates.csv
#     _input/sf12010countydistancemiles.csv
#     _input/FRBNY-SCE-Public-Microdata-Complete.csv
#     _input/cz00_eqv_v1.csv
# Outputs: 
#     _intermediate/covariates.csv
#     _intermediate/sci_cz_cz.tsv
#     _intermediate/inflexp_date_cz.csv

# Date: 16/07/2022
# written by: Lovisa Reiche

# Steps:
#     1. Specify data location
#     2. Prep the datasets
#     3. Create Proximity to Inflation (social and physical)


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



################################
##### 2. Prep the datasets #####
################################

# 2.1 need to convert FIPS to 2000 commuting zones
################################

# Read in table of FIPS, 2000 cz and 1990 cz
dat_geo <- read_csv2(dir.geo) %>%
  # select only fips and 2000 commuting zones
  select(fips = FIPS, cz2000 = "Commuting Zone ID, 2000", cz1990 = "Commuting Zone ID, 1990")

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
  subset(select = c(user_loc,fr_loc,scaled_sci,fr_loc_cz,user_loc_cz))

# Collapse by combining all from one location
for (i in unique(dat_sci$user_loc_cz)) {
  
  print(i)
  
  us_subset <- filter(dat_sci,user_loc_cz == i)
  U = uniq(us_subset$fr_loc_cz)
  a = accumarray(U$n,us_subset$scaled_sci)
  
  if (i==dat_sci$user_loc_cz[1]){
    dat_sci_final <- tibble(user_loc = rep(i,times=length(a)), fr_loc = as.character(U$b), sci = a)
  } else {
    dat_sci_final <- rbind(dat_sci_final2,tibble(user_loc = as.character(rep(i,times=length(a))), fr_loc = as.character(U$b), sci = a))
  }
  
}

# Get the share of total SCI from each county
dat_sci_final <- dat_sci_final %>%
  group_by(user_loc) %>%
  mutate(total_sci = sum(sci)) %>%
  mutate(share_sci = sci/total_sci) %>%
  ungroup

write_tsv(dat_sci_final,"../SocialInflationExpectation/_intermediate/sci_cz_cz.tsv")


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
  filter(!is.na(inflexp_median))


write_csv(dat_inflex_agg2,"../SocialInflationExpectation/_intermediate/inflexp_date_cz.csv")



############################################
##### 3. Create Proximity to Inflation #####
############################################


# 3.2 Social Proximity (by SCI)
################################


curr_dat <- dat_sci_final %>%
  # Join in the Inflation data
  inner_join(dat_inflex_agg2, by=c("fr_loc"="cz2000")) %>% 
  # Collapse and make the final weighted measure
  group_by(user_loc, date) %>% 
  summarise(sci_weighted_inflation = sum(inflexp_mean*share_sci)) %>%
  ungroup

write_csv(curr_dat, "../SocialInflationExpectation/_intermediate/sci_weighted_inflation.csv")



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


curr_dat <- dat_dist_final %>%
  mutate(cz2 = as.numeric(cz2)) %>%
  # Join in the Inflation data
  inner_join(dat_inflex_agg2, by=c("cz2"="cz2000")) %>% 
  # Collapse and make the final weighted measure
  group_by(cz1, date) %>% 
  summarise(dist_weighted_inflation = sum(inflexp_mean/(1+mi_to_cz))) %>%
  ungroup

write_csv(curr_dat, "../SocialInflationExpectation/_intermediate/dist_weighted_inflation.csv")

