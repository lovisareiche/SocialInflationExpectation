# Purpose: Prepare the datasets such that they are in the required shape for the next steps
#
# Inputs: 
#     _input/sci.tsv
#     _input/covariates.xlsx
#     _input/inflexp.xlsx
#     _input/geo_match.xlsx
#     _input/dist.xlsx
# Outputs: 
#     _intermediate/covariates.csv
#     _intermediate/sci.tsv
#     _intermediate/outwardness.csv
#     _intermediate/inflexp_date_cz.csv
#
# Date: 17/09/2022
# written by: Lovisa Reiche
#
# Steps:
#     1. Specify data location
#     2. Prep the SCI data
#     3. Prep Inflation Expectation data
#     4. Prep Distance data
#     5. Prep Covariates data


rm(list = ls())
library(tidyverse) # general commands
library(readxl) # to read excel files
library(pracma) # for matlab functions (uniq,acumarray)



########################################
##### This code is for US counties #####
########################################



# Do you want to look at US counties or EU countries?
l <- "US"

# Which survey?
s <- "FRBNY"


####################################
##### 1. Specify data location #####
####################################


#### FILL IN THESE LINEs BEFORE RUNNING ####

# SCI 
# https://data.humdata.org/dataset/social-connectedness-index'
dir.sci <- paste("../code/_input/",l,"/sci.tsv",sep="")

# Covariates
# https://opportunityinsights.org/data/?geographic_level=102&topic=0&paper_id=0#resource-listing
dir.covariates <- paste("../code/_input/",l,"/covariates.xlsx",sep="")
# Note: use 2010 FIPS and 1990 commuting zones

# Inflation
#  Survey of Consumer Expectations, © 2013-21 Federal Reserve Bank of New York (FRBNY)
dir.inflexp <- paste("../code/_input/",l,"/inflexp_",s,".xlsx",sep="")
# Note: use 2000 commuting zones

# Geographic IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.geo <- paste("../code/_input/",l,"/geo_match.xlsx",sep="")

# Population
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-total.html#par_textimage_70769902
dir.pop <- paste("../code/_input/",l,"/pop_",l,".xlsx",sep="")

# Distance
# https://data.nber.org/data/county-distance-database.html
dir.dist <- paste("../code/_input/",l,"/dist.xlsx",sep="")


# Inflation
# Hazell et al 2020, created in b2_create_state_cpi_nakamura
dir.cpi <- "../code/_intermediate/cpi_cz2000-timeseries_nakamura.csv"


############################
##### 2. Prep SCI data #####
############################



# Need to convert FIPS to 2000 commuting zones 
##############################################
# using population weights
##########################

# Read in table of FIPS, 2000 cz and 1990 cz
dat_geo <- read_xlsx(dir.geo) %>%
  # select only fips and 2000 commuting zones
  select(fips = FIPS, cz2000 = "Commuting Zone ID, 2000", cz1990 = "Commuting Zone ID, 1990") %>%
  mutate(cz1990 = as.character(cz1990),cz2000 = as.character(cz2000), fips = str_pad(as.character(fips), 5, "left", "0"))

# Read in population size in each county to aggregate population weighted
dat_pop <- read_xlsx(dir.pop) %>%
  mutate(STATE = str_pad(as.character(STATE), 2, "left", "0"), COUNTY = str_pad(as.character(COUNTY), 3, "left", "0")) %>%
  # create fips index for counties by combining state and county code
  unite("fips",STATE:COUNTY, sep = "") %>%
  select(fips, pop = POP2021)


# Read in SCI, goal: convert fips to cz2000 (more coarse)
dat_sci <- read_tsv(dir.sci) %>%
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
      mutate(fr_loc_cz = as.numeric(fr_loc_cz)) %>%
      
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
  dat_final
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
#dat_sci_final_2 <- weighting_type2(dat_sci)
#dat_sci_final_1 <- no_weighting(dat_sci)


# Get the share of total SCI from each county
dat_sci_final <- dat_sci_final_3 %>%
  group_by(user_loc) %>%
  mutate(total_sci = sum(sci)) %>%
  mutate(share_sci = sci/total_sci) %>%
  ungroup

outwardness_dat <- dat_sci_final %>%
  summarise(outwardness = (1-share_sci[user_loc==fr_loc]), user_loc = unique(user_loc))


write_tsv(dat_sci_final,paste("../code/_intermediate/sci_",l,".tsv",sep=""))
write_csv(outwardness_dat,paste("../code/_intermediate/outwardness_",l,".csv",sep=""))



##########################################
##### 3. Prep inflation expectations #####
##########################################



if (s == "FRBNY") {
  # Read in Inflation expectations micro-data
  dat_inflex <- read_xlsx(dir.inflexp) %>%
      rename(cz2000 = "_COMMUTING_ZONE", inflexp = "Q8v2part2", iqr = "Q9_iqr", cons = "Q26v2part2") %>% # rename as name leads to error
      mutate(inflexp = as.numeric(inflexp)) %>% # needs to be numeric for further operations
      mutate(date = as.character(date)) %>%
      mutate(date = parse_date(date, "%Y%m")) %>%
      group_by(cz2000) %>% 
      arrange(cz2000, date) %>% # for overview
      ungroup %>%
      na.omit
} else if (s == "Michigan") { 
  # THIS ONE STILL NEEDS TO BE WRITTEN!!!!!
  # Read in Inflation expectations micro-data
  dat_inflex <- read_xlsx(dir.inflexp) %>%
      rename(cz2000 = "Region", inflexp = "PX1Q2") %>% # rename
    # NOTE: Michigan only records region so not useful in this context
      mutate(inflexp = as.numeric(inflexp)) %>%
      subset(select = c(date,userid,inflexp,cz2000)) %>%
      mutate(date = as.character(date)) %>%
      mutate(date = parse_date(date, "%Y%m")) %>%
      group_by(cz2000) %>% 
      arrange(cz2000, date) %>% # for overview
      ungroup
}

# for inflation expectations point estimate
dat_inflex_count <- aggregate(dat_inflex$userid, by=subset(dat_inflex, select = c(date,cz2000)), FUN = length) %>%
  rename(obs = x)

dat_inflex_median <- aggregate(dat_inflex$inflexp, by=subset(dat_inflex, select = c(date,cz2000)), FUN = median) %>%
  rename(inflexp_median = x, loc = cz2000) %>%
  inner_join(dat_inflex_count, by = c("date", "loc" = "cz2000")) %>%
  filter(!is.na(inflexp_median)) %>%
  filter(obs>=3) %>%
  select(-obs)

write_csv(dat_inflex_median,paste("../code/_intermediate/inflexp_",l,"_",s,".csv",sep=""))

# for uncertainty
dat_inflex_sd <- aggregate(dat_inflex$inflexp, by=subset(dat_inflex, select = c(date,cz2000)), FUN = sd) %>%
  rename(inflexp_sd = x, loc = cz2000) %>%
  inner_join(dat_inflex_count, by = c("date", "loc" = "cz2000")) %>%
  filter(obs>=3) %>%
  select(-obs)
  
dat_inflex_uncertainty <- aggregate(dat_inflex$iqr, by=subset(dat_inflex, select = c(date,cz2000)), FUN = median) %>%
  rename(iqr_median = x, loc = cz2000) %>%
  inner_join(dat_inflex_sd, by = c("date", "loc")) %>%
  na.omit 

write_csv(dat_inflex_uncertainty,paste("../code/_intermediate/inflexp_uncertainty_",l,"_",s,".csv",sep=""))

# for consumption
dat_cons <- aggregate(dat_inflex$cons, by=subset(dat_inflex, select = c(date,cz2000)), FUN = median) %>%
  rename(cons = x, loc = cz2000) %>%
  inner_join(dat_inflex_count, by = c("date", "loc" = "cz2000")) %>%
  filter(obs>=3) %>%
  select(-obs)

write_csv(dat_cons,paste("../code/_intermediate/cons_",l,"_",s,".csv",sep=""))


#################################
##### 4. Prep Distance data #####
#################################



# Read in data for county distances
dat_dist <- read_xlsx(dir.dist) %>%
  # need to add own distance (zero)
  rbind(tibble(county1 = unique(c(county1,county2)), mi_to_county = rep(0,length(unique(c(county1,county2)))), county2 = unique(c(county1,county2))))

  # need to find combinations from right side also to left
dat_dist <- rbind(dat_dist, tibble(county1 = dat_dist$county2, mi_to_county = dat_dist$mi_to_county, county2 = dat_dist$county1)) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(county1 = str_pad(as.character(county1), 5, "left", "0"), county2 = str_pad(as.character(county2), 5, "left", "0")) %>% # mutate to characters for joining
  inner_join(dat_geo,by = c("county1"="fips")) %>%
  rename(user_loc = cz2000) %>%
  mutate(user_loc = as.numeric(user_loc)) %>% # mutate back for pracma functions
  subset(select = c(user_loc,mi_to_county,county2)) %>%
  inner_join(dat_geo,by = c("county2"="fips")) %>%
  rename(fr_loc = cz2000) %>%
  mutate(fr_loc = as.numeric(fr_loc)) %>% # mutate back for pracma functions
  subset(select = c(user_loc,mi_to_county,fr_loc))


# Since distances are given for counties we need to convert to commuting zones
# as distance we choose the average distance between cz1 and cz2

for (i in unique(dat_dist$user_loc)) {
  
  print(i)
  
  subset <- filter(dat_dist, user_loc == i)
  U = uniq(subset$fr_loc)
  a = accumarray(U$n,subset$mi_to_county, func = mean)
  
  if (i==dat_dist$user_loc[1]){
    dat_dist_final <- tibble(user_loc = rep(i,times=length(a)), fr_loc = as.character(U$b), dist = a)
  } else {
    dat_dist_final <- rbind(dat_dist_final,tibble(user_loc = rep(i,times=length(a)), fr_loc = as.character(U$b), dist = a))
  }
  
}

write_csv(dat_dist_final,paste("../code/_intermediate/dist_",l,".csv",sep=""))



###################################
##### 5. Prep Covariates data #####
###################################



# Read in Covariates, goal: assign 2000 cz to all areas in covariates
dat_covariates <- read_xlsx(dir.covariates) %>%
  rename(cz1990 = cz) %>% # rename for clarity
  mutate(cz1990 = as.character(cz1990)) %>%
  # left_join with geo data to use cz2000 instead of cz1990
  left_join(dat_geo,by = "cz1990") %>%
  rename(loc = cz2000)

write_csv(dat_covariates,paste("../code/_intermediate/covariates_",l,".csv",sep=""))


# Read in inflation data

dat_cpi <- read_csv(dir.cpi) %>%
  select(cz2000,date,pi_mean) %>%
  rename(cpi_inflation = pi_mean, loc = cz2000)

write_csv(dat_cpi,paste("../code/_intermediate/cpi_",l,".csv",sep=""))



