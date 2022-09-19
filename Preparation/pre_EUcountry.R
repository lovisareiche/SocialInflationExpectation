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



#########################################
##### This code is for EU countries #####
#########################################



# Do you want to look at US counties or EU countries?
l <- "EU"

# Which survey?
s <- "ECFIN"


####################################
##### 1. Specify data location #####
####################################


#### FILL IN THESE LINEs BEFORE RUNNING ####

# SCI 
# https://data.humdata.org/dataset/social-connectedness-index'
dir.sci <- paste("../SocialInflationExpectation/_input/",l,"/sci.tsv",sep="")

# Covariates
# https://opportunityinsights.org/data/?geographic_level=102&topic=0&paper_id=0#resource-listing
#dir.covariates <- paste("../SocialInflationExpectation/_input/",l,"/covariates.xlsx",sep="")
# Note: use 2010 FIPS and 1990 commuting zones

# Inflation
#  Consumer and Business Surveys, © 1985-2022 European Commission (ECFIN)
dir.inflexp <- paste("../SocialInflationExpectation/_input/",l,"/inflexp_",s,".xlsx",sep="")

# Geographic IDs
# List of ISO 3166 country codes
dir.geo <- paste("../SocialInflationExpectation/_input/",l,"/geo_match.xlsx",sep="")

# Population
# https://data.worldbank.org/indicator/SP.POP.TOTL?locations=1W
dir.pop <- paste("../SocialInflationExpectation/_input/",l,"/pop_",l,".xlsx",sep="")

# Distance
# Mayer, T. & Zignago, S. (2011), Notes on CEPII’s distances measures : the GeoDist Database, CEPII Working Paper 2011-25
# http://cepii.fr/cepii/en/bdd_modele/bdd_modele_item.asp?id=6
dir.dist <- paste("../SocialInflationExpectation/_input/",l,"/dist.xlsx",sep="")


# Inflation
# https://thedocs.worldbank.org/en/doc/483541579976288664-0050022020/Inflation-data
dir.cpi <- paste("../SocialInflationExpectation/_input/",l,"/inflation.xlsx",sep="")



###############################
##### 2. Prep Geo and Pop #####
###############################

# We want to put all datasets in terms of 2-digit and need geo for that

# Read in table of 2-digit, 3-digit and numeric code
dat_geo <- read_xlsx(dir.geo) %>%
  rename(alpha2 = "Alpha-2 code", alpha3 = "Alpha-3 code", num = "Numeric code")


# Read in population size in each country  in 3-digit
dat_pop <- read_xlsx(dir.pop) %>%
  rename(alpha3 = "Country Code", pop = "2021") %>%
  inner_join(dat_geo) %>%
  select(loc = alpha2, pop)


################################
##### 3. Prep SCI and Dist #####
################################

# SCI is already in right format

# Read in SCI in 2-digit
dat_sci <- read_tsv(dir.sci) %>%
  filter(!is.na(user_loc)) %>%
  filter(!is.na(fr_loc)) %>%
  filter(!is.na(scaled_sci)) %>%
  group_by(user_loc) %>%
  # add some additional measures
  mutate(total_sci = sum(scaled_sci)) %>%
  mutate(share_sci = scaled_sci/total_sci) %>%
  ungroup %>%
  rename(sci = scaled_sci)

dat_outwardness <- dat_sci %>%
  summarise(outwardness = (1-share_sci[user_loc==fr_loc]), user_loc = unique(user_loc))


write_tsv(dat_sci,paste("../SocialInflationExpectation/_intermediate/sci_",l,".tsv",sep=""))
write_csv(dat_outwardness,paste("../SocialInflationExpectation/_intermediate/outwardness_",l,".csv",sep=""))


# Distances need to be put in the right units (2-digit)

# Read in distances in 3-digit
dat_dist <- read_xlsx(dir.dist) %>%
  select(iso_o, iso_d, dist) %>%
  # inner_join with geo to write in 2-digit
  inner_join(dat_geo,by = c("iso_o"="alpha3")) %>%
  rename(user_loc = alpha2) %>%
  select(-num) %>%
  inner_join(dat_geo,by = c("iso_d"="alpha3")) %>%
  rename(fr_loc = alpha2) %>%
  select(user_loc,fr_loc,dist)

write_csv(dat_dist,paste("../SocialInflationExpectation/_intermediate/dist_",l,".csv",sep=""))


##############################################
##### 3. Prep CPI and Expected Inflation #####
##############################################

# Both need to be adjusted in units (2-digit) and reshaped

# Read in Inflation expectations balance statistic in 2-digit
# Already aggregated as micro-data is not public
dat_inflex <- read_xlsx(dir.inflexp) %>%
  rename(date = TOT)

# need the names to filter out columns that need to be stacked
countries <- colnames(dat_inflex)[2:length(dat_inflex)]

# reshape in the required format
dat_inflex <- data.frame(dat_inflex[,"date"],stack(dat_inflex[,countries])) %>%
  rename(loc = ind, inflexp_median = values) %>%
  filter(!is.na(inflexp_median)) %>%
  mutate(year = substr(date,1,4), month = substr(date,6,7))

# we want to get the date format in quarters as is for cpi data (this is in quarters as well but written out monthly)
for (i in 1:nrow(dat_inflex)) {
  if (dat_inflex$month[i] == "01") {
    dat_inflex$quarter[i] = 1
  }
  else if (dat_inflex$month[i] == "02") {
    dat_inflex$quarter[i] = 1
  }
  else if (dat_inflex$month[i] == "03") {
    dat_inflex$quarter[i] = 1
  }
  else if (dat_inflex$month[i] == "04") {
    dat_inflex$quarter[i] = 2
  }
  else if (dat_inflex$month[i] == "05") {
    dat_inflex$quarter[i] = 2
  }
  else if (dat_inflex$month[i] == "06") {
    dat_inflex$quarter[i] = 2
  }
  else if (dat_inflex$month[i] == "07") {
    dat_inflex$quarter[i] = 3
  }
  else if (dat_inflex$month[i] == "08") {
    dat_inflex$quarter[i] = 3
  }
  else if (dat_inflex$month[i] == "09") {
    dat_inflex$quarter[i] = 3
  }
  else if (dat_inflex$month[i] == "10") {
    dat_inflex$quarter[i] = 4
  }
  else if (dat_inflex$month[i] == "11") {
    dat_inflex$quarter[i] = 4
  }
  else if (dat_inflex$month[i] == "12") {
    dat_inflex$quarter[i] = 4
  }
}

dat_inflex <- dat_inflex %>%
  mutate(date = paste(year,quarter,sep = "-")) %>%
  select(-year,-month,-quarter)

#save
write_csv(dat_inflex,paste("../SocialInflationExpectation/_intermediate/inflexp_",l,"_",s,".csv",sep=""))


# Read in Inflation data
dat_cpi <- read_xlsx(dir.cpi) %>%
  rename(alpha3 = "Country Code") %>%
  inner_join(dat_geo) %>%
  select(-alpha3,-"IMF Country Code",-"Country Name",-num) %>%
  rename(loc = alpha2)

# need the names to filter out columns that need to be stacked
quarters <- colnames(dat_cpi)[1:length(dat_cpi)-1]

# reshape in the required format
dat_cpi <- data.frame(dat_cpi[,"loc"], stack(dat_cpi[,quarters])) %>%
  rename(date = ind, cpi_inflation = values) %>%
  filter(!is.na(cpi_inflation)) %>%
  mutate(year = substr(date,1,4), quarter = substr(date,5,5)) %>%
  mutate(date = paste(year,quarter,sep = "-")) %>%
  select(-year,-quarter)

#save
write_csv(dat_cpi,paste("../SocialInflationExpectation/_intermediate/cpi_",l,".csv",sep=""))





###################################
##### 5. Prep Covariates data #####
###################################

# This still needs to be worked on!

# Read in Covariates, goal: assign 2000 cz to all areas in covariates
dat_covariates <- read_xlsx(dir.covariates) %>%
  rename(cz1990 = cz) %>% # rename for clarity
  mutate(cz1990 = as.character(cz1990)) %>%
  # left_join with geo data to use cz2000 instead of cz1990
  left_join(dat_geo,by = "cz1990") %>%
  rename(loc = cz2000)

write_csv(dat_covariates,paste("../SocialInflationExpectation/_intermediate/covariates_",l,".csv",sep=""))


