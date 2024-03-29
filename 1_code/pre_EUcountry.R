# Purpose: Prepare the datasets such that they are in the required shape for the next steps
#
# Inputs: 
#     sci.tsv
#     covariates.xlsx
#     inflexp.xlsx
#     geo_match.xlsx
#     dist.xlsx
# Outputs: 
#     covariates.csv
#     sci.tsv
#     outwardness.csv
#     inflexp_date_cz.csv
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




rm(list=ls())
NAME <- 'pre_EUcountry' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'SocialInflationExpectations'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!


## -------
## Imports
## -------
### All the library imports go here

library(tidyverse) # general commands
library(readxl) # to read excel files
library(pracma) # for matlab functions (uniq,acumarray)
library(lubridate)



####################
##### Settings #####
####################

setwd(file.path(PROJECT_DIR, PROJECT))


# Do you want to look at US counties or EU countries?
l <- "EU"

# Which survey?
s <- "ECFIN"


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

if (!dir.exists(file.path(pipeline,'out',l))) {
  dir.create(file.path(pipeline,'out',l))
}

####################################
##### 1. Specify data location #####
####################################


#### FILL IN THESE LINEs BEFORE RUNNING ####

# SCI 
# https://data.humdata.org/dataset/social-connectedness-index'
dir.sci <- file.path('empirical', '0_data', 'external',l,'sci.tsv')

# Covariates
# https://ec.europa.eu/eurostat/web/main/data/database
dir.covariates <- file.path('empirical', '0_data', 'external',l,'covariates.xlsx')

# Inflation
#  Consumer and Business Surveys, © 1985-2022 European Commission (ECFIN)
dir.inflexp <- file.path('empirical', '0_data', 'external',l,'inflexp.xlsx')

# Geographic IDs
# List of ISO 3166 country codes
dir.geo <- file.path('empirical', '0_data', 'external',l,'geo_match.xlsx')

# Population
# https://data.worldbank.org/indicator/SP.POP.TOTL?locations=1W
dir.pop <- file.path('empirical', '0_data', 'external',l,'pop.xlsx')

# Distance
# Mayer, T. & Zignago, S. (2011), Notes on CEPII’s distances measures : the GeoDist Database, CEPII Working Paper 2011-25
# http://cepii.fr/cepii/en/bdd_modele/bdd_modele_item.asp?id=6
dir.dist <- file.path('empirical', '0_data', 'external',l,'dist.xlsx')


# Inflation
# https://thedocs.worldbank.org/en/doc/483541579976288664-0050022020/Inflation-data
dir.cpi <- file.path('empirical', '0_data', 'external',l,'inflation.xlsx')


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


write_tsv(dat_sci,file.path(pipeline, 'out',l, 'sci.tsv'))
write_csv(dat_outwardness,file.path(pipeline, 'out',l, 'outwardness.csv'))


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

write_csv(dat_dist,file.path(pipeline, 'out',l, 'dist.csv'))


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
  filter(!is.na(inflexp_median)) 
dat_inflex <- dat_inflex[,c(1,3,2)]

# set day to first of the month to align with other data
day(dat_inflex$date) <- 1

#save
write_csv(dat_inflex,file.path(pipeline, 'out',l, 'inflexp.csv'))


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
  arrange(loc,year,quarter)

dat_cpi <- data.frame(lapply(dat_cpi,rep,rep(3,nrow(dat_cpi))))

# ridiculously inefficient piece to add month  
for (i in 1:length(dat_cpi$quarter))
{
  if (i ==1)
  {
    dat_cpi$month[i] = 01
  }
  else if (i ==2)
  {
    dat_cpi$month[i] = 02
  }
  else if (i ==3)
  {
    dat_cpi$month[i] = 03
  }
  else if (i ==4)
  {
    dat_cpi$month[i] = 04
  }
  else if (i ==5)
  {
    dat_cpi$month[i] = 05
  }
  else if (i ==6)
  {
    dat_cpi$month[i] = 06
  }
  else if (i ==7)
  {
    dat_cpi$month[i] = 07
  }
  else if (i ==8)
  {
    dat_cpi$month[i] = 08
  }
  else if (i ==9)
  {
    dat_cpi$month[i] = 09
  }
  else if (i ==10)
  {
    dat_cpi$month[i] = 10
  }
  else if (i ==11)
  {
    dat_cpi$month[i] = 11
  }
  else if (i ==12)
  {
    dat_cpi$month[i] = 12
  }
  
  else if (dat_cpi$quarter[i] ==1 & dat_cpi$year[i] != dat_cpi$year[i-1])
  {
    dat_cpi$month[i] = 01
  }
  else if (dat_cpi$quarter[i] ==1 & dat_cpi$year[i] != dat_cpi$year[i-2])
  {
    dat_cpi$month[i] = 02
  }
  else if (dat_cpi$quarter[i] ==1 & dat_cpi$year[i] != dat_cpi$year[i-3])
  {
    dat_cpi$month[i] = 03
  }
  else if (dat_cpi$quarter[i] ==2 & dat_cpi$year[i] != dat_cpi$year[i-4])
  {
    dat_cpi$month[i] = 04
  }
  else if (dat_cpi$quarter[i] ==2 & dat_cpi$year[i] != dat_cpi$year[i-5])
  {
    dat_cpi$month[i] = 05
  }
  else if (dat_cpi$quarter[i] ==2 & dat_cpi$year[i] != dat_cpi$year[i-6])
  {
    dat_cpi$month[i] = 06
  }
  else if (dat_cpi$quarter[i] ==3 & dat_cpi$year[i] != dat_cpi$year[i-7])
  {
    dat_cpi$month[i] = 07
  }
  else if (dat_cpi$quarter[i] ==3 & dat_cpi$year[i] != dat_cpi$year[i-8])
  {
    dat_cpi$month[i] = 08
  }
  else if (dat_cpi$quarter[i] ==3 & dat_cpi$year[i] != dat_cpi$year[i-9])
  {
    dat_cpi$month[i] = 09
  }
  else if (dat_cpi$quarter[i] ==4 & dat_cpi$year[i] != dat_cpi$year[i-10])
  {
    dat_cpi$month[i] = 10
  }
  else if (dat_cpi$quarter[i] ==4 & dat_cpi$year[i] != dat_cpi$year[i-11])
  {
    dat_cpi$month[i] = 11
  }
  else if (dat_cpi$quarter[i] ==4 & dat_cpi$year[i] != dat_cpi$year[i-12])
  {
    dat_cpi$month[i] = 12
  }
}

dat_cpi <- dat_cpi %>%
  mutate(month = str_pad(as.character(month), 2, "left", "0")) %>%
  mutate(date = paste(year,month,sep="-")) %>%
  mutate(date = parse_date(date,"%Y-%m")) %>%
  select(-month,-year,-quarter)

#save
write_csv(dat_cpi,file.path(pipeline, 'out',l, 'cpi.csv'))



###################################
##### 5. Prep Covariates data #####
###################################


# Already in the right format

# Read in Covariates, goal: assign 2000 cz to all areas in covariates
dat_covariates <- read_xlsx(dir.covariates) %>%
  select(-GEO)

write_csv(dat_covariates,file.path(pipeline, 'out',l, 'covariates.csv'))


