# Purpose: Create time series of US states inflation data
# Inputs: 
# Outputs: 
# Date: 23/07/2022
# Steps:
#     1. Regional inflation time series
#     2. Match commuting zones to regions

library(tidyverse)
library(lubridate)
library(readxl)

#### FILL IN THIS LINE BEFORE RUNNING ####


# Geographic IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.geo <- "../SocialInflationExpectation/_input/cz00_eqv_v1.csv"

# Regional CPI data
# https://data.bls.gov/cgi-bin/dsrv
dir.e_n_c <- "../SocialInflationExpectation/_input/East North Central.xlsx"
dir.e_s_c <- "../SocialInflationExpectation/_input/East South Central.xlsx"
dir.m_a <- "../SocialInflationExpectation/_input/Middle Atlantic.xlsx"
dir.m <- "../SocialInflationExpectation/_input/Mountain.xlsx"
dir.n_e <- "../SocialInflationExpectation/_input/New England.xlsx"
dir.p <- "../SocialInflationExpectation/_input/Pacific.xlsx"
dir.s_a <- "../SocialInflationExpectation/_input/South Atlantic.xlsx"
dir.w_n_c <- "../SocialInflationExpectation/_input/West North Central.xlsx"
dir.w_s_c <- "../SocialInflationExpectation/_input/West North Central.xlsx"


############################################
##### 1. Regional inflation timeseries #####
############################################

# load states and regions to match
reg_states <- read_xlsx("../SocialInflationExpectation/_input/Regions to states.xlsx") 


# A little function to shape the time series inflation data 
shape_time_series <- function(path,r){
  
  dat <- read_xlsx(path)
    
  for (i in dat$Year) {
    if (i==dat$Year[1]){
      dat_pivot <- filter(dat,Year==i) %>%
        # Make the data long
        gather(month, CPI) %>% 
        tail(-1) %>%
        mutate(year = rep(i,12), Region = r) %>%
        mutate(Date = with(dat_pivot,paste(year,month,sep="-"))) %>%
        mutate(Date = parse_date(Date,"%Y-%b")) %>%
        select(CPI, Date, Region)
        
    } else {
      dat_2 <- filter(dat,Year==i) %>%
        # Make the data long
        gather(month, CPI) %>% 
        tail(-1) %>%
        mutate(year = rep(i,12), Region = r) %>%
        mutate(Date = with(dat_pivot,paste(year,month,sep="-"))) %>%
        mutate(Date = parse_date(Date,"%Y-%b")) %>%
        select(CPI, Date, Region)   
      dat_pivot <- rbind(dat_pivot,dat_2) %>%
        filter(!is.na(CPI))
      rm(dat_2)
    }
  }
  dat_pivot
}
  
    
# Run for all regions
cpi_e_n_c <- shape_time_series(dir.e_n_c, "East North Central")
cpi_e_s_c <- shape_time_series(dir.e_s_c, "East South Central")
cpi_m <- shape_time_series(dir.m, "Mountain")
cpi_m_a <- shape_time_series(dir.m_a, "Middle Atlantic")
cpi_n_e <- shape_time_series(dir.m, "New England")
cpi_p <- shape_time_series(dir.m, "Pacific")
cpi_s_a <- shape_time_series(dir.m, "South Atlantic")
cpi_w_n_c <- shape_time_series(dir.w_n_c, "West North Central")
cpi_w_s_c <- shape_time_series(dir.w_s_c, "West South Central")



###############################################
##### 2. Match commuting zones to regions #####
###############################################

# Read in table of FIPS, 2000 cz and 1990 cz
dat_geo <- read_csv2(dir.geo) %>%
  # select only fips and 2000 commuting zones
  select(fips = FIPS, cz2000 = "Commuting Zone ID, 2000") %>%
  mutate( state = as.numeric(substr(dat_geo$fips,1,2)))







