# Purpose: Create time series of US states inflation data
# Inputs: 
#     _input/East North Central.xlsx
#     _input/East South Central.xlsx
#     _input/Middle Atlantic.xlsx
#     _input/Mountain.xlsx
#     _input/New England.xlsx
#     _input/Pacific.xlsx
#     _input/South Atlantic.xlsx
#     _input/West North Central.xlsx
#     _input/West North Central.xlsx
# Outputs: 
#     _intermediate/cpi_cz2000-timeseries.csv
# Date: 23/07/2022
# Steps:
#     1. Regional inflation time series
#     2. Match commuting zones to inflation

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
        mutate(year = rep(i,12), Region = r)
      dat_pivot <- mutate(dat_pivot,Date = with(dat_pivot,paste(year,month,sep="-"))) %>%
        mutate(Date = parse_date(Date,"%Y-%b")) %>%
        select(Date, Region, CPI)  
    } else {
      dat_2 <- filter(dat,Year==i) %>%
        # Make the data long
        gather(month, CPI) %>% 
        tail(-1) %>%
        mutate(year = rep(i,12), Region = r)
      dat_2 <- mutate(dat_2,Date = with(dat_2,paste(year,month,sep="-"))) %>%
        mutate(Date = parse_date(Date,"%Y-%b")) %>%
        select(Date, Region, CPI)   
      dat_pivot <- rbind(dat_pivot,dat_2) %>%
        filter(!is.na(CPI))
      rm(dat_2)
    }
  }
  dat_pivot <- mutate(dat_pivot, CPI_mom = (CPI-dplyr::lag(CPI))/dplyr::lag(CPI) * 100) %>%
    filter(!is.na(CPI_mom))
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



#################################################
##### 2. Match commuting zones to inflation #####
#################################################


# 2.1 Match State and CZ
########################

# Read in table of FIPS, 2000 cz and 1990 cz
dat_geo <- read_csv2(dir.geo) %>%
  # select only fips and 2000 commuting zones
  select(fips = FIPS, cz2000 = "Commuting Zone ID, 2000") %>%
  # create state index from fips
  mutate( State = as.numeric(substr(fips,1,2))) %>%
  select(State, cz2000) %>%
  arrange(cz2000,State) %>%
  unique


# 2.2 Match Region and State
############################

# create table of states and regions
reg_states_pivot <-reg_states %>% 
  gather(Region, States) %>%
  filter(!is.na(States))


# 2.3 Match inflation to CZ
###########################

# join to get state-inflation timeseries
cpi <- rbind(cpi_e_n_c,cpi_e_s_c,cpi_m,cpi_m_a,cpi_n_e,cpi_p,cpi_s_a,cpi_w_n_c,cpi_w_s_c) %>%
  left_join(reg_states_pivot) %>%
  select(CPI,CPI_mom, Date, State = States, Region) %>%
  left_join(dat_geo) %>%
  #select(CPI,CPI_mom, Date, cz2000) %>%
  arrange(cz2000,Date) %>%
  # Take mean as one commuting zone can be in several states
  group_by(cz2000,Date) %>%
  mutate(cpi_mean = mean(CPI)) %>%
  mutate(cpi_mom_mean = mean(CPI_mom)) %>%
  ungroup %>%
  select(cpi_mean, cpi_mom_mean, Date, cz2000) %>%
  unique

write_csv(cpi, "../SocialInflationExpectation/_intermediate/cpi_cz2000-timeseries.csv")

