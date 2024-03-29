# Purpose: Make two "weighted" measures, 
#           Social Proximity to Inflation and Physical Proximity to Inflation
#
# Inputs: 
#     _intermediate/sci.tsv
#     _intermediate/cpi.csv
#     _intermediate/inflexp.csv
#     _input/dist.xlsx
#
# Outputs: 
#     _intermediate/spi_median.csv
#     _intermediate/spi_mean.csv
#     _intermediate/ppi_median.csv
#     _intermediate/ppi_mean.csv
#
# Date: 18/09/2022
# written by: Lovisa Reiche
#
# Steps:
#     1. Specify data location
#     2. Create Social Proximity to Inflation
#     3. Create Physical Proximity to Inflation

' Housekeeping'
rm(list = ls())
library(tidyverse)


########################
##### Make Choices #####
########################


# Do you want to look at US counties or EU countries?
l <- "EU"

# Which survey?
s <- "ECFIN"



####################################
##### 1. Specify data location #####
####################################



# SCI 
# created in pre file
dir.sci <- paste("../code/_intermediate/sci_",l,".tsv",sep="")
    
# Distance
# created in pre file
dir.dist <- paste("../code/_intermediate/dist_",l,".csv",sep="")

# Inflation expectations
# created in pre file
dir.inflexp <- paste("../code/_intermediate/inflexp_",l,"_",s,".csv",sep="")

# Inflation
# created in pre file
dir.cpi <- paste("../code/_intermediate/cpi_",l,".csv",sep="")


# Collect all data

dat_inflexp <- read_csv(dir.inflexp)
dat_sci <- read_tsv(dir.sci)
dat_cpi <- read_csv(dir.cpi) 
dat_dist <- read_csv(dir.dist)



###################################################
##### 2. Create Social Proximity to Inflation #####
###################################################


  spi_median <- dat_sci %>%
    # Join in the Inflation data for foreign cz
    inner_join(dat_inflexp, by=c("fr_loc"="loc")) %>%
    rename(inflexp_median_fr = inflexp_median) %>%
    filter(!is.na(inflexp_median_fr)) %>%
    # Join actual inflation data for foreign cz'
    inner_join(dat_cpi, by=c("fr_loc"="loc", "date")) %>%
    rename(cpi_inflation_fr = cpi_inflation) %>%
    # Join inflation exp for local cz by date
    inner_join(dat_inflexp, by=c("user_loc"="loc","date")) %>%
    rename(inflexp_median_user = inflexp_median) %>%
    filter(!is.na(inflexp_median_user)) %>%
    # Collapse and make the final weighted measure
    group_by(user_loc, date) %>% 
    # compute spi using differences
    mutate(SPI1 = sum((inflexp_median_fr)*share_sci)) %>%
    mutate(SPI2 = sum(abs(inflexp_median_fr-inflexp_median_user)*share_sci)) %>%
    summarise(SPI1, SPI2, SPI3 = sum((cpi_inflation_fr)*share_sci)) %>%
    distinct(.keep_all = TRUE) %>%
    ungroup
  
  write_csv(spi_median, paste("../SocialInflationExpectation/_intermediate/SPI_median_",l,".csv", sep = ""))


#####################################################
##### 3. Create Physical Proximity to Inflation #####
#####################################################


  dist_median <- dat_dist %>%
    summarise(user_loc , fr_loc , dist) %>%
    # Join in the Inflation data
    inner_join(dat_inflexp, by=c("fr_loc"="loc")) %>% 
    rename(inflexp_median_fr = inflexp_median) %>%
    filter(!is.na(inflexp_median_fr)) %>%
    # Join actual inflation data for foreign cz'
    inner_join(dat_cpi, by=c("fr_loc"="loc", "date")) %>%
    rename(cpi_inflation_fr = cpi_inflation) %>%
    # Join inflation exp for local cz by date
    inner_join(dat_inflexp, by=c("user_loc"="loc","date")) %>%
    rename(inflexp_median_user = inflexp_median) %>%
    filter(!is.na(inflexp_median_user)) %>%
    
    # Collapse and make the final weighted measure
    group_by(user_loc, date) %>% 
    mutate(PPI1 = sum((inflexp_median_fr)/(1+dist))) %>%
    mutate(PPI2 = sum(abs(inflexp_median_fr-inflexp_median_user)/(1+dist))) %>%
    summarise(PPI1, PPI2, PPI3 = sum((cpi_inflation_fr)/(1+dist))) %>%
    distinct(.keep_all = TRUE) %>%
    ungroup
  
  write_csv(dist_median, paste("../SocialInflationExpectation/_intermediate/PPI_median_",l,".csv", sep = ""))
