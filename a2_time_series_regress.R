# Purpose: Build the regression table for the time series analyses
#
# Inputs: 
#     _intermediate/inflexp_date_cz.csv
#     _intermediate/sci_weighted_inflation.csv
#     _intermediate/dist_weighted_inflation.csv
#     _intermediate/covariates.csv
#     _intermediate/cpi_cz2000-timeseries.csv
#     _intermediate/cpi_cz2000-timeseries_nakamura.csv
# Outputs: 
#     _output/time_series_regress_dat.csv
# Date: 21/07/22
# Steps:
#     1. Prep all data sources
#     2. Join all data sources
#     3. Panel regression

library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(pracma)
library(plm)
library(lmtest)

rm(list=ls())
####################################
##### 1. Prep all data sources #####
####################################


# Read in baseline inflation expectations data
# Built in a1_county_data_collect.R
dat_inflex <- read_csv("../SocialInflationExpectation/_intermediate/inflexp_date_cz.csv")

# Read in SCI-weighted inflation (Social Proximity to Inflation)
# Built in a1_county_data_collect.R
# sci_weighted_inflation_control <- read_csv("../SocialInflationExpectation/_intermediate/sci_weighted_inflation_control.csv") %>%
#   rename(sci_weighted_inflation_control=sci_weighted_inflation)
# sci_weighted_inflation <- read_csv("../SocialInflationExpectation/_intermediate/sci_weighted_inflation.csv")
SPI <- read_csv("../SocialInflationExpectation/_intermediate/SPI.csv")

# Read in distance-weighted inflation (Physical Proximity to Inflation)
# Built in a1_county_data_collect.R
# dist_weighted_inflation <- read_csv("../SocialInflationExpectation/_intermediate/dist_weighted_inflation.csv")
PPI <- read_csv("../SocialInflationExpectation/_intermediate/PPI.csv")

# Join the weighted cases measures
# weighted_inflation_dat <- dist_weighted_inflation %>% 
#   left_join(sci_weighted_inflation_control, by=c("cz1"="user_loc", "date"="date")) %>%
#   left_join(sci_weighted_inflation, by=c("cz1"="user_loc", "date"="date")) %>%
#   rename(cz2000 = cz1)


# Join the weighted cases measures
dat <- PPI %>% 
  left_join(SPI, by=c("cz1"="user_loc", "date"="date")) %>%
  rename(cz2000 = cz1)


### Choose covariates you want to control for ###

# Read in covariates
# Built in a1_county_data_collect.R
dat_covariates <- read_csv("../SocialInflationExpectation/_intermediate/covariates.csv") %>% 
  select(cz2000, med_hhinc2016, poor_share2010, rent_twobed2015) 

U = uniq(dat_covariates$cz2000)
a1 = accumarray(U$n,dat_covariates$med_hhinc2016, func = mean)
a2 = accumarray(U$n,dat_covariates$poor_share2010, func = mean)
a3 = accumarray(U$n,dat_covariates$rent_twobed2015, func = mean)

dat_covariates <- tibble(cz2000 = U$b, poor_share2010 = a2, med_hhinc2016 = a1, rent_twobed2015 = a3)


# Read in CPI data 
# Built in b1_create_state_cpi
dat_cpi <- read_csv("../SocialInflationExpectation/_intermediate/cpi_cz2000-timeseries_nakamura.csv") %>%
  #rename(date = Date) %>%
  arrange(cz2000, date) %>%
  unique

# Read in outwardness
# Built in a1_county_data_collect
dat_outward <- read_csv("../SocialInflationExpectation/_intermediate/outwardness.csv") %>%
  rename(cz2000 = user_loc)


#######################################
##### 2. Combine all data sources #####
#######################################

regress_dat <- dat_inflex %>% 
  # Join with social and physical distance to experiences inflation
  inner_join(dat) %>% 
  # Join with covariates (!!! No time variation here !!!)
  left_join(dat_covariates) %>% 
  # Join with outwardness (!!! No time variation here !!!)
  left_join(dat_outward) %>% 
  # Join with cpi (!!! Only availabe in 2018, on regional not cz basis !!!)
  inner_join(dat_cpi) 
  # name as in paper
  # rename(SPI = sci_weighted_inflation, SPI_control = sci_weighted_inflation_control, PPI = dist_weighted_inflation)


write_csv(regress_dat, "../SocialInflationExpectation/_output/time_series_regress_dat.csv")


###############################
##### 3. Panel regression #####
###############################


# 3.1. Estimate fixed effects and pooled ols
############################################

# estimate the fixed effects regression, no control with plm()
inflex_fe_mod_1 <- plm(inflexp_median ~ SPI, 
                    data = regress_dat,
                    index = c("cz2000", "date"), 
                    model = "within")
coeftest(inflex_fe_mod_1)

inflex_fe_mod_2 <- plm(inflexp_median ~ SPI + PPI + pi_mean, 
                     data = regress_dat,
                     index = c("cz2000", "date"), 
                     model = "within")
coeftest(inflex_fe_mod_2)

# estimate the pooled effects regression, no control with plm()
inflex_po_mod_1 <- plm(inflexp_median ~ SPI, 
                     data = regress_dat,
                     index = c("cz2000", "date"), 
                     model = "pooling")
coeftest(inflex_po_mod_1)

inflex_po_mod_2 <- plm(inflexp_median ~ SPI + PPI + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
                     data = regress_dat,
                     index = c("cz2000", "date"), 
                     model = "pooling")
coeftest(inflex_po_mod_2)


# 3.2. Plot scatters for check 
##############################

png("../SocialInflationExpectation/_intermediate/SPI.png", width = 500, height = 500)
plot(regress_dat$inflexp_median,regress_dat$SPI, xlab = "Median expectation", ylab="SPI")
dev.off()

png("../SocialInflationExpectation/_intermediate/PPI.png", width = 500, height = 500)
plot(regress_dat$inflexp_median,regress_dat$PPI, xlab = "Median expectation", ylab="PPI")
dev.off()


# 3.3. Write regression table 
#############################


#install.packages("stargazer")
library(stargazer)

dat <- data.frame(regress_dat) %>%
  select(inflexp_median,PPI,SPI,pi_mean, poor_share2010,med_hhinc2016,rent_twobed2015,outwardness)

stargazer(dat)

stargazer(inflex_fe_mod_1, inflex_fe_mod_2, inflex_po_mod_1, inflex_po_mod_2, title="Regression Results", align=TRUE)


# 3.4 Check correlation 
#######################

cor(regress_dat$PPI,regress_dat$SPI)


############################
##### 4. Further ideas #####
############################


# 4.1 Outwardness Ratio
#######################
# Split sample below and above outwardness mean

mo <- mean(dat_outward$outwardness)
# I noticed that the mean here is lower than the mean in the stargazer summary statistic. Could be because there it shows up multiple times per observation due to the time component...

# sample with outwardness below mean
sample1 <- regress_dat %>%
  filter(outwardness <= mo)

# sample with outwardness above mean
sample2 <- regress_dat %>%
  filter(outwardness > mo)

