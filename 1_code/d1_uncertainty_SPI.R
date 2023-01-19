# Purpose: Are biased commuting zones connected?
# Plan: Correlate bias and social connectedness to bias
#
# Inputs: 
#     _intermediate/sci.tsv
#     _intermediate/inflexp.csv
#     _intermediate/cpi.csv
#     _input/dist.xlsx
#
# Outputs: 
#
#
# Date: 28/09/2022
# written by: Lovisa Reiche
#
# Steps:
#     1. Specify data location
#     2. Compute bias
#     3. Create Social Proximity to Bias
#     4. Create Physical Proximity to Bias
#     5. Correlate

# Housekeeping
rm(list = ls())
library(tidyverse)
library(pracma) # for uniq accumarray
library(plm) # for panel linear regression
library(stargazer) # for writing regression tables




########################
##### Make Choices #####
########################


# Do you want to look at US counties or EU countries?
l <- "US"

# Which survey?
s <- "FRBNY"



####################################
##### 1. Specify data location #####
####################################



# SCI 
# created in pre file
dir.sci <- paste("../SocialInflationExpectation/_intermediate/sci_",l,".tsv",sep="")

# Distance
# created in pre file
dir.dist <- paste("../SocialInflationExpectation/_intermediate/dist_",l,".csv",sep="")

# Inflation expectations
# created in pre file
dir.uncertainty <- paste("../SocialInflationExpectation/_intermediate/inflexp_uncertainty_",l,"_",s,".csv",sep="")

# Inflation
# created in pre file
dir.cpi <- paste("../SocialInflationExpectation/_intermediate/cpi_",l,".csv",sep="")

# Read in covariates
# Built in pre file
dat_covariates <- read_csv(paste("../SocialInflationExpectation/_intermediate/covariates_",l,".csv", sep="")) %>% 
  select(loc, med_hhinc, poor_share, housing_cost) 

# for the US sample need to average as otherwise in counties
if (l == "US") {
  U = uniq(dat_covariates$loc)
  a1 = accumarray(U$n,dat_covariates$med_hhinc, func = mean)
  a2 = accumarray(U$n,dat_covariates$poor_share, func = mean)
  a3 = accumarray(U$n,dat_covariates$housing_cost, func = mean)
  
  dat_covariates <- tibble(loc = U$b, poor_share = a2, med_hhinc = a1, housing_cost = a3)
}

# Read in outwardness
# Built in a1_county_data_collect
dat_outward <- read_csv(paste("../SocialInflationExpectation/_intermediate/outwardness_",l,".csv", sep=""))


# Collect all data

dat_uncertainty <- read_csv(dir.uncertainty)
dat_sci <- read_tsv(dir.sci)
dat_cpi <- read_csv(dir.cpi) 
dat_dist <- read_csv(dir.dist)



#####################################################
##### 2. Create Social Proximity to Uncertainty #####
#####################################################



spu <- dat_sci %>%
  # Join uncertainty for foreign cz
  inner_join(dat_uncertainty, by=c("fr_loc"="loc")) %>%
  rename(iqr_median_fr = iqr_median) %>%
  rename(inflexp_sd_fr = inflexp_sd) %>% 
  na.omit %>%
  # Join uncertainty for local cz by loc and date
  inner_join(dat_uncertainty, by=c("user_loc"="loc","date")) %>%
  rename(iqr_median_user = iqr_median) %>%
  rename(inflexp_sd_user = inflexp_sd) %>% 
  na.omit %>%
  # Collapse and make the final weighted measure
  group_by(user_loc, date) %>% 
  # compute spu using differences
  mutate(SPU_iqr = sum(abs(iqr_median_fr-iqr_median_user)*share_sci)) %>%
  mutate(SPU_sd = sum(abs(inflexp_sd_fr-inflexp_sd_user)*share_sci)) %>%
  summarise(SPU_iqr,SPU_sd) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(spu, paste("../SocialInflationExpectation/_intermediate/SPU_",l,".csv", sep = ""))



################################################
##### 4. Create Physical Proximity to Bias #####
################################################


ppu <- dat_dist %>%
  summarise(user_loc , fr_loc , dist) %>%
  # Join uncertainty data
  inner_join(dat_uncertainty, by=c("fr_loc"="loc")) %>% 
  rename(iqr_median_fr = iqr_median) %>%
  rename(inflexp_sd_fr = inflexp_sd) %>% 
  na.omit %>%
  # Join bias for local cz by date
  inner_join(dat_uncertainty, by=c("user_loc"="loc","date")) %>%
  rename(iqr_median_user = iqr_median) %>%
  rename(inflexp_sd_user = inflexp_sd) %>% 
  na.omit %>%
  # Collapse and make the final weighted measure
  group_by(user_loc, date) %>% 
  mutate(PPU_iqr = sum(abs(iqr_median_fr-iqr_median_user)/(1+dist))) %>%
  mutate(PPU_sd = sum(abs(inflexp_sd_fr-inflexp_sd_user)/(1+dist))) %>%
  summarise(PPU_iqr,PPU_sd) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(ppu, paste("../SocialInflationExpectation/_intermediate/PPU_",l,".csv", sep = ""))



#########################
##### 5. Regression #####
#########################



regress_dat <- dat_uncertainty %>% 
  # Join with social and physical distance to experiences inflation
  left_join(ppu, by=c("loc"="user_loc", "date"="date")) %>% 
  left_join(spu, by=c("loc"="user_loc", "date"="date")) %>% 
  # Join with covariates (!!! No time variation here !!!)
  left_join(dat_covariates) %>% 
  # Join with outwardness (!!! No time variation here !!!)
  left_join(dat_outward, by=c("loc"="user_loc")) %>% 
  # Join with cpi (!!! Only availabe in 2018, on regional not cz basis !!!)
  left_join(dat_cpi) %>%
  # compute lag
  mutate(SPU_iqr_lag = dplyr::lag(SPU_iqr)) %>%
  mutate(PPU_iqr_lag = dplyr::lag(PPU_iqr)) %>%
  mutate(SPU_sd_lag = dplyr::lag(SPU_sd)) %>%
  mutate(PPU_sd_lag = dplyr::lag(PPU_sd)) %>%
  mutate(inflexp_sd_lag = dplyr::lag(inflexp_sd)) %>%
  mutate(iqr_median_lag = dplyr::lag(iqr_median)) %>%
  mutate(cpi_inflation_lag = dplyr::lag(cpi_inflation)) %>%
  # compute how much is adjusted for the next period
  mutate(inflexp_sd_chg = abs(inflexp_sd - inflexp_sd_lag)) %>%
  mutate(iqr_median_chg = abs(iqr_median - iqr_median_lag)) %>%
  filter(!is.na(iqr_median_chg)) %>%
  filter(!is.na(inflexp_sd_chg))

write_csv(regress_dat, paste("../SocialInflationExpectation/_output/regress_dat_",l,".csv", sep = ""))

# Create time dummies

dates <- unique(regress_dat$date)

T = data.frame(matrix(
  vector(), nrow(regress_dat), length(dates), dimnames=list(c(), dates)),
  stringsAsFactors=F)

for(i in 1:length(dates)){
  
  T[,i] <- ifelse(regress_dat$date == dates[i],1,0)
  
}

# create regression set with time dummies
regress_dat <- bind_cols(regress_dat,T)


# Table 1: Individual Interquartile Range
#########################################


tefe1 <- plm(iqr_median_chg ~ SPU_iqr_lag + PPU_iqr_lag + cpi_inflation_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe2 <- plm(iqr_median_chg ~ SPU_iqr_lag + PPU_iqr_lag + cpi_inflation_lag + iqr_median_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe3 <- plm(iqr_median_chg ~ SPU_iqr_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe4 <- plm(iqr_median_chg ~ PPU_iqr_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")


# Problem: with EU data we have 444 different dates, for time dummies I need to write them out which is not efficient. Need to find a better way for this.
# Solution: Write out the formula like this so it works in both contexts. Need to be careful with the order of the variables though!
f_time1 <- as.formula(paste('iqr_median_chg ~', paste(colnames(regress_dat)[c(14,15,20,9:12,23:ncol(regress_dat))], collapse='+')))
f_time2 <- as.formula(paste('iqr_median_chg ~', paste(colnames(regress_dat)[c(14,15,20,19,9:12,23:ncol(regress_dat))], collapse='+')))
f_time3 <- as.formula(paste('iqr_median_chg ~', paste(colnames(regress_dat)[c(14,23:ncol(regress_dat))], collapse='+')))
f_time4 <- as.formula(paste('iqr_median_chg ~', paste(colnames(regress_dat)[c(15,23:ncol(regress_dat))], collapse='+')))

tepo1 <- plm(f_time1, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

tepo2 <- plm(f_time2, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

tepo3 <- plm(f_time3, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

tepo4 <- plm(f_time4, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")


# save stargazer output in tex file
writeLines(capture.output(stargazer(tefe1,tefe2,tefe3,tefe4,tepo1,tepo2,tepo3,tepo4,title="Regression Results using Change in Individual Uncertainty",align=TRUE, label = "tab:uncertainty_iqr", model.names = TRUE)), paste("../SocialInflationExpectation/_output/Uncertainty_iqr_",l,".tex",sep = ""))



# Table 2: Aggregate Standard Deviation
#######################################


tefe1 <- plm(inflexp_sd_chg ~ SPU_sd_lag + PPU_sd_lag + cpi_inflation_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe2 <- plm(inflexp_sd_chg ~ SPU_sd_lag + PPU_sd_lag + cpi_inflation_lag + inflexp_sd_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe3 <- plm(inflexp_sd_chg ~ SPU_sd_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe4 <- plm(inflexp_sd_chg ~ PPU_sd_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")


# Problem: with EU data we have 444 different dates, for time dummies I need to write them out which is not efficient. Need to find a better way for this.
# Solution: Write out the formula like this so it works in both contexts. Need to be careful with the order of the variables though!
f_time1 <- as.formula(paste('inflexp_sd_chg ~', paste(colnames(regress_dat)[c(16,17,20,9:12,23:ncol(regress_dat))], collapse='+')))
f_time2 <- as.formula(paste('inflexp_sd_chg ~', paste(colnames(regress_dat)[c(16,17,20,18,9:12,23:ncol(regress_dat))], collapse='+')))
f_time3 <- as.formula(paste('inflexp_sd_chg ~', paste(colnames(regress_dat)[c(16,23:ncol(regress_dat))], collapse='+')))
f_time4 <- as.formula(paste('inflexp_sd_chg ~', paste(colnames(regress_dat)[c(17,23:ncol(regress_dat))], collapse='+')))

tepo1 <- plm(f_time1, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

tepo2 <- plm(f_time2, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

tepo3 <- plm(f_time3, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

tepo4 <- plm(f_time4, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")


# save stargazer output in tex file
writeLines(capture.output(stargazer(tefe1,tefe2,tefe3,tefe4,tepo1,tepo2,tepo3,tepo4,title="Regression Results using Change in Aggregate Uncertainty",align=TRUE, label = "tab:uncertainty_sd", model.names = TRUE)), paste("../SocialInflationExpectation/_output/Uncertainty_sd_",l,".tex",sep = ""))
