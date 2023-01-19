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
# Date: 24/09/2022
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

# Read in covariates
# Built in pre file
dat_covariates <- read_csv(paste("../code/_intermediate/covariates_",l,".csv", sep="")) %>% 
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
dat_outward <- read_csv(paste("../code/_intermediate/outwardness_",l,".csv", sep=""))


# Collect all data

dat_inflexp <- read_csv(dir.inflexp)
dat_sci <- read_tsv(dir.sci)
dat_cpi <- read_csv(dir.cpi) 
dat_dist <- read_csv(dir.dist)



###########################
##### 2. Compute Bias #####
###########################



dat_bias <- dat_cpi %>%
  group_by(loc) %>%
  mutate(cpi_lead = dplyr::lead(cpi_inflation,12)) %>%
  ungroup %>%
  filter(!is.na(cpi_lead)) %>%
  inner_join(dat_inflexp) %>%
  # bias is the difference between expectations and actual inflation (t+12)
  mutate(bias = inflexp_median - cpi_lead) %>%
  select(loc,date,bias,inflexp_median)



##############################################
##### 3. Create Social Proximity to Bias #####
##############################################



spb <- dat_sci %>%
  # Join bias for foreign cz
  inner_join(dat_bias, by=c("fr_loc"="loc")) %>%
  rename(bias_fr = bias) %>%
  filter(!is.na(bias_fr)) %>%
  # Join bias for local cz by loc and date
  inner_join(dat_bias, by=c("user_loc"="loc","date")) %>%
  rename(bias_user = bias) %>%
  filter(!is.na(bias_user)) %>%
  # Collapse and make the final weighted measure
  group_by(user_loc, date) %>% 
  # compute spi using actual and differences
  mutate(SPB2 = sum(abs(bias_fr-bias_user)*share_sci)) %>%
  summarise(SPB2) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(spb, paste("../code/_intermediate/SPB_",l,".csv", sep = ""))



################################################
##### 4. Create Physical Proximity to Bias #####
################################################


ppb <- dat_dist %>%
  summarise(user_loc , fr_loc , dist) %>%
  # Join bias data
  inner_join(dat_bias, by=c("fr_loc"="loc")) %>% 
  rename(bias_fr = bias) %>%
  filter(!is.na(bias_fr)) %>%
  # Join bias for local cz by date
  inner_join(dat_bias, by=c("user_loc"="loc","date")) %>%
  rename(bias_user = bias) %>%
  filter(!is.na(bias_user)) %>%
  # Collapse and make the final weighted measure
  group_by(user_loc, date) %>% 
  mutate(PPB2 = sum(abs(bias_fr-bias_user)/(1+dist))) %>%
  summarise(PPB2) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup

write_csv(ppb, paste("../code/_intermediate/PPB_",l,".csv", sep = ""))



#########################
##### 5. Regression #####
#########################



regress_dat <- dat_bias %>% 
  # Join with social and physical distance to experiences inflation
  left_join(ppb, by=c("loc"="user_loc", "date"="date")) %>% 
  left_join(spb, by=c("loc"="user_loc", "date"="date")) %>% 
  # Join with covariates (!!! No time variation here !!!)
  left_join(dat_covariates) %>% 
  # Join with outwardness (!!! No time variation here !!!)
  left_join(dat_outward, by=c("loc"="user_loc")) %>% 
  # Join with cpi (!!! Only availabe in 2018, on regional not cz basis !!!)
  left_join(dat_cpi) %>%
  # compute lag
  mutate(bias_lag = dplyr::lag(bias)) %>%
  mutate(SPB_lag = dplyr::lag(SPB2)) %>%
  mutate(PPB_lag = dplyr::lag(PPB2)) %>%
  mutate(cpi_inflation_lag = dplyr::lag(cpi_inflation)) %>%
  mutate(inflexp_median_lag = dplyr::lag(inflexp_median)) %>%
  # compute how much is adjusted for the next period
  mutate(bias_chg = abs(bias - bias_lag)) %>%
  filter(!is.na(bias_chg))


write_csv(regress_dat, paste("../code/_output/regress_dat_",l,".csv", sep = ""))

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


# Table 1: Difference and Lag computation
#########################################


tefe1 <- plm(bias_chg ~ SPB_lag + PPB_lag + cpi_inflation_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe2 <- plm(bias_chg ~ SPB_lag + PPB_lag + cpi_inflation_lag + inflexp_median_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")


tefe3 <- plm(bias_chg ~ SPB_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

tefe4 <- plm(bias_chg ~ PPB_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")


# Problem: with EU data we have 444 different dates, for time dummies I need to write them out which is not efficient. Need to find a better way for this.
# Solution: Write out the formula like this so it works in both contexts. Need to be careful with the order of the variables though!
f_time1 <- as.formula(paste('bias_chg ~', paste(colnames(regress_dat)[c(13:15,7:10,18:ncol(regress_dat))], collapse='+')))
f_time2 <- as.formula(paste('bias_chg ~', paste(colnames(regress_dat)[c(13:16,7:10,18:ncol(regress_dat))], collapse='+')))
f_time3 <- as.formula(paste('bias_chg ~', paste(colnames(regress_dat)[c(13,18:ncol(regress_dat))], collapse='+')))
f_time4 <- as.formula(paste('bias_chg ~', paste(colnames(regress_dat)[c(14,18:ncol(regress_dat))], collapse='+')))

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
writeLines(capture.output(stargazer(tefe1,tefe2,tefe3,tefe4,tepo1,tepo2,tepo3,tepo4,title="Regression Results using Chnage in Bias and Differencing",align=TRUE, label = "tab:regbias2", model.names = TRUE)), paste("../code/_output/Bias2_",l,".tex",sep = ""))


