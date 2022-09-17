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
library(pracma) # for uniq accumarray
library(plm) # for panel linear regression
library(psychTools) # for writing latex
library(stargazer) # for writing regression tables

rm(list=ls())
####################################
##### 1. Prep all data sources #####
####################################


# Read in baseline inflation expectations data
# Built in a1_county_data_collect.R
dat_inflex <- read_csv("../SocialInflationExpectation/_intermediate/inflexp_date_cz.csv")

# choose mean or median computation of SPI and PPI here
c <- "median"

# Read in SCI-weighted inflation (Social Proximity to Inflation)
# Built in a1_county_data_collect.R
SPI <- read_csv(paste("../SocialInflationExpectation/_intermediate/SPI_",c,".csv",sep=""))

# Read in distance-weighted inflation (Physical Proximity to Inflation)
# Built in a1_county_data_collect.R
# dist_weighted_inflation <- read_csv("../SocialInflationExpectation/_intermediate/dist_weighted_inflation.csv")
PPI <- read_csv(paste("../SocialInflationExpectation/_intermediate/PPI_",c,".csv",sep=""))


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
  arrange(cz2000, date) %>%
  unique %>%
  select(cz2000,date,pi_mean) %>%
  rename(state_inflation = pi_mean)

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
  inner_join(dat_cpi) %>%
  # compute l
  mutate(inflexp_lead = dplyr::lead(inflexp_median)) %>%
  mutate(inflexp_lag = dplyr::lag(inflexp_median)) %>%
  mutate(inflexp_lead = dplyr::lead(inflexp_median)) %>%
  mutate(SPI_lag = dplyr::lag(SPI2)) %>%
  mutate(PPI_lag = dplyr::lag(PPI2)) %>%
  mutate(state_inflation_lag = dplyr::lag(state_inflation)) %>%
  # compute how much is adjusted for the next period
  mutate(inflexp_chg_lead = inflexp_lead - inflexp_median) %>%
  mutate(inflexp_chg_lag = inflexp_median - inflexp_lag) %>%
  #filter(!is.na(inflexp_lead)) %>%
  filter(!is.na(inflexp_lag))


write_csv(regress_dat, "../SocialInflationExpectation/_output/time_series_regress_dat.csv")


cor2latex(regress_dat[,c(3,7,10,12:16,23)],use = "pairwise", method="pearson", adjust="holm",stars=FALSE,
          digits=2,rowlabels=TRUE,lower=TRUE,apa=TRUE,short.names=TRUE,
          font.size ="scriptsize", heading="Correlation",
          caption="cor2latex",label="tab:cor",silent=FALSE,file=NULL,append=FALSE,cut=0,big=0)


###############################
##### 3. Panel regression #####
###############################


# we choose to use computation 2
# we are interested in the effect of social and physical proximity to inflation on how consumers adjust


# Summary Statistics
####################

dat <- data.frame(regress_dat) %>%
  select(inflexp_chg_lead,inflexp_chg_lag,inflexp_median,SPI2,PPI2,state_inflation, poor_share2010,med_hhinc2016,rent_twobed2015,outwardness)

stargazer(dat)


# Table 1: Fixed Effects Specifications
#######################################

# all
#fe1 <- plm(inflexp_chg_lead ~ SPI2 + PPI2 + state_inflation + inflexp_median, data = regress_dat,index = c("cz2000", "date"), model = "within")
fe1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + inflexp_lag, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "within")
# with time fixed effects
# tefe1 <- plm(inflexp_chg ~ SPI2 + PPI2 + state_inflation + inflexp_median, data = regress_dat,index = c("cz2000", "date"), model = "within",effect = "twoways")
tefe1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + inflexp_lag, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "within",
           effect = "twoways")

# no inflexp median
#fe2 <- plm(inflexp_chg ~ SPI2 + PPI2 + state_inflation, data = regress_dat,index = c("cz2000", "date"), model = "within")
fe2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "within")
# with time fixed effects
#tefe2 <- plm(inflexp_chg ~ SPI2 + PPI2 + state_inflation, data = regress_dat,index = c("cz2000", "date"), model = "within",effect = "twoways")
tefe2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "within",
             effect = "twoways")

# only SPI
#fe3 <- plm(inflexp_chg ~ SPI2, data = regress_dat,index = c("cz2000", "date"), model = "within")
fe3 <- plm(inflexp_chg_lag ~ SPI_lag, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "within")
# with time fixed effects
#tefe3 <- plm(inflexp_chg ~ SPI2, data = regress_dat,index = c("cz2000", "date"), model = "within",effect = "twoways")
tefe3 <- plm(inflexp_chg_lag ~ SPI_lag, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "within",
             effect = "twoways")

# only PPI
#fe4 <- plm(inflexp_chg ~ PPI2,  data = regress_dat, index = c("cz2000", "date"),  model = "within")
fe4 <- plm(inflexp_chg_lag ~ PPI_lag, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "within")
# with time fixed effects
#tefe4 <- plm(inflexp_chg ~ PPI2,  data = regress_dat, index = c("cz2000", "date"),  model = "within", effect = "twoways")
tefe4 <- plm(inflexp_chg_lag ~ PPI_lag, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "within",
             effect = "twoways")


stargazer(fe1,tefe1,fe2,tefe2,fe3,tefe3,fe4,tefe4,title="Fixed Effects Regression Results",align=TRUE, label = "tab:regfe", model.names = TRUE)


# Table 2: Pooled OLS Specification
###################################

# Create time dummies

dates <- unique(regress_dat$date)

T = data.frame(matrix(
  vector(), 5802, 55, dimnames=list(c(), dates)),
  stringsAsFactors=F)

for(i in 1:length(dates)){
  
  T[,i] <- ifelse(regress_dat$date == dates[i],1,0)
  
}

# create regression set with time dummies
regress_dat <- bind_cols(regress_dat,T)

# all
#po1 <- plm(inflexp_chg ~ SPI2 + PPI2 + state_inflation + inflexp_median + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness,  data = regress_dat, index = c("cz2000", "date"),   model = "pooling")
po1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + inflexp_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "pooling")
# with time fixed effects
#tepo1 <- plm(inflexp_chg ~ SPI2 + PPI2 + state_inflation + inflexp_median + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,   data = regress_dat,  index = c("cz2000", "date"),  model = "pooling")
tepo1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + inflexp_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "pooling")

# remove inflexp
#po2 <- plm(inflexp_chg ~ SPI2 + PPI2 + state_inflation + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness,  data = regress_dat, index = c("cz2000", "date"),  model = "pooling")
po2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "pooling")
# with time fixed effects
#tepo2 <- plm(inflexp_chg ~ SPI2 + PPI2 + state_inflation + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,   data = regress_dat, index = c("cz2000", "date"),  model = "pooling")
tepo2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "pooling")

# SPI only
#po3 <- plm(inflexp_chg ~ SPI2,  data = regress_dat, index = c("cz2000", "date"),  model = "pooling")
po3 <- plm(inflexp_chg_lag ~ SPI_lag, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "pooling")
# with time fixed effects
#tepo3 <- plm(inflexp_chg ~ SPI2 + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,  data = regress_dat, index = c("cz2000", "date"),  model = "pooling")
tepo3 <- plm(inflexp_chg_lag ~ SPI_lag + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "pooling")

# PPI only
#po4 <- plm(inflexp_chg ~ PPI2,   data = regress_dat,  index = c("cz2000", "date"),   model = "pooling")
po4 <- plm(inflexp_chg_lag ~ PPI_lag, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "pooling")
# with time fixed effects
#tepo4 <- plm(inflexp_chg ~ PPI2 + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,  data = regress_dat, index = c("cz2000", "date"),  model = "pooling")
tepo4 <- plm(inflexp_chg_lag ~ PPI_lag + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "pooling")


stargazer(po1,tepo1,po2,tepo2,po3,tepo3,po4,tepo4,title="Pooled OLS Regression Results",align=TRUE, label = "tab:regpo", model.names = TRUE)


# Scatter Plots
###############


pdf(file = paste("../SocialInflationExpectation/_output/SPI2_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_chg_lag, 
     y = regress_dat$SPI_lag,
     xlab = "Change in Median Inflation Expectations",
     ylab = "Social Proximity to Inflation (computation type 2)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(tepo3, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI2_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_chg_lag, 
     y = regress_dat$PPI_lag,
     xlab = "Median Inflation Expectations",
     ylab = "Physical Proximity to Inflation (computation type 2)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(tepo4, lwd = 1.5)

dev.off()



############################
##### 4. Further ideas #####
############################


# 4.1 Outwardness Ratio
#######################

# Split sample below and above outwardness mean

mo <- mean(regress_dat$outwardness)
# I noticed that the mean here is lower than the mean in the stargazer summary statistic. Could be because there it shows up multiple times per observation due to the time component...

# sample with outwardness below mean
sample1 <- regress_dat %>%
  filter(outwardness <= mo)

# sample with outwardness above mean
sample2 <- regress_dat %>%
  filter(outwardness > mo)

# Run same regression split for both samples

# Sample 1: Low outwardness

# fixed effects
fe2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag, 
           data = sample1,
           index = c("cz2000", "date"), 
           model = "within")

# with time fixed effects
tefe2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag, 
             data = sample1,
             index = c("cz2000", "date"), 
             model = "within",
             effect = "twoways")

# pooled ols
po2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag+ state_inflation_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
           data = sample1,
           index = c("cz2000", "date"), 
           model = "pooling")

# with time fixed effects
tepo2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = sample1,
             index = c("cz2000", "date"), 
             model = "pooling")

# Sample 2: High outwardness

# fixed effects
fe2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag, 
              data = sample2,
              index = c("cz2000", "date"), 
              model = "within")

# with time fixed effects
tefe2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag, 
                data = sample2,
                index = c("cz2000", "date"), 
                model = "within",
                effect = "twoways")

# pooled ols
po2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
              data = sample2,
              index = c("cz2000", "date"), 
              model = "pooling")

# with time fixed effects
tepo2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + state_inflation_lag + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
                data = sample2,
                index = c("cz2000", "date"), 
                model = "pooling")


stargazer(fe2o1,tefe2o1,po2o1,tepo2o1,fe2o2,tefe2o2,po2o2,tepo2o2,title="Regression Results Split by Outwardness",align=TRUE, label = "tab:regout", model.names = TRUE)


