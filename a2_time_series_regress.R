# Purpose: Build the regression table for the time series analyses
#
# Inputs: 
#     _intermediate/inflexp_?_?.csv
#     _intermediate/SPI_?.csv
#     _intermediate/PPI_?.csv
#     _intermediate/covariates_?.csv
#     _intermediate/cpi_?.csv
#
# Outputs: 
#     _output/time_series_regress_dat.csv
#
# Date: 18/09/22
#
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

########################
##### Make Choices #####
########################


# Do you want to look at US counties or EU countries?
l <- "EU"

# Which survey?
s <- "ECFIN"


# Do you want to look at mean or median computation?
c <- "median"



###################################
##### 1. Read in data sources #####
###################################


### Choose covariates you want to control for ###
### These must be adjusted given the location ###


# Read in covariates
# Built in pre file
dat_covariates <- read_csv(paste("../SocialInflationExpectation/_intermediate/covariates_",l,".csv", sep="")) %>% 
  select(loc, med_hhinc, poor_share, housing_cost) 

if (l == "US") {
  U = uniq(dat_covariates$loc)
  a1 = accumarray(U$n,dat_covariates$med_hhinc, func = mean)
  a2 = accumarray(U$n,dat_covariates$poor_share, func = mean)
  a3 = accumarray(U$n,dat_covariates$housing_cost, func = mean)
  
  dat_covariates <- tibble(loc = U$b, poor_share = a2, med_hhinc = a1, housing_cost = a3)
}


# Read in CPI data 
# Built in b1_create_state_cpi
dat_cpi <- read_csv(paste("../SocialInflationExpectation/_intermediate/cpi_",l,".csv", sep = ""))

# Read in outwardness
# Built in a1_county_data_collect
dat_outward <- read_csv(paste("../SocialInflationExpectation/_intermediate/outwardness_",l,".csv", sep=""))

# Read in baseline inflation expectations data
# Built in a1_county_data_collect.R
dat_inflexp <- read_csv(paste("../SocialInflationExpectation/_intermediate/inflexp_",l,"_",s,".csv", sep = ""))

# Read in SCI-weighted inflation (Social Proximity to Inflation)
# Built in a1_create_SPI_PPI.R
SPI <- read_csv(paste("../SocialInflationExpectation/_intermediate/SPI_",c,"_",l,".csv",sep="")) 

# Read in distance-weighted inflation (Physical Proximity to Inflation)
# Built in a1_county_data_collect.R
# dist_weighted_inflation <- read_csv("../SocialInflationExpectation/_intermediate/dist_weighted_inflation.csv")
PPI <- read_csv(paste("../SocialInflationExpectation/_intermediate/PPI_",c,"_",l,".csv",sep="")) 


#######################################
##### 2. Combine all data sources #####
#######################################

regress_dat <- dat_inflexp %>% 
  # Join with social and physical distance to experiences inflation
  left_join(PPI, by=c("loc"="user_loc", "date"="date")) %>% 
  left_join(SPI, by=c("loc"="user_loc", "date"="date")) %>% 
  # Join with covariates (!!! No time variation here !!!)
  left_join(dat_covariates) %>% 
  # Join with outwardness (!!! No time variation here !!!)
  left_join(dat_outward, by=c("loc"="user_loc")) %>% 
  # Join with cpi (!!! Only availabe in 2018, on regional not cz basis !!!)
  inner_join(dat_cpi) %>%
  # compute l
  mutate(inflexp_lead = dplyr::lead(inflexp_median)) %>%
  mutate(inflexp_lag = dplyr::lag(inflexp_median)) %>%
  mutate(inflexp_lead = dplyr::lead(inflexp_median)) %>%
  mutate(SPI_lag = dplyr::lag(SPI2)) %>%
  mutate(PPI_lag = dplyr::lag(PPI2)) %>%
  mutate(cpi_inflation_lag = dplyr::lag(cpi_inflation)) %>%
  # compute how much is adjusted for the next period
  mutate(inflexp_chg_lead = inflexp_lead - inflexp_median) %>%
  mutate(inflexp_chg_lag = inflexp_median - inflexp_lag) %>%
  #filter(!is.na(inflexp_lead)) %>%
  filter(!is.na(inflexp_lag))


write_csv(regress_dat, paste("../SocialInflationExpectation/_output/regress_dat_",l,".csv", sep = ""))


cor2latex(regress_dat[,c(3,5,8,10:14,21)],use = "pairwise", method="pearson", adjust="holm",stars=FALSE,
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
  select(inflexp_chg_lead,inflexp_chg_lag,inflexp_median,SPI2,PPI2,cpi_inflation, poor_share,med_hhinc,housing_cost,outwardness)

stargazer(dat)


# Table 1: Fixed Effects Specifications
#######################################

# all
#fe1 <- plm(inflexp_chg_lead ~ SPI2 + PPI2 + cpi_inflation + inflexp_median, data = regress_dat,index = c("loc", "date"), model = "within")
fe1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + inflexp_lag, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "within")
# with time fixed effects
# tefe1 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + inflexp_median, data = regress_dat,index = c("loc", "date"), model = "within",effect = "twoways")
tefe1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + inflexp_lag, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "within",
           effect = "twoways")

# no inflexp median
#fe2 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation, data = regress_dat,index = c("loc", "date"), model = "within")
fe2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "within")
# with time fixed effects
#tefe2 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation, data = regress_dat,index = c("loc", "date"), model = "within",effect = "twoways")
tefe2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

# only SPI
#fe3 <- plm(inflexp_chg ~ SPI2, data = regress_dat,index = c("loc", "date"), model = "within")
fe3 <- plm(inflexp_chg_lag ~ SPI_lag, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "within")
# with time fixed effects
#tefe3 <- plm(inflexp_chg ~ SPI2, data = regress_dat,index = c("loc", "date"), model = "within",effect = "twoways")
tefe3 <- plm(inflexp_chg_lag ~ SPI_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

# only PPI
#fe4 <- plm(inflexp_chg ~ PPI2,  data = regress_dat, index = c("loc", "date"),  model = "within")
fe4 <- plm(inflexp_chg_lag ~ PPI_lag, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "within")
# with time fixed effects
#tefe4 <- plm(inflexp_chg ~ PPI2,  data = regress_dat, index = c("loc", "date"),  model = "within", effect = "twoways")
tefe4 <- plm(inflexp_chg_lag ~ PPI_lag, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")


stargazer(fe1,tefe1,fe2,tefe2,fe3,tefe3,fe4,tefe4,title="Fixed Effects Regression Results",align=TRUE, label = "tab:regfe", model.names = TRUE)


# Table 2: Pooled OLS Specification
###################################

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

# all
#po1 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + inflexp_median + poor_share + med_hhinc + housing_cost + outwardness,  data = regress_dat, index = c("loc", "date"),   model = "pooling")
po1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + inflexp_lag + poor_share + med_hhinc + housing_cost + outwardness, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo1 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + inflexp_median + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,   data = regress_dat,  index = c("loc", "date"),  model = "pooling")
tepo1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + inflexp_lag + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

# remove inflexp
#po2 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + poor_share + med_hhinc + housing_cost + outwardness,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
po2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + poor_share + med_hhinc + housing_cost + outwardness, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo2 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,   data = regress_dat, index = c("loc", "date"),  model = "pooling")
tepo2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

# SPI only
#po3 <- plm(inflexp_chg ~ SPI2,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
po3 <- plm(inflexp_chg_lag ~ SPI_lag, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo3 <- plm(inflexp_chg ~ SPI2 + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
tepo3 <- plm(inflexp_chg_lag ~ SPI_lag + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

# PPI only
#po4 <- plm(inflexp_chg ~ PPI2,   data = regress_dat,  index = c("loc", "date"),   model = "pooling")
po4 <- plm(inflexp_chg_lag ~ PPI_lag, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo4 <- plm(inflexp_chg ~ PPI2 + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
tepo4 <- plm(inflexp_chg_lag ~ PPI_lag + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = regress_dat,
             index = c("loc", "date"), 
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

mo <- mean(regress_dat$outwardness, na.rm = TRUE)
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
fe2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
           data = sample1,
           index = c("loc", "date"), 
           model = "within")

# with time fixed effects
tefe2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
             data = sample1,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

# pooled ols
po2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag+ cpi_inflation_lag + poor_share + med_hhinc + housing_cost + outwardness, 
           data = sample1,
           index = c("loc", "date"), 
           model = "pooling")

# with time fixed effects
tepo2o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
             data = sample1,
             index = c("loc", "date"), 
             model = "pooling")

# Sample 2: High outwardness

# fixed effects
fe2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
              data = sample2,
              index = c("loc", "date"), 
              model = "within")

# with time fixed effects
tefe2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
                data = sample2,
                index = c("loc", "date"), 
                model = "within",
                effect = "twoways")

# pooled ols
po2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + poor_share + med_hhinc + housing_cost + outwardness, 
              data = sample2,
              index = c("loc", "date"), 
              model = "pooling")

# with time fixed effects
tepo2o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010, 
                data = sample2,
                index = c("loc", "date"), 
                model = "pooling")


stargazer(fe2o1,tefe2o1,po2o1,tepo2o1,fe2o2,tefe2o2,po2o2,tepo2o2,title="Regression Results Split by Outwardness",align=TRUE, label = "tab:regout", model.names = TRUE)


