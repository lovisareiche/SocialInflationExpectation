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
l <- "US"

# Which survey?
s <- "FRBNY"


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

# for the US sample need to average as otherwise in counties
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
  mutate(inflexp_lag = dplyr::lag(inflexp_median)) %>%
  mutate(inflexp_lead = dplyr::lead(inflexp_median)) %>%
  mutate(SPI_lag = dplyr::lag(SPI2)) %>%
  mutate(PPI_lag = dplyr::lag(PPI2)) %>%
  mutate(cpi_inflation_lag = dplyr::lag(cpi_inflation)) %>%
  # compute how much is adjusted for the next period
  mutate(inflexp_chg_lead = abs(inflexp_lead - inflexp_median)) %>%
  mutate(inflexp_chg_lag = abs(inflexp_median - inflexp_lag)) %>%
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

# save stargazer output in tex file
writeLines(capture.output(stargazer(fe1,tefe1,fe2,tefe2,fe3,tefe3,fe4,tefe4,title="Fixed Effects Regression Results",align=TRUE, label = "tab:regfe", model.names = TRUE)), paste("../SocialInflationExpectation/_output/FE_",l,".tex",sep = ""))


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


# Problem: with EU data we have 444 different dates, for time dummies I need to write them out which is not efficient. Need to find a better way for this.
# Solution: Write out the formula like this so it works in both contexts. Need to be careful with the order of the variables though!
f1 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(17,18,19,10,11,12,13,15)], collapse='+')))
f_time1 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(17,18,19,10,11,12,13,15,22:ncol(regress_dat))], collapse='+')))
f2 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(17,18,19,10,11,12,13)], collapse='+')))
f_time2 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(17,18,19,10,11,12,13,22:ncol(regress_dat))], collapse='+')))
f3 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(17)], collapse='+')))
f_time3 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(17,22:ncol(regress_dat))], collapse='+')))
f4 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(18)], collapse='+')))
f_time4 <- as.formula(paste('inflexp_chg_lag ~', paste(colnames(regress_dat)[c(18,22:ncol(regress_dat))], collapse='+')))


# all
#po1 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + inflexp_median + poor_share + med_hhinc + housing_cost + outwardness,  data = regress_dat, index = c("loc", "date"),   model = "pooling")
po1 <- plm(f1,
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo1 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + inflexp_median + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,   data = regress_dat,  index = c("loc", "date"),  model = "pooling")
tepo1 <- plm(f_time1, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

# remove inflexp
#po2 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + poor_share + med_hhinc + housing_cost + outwardness,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
po2 <- plm(f2, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo2 <- plm(inflexp_chg ~ SPI2 + PPI2 + cpi_inflation + poor_share + med_hhinc + housing_cost + outwardness + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,   data = regress_dat, index = c("loc", "date"),  model = "pooling")
tepo2 <- plm(f_time2, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

# SPI only
#po3 <- plm(inflexp_chg ~ SPI2,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
po3 <- plm(f3, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo3 <- plm(inflexp_chg ~ SPI2 + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
tepo3 <- plm(f_time3, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

# PPI only
#po4 <- plm(inflexp_chg ~ PPI2,   data = regress_dat,  index = c("loc", "date"),   model = "pooling")
po4 <- plm(f4, 
           data = regress_dat,
           index = c("loc", "date"), 
           model = "pooling")
# with time fixed effects
#tepo4 <- plm(inflexp_chg ~ PPI2 + X15949 + X16314 + X16405 + X16467 + X16495 + X16526 + X16556 + X16587 + X16617 + X16648 + X16679 + X16709 + X16740 + X16770 + X16801 + X16832 + X16861 + X16892 + X16922 + X16953 + X16983 + X17014 + X17045 + X17075 + X17106 + X17136 + X17167 + X17198 + X17226 + X17257 + X17318 + X17348 + X17379 + X17410 + X17440 + X15857 + X15887 + X15979 + X16040 + X16071 + X16102 + X16130 + X16161 + X16191 + X16222 + X16252 + X16283 + X16344 + X16375 + X16436 + X17287 + X17471 + X17501 + X16010,  data = regress_dat, index = c("loc", "date"),  model = "pooling")
tepo4 <- plm(f_time4, 
             data = regress_dat,
             index = c("loc", "date"), 
             model = "pooling")

# save stargazer output in tex file
writeLines(capture.output(stargazer(po1,tepo1,po2,tepo2,po3,tepo3,po4,tepo4,title="Pooled OLS Regression Results",align=TRUE, label = "tab:regpo", model.names = TRUE)), paste("../SocialInflationExpectation/_output/POLS_",l,".tex",sep = ""))


# Scatter Plots
###############


pdf(file = paste("../SocialInflationExpectation/_output/SPI2_",l,".pdf",sep=""), width=6, height=6)

plot(y = regress_dat$inflexp_chg_lag, 
     x = regress_dat$SPI_lag,
     ylab = "Change in Median Inflation Expectations",
     xlab = "Social Proximity to Inflation",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(tepo3, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI2_",l,".pdf",sep=""), width=6, height=6)

plot(y = regress_dat$inflexp_chg_lag, 
     x = regress_dat$PPI_lag,
     ylab = "Change in Median Inflation Expectations",
     xlab = "Physical Proximity to Inflation",
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

quantiles <- quantile(regress_dat$outwardness, na.rm = TRUE)
# I noticed that the mean here is lower than the mean in the stargazer summary statistic. Could be because there it shows up multiple times per observation due to the time component...

# sample with outwardness below 25% quantile
sample1 <- regress_dat %>%
  filter(outwardness <= quantiles["25%"])

# sample with outwardness below 50% but above 25%
sample2 <- regress_dat %>%
  filter(outwardness > quantiles["25%"] & outwardness <= quantiles["50%"])

# sample with outwardness below 75% but above 50%
sample3 <- regress_dat %>%
  filter(outwardness > quantiles["50%"] & outwardness <= quantiles["75%"])

# sample with outwardness above 75%
sample4 <- regress_dat %>%
  filter(outwardness > quantiles["75%"])

# Run same regression split for both samples

# Sample 1: Low outwardness

# with time fixed effects
o1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
             data = sample1,
             index = c("loc", "date"), 
             model = "within",
             effect = "twoways")

# with time fixed effects
o12 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
               data = sample1,
               index = c("loc", "date"), 
               model = "within",
               effect = "twoways")


# Sample 2: Low outwardness


# with time fixed effects
o2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
              data = sample2,
              index = c("loc", "date"), 
              model = "within",
              effect = "twoways")

# with time fixed effects
o22 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
                data = sample2,
                index = c("loc", "date"), 
                model = "within",
                effect = "twoways")

# Sample 3: High outwardness


# with time fixed effects
o3 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
              data = sample3,
              index = c("loc", "date"), 
              model = "within",
              effect = "twoways")

# with time fixed effects
o32 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
                data = sample3,
                index = c("loc", "date"), 
                model = "within",
                effect = "twoways")

# Sample 4: High outwardness


# with time fixed effects
o4 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
              data = sample4,
              index = c("loc", "date"), 
              model = "within",
              effect = "twoways")

# with time fixed effects
o42 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
                data = sample4,
                index = c("loc", "date"), 
                model = "within",
                effect = "twoways")


# save stargazer output in tex file
writeLines(capture.output(stargazer(o1,o12, o2,o22, o3,o32, o4,o42, title="Regression Results Split by Outwardness",align=TRUE, label = "tab:regout", model.names = TRUE)), paste("../SocialInflationExpectation/_output/Outwardness_",l,".tex",sep = ""))



# 4.2 Inflexp Split
#######################

# Split sample according to inflexp quintiles

quantiles <- quantile(regress_dat$inflexp_median, na.rm = TRUE)
# I noticed that the mean here is lower than the mean in the stargazer summary statistic. Could be because there it shows up multiple times per observation due to the time component...

# sample with outwardness below 25% quantile
sample1 <- regress_dat %>%
  filter(inflexp_median <= quantiles["25%"])

# sample with outwardness below 50% but above 25%
sample2 <- regress_dat %>%
  filter(inflexp_median > quantiles["25%"] & outwardness <= quantiles["50%"])

# sample with outwardness below 75% but above 50%
sample3 <- regress_dat %>%
  filter(inflexp_median > quantiles["50%"] & outwardness <= quantiles["75%"])

# sample with outwardness above 75%
sample4 <- regress_dat %>%
  filter(inflexp_median > quantiles["75%"])

# Run same regression split for both samples

# Sample 1: Low outwardness

# with time fixed effects
i1 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
          data = sample1,
          index = c("loc", "date"), 
          model = "within",
          effect = "twoways")

# with time fixed effects
i12 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
           data = sample1,
           index = c("loc", "date"), 
           model = "within",
           effect = "twoways")


# Sample 2: Low outwardness


# with time fixed effects
i2 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
          data = sample2,
          index = c("loc", "date"), 
          model = "within",
          effect = "twoways")

# with time fixed effects
i22 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
           data = sample2,
           index = c("loc", "date"), 
           model = "within",
           effect = "twoways")

# Sample 3: High outwardness


# with time fixed effects
i3 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
          data = sample3,
          index = c("loc", "date"), 
          model = "within",
          effect = "twoways")

# with time fixed effects
i32 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
           data = sample3,
           index = c("loc", "date"), 
           model = "within",
           effect = "twoways")

# Sample 4: High outwardness


# with time fixed effects
i4 <- plm(inflexp_chg_lag ~ SPI_lag + PPI_lag + cpi_inflation_lag, 
          data = sample4,
          index = c("loc", "date"), 
          model = "within",
          effect = "twoways")

# with time fixed effects
i42 <- plm(inflexp_chg_lag ~ SPI_lag+ PPI_lag + cpi_inflation_lag + inflexp_lag, 
           data = sample4,
           index = c("loc", "date"), 
           model = "within",
           effect = "twoways")


# save stargazer output in tex file
writeLines(capture.output(stargazer(i1,i12, i2,i22, i3,i32, i4,i42, title="Regression Results Split by Inflation Expectations",align=TRUE, label = "tab:regexp", model.names = TRUE)), paste("../SocialInflationExpectation/_output/InflexpSplit_",l,".tex",sep = ""))

