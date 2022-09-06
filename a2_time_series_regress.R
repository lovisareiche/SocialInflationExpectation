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

rm(list=ls())
####################################
##### 1. Prep all data sources #####
####################################


# Read in baseline inflation expectations data
# Built in a1_county_data_collect.R
dat_inflex <- read_csv("../SocialInflationExpectation/_intermediate/inflexp_date_cz.csv")

# choose mean or median computation of SPI and PPI here
c <- "mean"

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
  inner_join(dat_cpi) %>%
  # name as in paper
  # rename(SPI = sci_weighted_inflation, SPI_control = sci_weighted_inflation_control, PPI = dist_weighted_inflation)
  mutate(cz2000 = factor(cz2000), date = factor(date))


write_csv(regress_dat, "../SocialInflationExpectation/_output/time_series_regress_dat.csv")


cor2latex(regress_dat[,c(3,6:16)],use = "pairwise", method="pearson", adjust="holm",stars=FALSE,
          digits=2,rowlabels=TRUE,lower=TRUE,apa=TRUE,short.names=TRUE,
          font.size ="scriptsize", heading="Correlation",
          caption="cor2latex",label="tab:cor",silent=FALSE,file=NULL,append=FALSE,cut=0,big=0)

cor(regress_dat[,c(3,6:16)],use = "complete.obs")

###############################
##### 3. Panel regression #####
###############################


# 3.1. Estimate fixed effects and pooled ols
############################################

# Computation 1

# estimate the fixed effects regression with plm()
fe1 <- plm(inflexp_median ~ SPI1 + PPI1 + pi_mean, 
                     data = regress_dat,
                     index = c("cz2000", "date"), 
                     model = "within")
#coeftest(fe1, vcov. = vcovHC, type = "HC1")

po1 <- plm(inflexp_median ~ SPI1 + PPI1 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
                     data = regress_dat,
                     index = c("cz2000", "date"), 
                     model = "pooling")
#coeftest(po1, vcov. = vcovHC, type = "HC1")

# Computation 2

# estimate the fixed effects regression with plm()
fe2 <- plm(inflexp_median ~ SPI2 + PPI2 + pi_mean, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "within")
#coeftest(fe2, vcov. = vcovHC, type = "HC1")

po2 <- plm(inflexp_median ~ SPI2 + PPI2 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "pooling")
#coeftest(po2, vcov. = vcovHC, type = "HC1")

# Computation 3

# estimate the fixed effects regression with plm()
fe3 <- plm(inflexp_median ~ SPI3 + PPI3 + pi_mean, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "within")
#coeftest(fe3, vcov. = vcovHC, type = "HC1")

po3 <- plm(inflexp_median ~ SPI3 + PPI3 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
           data = regress_dat,
           index = c("cz2000", "date"), 
           model = "pooling")
#coeftest(po3, vcov. = vcovHC, type = "HC1")


# 3.2. Plot scatters for check 
##############################

# Computation 1

pdf(file = paste("../SocialInflationExpectation/_output/SPI1_",c,".pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_median, 
     y = regress_dat$SPI1,
     xlab = "Median Inflation Expectations",
     ylab = "Social Proximity to Inflation (computation type 1)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(po1, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI1_",c,".pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_median, 
     y = regress_dat$PPI1,
     xlab = "Median Inflation Expectations",
     ylab = "Physical Proximity to Inflation (computation type 1)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(a = po1[["coefficients"]][1], b = po1[["coefficients"]][3], lwd = 1.5)

dev.off()

# Computation 2

pdf(file = paste("../SocialInflationExpectation/_output/SPI2_",c,".pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_median, 
     y = regress_dat$SPI2,
     xlab = "Median Inflation Expectations",
     ylab = "Social Proximity to Inflation (computation type 2)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(po2, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI2_",c,".pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_median, 
     y = regress_dat$PPI2,
     xlab = "Median Inflation Expectations",
     ylab = "Physical Proximity to Inflation (computation type 2)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(a = po2[["coefficients"]][1], b = po2[["coefficients"]][3], lwd = 1.5)

dev.off()

# Computation 3

pdf(file = paste("../SocialInflationExpectation/_output/SPI3_",c,".pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_median, 
     y = regress_dat$SPI3,
     xlab = "Median Inflation Expectations",
     ylab = "Social Proximity to Inflation (computation type 3)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(po3, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI3_",c,".pdf",sep=""), width=6, height=6)

plot(x = regress_dat$inflexp_median, 
     y = regress_dat$PPI3,
     xlab = "Median Inflation Expectations",
     ylab = "Physical Proximity to Inflation (computation type 3)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(a = po3[["coefficients"]][1], b = po3[["coefficients"]][3], lwd = 1.5)

dev.off()


# 3.3. Write regression table 
#############################


#install.packages("stargazer")
library(stargazer)

dat <- data.frame(regress_dat) %>%
  select(inflexp_median,PPI1,PPI2,PPI3,SPI1,SPI2,SPI3,pi_mean, poor_share2010,med_hhinc2016,rent_twobed2015,outwardness)

stargazer(dat)

stargazer(fe1,po1,fe2,po2,fe3,po3, title="Regression Results", align=TRUE)



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

# Run same regression split for both samples

# estimate the fixed effects regression, no control with plm()
fe_o1 <- plm(inflexp_median ~ SPI2 + PPI2 + pi_mean, 
                       data = sample1,
                       index = c("cz2000", "date"), 
                       model = "within")

# estimate the pooled effects regression, no control with plm()
po_o1 <- plm(inflexp_median ~ SPI2 + PPI2 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015, 
                       data = sample1,
                       index = c("cz2000", "date"), 
                       model = "pooling")

# estimate the fixed effects regression, no control with plm()
fe_o2 <- plm(inflexp_median ~ SPI2 + PPI2 + pi_mean, 
                             data = sample2,
                             index = c("cz2000", "date"), 
                             model = "within")

# estimate the pooled effects regression, no control with plm()
po_o2 <- plm(inflexp_median ~ SPI2 + PPI2 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015, 
                             data = sample2,
                             index = c("cz2000", "date"), 
                             model = "pooling")


# Write regression table

#install.packages("stargazer")
#library(stargazer)

stargazer(fe_o1, po_o1, fe_o2, po_o2, title="Regression Results Split by Outwardness", align=TRUE, label = "tab:regout")



# 4.2 Time Fixed Effects
########################

# via plm()
tefe1 <- plm(inflexp_median ~ SPI1 + PPI1 + pi_mean, 
                        data = regress_dat,
                        index = c("cz2000", "date"), 
                        model = "within", 
                        effect = "twoways")

tefe2 <- plm(inflexp_median ~ SPI2 + PPI2 + pi_mean, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "within", 
             effect = "twoways")

tefe3 <- plm(inflexp_median ~ SPI3 + PPI3 + pi_mean, 
             data = regress_dat,
             index = c("cz2000", "date"), 
             model = "within", 
             effect = "twoways")

stargazer(tefe1,tefe2,tefe3,title="Regression Results",align=TRUE, label = "tab:regtefe", model.names = TRUE)



# 4.3 Use change in expectations as dependent variable
######################################################

regress_dat2 <- regress_dat %>%
  arrange(cz2000,date) %>%
  group_by(cz2000) %>%
  # compute lead
  mutate(inflexp_lead = dplyr::lead(inflexp_median)) %>%
  # compute how much is adjusted for the next period
  mutate(inflexp_chg = inflexp_lead - inflexp_median) %>%
  filter(!is.na(inflexp_lead))


chg_fe1 <- plm(inflexp_chg ~ SPI1 + PPI1 + pi_mean, 
           data = regress_dat2,
           index = c("cz2000", "date"), 
           model = "within")

chg_po1 <- plm(inflexp_chg ~ SPI1 + PPI1 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
           data = regress_dat2,
           index = c("cz2000", "date"), 
           model = "pooling")

chg_fe2 <- plm(inflexp_chg ~ SPI2 + PPI2 + pi_mean, 
               data = regress_dat2,
               index = c("cz2000", "date"), 
               model = "within")

chg_po2 <- plm(inflexp_chg ~ SPI2 + PPI2 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
               data = regress_dat2,
               index = c("cz2000", "date"), 
               model = "pooling")

chg_fe3 <- plm(inflexp_chg ~ SPI3 + PPI3 + pi_mean, 
               data = regress_dat2,
               index = c("cz2000", "date"), 
               model = "within")

chg_po3 <- plm(inflexp_chg ~ SPI3 + PPI3 + pi_mean + poor_share2010 + med_hhinc2016 + rent_twobed2015 + outwardness, 
               data = regress_dat2,
               index = c("cz2000", "date"), 
               model = "pooling")


stargazer(chg_fe1,chg_po1,chg_fe2,chg_po2,chg_fe3,chg_po3,title="Regression Results",align=TRUE, label = "tab:reglead", model.names = TRUE)




# Computation 1

pdf(file = paste("../SocialInflationExpectation/_output/SPI1_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat2$inflexp_chg, 
     y = regress_dat2$SPI1,
     xlab = "Chnage in Median Inflation Expectations",
     ylab = "Social Proximity to Inflation (computation type 1)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(po1, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI1_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat2$inflexp_chg, 
     y = regress_dat2$PPI1,
     xlab = "Median Inflation Expectations",
     ylab = "Physical Proximity to Inflation (computation type 1)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(a = po1[["coefficients"]][1], b = po1[["coefficients"]][3], lwd = 1.5)

dev.off()

# Computation 2

pdf(file = paste("../SocialInflationExpectation/_output/SPI2_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat2$inflexp_chg, 
     y = regress_dat2$SPI2,
     xlab = "Chnage in Median Inflation Expectations",
     ylab = "Social Proximity to Inflation (computation type 2)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(po2, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI2_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat2$inflexp_chg, 
     y = regress_dat2$PPI2,
     xlab = "Median Inflation Expectations",
     ylab = "Physical Proximity to Inflation (computation type 2)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(a = po2[["coefficients"]][1], b = po1[["coefficients"]][3], lwd = 1.5)

dev.off()

# Computation 3

pdf(file = paste("../SocialInflationExpectation/_output/SPI3_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat2$inflexp_chg, 
     y = regress_dat2$SPI3,
     xlab = "Chnage in Median Inflation Expectations",
     ylab = "Social Proximity to Inflation (computation type 3)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(po3, lwd = 1.5)

dev.off()

pdf(file = paste("../SocialInflationExpectation/_output/PPI3_",c,"_chg.pdf",sep=""), width=6, height=6)

plot(x = regress_dat2$inflexp_chg, 
     y = regress_dat2$PPI3,
     xlab = "Median Inflation Expectations",
     ylab = "Physical Proximity to Inflation (computation type 3)",
     pch = 20, 
     col = "steelblue")


# add the regression line to plot
abline(a = po3[["coefficients"]][1], b = po1[["coefficients"]][3], lwd = 1.5)

dev.off()


