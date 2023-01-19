# Purpose: Case study analysis
#
# Inputs: 
#     _intermediate/inflexp.csv
#     _intermediate/sci.tsv
#     _intermediate/cpi.csv
#
# Outputs: 
#
# Date: 26/07/22
#
# Steps:
#     1. Read in data
#     2. Find case studies
#     3. Match inflation expectations to sci with cases
#     4. Show expectations before and after


rm(list = ls())
library(tidyverse)
library(readxl) # to read excel files
library(lubridate) # for date management
library(stargazer) # to write regression tables
library(plm) # for linear panel models


########################
##### Make Choices #####
########################


# Do you want to look at US counties or EU countries?
l <- "US"

# Which survey?
s <- "FRBNY"



###########################
##### 1. Read in data #####
###########################



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

# Geographic IDs
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
dir.geo <- paste("../code/_input/",l,"/geo_match.csv",sep="")


dat_inflexp <- read_csv(dir.inflexp)
dat_sci <- read_tsv(dir.sci)
dat_cpi <- read_csv(dir.cpi) 
dat_dist <- read_csv(dir.dist)
dat_geo <- read_csv2(dir.geo) %>%
  rename(loc = "Commuting Zone ID, 2000", area = "Metropolitan Area, 2003") %>%
  select(loc, area) %>%
  filter(!is.na(area)) %>%
  unique



################################
##### 2. Find case studies #####
################################



# cpi with dates we have expectations for
cpi_used <- dat_cpi[dat_cpi$date <= max(dat_inflexp$date) & dat_cpi$date >= min(dat_inflexp$date),] %>%
  group_by(loc) %>%
  mutate(cpi_chg = cpi_inflation - dplyr::lag(cpi_inflation)) %>%
  ungroup %>%
  mutate(year = substr(date,1,4)) %>%
  group_by(loc,year) %>%
  mutate(cpi_average = mean(cpi_inflation)) %>%
  ungroup %>%
  select(-year) %>%
  inner_join(dat_geo)


# 2.1 Date-CZ pairs with highest cpi in levels

cpi_max1 <- cpi_used[cpi_used$cpi_inflation == max(cpi_used$cpi_inflation),]
# we find that the highest inflation rate was in Connecticut 2014


# 2.2 Date-CZ pairs with the highest cpi month-on-month change

cpi_max2 <- cpi_used[cpi_used$cpi_chg == max(cpi_used$cpi_chg, na.rm = TRUE),] %>%
  filter(!is.na(cpi_chg))

# 2.3 Date-CZ pairs with the highest average 12 months inflation rate 

cpi_max3 <- cpi_used[cpi_used$cpi_average == max(cpi_used$cpi_average),]

# Calculate changes in expectations in all regions

dat_inflexp <- dat_inflexp %>%
  group_by(loc) %>%
  # change of current period versus last period
  mutate(inflexp_chg = inflexp_median - dplyr::lag(inflexp_median)) %>%
  ungroup

#########################################
##### 3. Use highest inflation rate #####
#########################################

# Q: Does being connected to the high inflation region affect change in inflation expectations?

# Identify dates

date_0 <- unique(cpi_max1$date)
date_1 <- date_0 %m+% months(1)   

# Create sample for both time periods

sample_0 <- dat_inflexp %>%
  # only use period identified in case
  filter(date == date_0)

sample_1 <- dat_inflexp %>%
  # only use period identified in case
  filter(date == date_1)

# 3.1.Match sci to expectations
###############################

# for case 1: high index
# change after inflation event
sci_1 <- dat_sci %>%
  # inflation expectations in the user's location
  inner_join(sample_1, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max1$loc)))


pdf(file = paste("../code/_output/Case1_sci1_",l,".pdf",sep=""), width=6, height=6)

plot(y = sci_1$inflexp_chg, 
     x = sci_1$share_sci,
     ylab = "Change in Median Inflation Expectations after 2014Q2",
     xlab = "Social Proximity to Connecticut",
     pch = 20, 
     col = "steelblue")

# Linear fit
abline(lm(sci_1$inflexp_chg ~ sci_1$share_sci),lwd = 1.5)


dev.off()

# for case 1: high index
# change during inflation event
sci_0 <- dat_sci %>%
  # inflation expectations in the user's location
  inner_join(sample_0, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max1$loc)))


pdf(file = paste("../code/_output/Case1_sci0_",l,".pdf",sep=""), width=6, height=6)

plot(y = sci_0$inflexp_chg, 
     x = sci_0$share_sci,
     ylab = "Change in Median Inflation Expectations in 2014Q2",
     xlab = "Social Proximity to Connecticut",
     pch = 20, 
     col = "steelblue")


# Linear fit
abline(lm(sci_0$inflexp_chg ~ sci_0$share_sci), lwd = 1.5)


dev.off()

# 3.2. Match dist to expectations
#################################

# for case 1: high index
# change after inflation event
dist_1 <- dat_dist %>%
  # inflation expectations in the user's location
  inner_join(sample_1, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max1$loc)))


pdf(file = paste("../code/_output/Case1_dist1_",l,".pdf",sep=""), width=6, height=6)

plot(y = dist_1$inflexp_chg, 
     x = dist_1$dist,
     ylab = "Change in Median Inflation Expectations after 2014Q2",
     xlab = "Physical Proximity to Connecticut",
     pch = 20, 
     col = "steelblue")

# Linear fit
abline(lm(dist_1$inflexp_chg ~ dist_1$dist),lwd = 1.5)


dev.off()

# for case 1: high index
# change during inflation event
dist_0 <- dat_dist %>%
  # inflation expectations in the user's location
  inner_join(sample_0, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max1$loc)))


pdf(file = paste("../code/_output/Case1_dist0_",l,".pdf",sep=""), width=6, height=6)

plot(y = dist_0$inflexp_chg, 
     x = dist_0$dist,
     ylab = "Change in Median Inflation Expectations in 2014Q2",
     xlab = "Physical Proximity to Connecticut",
     pch = 20, 
     col = "steelblue")


# Linear fit
abline(lm(dist_0$inflexp_chg ~ dist_0$dist), lwd = 1.5)


dev.off()


# Regression
############



reg <- sci_0 %>%
  select(user_loc,fr_loc,date,share_sci,inflexp_chg) %>%
  rename(date_0 = date, inflexp_chg_0 = inflexp_chg) %>%
  full_join(sci_1, by = c("fr_loc","user_loc")) %>%
  rename(date_1 = date, inflexp_chg_1 = inflexp_chg) %>%
  # share sci is the same across both so just take mean to compile
  rowwise() %>%
  mutate(share_sci = mean(c(share_sci.x,share_sci.y),na.rm = TRUE)) %>%
  ungroup %>%
  select(user_loc, fr_loc, date_0, date_1, share_sci, inflexp_chg_0, inflexp_chg_1) %>%
  # do the same for distance
  full_join(dist_0, by = c("user_loc","fr_loc", "date_0"="date", "inflexp_chg_0"="inflexp_chg")) %>%
  select(-inflexp_median) %>%
  full_join(dist_1, by = c("user_loc","fr_loc", "date_1"="date", "inflexp_chg_1"="inflexp_chg")) %>%
  select(-inflexp_median,-fr_loc) %>%
  rowwise() %>%
  mutate(dist = mean(c(dist.x,dist.y),na.rm = TRUE)) %>%
  select(-dist.x,-dist.y) %>%
  rename(loc = user_loc) %>%
  ungroup


# Summary Statistic

dat <- data.frame(reg) %>%
  select(inflexp_chg_0, inflexp_chg_1, share_sci, dist)

stargazer(dat)

# Fixed Effects

reg$dist[is.nan(reg$dist)]<-NA

reg_0 <- filter(reg,!is.na(inflexp_chg_0)) %>%
  select(-date_1,-inflexp_chg_1)

po_0 <- plm(inflexp_chg_0 ~ share_sci + dist, 
           data = reg_0,
           index = c("loc", "date_0"), 
           model = "pooling")

reg_1 <- filter(reg,!is.na(inflexp_chg_1)) %>%
  select(-date_0,-inflexp_chg_0)

po_1 <- plm(inflexp_chg_1 ~ share_sci + dist, 
            data = reg_1,
            index = c("loc", "date_1"), 
            model = "pooling")

writeLines(capture.output(stargazer(po_0,po_1,title="Pooled OLS Regression Results",align=TRUE, label = "tab:regpo_case:ct", model.names = TRUE)), paste("../code/_output/Case1PO_",l,".tex",sep = ""))



################################################
##### 4. Use highest month on month change #####
################################################



# Q: Does being connected to the high inflation region affect change in inflation expectations?

# Identify dates

date_0 <- unique(cpi_max2$date)
date_1 <- date_0 %m+% months(1)   

# Create sample for both time periods

sample_0 <- dat_inflexp %>%
  # only use period identified in case
  filter(date == date_0)

sample_1 <- dat_inflexp %>%
  # only use period identified in case
  filter(date == date_1)


# 3.1.Match sci to expectations
###############################

# change after inflation event
sci_1 <- dat_sci %>%
  # inflation expectations in the user's location
  inner_join(sample_1, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max2$loc)))


pdf(file = paste("../code/_output/Case2_sci1_",l,".pdf",sep=""), width=6, height=6)

plot(y = sci_1$inflexp_chg, 
     x = sci_1$share_sci,
     ylab = "Change in Median Inflation Expectations April to May 2017",
     xlab = "Social Proximity to Arkansas",
     pch = 20, 
     col = "steelblue")

# Linear fit
abline(lm(sci_1$inflexp_chg ~ sci_1$share_sci),lwd = 1.5)


dev.off()

# change during inflation event
sci_0 <- dat_sci %>%
  # inflation expectations in the user's location
  inner_join(sample_0, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max2$loc)))


pdf(file = paste("../code/_output/Case2_sci0_",l,".pdf",sep=""), width=6, height=6)

plot(y = sci_0$inflexp_chg, 
     x = sci_0$share_sci,
     ylab = "Change in Median Inflation Expectations March to April 2017",
     xlab = "Social Proximity to Arkansas",
     pch = 20, 
     col = "steelblue")


# Linear fit
abline(lm(sci_0$inflexp_chg ~ sci_0$share_sci), lwd = 1.5)


dev.off()

# 3.2. Match dist to expectations
#################################

# change after inflation event
dist_1 <- dat_dist %>%
  # inflation expectations in the user's location
  inner_join(sample_1, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max2$loc)))


pdf(file = paste("../code/_output/Case2_dist1_",l,".pdf",sep=""), width=6, height=6)

plot(y = dist_1$inflexp_chg, 
     x = dist_1$dist,
     ylab = "Change in Median Inflation Expectations April to May 2017",
     xlab = "Physical Proximity to Arkansas",
     pch = 20, 
     col = "steelblue")

# Linear fit
abline(lm(dist_1$inflexp_chg ~ dist_1$dist),lwd = 1.5)


dev.off()

# for case 1: high index
# change during inflation event
dist_0 <- dat_dist %>%
  # inflation expectations in the user's location
  inner_join(sample_0, by = c("user_loc" = "loc")) %>%
  # select only connections to the affected cz
  filter(is.element(fr_loc, unique(cpi_max2$loc)))


pdf(file = paste("../code/_output/Case2_dist0_",l,".pdf",sep=""), width=6, height=6)

plot(y = dist_0$inflexp_chg, 
     x = dist_0$dist,
     ylab = "Change in Median Inflation Expectations March to April 2017",
     xlab = "Physical Proximity to Arkansas",
     pch = 20, 
     col = "steelblue")


# Linear fit
abline(lm(dist_0$inflexp_chg ~ dist_0$dist), lwd = 1.5)


dev.off()


# Regression 
############



reg <- sci_0 %>%
  select(user_loc,fr_loc,date,share_sci,inflexp_chg) %>%
  rename(date_0 = date, inflexp_chg_0 = inflexp_chg) %>%
  full_join(sci_1, by = c("fr_loc","user_loc")) %>%
  rename(date_1 = date, inflexp_chg_1 = inflexp_chg) %>%
  # share sci is the same across both so just take mean to compile
  rowwise() %>%
  mutate(share_sci = mean(c(share_sci.x,share_sci.y),na.rm = TRUE)) %>%
  ungroup %>%
  select(user_loc, fr_loc, date_0, date_1, share_sci, inflexp_chg_0, inflexp_chg_1) %>%
  # do the same for distance
  full_join(dist_0, by = c("user_loc","fr_loc", "date_0"="date", "inflexp_chg_0"="inflexp_chg")) %>%
  select(-inflexp_median) %>%
  full_join(dist_1, by = c("user_loc","fr_loc", "date_1"="date", "inflexp_chg_1"="inflexp_chg")) %>%
  select(-inflexp_median,-fr_loc) %>%
  rowwise() %>%
  mutate(dist = mean(c(dist.x,dist.y),na.rm = TRUE)) %>%
  select(-dist.x,-dist.y) %>%
  rename(loc = user_loc) %>%
  ungroup


# Summary Statistic

dat <- data.frame(reg) %>%
  select(inflexp_chg_0, inflexp_chg_1, share_sci, dist)

stargazer(dat)


reg$dist[is.nan(reg$dist)]<-NA

reg_0 <- filter(reg,!is.na(inflexp_chg_0)) %>%
  select(-date_1,-inflexp_chg_1)

po_0 <- plm(inflexp_chg_0 ~ share_sci + dist, 
            data = reg_0,
            index = c("loc", "date_0"), 
            model = "pooling")

reg_1 <- filter(reg,!is.na(inflexp_chg_1)) %>%
  select(-date_0,-inflexp_chg_0)

po_1 <- plm(inflexp_chg_1 ~ share_sci + dist, 
            data = reg_1,
            index = c("loc", "date_1"), 
            model = "pooling")

writeLines(capture.output(stargazer(po_0,po_1,title="Pooled OLS Regression Results",align=TRUE, label = "tab:regpo_case:ar", model.names = TRUE)), paste("../code/_output/Case2PO_",l,".tex",sep = ""))

