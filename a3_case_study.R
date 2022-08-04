# Purpose: Case study analysis
#
# Inputs: 
#     _intermediate/inflexp_date_cz.csv
#     _intermediate/sci_cz_cz.tsv
#     _intermediate/cpi_cz2000-timeseries.csv
# Outputs: 
# Date: 26/07/22
# Steps:
#     1. Read in data
#     2. Find case studies
#     3. Match inflation expectations to sci with cases
#     4. Show expectations before and after


library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(pracma)
library(plm)
library(lmtest)


###########################
##### 1. Read in data #####
###########################

# SCI
# Built in a1_county_data_collect
sci <- read_tsv("../SocialInflationExpectation/_intermediate/sci_cz_cz.tsv")
sci_control <- read_tsv("../SocialInflationExpectation/_intermediate/sci_cz_cz_control.tsv")

# CPI
# Built in b1_create_state_cpi
cpi <- read_csv("../SocialInflationExpectation/_intermediate/cpi_cz2000-timeseries.csv")

# Inflex
# Built in a1_county_data_collect
inflex <- read_csv("../SocialInflationExpectation/_intermediate/inflexp_date_cz.csv")


################################
##### 2. Find case studies #####
################################

# cpi with dates we have expectations for
cpi_used <- cpi[cpi$Date <= max(inflex$date),]


# 2.1 Date-CZ pairs with highest cpi in levels
##############################################

cpi_max1 <- cpi_used[cpi_used$cpi_mean == max(cpi_used$cpi_mean),]


# 2.2 Date-CZ pairs with the highest cpi month-on-month change
##############################################################

cpi_max2 <- cpi_used[cpi_used$cpi_mom_mean == max(cpi_used$cpi_mom_mean),]


#############################################################
##### 3. Match inflation expectations to sci with cases #####
#############################################################


# 3.1 calculate changes in expectations in all regions
######################################################

inflex_sub1 <- inflex %>%
  group_by(cz2000) %>%
  # change of current period versus last period
  mutate(chg_inflex_med = inflexp_median - dplyr::lag(inflexp_median)) %>%
  # only use period identified in case
  filter(date == unique(cpi_max1$Date))

# same procedure for alternative case
inflex_sub2 <- inflex %>%
  group_by(cz2000) %>%
  mutate(chg_inflex_med = inflexp_median - dplyr::lag(inflexp_median)) %>%
  filter(date == unique(cpi_max2$Date))

# Note that this is less than for all commuting zones because for some we do not have data for these specific months


# 3.2 Match sci to expectations
#################################################################

# for case 1: high index
dat1 <- sci_control %>%
  arrange(user_loc,fr_loc) %>%
  # inflation expectations in the user's location
  inner_join(inflex_sub1, by = c("user_loc" = "cz2000")) %>%
  # share of connections with cpi experience
  group_by(user_loc) %>%
  summarise(share_high = sum(share_sci[fr_loc %in% cpi_max1$cz2000]), chg_inflex_med, inflexp_median) %>%
  ungroup %>%
  unique

# for case 2: high rate of change
dat2 <- sci_control %>%
  # inflation expectations in the user's location
  inner_join(inflex_sub2, by = c("user_loc" = "cz2000")) %>%
  # share of connections with cpi experience
  group_by(user_loc) %>%
  summarise(share_high = sum(share_sci[fr_loc %in% cpi_max2$cz2000]), chg_inflex_med, inflexp_median) %>%
  ungroup %>%
  unique

# save scatter plot for case 1
png("../SocialInflationExpectation/_output/case_study_1.png", width = 500, height = 500)
plot(dat1$share_high,dat1$chg_inflex_med, main = "Case Study 1", xlab = "Share of friends in high inflation region", ylab="Median inflation expectations")
text(paste("Correlation:", round(cor(dat1$share_high,dat1$chg_inflex_med), 2)), x = 0.2, y = 60)
dev.off()
  
# save scatter plot for case 2
png("../SocialInflationExpectation/_output/case_study_2.png", width = 500, height = 500)
plot(dat2$share_high,dat2$chg_inflex_med, main = "Case Study 2", xlab = "Share of friends in high inflation region", ylab="Median inflation expectations")
text(paste("Correlation:", round(cor(dat2$share_high,dat2$chg_inflex_med), 2)), x = 0.2, y = 60)
dev.off()


#################################################
##### 4. Show expectations before and after #####
#################################################

# function to add and subtract months
add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)


inflex_sub3 <- inflex %>%
  # only use period identified in case plus one before and one after
  filter(date == unique(cpi_max1$Date) | date == add.months(unique(cpi_max1$Date),1)[2] | date == add.months(unique(cpi_max1$Date),-1)[2])


dat3 <- sci_control %>%
  arrange(user_loc,fr_loc) %>%
  # inflation expectations in the user's location
  inner_join(inflex_sub3, by = c("user_loc" = "cz2000")) %>%
  # share of connections with cpi experience
  group_by(user_loc) %>%
  summarise(share_high = sum(share_sci[fr_loc %in% cpi_max1$cz2000]), inflexp_median, date) %>%
  ungroup %>%
  unique


inflex_sub4 <- inflex %>%
  # only use period identified in case plus one before and one after
  filter(date == unique(cpi_max2$Date) | date == add.months(unique(cpi_max2$Date),1)[2] | date == add.months(unique(cpi_max2$Date),-1)[2])


dat4 <- sci_control %>%
  arrange(user_loc,fr_loc) %>%
  # inflation expectations in the user's location
  inner_join(inflex_sub4, by = c("user_loc" = "cz2000")) %>%
  # share of connections with cpi experience
  group_by(user_loc) %>%
  summarise(share_high = sum(share_sci[fr_loc %in% cpi_max2$cz2000]), inflexp_median, date) %>%
  ungroup %>%
  unique


# save scatter plot for case 4
png("../SocialInflationExpectation/_output/case_study_4.png", width = 500, height = 500)
# plot points prior to inflation exposure in red
plot(dat4$share_high[dat4$date == add.months(unique(cpi_max2$Date),-1)[2]],dat4$inflexp_median[dat4$date == add.months(unique(cpi_max2$Date),-1)[2]], main = "Case Study 4", xlab = "Share of friends in high inflation region", ylab="Median inflation expectations", col = 2)
# plot points during inflation exposure in green
points(dat4$share_high[dat4$date == add.months(unique(cpi_max2$Date),1)[1]],dat4$inflexp_median[dat4$date == add.months(unique(cpi_max2$Date),1)[1]], col = 3)
# plot points after inflation exposure in blue
points(dat4$share_high[dat4$date == add.months(unique(cpi_max2$Date),1)[2]],dat4$inflexp_median[dat4$date == add.months(unique(cpi_max2$Date),1)[2]], col = 4)
dev.off()
