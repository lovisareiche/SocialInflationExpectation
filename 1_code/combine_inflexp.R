

library(tidyverse)
library(lubridate)

dat_13_16 <- read_csv2("D:/Lovisa/Studium/Oxford/Department of Economics/RA/data/FRBNY-SCE-Public-Microdata-Complete-13-16.csv")

dat_17_19 <- read_csv2("D:/Lovisa/Studium/Oxford/Department of Economics/RA/data/FRBNY-SCE-Public-Microdata-Complete-17-19.csv")

dat_20_22 <- read_csv2("D:/Lovisa/Studium/Oxford/Department of Economics/RA/data/frbny-sce-public-microdata-complete-20-present.csv")

# check that they have the same columns

colnames(dat_13_16) == colnames(dat_17_19)
colnames(dat_13_16) == colnames(dat_20_22)

# bind them together

inflexp_dat <- rbind(dat_13_16,dat_17_19,dat_20_22)


write_csv(inflexp_dat, "../SocialInflationExpectation/_input/FRBNY-SCE-Public-Microdata-Complete.csv")




