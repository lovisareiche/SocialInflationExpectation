# Some code to not be deleted

# First summarise foreign location in commuting zones
for (i in unique(dat_sci$user_loc)) {
  
  print(i)
  
  us_subset <- filter(dat_sci,user_loc == i)
  U = uniq(us_subset$cz2000)
  a = accumarray(U$n,us_subset$scaled_sci)
  
  if (i==min(dat_sci$user_loc)){
    dat_sci_final <- tibble(user_loc_fips = rep(i,times=length(a)), fr_loc_cz2000 = as.character(U$b), sci = a)
  } else {
    dat_sci_final <- rbind(dat_sci_final,tibble(user_loc_fips = rep(i,times=length(a)), fr_loc_cz2000 = as.character(U$b), sci = a))
  }
  
}

write_tsv(dat_sci_final,"../SocialInflationExpectation/_intermediate/sci_county_cz.tsv")

# Then summarise user locations
