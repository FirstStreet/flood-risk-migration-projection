##########################################
# AUTHOR: Evelyn G Shu                   #
# CONTACT: evelyn@firststreet.org        #
# DATE CREATED: 23 Oct, 2023             #
# ABOUT: Propensity Score Matching by ST #
##########################################

setwd("/climate_mig")

#############################
##### Require Packages #####
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(dplyr, MatchIt, data.table, stringr, bit64)


dataset_h_cov <- c('job_density', 'job_growth_rate',
                   'pop_2000','pop_density','med_income',
                   'med_homeval','mdi', 'employ_rate', 'dist_river','dist_coast')

working <- getwd()
st_files3 <- list.files(paste0(working,"DATA/state_files"), pattern="propensity_matching_")

for(i in st_files3){
  start <- Sys.time()
  print(i)
  
  full <- fread(paste0(working,"DATA/state_files/",i), stringsAsFactors = F, data.table = F)
  full$tract_id <- str_pad(as.character(full$tract_id), 11, side = "left", pad = "0")
  full$block_id <- str_pad(as.character(full$block_id), 15, side = "left", pad = "0")
  
  print(with(full, t.test(block_chg ~ treatment)))

  #############################
  # Making a dataframe without NA 
  full <- full %>%  # MatchIt does not allow missing values
    dplyr::select(tract_id, block_id, county_id, state_id, 
                  pop_2000, pop_2020, job_density, job_growth_rate,
                  pop_density, med_income, med_homeval,
                  mdi, dist_river, dist_coast, fl_reports, noaa_evnt,
                  long, lat, prop_2y_c, prop_5y_c, prop_20y_c, prop_100y_c,
                  prop_500y_c, prop_2y_f, prop_5y_f, prop_20y_f, prop_100y_f,
                  prop_500y_f, treatment,
                  block_chg, employ_rate, ssp22020, ssp22050, future_chg, 
                  one_of(dataset_h_cov)) %>% 
    na.omit()
  
  full$treat5_2y<-ifelse(full$prop_2y_c > mean(full$prop_2y_c, na.rm=T),
                         1, 0)
  full$treat5_5y<-ifelse(full$prop_5y_c > mean(full$prop_5y_c, na.rm=T),
                         1, 0)
  full$treat5_20y<-ifelse(full$prop_20y_c > mean(full$prop_20y_c, na.rm=T),
                          1, 0)
  full$treat5_100y<-ifelse(full$prop_100y_c > mean(full$prop_100y_c, na.rm=T),
                           1, 0)
  full$treat5_500y<-ifelse(full$prop_500y_c > mean(full$prop_500y_c, na.rm=T),
                           1, 0)

  #############################
  # matching to create sample
  mod_match <- matchit(treatment ~ job_density +job_growth_rate  
                       +pop_2000 +pop_density +med_income 
                       +med_homeval +mdi +employ_rate +dist_river +dist_coast,
                       method = "nearest", data = full)
  dta_m <- match.data(mod_match)
  
  print(dim(dta_m))
  
  fwrite(dta_m, paste0("DATA/state_files/msample_",i))
  gc()
  end <- Sys.time()
  print(end-start)
}
