##########################################
# AUTHOR: Evelyn G Shu                   #
# CONTACT: evelyn@firststreet.org        #
# DATE CREATED: 23 Oct, 2023             #
# ABOUT: Data prep pop and psm           #
##########################################

setwd("~/climate_mig")

##########################
#### install packages ####
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(data.table, stringr, sf, dplyr)

##########################
#### Get pop data ########

pop_hist <- fread("past_pops.csv", stringsAsFactors = F, data.table = F, select = c(1,9:27,3))

pop_hist$block_id <- str_pad(as.character(pop_hist$block_id), 15, side = "left", pad = "0")
pop_hist$tract_id <- substr(pop_hist$block_id,1,11)
pop_hist$county_id <- substr(pop_hist$block_id,1,5)
pop_hist$state_id <- substr(pop_hist$block_id,1,2)

# add state abbreviation and fips crosswalk
state_id <- fread("state_id.csv", stringsAsFactors = F, data.table = F)
state_id$state_id <- str_pad(as.character(state_id$state_id), 2, side = "left", pad = "0")
pop_hist <- merge(pop_hist, state_id, by = "state_id", all.x = T)

states <- c("ky", "tn", "wv", "mi", "co",
           "in", "mn", "il", "ar", "mo",
           "ia", "az", "ut", "nv", "ok",
           "ne", "sd", "nd", "nm", "wy",
           "mt", "oh", "wi", "ks", "id",
           "de", "dc", "ct", "me", "or",
           "va", "ri", "vt", "md", "pa",
           "nj", "ny", "ma", "wa", "nh",
           "fl", "ga", "sc", "nc", "tx",
           "al", "la", "ms", "ca")

working <- getwd()
st_files <- list.files(paste0(working,"/DATA/state_files"), pattern="past_pop")


for (i in st_files){
  start <- Sys.time()
  print(i)
  
  # Propensity Matching Characterisics
  data <- fread(paste0(working,"DATA/state_files/",i), stringsAsFactors = F, data.table = F)
  score <- fread("score_variables.csv", stringsAsFactors = F, data.table = F)
  score$tract_id <- str_pad(as.character(score$tract), 11, side = "left", pad = "0")
  score$cty <- str_pad(as.character(score$cty), 5, side = "left", pad = "0")
  score$tract <- NULL
  
  data$tract_id <- str_pad(as.character(data$tract_id), 11, side = "left", pad = "0")
  
  full <- merge(data, score, by = "tract_id", all.x = T)
  
  # read county shape files
  base <- st_read("~/cb_2018_us_county_500k.shp")
  base <- base[,c(5,6)]
  base$geometry <- NULL
  
  # historic data compilation: County Level NOAA Disaster Declarations
  files <- list.files("~/data")
  for (file in files[1:5]){
    print(file)
    data <- fread(paste0(working,"/data/",file), stringsAsFactors = F, data.table = F)
    data$cnty_id <- str_pad(as.character(data$cntyidfp), 5, side = "left", pad = "0")
    base <- merge(base, data, by.x = "GEOID", by.y = "cnty_id", all.x = T)
  }
  
  names(base) <- c("county_id",
                   "name",
                   "diast_9600",
                   "diast_0105",
                   "diast_0610",
                   "diast_1115",
                   "diast_1620")
  
  base[is.na(base)] <- 0
  
  full <- merge(full, base, by = "county_id", all.x = T)
  
  
  # historic data compilation: proportion of properties at risk by RP scenario
  data <- fread("~/DATA/property_proportion.csv", 
                stringsAsFactors = F, data.table = F)
  
  data$tabblock_id <- str_pad(as.character(data$tabblock_id), 15, side = "left", pad = "0")
  data[is.na(data)] <- 0 # NULL values are those with no flooding
  
  full <- merge(full, data, by.x = "block_id", by.y = "tabblock_id", all.x = T)
  full$proportion_10yr <- ifelse(is.na(full$proportion_10yr), 0, full$proportion_10yr)
  full$proportion_5yr <- ifelse(is.na(full$proportion_5yr), 0, full$proportion_5yr)
  full$proportion_2yr <- ifelse(is.na(full$proportion_2yr), 0, full$proportion_2yr)
  full$proportion_20yr <- ifelse(is.na(full$proportion_20yr), 0, full$proportion_20yr)
  
  # historic data compilation: Tract Level Local Flood Reports
  data <- fread("~/DATA/nlp_flood_report_tract.csv", 
                stringsAsFactors = F, data.table = F)
  data$tract_id <- str_pad(as.character(data$j_tract_id), 11, side = "left", pad = "0")
  
  data$count <- 1
  tract_fl_report <- aggregate(data$count,
                               by = list(data$tract_id),
                               FUN = sum, na.rm = T)
  names(tract_fl_report) <- c("tract_id", "fl_reports")
  tract_fl_report$fl_hist <- ifelse(tract_fl_report$fl_reports>=1, 1, 0)
           
  full <- merge(full, tract_fl_report, by = "tract_id", all.x = T)
  full$fl_reports <- ifelse(is.na(full$fl_reports), 0, full$fl_reports)
  
  ######################################
  # Working File for Propensity Matching Script/Process
  fwrite (full, paste0(working,"DATA/state_files/propensity_matching_",i,".csv"))
          gc()
          end <- Sys.time()
          print(end - start)
}

st_files2 <- list.files(paste0(working,"DATA/state_files"), pattern="propensity_matching_")

for(i in st_files2){
  start <- Sys.time()
  print(i)
  
  full <- fread(paste0(working,"DATA/state_files/",i), stringsAsFactors = F, data.table = F)
  full$tract_id <- str_pad(as.character(full$tract_id), 11, side = "left", pad = "0")
  full$block_id <- str_pad(as.character(full$block_id), 15, side = "left", pad = "0")
  
  ################################
  # adding lat and long data, at tract level to serve as shift share analysis control
  data<-fread("~/DATA/lat_long.csv",
             stringsAsFactors = F, data.table = F)
  data$tract_id <- str_pad(as.character(data$tract_id), 11, side = "left", pad = "0")
  full <- merge(full, data, by = "tract_id", all.x = T)
  
  ################################
  # Grouping Treatment and Control by Flood Exposure
  # here other treatment variables may be added for testing
  full$treatment <- ifelse(full$prop_2y_c > mean(full$prop_2y_c, na.rm=T) |
                              full$prop_5y_c > mean(full$prop_5y_c, na.rm=T) |
                              full$prop_20y_c > mean(full$prop_20y_c, na.rm=T) |
                              full$prop_100y_c > mean(full$prop_100y_c, na.rm=T) |
                              full$prop_500y_c > mean(full$prop_500y_c, na.rm=T),
                            1, 0)
  
  ################################
  # Adding in Future Projections
  
  data <- fread("/DATA/full_block_projections.csv")
  data$block_id <- str_pad(as.character(data$GEOID), 15, side = "left", pad = "0")
  
  data$future_chg <- (data$ssp22050 - data$ssp22020)/30
  data$future_chg[!is.finite(data$future_chg)] <- NA

  full$block_id <- str_pad(as.character(full$block_id), 15, side = "left", pad = "0")
  
  full <- merge(full, dplyr::select(data, block_id, ssp22020, ssp22050, future_chg),
                by = "block_id", all.x = T)
  
  ################################
  # create additional variables
  full$block_chg <-as.numeric(full$pop_2020 - full$pop_2000)/20
  full$block_chg[!is.finite(full$block_chg)] <- NA
  full$employ_rate <- as.numeric((full$employed)/(full$labor_force))
  
  ################################
  # Working File for Propensity Matching Script/Process
  
  fwrite (full, paste0(working,"DATA/state_files/",i))
  gc()
  end <- Sys.time()
  print(end - start)
}
