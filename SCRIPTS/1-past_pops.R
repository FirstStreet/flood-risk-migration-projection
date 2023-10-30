##########################################
# AUTHOR: Evelyn G Shu                   #
# CONTACT: evelyn@firststreet.org        #
# DATE CREATED: 23 Oct, 2023             #
# ABOUT: Historic pop data               #
##########################################

setwd("~/climate-migration")

################################
#### load required packages ####
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(stringr, tidycensus, dplyr, ipumsr, sf, data.table, maptools, sp, rgdal, spData, spDataLarge)

################################
## getting past pop data by block
census_api_key()

states <- c("ky", "tn", "wv", "mi", "co",
            "in", "mn", "il", "ar", "mo",
            "az", "ut", "ok", "nv", "ia",
            "ne", "sd", "nd", "nm", "wy",
            "mt", "oh", "wi", "ks", "id",
            "dc", "ct", "me", "or", "nj",
            "va", "ri", "vt", "md", "pa",
            "ny", "ma", "wa", "nh", "de",
            "fl", "ga", "sc", "nc", "tx",
            "al", "la", "ms", "ca")

for(i in states){
  print(i)
  start <- Sys.time()
  
  past_pops_2000<-data.frame()
  past_pops_2010<-data.frame()
  past_pops_2020<-data.frame()
  
  data <- get_decennial(state= i,
                        geography="block",
                        variables="P001001",
                        geometry=T,
                        output="wide",
                        year=2000)
  past_pops_2000 <- rbind(past_pops_2000,data)
  past_pops_2000 <- st_centroid(past_pops_2000)
  names(past_pops_2000)<-c("tabblock_id","name","pop_2000", "geom")
  
  data <- get_decennial(state= i,
                        geography="block",
                        variables="P001001",
                        geometry=T,
                        output="wide",
                        year=2010)
  past_pops_2010 <- rbind(past_pops_2010,data)
  past_pops_2010 <- st_centroid(past_pops_2010)
  names(past_pops_2010)<-c("tabblock_id","name","pop_2010", "geom")
  
  data <- get_decennial(state= i,
                        geography="block",
                        variables="P1_001N", #census variable names changed for 2020
                        geometry=T,
                        output="wide",
                        year=2020,
                        sumfile="pl")
  past_pops_2020 <- rbind(past_pops_2020,data)
  past_pops_2020 <- st_centroid(past_pops_2020)
  names(past_pops_2020)<-c("tabblock_id","name","pop_2020", "geom")

  st_geometry(past_pops_2000) <- "geom"
  st_geometry(past_pops_2010) <- "geom"
  st_geometry(past_pops_2020) <- "geom"
  
  full <- st_join(past_pops_2020, 
                  past_pops_2010, 
                  join = st_nearest_feature)
  
  full <- st_join(full, 
                  past_pops_2000, 
                  join = st_nearest_feature)
  
  full$geom <- NULL
  fwrite(full, paste0("clim_mig_pops_",i,".csv"))
  
  end <- Sys.time()
  print(end-start)
}

rm(list=ls())
gc()

files <- list.files(path = ".", pattern = "clim_mig_pops")

full <- data.frame()
for (i in files){
  data <- fread(i, stringsAsFactors = F, data.table = F)
  full <- rbind(full, data)
}

rm(list=ls()[! ls() %in% c("full")])
gc()

require(data.table)
fwrite(full, "past_pops.csv")
