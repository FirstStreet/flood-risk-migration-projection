##########################################
# AUTHOR: Evelyn G Shu                   #
# CONTACT: evelyn@firststreet.org        #
# DATE CREATED: 23 Oct, 2023             #
# ABOUT: Future downscaling block pops   #
##########################################

setwd("/climate_mig")

############################
#### Require Packages #####
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(foreign, tidycensus, stringr, dplyr)

############################
# load Hauer County level population data to downscale
county_proj <- read.csv("pop_by_ssp_county_projections.csv", stringsAsFactors = F, data.table = F)

# Download Census Data
census_api_key()
census_data <- load_variables(2010, "sf1", cache=T)
fwrite(census_data, "census_variables.csv")

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

full<-data.frame()

for(i in states){ 
  data <- get_decennial(state= i, 
                        geography="block",
                        variables="P001001",
                        geometry=T,
                        output="wide")
  data <- st_transform(data, 4269)
  full <- rbind(data,full)
}

############################
#### Create Proportions #####
full$GEOID <- str_pad(full$GEOID, 15, side=c("left"), pad="0")
full$cnty_id <- substr(full$GEOID,1,5)

agg_sum <- aggregate(full$P001001, 
                     by = list(full$cny_id), 
                     FUN = sum, 
                     na.rm = T)
names(agg_sum)<-c("county","pop")

full<-merge(full, agg_sum, by = "cnty_id")
names(full)<-c("cnty_id","GEOID","NAME","block_pop","county_pop")

full$block_prop <- full$block_pop / full$county_pop

############################
# Applying Proportions to SSP County Data 
county_proj$cnty_id<- str_pad(county_proj$geoid, 5, side=c("left"), pad="0")

#excluding columns not needed such as for years >=2060, but if using further flood projections may extend
full2<-merge(full, select(county_proj, ssp12020:ssp52055, cnty_id), by="cnty_id")

# create a looping variable for all ssp_year combinations
# loop for rewriting ssp_years by population proportions
vars <- c(7:46)
for (v in vars){
  print(v)
  full2[v] <- full2[v] * full2$block_prop
}

fwrite(full2, "DATA/full_block_projections.csv")
