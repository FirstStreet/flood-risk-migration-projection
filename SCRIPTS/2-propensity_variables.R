##########################################
# AUTHOR: Evelyn G Shu                   #
# CONTACT: evelyn@firststreet.org        #
# DATE CREATED: 23 Oct, 2023             #
# ABOUT: Manage propensity variables     #
##########################################

setwd("~/climate_mig")

################################
#### load required packages ####
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(stringr, tidycensus, ipumsr, data.table, readxl)

################################
# read job growth data 
job_growth<-read.csv("DATA/job_growth.csv") 
job_growth$cty<-substr(job_growth$cty, 4,8)

# read job density data
job_density<-read.csv("DATA/job_density.csv")
job_density$tract<-str_pad(job_density$tract, 11, side=c("left"),pad="0")
job_density$cty<-substr(job_density$tract,1,5)

score_var<-data_frame()
score_var<-merge(select(job_density, tract, cty, Density_of_Jobs_in_2013), 
                 select(job_growth, cty, Job_Growth_Rate_from_2004_to_2013), 
                 by="cty", 
                 all.x=T, all.y=T)

# read pop density data
pop_density<-read.csv("DATA/tract_popdensity.csv")
pop_density$tract<-str_pad(pop_density$tract, 11, side=c("left"),pad="0")
pop_density$cty<-substr(pop_density$tract,1,5)

score_var<-merge(score_var,
                 select(pop_density, tract, Population_Density_in_2010),
                 by="tract", 
                 all.x=T, all.y=T)

# read distance of centroid of tract to nearest coast
dist_coast<-read.csv("DATA/dist_2_coast.csv")
dist_coast <- dist_coast %>% select(one_of('tract_id','HubDist'))
dist_coast$tract_id <- str_pad(as.character(dist_coast$tract_id), 11, side = "left", pad = "0")
names(dist_coast)<-c("tract","dist_coast")
dist_coast$cty <- substr(dist_coast$tract, 1,5)

score_var<-merge(dist_coast,
                 select(mdi_rate, cty, mdi),
                 by="cty", 
                 all.x=T, all.y=T)

# read distance of centroid of tract to nearest river
dist_river<-read.csv("DATA/dist_2_river.csv")
dist_river <- dist_river %>% select(one_of('tract_id','HubDist'))
dist_river$tract_id <- str_pad(as.character(dist_river$tract_id), 11, side = "left", pad = "0")
names(dist_river)<-c("tract","dist_river")

score_var<-merge(score_var,
                 dist_river,
                 by="tract", 
                 all.x=T, all.y=T)

# read census demographic data
# census_data <- load_variables(2020, "pl", cache=T) 
# census_data <- load_variables(2020, "acs5", cache=T)

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

census_score_vars<-data.frame()

vars <- c(med_income = 'B06011_001',
          med_homeval = 'B25077_001',
          employed = 'B23025_004',
          unemployed = 'B23025_005',
          labor_force = 'B23025_002',
          nlabor_force = 'B23025_007')

for(i in states){
  data <- get_acs(state= i,
                  geography="tract",
                  variables=vars,
                  survey="acs5",
                  output="wide")
   census_score_vars <- rbind(census_score_vars,data)
 }

census_score_vars <- select(census_score_vars, GEOID, med_incomeE, med_homevalE, employedE, unemployedE, labor_forceE, nlabor_forceE)
names(census_score_vars) <- c("tract", "med_income", "med_homeval", "employed", "unemployed", "labor_force", "nlabor_force")
census_score_vars$tract <- str_pad(census_score_vars$tract, 11, side=c("left"), pad="0")
census_score_vars$tract <- str_pad(census_score_vars$tract, 11, side=c("left"), pad="0")

score_var$tract <- str_pad(score_var$tract, 11, side=c("left"), pad="0")
score_var <- merge(census_score_vars2, score_var, by="tract", all.x=T, all.y=T)

fwrite(score_var, "DATA/score_variables.csv")
