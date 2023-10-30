##########################################
# AUTHOR: Evelyn G Shu                   #
# CONTACT: evelyn@firststreet.org        #
# DATE CREATED: 23 Oct, 2023             #
# ABOUT: Future pop projections          #
##########################################

setwd("/climate_mig")

#############################
##### Require Packages #####
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(dplyr, data.table, stringr, bit64)

#############################
# load future block projections
block_projections <- fread("~/DATA/full_block_projections.csv", stringsAsFactors = F, data.table = F)
block_projections$block_id <- str_pad(as.character(block_projections$GEOID), 15, side = "left", pad = "0")
block_projections$tract_id <- substring(block_projections$block_id, 1, 11)
block_projections$state_id <- substring(block_projections$tract_id, 1,2)

block_projections <- block_projections[,c("ssp22020","ssp22025","ssp22050","ssp22055","block_id","tract_id")]
block_projections$ssp2_cur <- (block_projections$ssp22020 + block_projections$ssp22025)/2
block_projections$ssp2_fut <- (block_projections$ssp22050 + block_projections$ssp22055)/2

# load flood data
fsf_flood <- fread("~/DATA/property_proportion.csv", stringsAsFactors = F, data.table = F)
  
fsf_flood$block_id <- str_pad(as.character(fsf_flood$block_id), 15, side = "left", pad = "0")
fsf_flood[is.na(fsf_flood)] <- 0 # NULL values are those with no flooding

block_projections <- merge(block_projections, fsf_flood, by = "block_id", all.x = T)

# add state abbreviation and fips crosswalk
state_id <- fread("state_id.csv", stringsAsFactors = F, data.table = F)
state_id$state_id <- str_pad(as.character(state_id$state_id), 2, side = "left", pad = "0")

block_projections <- merge(block_projections, state_id, by = "state_id", all.x = T)

# create needed future variables
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

# load coefficients
coeff_df_long <- fread("~/DATA/coeff_df_long.csv", stringsAsFactors = F, data.table = F)

for (i in states){
 start <- Sys.time()
 print(i)
 
 data <- subset(block_projections, st_abbr == i)
 data$treat5_5y<-ifelse(data$prop_5y_c > mean(data$prop_5y_c, na.rm=T), 1, 0)
 data$treat5_20y<-ifelse(data$prop_20y_c > mean(data$prop_20y_c, na.rm=T), 1, 0)
 data$treat5_100y<-ifelse(data$prop_100y_c > mean(data$prop_100y_c, na.rm=T), 1, 0)
 data$treat5_500y<-ifelse(data$prop_500y_c > mean(data$prop_500y_c, na.rm=T), 1, 0)

  coeff_df <- subset(coeff_df_long, state==i)
  prop5 <- subset(coeff_df, coeff_df$coeff_names=="prop_5y_c")
  prop20 <- subset(coeff_df, coeff_df$coeff_names=="prop_20y_c")
  prop100 <- subset(coeff_df, coeff_df$coeff_names=="prop_100y_c")
  prop500 <- subset(coeff_df, coeff_df$coeff_names=="prop_500y_c")
  treat5 <- subset(coeff_df, coeff_df$coeff_names=="treat5_5y")
  treat20 <- subset(coeff_df, coeff_df$coeff_names=="treat5_20y")
  treat100 <- subset(coeff_df, coeff_df$coeff_names=="treat5_100y")
  treat500 <- subset(coeff_df, coeff_df$coeff_names=="treat5_500y")
  int5 <- subset(coeff_df, coeff_df$coeff_names=="prop_5y_c:treat5_5y")
  int20 <- subset(coeff_df, coeff_df$coeff_names=="prop_20y_c:treat5_20y")
  int100 <- subset(coeff_df, coeff_df$coeff_names=="prop_100y_c:treat5_100y")
  int500 <- subset(coeff_df, coeff_df$coeff_names=="prop_500y_c:treat5_500y")
  prop5_sq<- subset(coeff_df, coeff_df$coeff_names=="I(prop_5y_c^2)")
  prop20_sq<- subset(coeff_df, coeff_df$coeff_names=="I(prop_20y_c^2)")
  prop100_sq<- subset(coeff_df, coeff_df$coeff_names=="I(prop_100y_c^2)")
  prop500_sq<- subset(coeff_df, coeff_df$coeff_names=="I(prop_500y_c^2)")

  # baseline growth
  data$b_chg_fut <- ((data$ssp2_fut - data$ssp2_cur)/data$ssp2_cur)*(100/30)
  data$b_chg_fut <- ifelse(is.na(data$b_chg_fut), 0, full$b_chg_fut)

  # adjusted growth
  data$p_fut <- data$ssp2_cur + (data$ssp2_cur * (30/10000) * ((100 * data$b_chg_fut) 
                                    +(ifelse(length(prop5$Estimate)==1,prop5$Estimate,0)*data$prop_5y_f)
                                    +(ifelse(length(treat5$Estimate)==1,treat5$Estimate,0)*data$treat5_5y)
                                    +(ifelse(length(prop20$Estimate)==1,prop20$Estimate,0)*data$prop_20y_f)
                                    +(ifelse(length(treat20$Estimate)==1,treat20$Estimate,0)*data$treat5_20y)
                                    +(ifelse(length(int5$Estimate)==1,int5$Estimate,0)*data$prop_5y_f*data$treat5_5y)
                                    +(ifelse(length(int20$Estimate)==1,int20$Estimate,0)*data$prop_20y_f*data$treat5_20y)
                                    +(ifelse(length(prop100$Estimate)==1,prop100$Estimate,0)*data$prop_100y_f)
                                    +(ifelse(length(treat100$Estimate)==1,treat100$Estimate,0)*data$treat5_100y)
                                    +(ifelse(length(prop500$Estimate)==1,prop500$Estimate,0)*data$prop_500y_f)
                                    +(ifelse(length(treat500$Estimate)==1,treat500$Estimate,0)*data$treat5_500y)
                                    +(ifelse(length(int100$Estimate)==1,int100$Estimate,0)*data$prop_100y_f*data$treat5_100y)
                                    +(ifelse(length(int500$Estimate)==1,int500$Estimate,0)*data$prop_500y_f*data$treat5_500y)
                                    +(ifelse(length(prop5_sq$Estimate)==1,prop5_sq$Estimate,0)*((data$prop_5y_f)^2))
                                    +(ifelse(length(prop20_sq$Estimate)==1,prop20_sq$Estimate,0)*((data$prop_20y_f)^2))
                                    +(ifelse(length(prop100_sq$Estimate)==1,prop100_sq$Estimate,0)*((data$prop_100y_f)^2))
                                    +(ifelse(length(prop500_sq$Estimate)==1,prop500_sq$Estimate,0)*((data$prop_500y_f)^2))
                                    ))
 
  data$p_fut <- ifelse(data$p_fut < 0, 0, data$p_fut)

  # rebalance future by county
  data$tract_id<-str_pad(as.character(data$tract_id), 11, side="left", pad="0")
  data$cnty_id <- substr(data$tract_id, 0, 5)
  
  ssp_agg_cnty <- aggregate(data$ssp2_fut, by = list(data$cnty_id), FUN = sum, na.rm = T)
  names(ssp_agg_cnty)<-c("cnty_id","cnty_total_b")
  ssp_agg_cnty$cnty_total_b <- as.numeric(ssp_agg_cnty$cnty_total_b)
  
  res_agg_cnty <- aggregate(data$p_fut, by = list(data$cnty_id), FUN = sum, na.rm = T)
  names(res_agg_cnty)<-c("cnty_id","cnty_total_p")
  res_agg_cnty$cnty_total_p <- as.numeric(res_agg_cnty$cnty_total_p)
  
  data <- merge(data, ssp_agg_cnty, by="cnty_id", all.x=T)
  full <- merge(data, res_agg_cnty, by="cnty_id", all.x=T)
  data$p_prop_cnty <- as.numeric(data$p_fut / data$cnty_total_p)
  data$p_fut <- as.numeric(data$p_prop_cnty * data$cnty_total_b)
  data$cnty_total_p <- NULL
  
  
 fwrite(data, paste0(working,"DATA/state_files/fut_proj_",i,".csv"))
 gc()
 end <- Sys.time()
 print(end - start)
}
