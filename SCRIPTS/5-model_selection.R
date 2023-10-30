##########################################
# AUTHOR: Evelyn G Shu                   #
# CONTACT: evelyn@firststreet.org        #
# DATE CREATED: 23 Oct, 2023             #
# ABOUT: MASS OLS Model selection by ST  #
##########################################

setwd("/climate_mig")

#############################
##### Require Packages #####
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(dplyr, data.table, stringr, bit64, MASS, functional)


working <- getwd()
st_files4 <- list.files(paste0(working,"/DATA/state_files"),
                        pattern="msample_")
# remove dc and then run separately without as.factor(county)
st_files4 <- st_files4[-7] 

coeff_df <- data.frame()

for(i in st_files4){
  start <- Sys.time()
  st <- substring(i, 38, 39)
  print(st)
    
  full <- fread(paste0(working,"/DATA/state_files/",i), stringsAsFactors = F, data.table = F)
  
  full$p_blk_chg <- (((full$pop_2020 - full$pop_2000)/(full$pop_2000))*100)/20
  full <- subset(full, is.finite(full$p_blk_chg))
  
  ##### MASS OLS Model Selection - Reg Flooding #####
  req_model <- lm(p_blk_chg~ prop_5y_c*treat5_5y +prop_20y_c*treat5_20y 
                    +prop_100y_c*treat5_100y + prop_500y_c*treat5_500y,
                    data=full)
  
  full_model <- lm(p_blk_chg~ prop_5y_c*treat5_5y 
                   +I(prop_5y_c^2)
                   +prop_20y_c*treat5_20y
                   +I(prop_20y_c^2)
                   +prop_100y_c*treat5_100y
                   +I(prop_100y_c^2)
                   +prop_500y_c*treat5_500y 
                   +I(prop_500y_c^2)
                   +job_density 
                   +job_growth_rate  
                   +pop_2000*pop_density 
                   +med_income
                   +I(med_income^2)
                   +med_homeval 
                   +mdi*employ_rate 
                   +dist_river*dist_coast
                   +I(dist_coast^2)
                   +I(dist_river^2)
                   +fl_hist
                   +long
                   +lat 
                   #+as.factor(county_id) # remove this for dc
                   +future_chg
                   +I(future_chg^2),
                   data=full)
  
  step_model <- stepAIC(req_model,
                          scope=list(lower=formula(req_model),
                                     upper=formula(full_model)),
                          direction="both",
                          trace=F)
  
  print(summary(step_model))
  
  matrix_coeff <- as.data.frame(summary(step_model)$coefficients)
  matrix_coeff <- matrix_coeff[,c("Estimate","Pr(>|t|)")]
  
  matrix_coeff$coeff_names <- NULL
  matrix_coeff$coeff_names <- row.names(matrix_coeff)
  
  matrix_coeff <- matrix_coeff[matrix_coeff$coeff_names %in% 
                                        c("prop_5y_c",
                                          "treat5_5y",
                                          "I(prop_5y_c^2)",
                                          "prop_5y_c:treat5_5y",
                                          "prop_20y_c",
                                          "treat5_20y",
                                          "I(prop_20y_c^2)",
                                          "prop_20y_c:treat5_20y",
                                          "prop_100y_c",
                                          "treat5_100y",
                                          "I(prop_100y_c^2)",
                                          "prop_100y_c:treat5_100y",
                                          "prop_500y_c",
                                          "treat5_500y",
                                          "I(prop_500y_c^2)",
                                          "prop_500y_c:treat5_500y"),]
  
  matrix_coeff$state <- paste0(st)
  matrix_coeff$Estimate <- ifelse(matrix_coeff$`Pr(>|t|)` < 0.1, 
                                   matrix_coeff$Estimate, 0)
  
  coeff_df <- rbind(coeff_df, matrix_coeff)
  fwrite(coeff_df, "DATA/coeff_df.csv")

  gc()
  end <- Sys.time()
  print(end-start)
}
