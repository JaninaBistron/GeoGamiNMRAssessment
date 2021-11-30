# Parameter Optimization for Sensitivity Analysis on the thresholds (for accuracy and process data)

# Under construction!!!

# Libraries --------------------------------------------------------------------------------------------------------

library("tidyverse")
library("lubridate")
library("data.table")
library("rgdal")
library("sf")
library("sp")
library("psych")
library("GA")

# Load files -------------------------------------------------------------------------------------------------------

# choose location
  location = "Senden"
  #location = "Duelmen"

if (location == "Senden"){
    load("results/senden/raw_data.Rda")
    load("results/senden/taskperformance_loc.Rda")
    load("results/senden/taskperformance_dir.Rda")
  } else {
      load("results/duelmen/raw_data.Rda")
      load("results/duelmen/taskperformance_loc.Rda")
      load("results/duelmen/taskperformance_dir.Rda")
    }

participants <- nrow(logfile)

# Create functions -------------------------------------------------------------------------------------------------

# For determining the mean of reliability and validity coefficient (for all tasks incl. process data)
  
  RelValCalc <- function(x) {
    LOClimit <- x[1]
    locpanlimit <- x[2]
    loczoomlimit <- x[3]
    LNVlimit <- x[4] 
    lnvroutefactor <- x[5]
    DMlimit <- x[6]
    DAlimit <- x[7]
    daturnlimit <- x[8]
    
    # calculate reliability and validity
    
    source("calculations/1_data_and_scoring/06_score_loctasks.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/07_score_loc.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/08_score_lnv.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/09_score_dirtasks.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/10_score_dm.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/11_score_da.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/01_cronbachsalpha_itemtotal.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/02_taskdifficulty.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/03_splithalfreliability.R", encoding="utf-8", local = TRUE)
    source("calculations/3_testeval_scoredistrib_validity/01_totalscores.R", encoding="utf-8", local = TRUE)
    source("calculations/3_testeval_scoredistrib_validity/02_validity.R", encoding="utf-8", local = TRUE)
    
    rel_val <- (rel_Totalc + cor_valid_total_c) / 2
    
    return(rel_val)
  }

# For determining one minus the mean of reliability and validity coefficient (for all tasks incl. process data)

  RelValCalc_min <- function(x) {
    rel_val = 1 - RelValCalc (x)
    return(rel_val)
  }
  
# For determining the reliability coefficient (for all tasks incl. process data)

  RelCalc <- function(x) {
    LOClimit <- x[1]
    locpanlimit <- x[2]
    loczoomlimit <- x[3]
    LNVlimit <- x[4] 
    lnvroutefactor <- x[5]
    DMlimit <- x[6]
    DAlimit <- x[7]
    daturnlimit <- x[8]
    
    # calculate reliability and validity
    
    source("calculations/1_data_and_scoring/06_score_loctasks.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/07_score_loc.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/08_score_lnv.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/09_score_dirtasks.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/10_score_dm.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/11_score_da.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/01_cronbachsalpha_itemtotal.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/02_taskdifficulty.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/03_splithalfreliability.R", encoding="utf-8", local = TRUE)
    source("calculations/3_testeval_scoredistrib_validity/01_totalscores.R", encoding="utf-8", local = TRUE)
    source("calculations/3_testeval_scoredistrib_validity/02_validity.R", encoding="utf-8", local = TRUE)
    
    rel <- rel_Totalc
    
    return(rel)
  }
  
# For determining one minus the reliability coefficient (for all tasks incl. process data)
  
  RelCalc_min <- function(x) {
    rel = 1 - RelCalc (x)
    return(rel)
  }

# For determining the validity coefficient (for all tasks incl. process data)
  
  ValCalc <- function(x) {
    LOClimit <- x[1]
    locpanlimit <- x[2]
    loczoomlimit <- x[3]
    LNVlimit <- x[4] 
    lnvroutefactor <- x[5]
    DMlimit <- x[6]
    DAlimit <- x[7]
    daturnlimit <- x[8]
    
    # calculate reliability and validity
    
    source("calculations/1_data_and_scoring/06_score_loctasks.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/07_score_loc.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/08_score_lnv.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/09_score_dirtasks.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/10_score_dm.R", encoding="utf-8", local = TRUE)
    source("calculations/1_data_and_scoring/11_score_da.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/01_cronbachsalpha_itemtotal.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/02_taskdifficulty.R", encoding="utf-8", local = TRUE)
    source("calculations/2_testeval_items_reliability/03_splithalfreliability.R", encoding="utf-8", local = TRUE)
    source("calculations/3_testeval_scoredistrib_validity/01_totalscores.R", encoding="utf-8", local = TRUE)
    source("calculations/3_testeval_scoredistrib_validity/02_validity.R", encoding="utf-8", local = TRUE)
    
    val <- cor_valid_total_c
    
    return(val)
  }
  
# For determining one minus the validity coefficient (for all tasks incl. process data)
  
  ValCalc_min <- function(x) {
    val = 1 - ValCalc (x)
    return(val)
  }

# Parameter Optimization -------------------------------------------------------------------------------------------

  # suggested optimal thresholds (for max. reliability and validity coefficients)
    LOClimit_sug <- 10
    locpanlimit_sug <- 25
    loczoomlimit_sug <- 10
    LNVlimit_sug <- 5
    lnvroutefactor_sug <- 2
    DMlimit_sug <- 25
    DAlimit_sug <- 25
    daturnlimit_sug <- 500
    suggestedThres <- matrix(c(LOClimit_sug,locpanlimit_sug,loczoomlimit_sug,LNVlimit_sug,lnvroutefactor_sug,DMlimit_sug,DAlimit_sug,daturnlimit_sug), nrow = 1, ncol = 8, byrow = TRUE)
    
  # suggested sub-optimal thresholds (for min. reliability and validity coefficients)
    LOClimit_sug_min <- 20
    locpanlimit_sug_min <- 40
    loczoomlimit_sug_min <- 40
    LNVlimit_sug_min <- 20
    lnvroutefactor_sug_min <- 2
    DMlimit_sug_min <- 5
    DAlimit_sug_min <- 5
    daturnlimit_sug_min <- 1200
    suggestedThres_min <- matrix(c(LOClimit_sug_min,locpanlimit_sug_min,loczoomlimit_sug_min,LNVlimit_sug_min,lnvroutefactor_sug_min,DMlimit_sug_min,DAlimit_sug_min,daturnlimit_sug_min), nrow = 1, ncol = 8, byrow = TRUE)

  # genetic algorithms (see: https://cran.r-project.org/web/packages/GA/vignettes/GA.html)
    
      # for the mean of reliability and validity coefficient
      GA_RelVal_max <- ga(type = "real-valued", # max. coefficients
               fitness =  RelValCalc,
               lower =  c(5, 5, 5, 5, 2, 5, 5, 200), upper =  c(20, 40, 40, 20, 5, 30, 30, 1200),
               suggestions = suggestedThres,
               popSize = 50, maxiter = 100)
      GA_RelVal_min <- ga(type = "real-valued", # min. coefficients
                fitness =  RelValCalc_min,
                lower =  c(5, 5, 5, 5, 2, 5, 5, 200), upper =  c(20, 40, 40, 20, 5, 30, 30, 1200),
                suggestions = suggestedThres_min,
                popSize = 50, maxiter = 100)
      
      # for reliability coefficient
      GA_Rel_max <- ga(type = "real-valued", # max. coefficients
                fitness =  RelCalc,
                lower =  c(5, 5, 5, 5, 2, 5, 5, 200), upper =  c(20, 40, 40, 20, 5, 30, 30, 1200),
                suggestions = suggestedThres,
                popSize = 50, maxiter = 100)
      GA_Rel_min <- ga(type = "real-valued", # min. coefficients
                fitness =  RelCalc_min,
                lower =  c(5, 5, 5, 5, 2, 5, 5, 200), upper =  c(20, 40, 40, 20, 5, 30, 30, 1200),
                suggestions = suggestedThres_min,
                popSize = 50, maxiter = 100)
      
      # for validity coefficient
      GA_Val_max <- ga(type = "real-valued", # max. coefficients
                fitness =  ValCalc,
                lower =  c(5, 5, 5, 5, 2, 5, 5, 200), upper =  c(20, 40, 40, 20, 5, 30, 30, 1200),
                suggestions = suggestedThres,
                popSize = 50, maxiter = 100)
      GA_Val_min <- ga(type = "real-valued", # min. coefficients
                fitness =  ValCalc_min,
                lower =  c(5, 5, 5, 5, 2, 5, 5, 200), upper =  c(20, 40, 40, 20, 5, 30, 30, 1200),
                suggestions = suggestedThres_min,
                popSize = 50, maxiter = 100)
  
  # show results of parameter optimization
    
      summary(GA_RelVal_max)
      plot(GA_RelVal_max, main="Approx. of max. mean of reliability & validity coefficient")
      summary(GA_RelVal_min)
      plot(GA_RelVal_min, main="Approx. of min. mean of reliability & validity coefficient")
      
      summary(GA_Rel_max)
      plot(GA_Rel_max, main="Approx. of max. reliability coefficient")
      summary(GA_Rel_min)
      plot(GA_Rel_min, main="Approx. of min. reliability coefficient")   
      
      summary(GA_Val_max)
      plot(GA_Val_max, main="Approx. of max. validity coefficient")
      summary(GA_Val_min)
      plot(GA_Val_min, main="Approx. of min.validity coefficients")
      
# Save Files -------------------------------------------------------------------------------------------------------
# files on the parameter optimization can be found in the results folder of the project
      
if(location=="Senden"){
  save(GA_RelVal_max,file="results/senden/sensanalysis/relval_max.Rda")
  save(GA_Rel_max,file="results/senden/sensanalysis/rel_max.Rda")
  save(GA_Val_max,file="results/senden/sensanalysis/val_max.Rda")
  save(GA_RelVal_min,file="results/senden/sensanalysis/relval_min.Rda")
  save(GA_Rel_min,file="results/senden/sensanalysis/rel_min.Rda")
  save(GA_Val_min,file="results/senden/sensanalysis/val_min.Rda")
} else {
  if(location=="Duelmen"){
    save(GA_RelVal_max,file="results/duelmen/sensanalysis/relval_max.Rda")
    save(GA_Rel_max,file="results/duelmen/sensanalysis/rel_max.Rda")
    save(GA_Val_max,file="results/duelmen/sensanalysis/val_max.Rda")
    save(GA_RelVal_min,file="results/duelmen/sensanalysis/relval_min.Rda")
    save(GA_Rel_min,file="results/duelmen/sensanalysis/rel_min.Rda")
    save(GA_Val_min,file="results/duelmen/sensanalysis/val_min.Rda")
  } else {
      save(GA_RelVal_max,file="results/custom/sensanalysis/relval_max.Rda")
      save(GA_Rel_max,file="results/custom/sensanalysis/rel_max.Rda")
      save(GA_Val_max,file="results/custom/sensanalysis/val_max.Rda")
      save(GA_RelVal_min,file="results/custom/sensanalysis/relval_min.Rda")
      save(GA_Rel_min,file="results/custom/sensanalysis/rel_min.Rda")
      save(GA_Val_min,file="results/custom/sensanalysis/ral_min.Rda")
  }
}
