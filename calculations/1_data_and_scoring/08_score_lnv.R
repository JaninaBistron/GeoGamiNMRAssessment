# Calculate Score for Self-Location Tasks

# calculate Limit Value 'lnvroutelimit' - individual for each task  ----------------------------------------------------
    
    # only correct answers - set wrong answers to 0
      locationtasks_raw_correct <- locationtasks_raw
      for(n in 1:participants){
        for(a in 1:6){
          if (locationtasks_raw[[32+4*a]][[n]] >= LNVlimit){
            locationtasks_raw_correct[[34+4*a]][[n]] <- NA
          }
        }
      }
      
      # determine lnvroutelimit data.frame
      lnvroutelimit <- data.frame(matrix(ncol=1, nrow=6))
      for(a in 1:6){
        
        # minimum of correct answers multiplied with 3
        lnvroutelimit[[1]][[a]] <- min(locationtasks_raw_correct[[34+4*a]], na.rm=T)*lnvroutefactor
      }

# calculate scoring ---------------------------------------------------------------------------------------------------
for(n in 1:participants){
  
  # LNV
  
  for(a in 1:6){
    if (locationtasks_raw[[32+4*a]][[n]] <= LNVlimit){
      locationtasks_cat[[19+2*a]][[n]] <- 1
    } else {
      locationtasks_cat[[19+2*a]][[n]] <- 0
    }
  }    
  
  # lnvroute
  for(a in 1:6){
    if (locationtasks_raw[[34+4*a]][[n]] <= lnvroutelimit[[1]][[a]]){
      locationtasks_cat[[20+2*a]][[n]] <- 1
    } else {
      locationtasks_cat[[20+2*a]][[n]] <- 0
    }
  }
  
  # score
  for (a in 1:6){
    if (locationtasks_cat[[19+2*a]][[n]] == 1){
      if (locationtasks_cat[[20+2*a]][[n]] == 1){
        locationtasks_cat[[38+a]][[n]] <- 2
      } else {
        locationtasks_cat[[38+a]][[n]] <- 1
      }
    } else {
      locationtasks_cat[[38+a]][[n]] <- 0
    }
  }
}