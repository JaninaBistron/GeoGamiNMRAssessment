# Calculate Score for Self-Location-Tasks

for(n in 1:participants)
{
  locationtasks_cat$participant <- locationtasks_raw$participant
  locationtasks_cat$time <- locationtasks_raw$time
  
  # LOC
  for(a in 1:6){
    if (locationtasks_raw[[1+5*a]][[n]] <= LOClimit){
      locationtasks_cat[[3*a]][[n]] <- 1
    } else {
      locationtasks_cat[[3*a]][[n]] <- 0
    }
  }
  
  # locpan
  for(a in 1:6){
    if (locationtasks_raw[[3+5*a]][[n]] <= locpanlimit){
      locationtasks_cat[[1+3*a]][[n]] <- 1
    } else {
      locationtasks_cat[[1+3*a]][[n]] <- 0
    }
  }

  # loczoom
  for(a in 1:6){
    if (locationtasks_raw[[4+5*a]][[n]] <= loczoomlimit){
      locationtasks_cat[[2+3*a]][[n]] <- 1
    } else {
      locationtasks_cat[[2+3*a]][[n]] <- 0
    }
  }
  
  # score
  for (a in 1: 6){
    if (locationtasks_cat[[3*a]][[n]] == 1){
      if (locationtasks_cat[[1+3*a]][[n]] == 1 && locationtasks_cat[[2+3*a]][[n]] == 1){
        locationtasks_cat[[32+a]][[n]] <- 2
      } else {
        locationtasks_cat[[32+a]][[n]] <- 1
      }
    } else {
      locationtasks_cat[[32+a]][[n]] <- 0
    }
  }

}