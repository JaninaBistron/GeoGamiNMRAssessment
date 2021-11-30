# Calculate Score for Adopt-View-Direction Tasks

for(n in 1:participants){
  
  # DA
  for(a in 1:4){
    if (vdirectiontasks_raw[[7+2*a]][[n]] <= DAlimit){
      vdirectiontasks_cat[[5+2*a]][[n]] <- 1
    } else {
      vdirectiontasks_cat[[5+2*a]][[n]] <- 0
    }
  }
  
  # daturn
  for(a in 1:4){
    if (vdirectiontasks_raw[[8+2*a]][[n]] <= daturnlimit){
      vdirectiontasks_cat[[6+2*a]][[n]] <- 1
    } else {
      vdirectiontasks_cat[[6+2*a]][[n]] <- 0
    }
  }
    
  # score
  for (a in 1: 4){
    if (vdirectiontasks_cat[[5+2*a]][[n]] == 1){
      if (vdirectiontasks_cat[[6+2*a]][[n]] == 1){
        vdirectiontasks_cat[[18+a]][[n]] <- 2
      } else {
        vdirectiontasks_cat[[18+a]][[n]] <- 1
      }
    } else {
      vdirectiontasks_cat[[18+a]][[n]] <- 0
    }
  }
  
}