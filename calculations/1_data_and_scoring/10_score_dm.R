# Calculate Score for Mark-View-Direction Tasks

for(n in 1:participants){
  vdirectiontasks_cat$participant_ <- vdirectiontasks_raw$participant
  vdirectiontasks_cat$time_ <- vdirectiontasks_raw$time
  
  # DM
  for(a in 1:4){
    if (vdirectiontasks_raw[[4+a]][[n]] <= DMlimit){
      vdirectiontasks_cat[[2+a]][[n]] <- 1
    } else {
      vdirectiontasks_cat[[2+a]][[n]] <- 0
    }
  }
  
  # score
  for (a in 1: 4){
    vdirectiontasks_cat[[14+a]][[n]] <- 2*(vdirectiontasks_cat[[2+a]][[n]])
  }
   
}