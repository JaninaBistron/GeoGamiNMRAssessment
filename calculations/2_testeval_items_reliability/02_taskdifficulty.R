# Calculate Task difficulty

# Task Difficulties  ---------------------------------------------------------------------------------------------------

  # new data.frames for task difficulties
  taskdifficulties_locr <- data.frame(matrix(ncol=3, nrow=6))
  colnames(taskdifficulties_locr) <- c("task", "difficulty", "number")
  taskdifficulties_locr$number <- c(seq(1:6))
  taskdifficulties_lnvr <- data.frame(matrix(ncol=2, nrow=6))
  colnames(taskdifficulties_lnvr) <- c("task", "difficulty")
  taskdifficulties_lnvr$number <- c(seq(1:6))
  taskdifficulties_locc <- data.frame(matrix(ncol=2, nrow=6))
  colnames(taskdifficulties_locc) <- c("task", "difficulty")
  taskdifficulties_locc$number <- c(seq(1:6))
  taskdifficulties_lnvc <- data.frame(matrix(ncol=2, nrow=6))
  colnames(taskdifficulties_lnvc) <- c("task", "difficulty")
  taskdifficulties_lnvc$number <- c(seq(1:6))
  
  taskdifficulties_dm <- data.frame(matrix(ncol=2, nrow=4))
  colnames(taskdifficulties_dm) <- c("task", "difficulty")
  taskdifficulties_dm$number <- c(seq(1:4))
  taskdifficulties_da <- data.frame(matrix(ncol=2, nrow=4))
  colnames(taskdifficulties_da) <- c("task", "difficulty")
  taskdifficulties_da$number <- c(seq(1:4))
  taskdifficulties_dmc <- data.frame(matrix(ncol=2, nrow=4))
  colnames(taskdifficulties_dmc) <- c("task", "difficulty")
  taskdifficulties_dmc$number <- c(seq(1:4))
  taskdifficulties_dac <- data.frame(matrix(ncol=2, nrow=4))
  colnames(taskdifficulties_dac) <- c("task", "difficulty")
  taskdifficulties_dac$number <- c(seq(1:4))

  # fill data.frames with difficulties
    # name tasks
    taskdifficulties_locr$task <- c("LOC1r", "LOC2r", "LOC3r", "LOC4r", "LOC5r", "LOC6r") 
    taskdifficulties_lnvr$task <- c("LNV1r", "LNV2r", "LNV3r", "LNV4r", "LNV5r", "LNV6r")
    taskdifficulties_locc$task <- c("LOC1c", "LOC2c", "LOC3c", "LOC4c", "LOC5c", "LOC6c")
    taskdifficulties_lnvc$task <- c("LNV1c", "LNV2c", "LNV3c", "LNV4c", "LNV5c", "LNV6c")
    taskdifficulties_dm$task <- c("DM1", "DM2", "DM3", "DM4")
    taskdifficulties_da$task <- c("DA1", "DA2", "DA3", "DA4")
    taskdifficulties_dmc$task <- c("DM1c", "DM2c", "DM3c", "DM4c")
    taskdifficulties_dac$task <- c("DA1c", "DA2c", "DA3c", "DA4c")
  
    for(a in 1:6){
      # LOCr
      taskdifficulties_locr$difficulty[[a]] <- sum(locationtasks_cat[[3*a]])/participants
      # LNVr
      taskdifficulties_lnvr$difficulty[[a]] <- sum(locationtasks_cat[[19+2*a]])/participants
      # LOCc
      taskdifficulties_locc$difficulty[[a]] <- sum(locationtasks_cat[[32+a]])/(2*participants)
      # LNVc
      taskdifficulties_lnvc$difficulty[[a]] <- sum(locationtasks_cat[[38+a]])/(2*participants)
    }
    for(a in 1:4){
      # DM
      taskdifficulties_dm$difficulty[[a]] <- sum(vdirectiontasks_cat[[2+a]])/participants
      # DA
      taskdifficulties_da$difficulty[[a]] <- sum(vdirectiontasks_cat[[5+2*a]])/participants
      # DMc
      taskdifficulties_dmc$difficulty[[a]] <- sum(vdirectiontasks_cat[[14+a]])/(2*participants)
      # DAc
      taskdifficulties_dac$difficulty[[a]] <- sum(vdirectiontasks_cat[[18+a]])/(2*participants)
    }
    
    # Sort task difficulties
    taskdifficulties_locr <- arrange(taskdifficulties_locr, difficulty)
    taskdifficulties_lnvr <- arrange(taskdifficulties_lnvr, difficulty)
    taskdifficulties_locc <- arrange(taskdifficulties_locc, difficulty)
    taskdifficulties_lnvc <- arrange(taskdifficulties_lnvc, difficulty)
    taskdifficulties_dm <- arrange(taskdifficulties_dm, difficulty)
    taskdifficulties_da <- arrange(taskdifficulties_da, difficulty)
    taskdifficulties_dmc <- arrange(taskdifficulties_dmc, difficulty)
    taskdifficulties_dac <- arrange(taskdifficulties_dac, difficulty)

# Task Twins  ---------------------------------------------------------------------------------------------------------
  
  # create data frame for task twins
  task_twins <- data.frame(matrix(ncol=20, nrow=2))
  # e = easy, m = middle, h = hard
  colnames(task_twins) <- c("LOCrTWINe","LOCrTWINm","LOCrTWINh",
                            "LNVrTWINe","LNVrTWINm","LNVrTWINh",
                            "LOCcTWINe","LOCcTWINm","LOCcTWINh",
                            "LNVcTWINe","LNVcTWINm","LNVcTWINh",
                            "DMTWINe","DMTWINh",
                            "DATWINe","DATWINh",
                            "DMcTWINe","DMcTWINh",
                            "DAcTWINe","DAcTWINh")
  
  # fill data.frame with task twins
  task_twins$LOCrTWINe[1] <- taskdifficulties_locr$task[[1]]
  task_twins$LOCrTWINe[2] <- taskdifficulties_locr$task[[2]]
  task_twins$LOCrTWINm[1] <- taskdifficulties_locr$task[[4]]
  task_twins$LOCrTWINm[2] <- taskdifficulties_locr$task[[3]]
  task_twins$LOCrTWINh[1] <- taskdifficulties_locr$task[[5]]
  task_twins$LOCrTWINh[2] <- taskdifficulties_locr$task[[6]]
  
  task_twins$LNVrTWINe[2] <- taskdifficulties_lnvr$task[[1]]
  task_twins$LNVrTWINe[1] <- taskdifficulties_lnvr$task[[2]]
  task_twins$LNVrTWINm[2] <- taskdifficulties_lnvr$task[[4]]
  task_twins$LNVrTWINm[1] <- taskdifficulties_lnvr$task[[3]]
  task_twins$LNVrTWINh[2] <- taskdifficulties_lnvr$task[[5]]
  task_twins$LNVrTWINh[1] <- taskdifficulties_lnvr$task[[6]]
  
  task_twins$LOCcTWINe[1] <- taskdifficulties_locc$task[[1]]
  task_twins$LOCcTWINe[2] <- taskdifficulties_locc$task[[2]]
  task_twins$LOCcTWINm[1] <- taskdifficulties_locc$task[[4]]
  task_twins$LOCcTWINm[2] <- taskdifficulties_locc$task[[3]]
  task_twins$LOCcTWINh[1] <- taskdifficulties_locc$task[[5]]
  task_twins$LOCcTWINh[2] <- taskdifficulties_locc$task[[6]]
  
  task_twins$LNVcTWINe[2] <- taskdifficulties_lnvc$task[[1]]
  task_twins$LNVcTWINe[1] <- taskdifficulties_lnvc$task[[2]]
  task_twins$LNVcTWINm[2] <- taskdifficulties_lnvc$task[[4]]
  task_twins$LNVcTWINm[1] <- taskdifficulties_lnvc$task[[3]]
  task_twins$LNVcTWINh[2] <- taskdifficulties_lnvc$task[[5]]
  task_twins$LNVcTWINh[1] <- taskdifficulties_lnvc$task[[6]]
  
  task_twins$DMTWINe[1] <- taskdifficulties_dm$task[[1]]
  task_twins$DMTWINe[2] <- taskdifficulties_dm$task[[2]]
  task_twins$DMTWINh[1] <- taskdifficulties_dm$task[[4]]
  task_twins$DMTWINh[2] <- taskdifficulties_dm$task[[3]]
  
  task_twins$DATWINe[1] <- taskdifficulties_da$task[[1]]
  task_twins$DATWINe[2] <- taskdifficulties_da$task[[2]]
  task_twins$DATWINh[1] <- taskdifficulties_da$task[[4]]
  task_twins$DATWINh[2] <- taskdifficulties_da$task[[3]]
  
  task_twins$DMcTWINe[1] <- taskdifficulties_dmc$task[[1]]
  task_twins$DMcTWINe[2] <- taskdifficulties_dmc$task[[2]]
  task_twins$DMcTWINh[1] <- taskdifficulties_dmc$task[[4]]
  task_twins$DMcTWINh[2] <- taskdifficulties_dmc$task[[3]]
  
  task_twins$DAcTWINe[1] <- taskdifficulties_dac$task[[1]]
  task_twins$DAcTWINe[2] <- taskdifficulties_dac$task[[2]]
  task_twins$DAcTWINh[1] <- taskdifficulties_dac$task[[4]]
  task_twins$DAcTWINh[2] <- taskdifficulties_dac$task[[3]]