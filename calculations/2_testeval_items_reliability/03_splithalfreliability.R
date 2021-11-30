# Calculate Test Half-Scores and Split-Half-Reliability

# Testhalfscores -------------------------------------------------------------------------------------------------------

  # create new data.frame
  testhalfscores <- data.frame(matrix(ncol=29, nrow=participants))
  colnames(testhalfscores) <- c("participant", "LOCrT1", "LOCrT2", "LNVrT1", "LNVrT2", "LOCcT1", "LOCcT2", "LNVcT1", "LNVcT2", "DMT1", "DMT2", "DAT1", "DAT2", "DMcT1", "DMcT2", "DAcT1", "DAcT2", "LrT1", "LrT2", "LcT1", "LcT2", "DT1", "DT2", "DTc1", "DTc2", "TotalrT1", "TotalrT2", "TotalcT1", "TotalcT2")
  testhalfscores$participant <- c(seq(1:participants))
  
  # fill data.frame with score data (the order of tasks in each test half is similar to the order of task twins)
  for(n in 1:participants){
    testhalfscores$LOCrT1[[n]] <- locationtasks_cat[[3*taskdifficulties_locr$number[[1]]]][[n]] + locationtasks_cat[[3*taskdifficulties_locr$number[[4]]]][[n]] + locationtasks_cat[[3*taskdifficulties_locr$number[[5]]]][[n]]
    testhalfscores$LOCrT2[[n]] <- locationtasks_cat[[3*taskdifficulties_locr$number[[2]]]][[n]] + locationtasks_cat[[3*taskdifficulties_locr$number[[3]]]][[n]] + locationtasks_cat[[3*taskdifficulties_locr$number[[6]]]][[n]]
    testhalfscores$LNVrT2[[n]] <- locationtasks_cat[[19+2*taskdifficulties_lnvr$number[[1]]]][[n]] + locationtasks_cat[[19+2*taskdifficulties_lnvr$number[[4]]]][[n]] + locationtasks_cat[[19+2*taskdifficulties_lnvr$number[[5]]]][[n]]
    testhalfscores$LNVrT1[[n]] <- locationtasks_cat[[19+2*taskdifficulties_lnvr$number[[2]]]][[n]] + locationtasks_cat[[19+2*taskdifficulties_lnvr$number[[3]]]][[n]] + locationtasks_cat[[19+2*taskdifficulties_lnvr$number[[6]]]][[n]]
    testhalfscores$LOCcT1[[n]] <- locationtasks_cat[[32+taskdifficulties_locc$number[[1]]]][[n]] + locationtasks_cat[[32+taskdifficulties_locc$number[[4]]]][[n]] + locationtasks_cat[[32+taskdifficulties_locc$number[[5]]]][[n]]
    testhalfscores$LOCcT2[[n]] <- locationtasks_cat[[32+taskdifficulties_locc$number[[2]]]][[n]] + locationtasks_cat[[32+taskdifficulties_locc$number[[3]]]][[n]] + locationtasks_cat[[32+taskdifficulties_locc$number[[6]]]][[n]]
    testhalfscores$LNVcT2[[n]] <- locationtasks_cat[[38+taskdifficulties_lnvc$number[[1]]]][[n]] + locationtasks_cat[[38+taskdifficulties_lnvc$number[[4]]]][[n]] + locationtasks_cat[[38+taskdifficulties_lnvc$number[[5]]]][[n]]
    testhalfscores$LNVcT1[[n]] <- locationtasks_cat[[38+taskdifficulties_lnvc$number[[2]]]][[n]] + locationtasks_cat[[38+taskdifficulties_lnvc$number[[3]]]][[n]] + locationtasks_cat[[38+taskdifficulties_lnvc$number[[6]]]][[n]]
    testhalfscores$DMT1[[n]] <- vdirectiontasks_cat[[2+taskdifficulties_dm$number[[1]]]][[n]] + vdirectiontasks_cat[[2+taskdifficulties_dm$number[[4]]]][[n]]
    testhalfscores$DMT2[[n]] <- vdirectiontasks_cat[[2+taskdifficulties_dm$number[[2]]]][[n]] + vdirectiontasks_cat[[2+taskdifficulties_dm$number[[3]]]][[n]]
    testhalfscores$DAT1[[n]] <- vdirectiontasks_cat[[5+2*+taskdifficulties_da$number[[1]]]][[n]] + vdirectiontasks_cat[[5+2*taskdifficulties_da$number[[4]]]][[n]]
    testhalfscores$DAT2[[n]] <- vdirectiontasks_cat[[5+2*taskdifficulties_da$number[[2]]]][[n]] + vdirectiontasks_cat[[5+2*taskdifficulties_da$number[[3]]]][[n]]
    testhalfscores$DMcT1[[n]] <- vdirectiontasks_cat[[14+taskdifficulties_dmc$number[[1]]]][[n]] + vdirectiontasks_cat[[14+taskdifficulties_dmc$number[[4]]]][[n]]
    testhalfscores$DMcT2[[n]] <- vdirectiontasks_cat[[14+taskdifficulties_dmc$number[[2]]]][[n]] + vdirectiontasks_cat[[14+taskdifficulties_dmc$number[[3]]]][[n]]
    testhalfscores$DAcT1[[n]] <- vdirectiontasks_cat[[18+taskdifficulties_dac$number[[1]]]][[n]] + vdirectiontasks_cat[[18+taskdifficulties_dac$number[[4]]]][[n]]
    testhalfscores$DAcT2[[n]] <- vdirectiontasks_cat[[18+taskdifficulties_dac$number[[2]]]][[n]] + vdirectiontasks_cat[[18+taskdifficulties_dac$number[[3]]]][[n]]
  }
  
# Testhalfscores AFTER dropping tasks  ---------------------------------------------------------------------------------

    # store dropped task names and their twin partner in a vector
    tasks_to_delete <- c()
    for(d1 in 1:length(all_dropped_tasks)){
      for(d2 in 1:20){
        if(!is.null(all_dropped_tasks)){ # excluding empty values
          if(task_twins[[d2]][[1]]==all_dropped_tasks[d1] | task_twins[[d2]][[2]]==all_dropped_tasks[d1]){
            d4 <- d1*2
            d3 <- d4-1
            tasks_to_delete[d3] <- task_twins[[d2]][[1]]
            tasks_to_delete[d4] <- task_twins[[d2]][[2]]
          }
        }
      }
    }
    tasks_to_delete <- unique(tasks_to_delete) # delete distinct values
    
    # data.frame for testhalfscores without dropped tasks
    testhalfscores_droppedtasks <- testhalfscores
    
    # delete unneeded columns
    testhalfscores_droppedtasks <- dplyr::select(testhalfscores_droppedtasks, -LOCrT1, -LOCrT2, -LNVrT1, -LNVrT2, -DMT1, -DMT2, -DAT1, -DAT2, -LrT1, - LrT2, -DT1, -DT2, -TotalrT1, -TotalrT2)
    
    # data.frame with dropped tasks points for each participant
    for(n in 1:participants){
      # set sum of points to delete to 0 for each participant
      points_to_delete_sum_loc_T1 <- 0
      points_to_delete_sum_loc_T2 <- 0
      points_to_delete_sum_lnv_T1 <- 0
      points_to_delete_sum_lnv_T2 <- 0
      points_to_delete_sum_dm_T1 <- 0
      points_to_delete_sum_dm_T2 <- 0
      points_to_delete_sum_da_T1 <- 0
      points_to_delete_sum_da_T2 <- 0
      for(d in 1:length(tasks_to_delete)){
        # sum of all points to delete - sorted by task type
        if (tasks_to_delete[d] == "LOC1c" | tasks_to_delete[d] == "LOC2c" | tasks_to_delete[d] == "LOC3c" | tasks_to_delete[d] == "LOC4c" | tasks_to_delete[d] == "LOC5c" | tasks_to_delete[d] == "LOC6c"){
          if (tasks_to_delete[d] == task_twins$LOCcTWINe[1] | tasks_to_delete[d] == task_twins$LOCcTWINm[1] | tasks_to_delete[d] == task_twins$LOCcTWINh[1]){
            # if this task belongs to the first test half
            points_to_delete_sum_loc_T1 <- points_to_delete_sum_loc_T1 + locationtasks_cat[tasks_to_delete[d]][[1]][[n]]
          }else{
            # if this task belongs to the second test half
            points_to_delete_sum_loc_T2 <- points_to_delete_sum_loc_T2 + locationtasks_cat[tasks_to_delete[d]][[1]][[n]]
          }
        }
        if (tasks_to_delete[d] == "LNV1c" | tasks_to_delete[d] == "LNV2c" | tasks_to_delete[d] == "LNV3c" | tasks_to_delete[d] == "LNV4c" | tasks_to_delete[d] == "LNV5c" | tasks_to_delete[d] == "LNV6c"){
          if (tasks_to_delete[d] == task_twins$LNVcTWINe[1] | tasks_to_delete[d] == task_twins$LNVcTWINm[1] | tasks_to_delete[d] == task_twins$LNVcTWINh[1]){
            # if this task belongs to the first test half
            points_to_delete_sum_lnv_T1 <- points_to_delete_sum_lnv_T1 + locationtasks_cat[tasks_to_delete[d]][[1]][[n]]
          }else{
            # if this task belongs to the second test half
            points_to_delete_sum_lnv_T2 <- points_to_delete_sum_lnv_T2 + locationtasks_cat[tasks_to_delete[d]][[1]][[n]]
          }        
        }
        if (tasks_to_delete[d] == "DM1c" | tasks_to_delete[d] == "DM2c" | tasks_to_delete[d] == "DM3c" | tasks_to_delete[d] == "DM4c"){
          if (tasks_to_delete[d] == task_twins$DMcTWINe[1] | tasks_to_delete[d] == task_twins$DMcTWINh[1]){
            # if this task belongs to the first test half
            points_to_delete_sum_dm_T1 <- points_to_delete_sum_dm_T1 + vdirectiontasks_cat[tasks_to_delete[d]][[1]][[n]]
          }else{
            # if this task belongs to the second test half
            points_to_delete_sum_dm_T2 <- points_to_delete_sum_dm_T2 + vdirectiontasks_cat[tasks_to_delete[d]][[1]][[n]]
          }
        }
        if (tasks_to_delete[d] == "DA1c" | tasks_to_delete[d] == "DA2c" | tasks_to_delete[d] == "DA3c" | tasks_to_delete[d] == "DA4c"){
          if (tasks_to_delete[d] == task_twins$DAcTWINe[1] | tasks_to_delete[d] == task_twins$DAcTWINh[1]){
            # if this task belongs to the first test half
            points_to_delete_sum_da_T1 <- points_to_delete_sum_da_T1 + vdirectiontasks_cat[tasks_to_delete[d]][[1]][[n]]
          }else{
            # if this task belongs to the second test half
            points_to_delete_sum_da_T2 <- points_to_delete_sum_da_T2 + vdirectiontasks_cat[tasks_to_delete[d]][[1]][[n]]
          }
        }
      }
      # delete points
      testhalfscores_droppedtasks$LOCcT1[[n]] <- testhalfscores$LOCcT1[[n]] - points_to_delete_sum_loc_T1
      testhalfscores_droppedtasks$LOCcT2[[n]] <- testhalfscores$LOCcT2[[n]] - points_to_delete_sum_loc_T2
      testhalfscores_droppedtasks$LNVcT1[[n]] <- testhalfscores$LNVcT1[[n]] - points_to_delete_sum_lnv_T1
      testhalfscores_droppedtasks$LNVcT2[[n]] <- testhalfscores$LNVcT2[[n]] - points_to_delete_sum_lnv_T2
      testhalfscores_droppedtasks$DMcT1[[n]] <- testhalfscores$DMcT1[[n]] - points_to_delete_sum_dm_T1
      testhalfscores_droppedtasks$DMcT2[[n]] <- testhalfscores$DMcT2[[n]] - points_to_delete_sum_dm_T2
      testhalfscores_droppedtasks$DAcT1[[n]] <- testhalfscores$DAcT1[[n]] - points_to_delete_sum_da_T1
      testhalfscores_droppedtasks$DAcT2[[n]] <- testhalfscores$DAcT2[[n]] - points_to_delete_sum_da_T2
    }
    
# Calculate Split-Half Reliability  ------------------------------------------------------------------------------------
  
  # Total Scores
  testhalfscores$LrT1 <- testhalfscores$LOCrT1 + testhalfscores$LNVrT2
  testhalfscores$LrT2 <- testhalfscores$LOCrT2 + testhalfscores$LNVrT1
  testhalfscores$LcT1 <- testhalfscores$LOCcT1 + testhalfscores$LNVcT2
  testhalfscores$LcT2 <- testhalfscores$LOCcT2 + testhalfscores$LNVcT1
  testhalfscores$DT1 <- testhalfscores$DMT1 + testhalfscores$DAT2
  testhalfscores$DT2 <- testhalfscores$DMT2 + testhalfscores$DAT1
  testhalfscores$DTc1 <- testhalfscores$DMcT1 + testhalfscores$DAcT2
  testhalfscores$DTc2 <- testhalfscores$DMcT2 + testhalfscores$DAcT1
  testhalfscores$TotalrT1 <- testhalfscores$LrT1 + testhalfscores$DT2
  testhalfscores$TotalrT2 <- testhalfscores$LrT2 + testhalfscores$DT1
  testhalfscores$TotalcT1 <- testhalfscores$LcT1 + testhalfscores$DTc2
  testhalfscores$TotalcT2 <- testhalfscores$LcT2 + testhalfscores$DTc1
  # Total Scores after dropping tasks
  testhalfscores_droppedtasks$LcT1 <- testhalfscores_droppedtasks$LOCcT1 + testhalfscores_droppedtasks$LNVcT2
  testhalfscores_droppedtasks$LcT2 <- testhalfscores_droppedtasks$LOCcT2 + testhalfscores_droppedtasks$LNVcT1
  testhalfscores_droppedtasks$DTc1 <- testhalfscores_droppedtasks$DMcT1 + testhalfscores_droppedtasks$DAcT2
  testhalfscores_droppedtasks$DTc2 <- testhalfscores_droppedtasks$DMcT2 + testhalfscores_droppedtasks$DAcT1
  testhalfscores_droppedtasks$TotalcT1 <- testhalfscores_droppedtasks$LcT1 + testhalfscores_droppedtasks$DTc2
  testhalfscores_droppedtasks$TotalcT2 <- testhalfscores_droppedtasks$LcT2 + testhalfscores_droppedtasks$DTc1
  
  # correlation coefficients for groups of tasks (spearman's rho)
  cor_rel_Lr <- suppressWarnings(cor.test(testhalfscores$LrT1, testhalfscores$LrT2, method="spearman")$estimate[[1]])
  cor_rel_Lc <- suppressWarnings(cor.test(testhalfscores$LcT1, testhalfscores$LcT2, method="spearman")$estimate[[1]])
  cor_rel_Totalr <- suppressWarnings(cor.test(testhalfscores$TotalrT1, testhalfscores$TotalrT2, method="spearman")$estimate[[1]])
  cor_rel_Totalc <- suppressWarnings(cor.test(testhalfscores$TotalcT1, testhalfscores$TotalcT2, method="spearman")$estimate[[1]])
  # correlation coefficients for groups of tasks after dropping tasks (spearman's rho)
  cor_rel_Lc_drop <- suppressWarnings(cor.test(testhalfscores_droppedtasks$LcT1, testhalfscores_droppedtasks$LcT2, method="spearman")$estimate[[1]])
  cor_rel_Totalc_drop <- suppressWarnings(cor.test(testhalfscores_droppedtasks$TotalcT1, testhalfscores_droppedtasks$TotalcT2, method="spearman")$estimate[[1]])
  
  # p-values for spearman's rho
  cor_rel_Lr_p <- suppressWarnings(cor.test(testhalfscores$LrT1, testhalfscores$LrT2, method="spearman")$p.value)
  cor_rel_Lc_p <- suppressWarnings(cor.test(testhalfscores$LcT1, testhalfscores$LcT2, method="spearman")$p.value)
  cor_rel_Totalr_p <- suppressWarnings(cor.test(testhalfscores$TotalrT1, testhalfscores$TotalrT2, method="spearman")$p.value)
  cor_rel_Totalc_p <- suppressWarnings(cor.test(testhalfscores$TotalcT1, testhalfscores$TotalcT2, method="spearman")$p.value)
  # p-values for spearman's rho - after dropping tasks
  cor_rel_Lc_drop_p <- suppressWarnings(cor.test(testhalfscores_droppedtasks$LcT1, testhalfscores_droppedtasks$LcT2, method="spearman")$p.value)
  cor_rel_Totalc_drop_p <- suppressWarnings(cor.test(testhalfscores_droppedtasks$TotalcT1, testhalfscores_droppedtasks$TotalcT2, method="spearman")$p.value)
  
  # Spli-half reliability - Spearman Brown Formula
  rel_Lr <- 2*cor_rel_Lr/(1+cor_rel_Lr)
  rel_Lc <- 2*cor_rel_Lc/(1+cor_rel_Lc)
  rel_Totalr <- 2*cor_rel_Totalr/(1+cor_rel_Totalr)
  rel_Totalc <- 2*cor_rel_Totalc/(1+cor_rel_Totalc)
  # Spli-half reliability after dropping tasks - Spearman Brown Formula
  rel_Lc_drop <- 2*cor_rel_Lc_drop/(1+cor_rel_Lc_drop)
  rel_Totalc_drop <- 2*cor_rel_Totalc_drop/(1+cor_rel_Totalc_drop)
  
 