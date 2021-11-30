# Calculate Cronbach's Alpha and Item-Total Correlation of each Task

# Create Data.Frames with Scores of different combinations of tasks ----------------------------------------------------
  
  # Scoring for LOCr and LNVr tasks (Location Tasks without Process Data)
    locationtasks_cat_r <- dplyr::select(locationtasks_cat, -(33:44), -participant, -time, -locpan1, -locpan2, -locpan3, -locpan4, -locpan5, -locpan6, -loczoom1, -loczoom2, -loczoom3, -loczoom4, -loczoom5, -loczoom6, -lnvroute1, -lnvroute2, -lnvroute3, -lnvroute4, -lnvroute5, -lnvroute6)
  
  # Scoring for LOCc and LNVc tasks (Location Tasks with Process Data)
    locationtasks_cat_c <- dplyr::select(locationtasks_cat, -(1:32))

  # Scoring for DM and DA tasks (Direction Tasks without Process Data)
    vdirectiontasks_cat_r <- dplyr::select(vdirectiontasks_cat, -(15:22), -participant_, -time_, -daturn1, -daturn2, - daturn3, - daturn4)
  
  # Scoring for DMc and DAc tasks (Direction Tasks with Process Data)
    vdirectiontasks_cat_c <- dplyr::select(vdirectiontasks_cat, -(1:14))
    
  # Scoring for all tasks (without Process Data)
    tasks_all <- cbind(locationtasks_cat_r,vdirectiontasks_cat_r)
      
  # Scoring for all tasks c (with Process Data)
    tasks_all_c <- cbind(locationtasks_cat_c,vdirectiontasks_cat_c)

# Create empty list for dropped tasks  ---------------------------------------------------------------------------------
    all_dropped_tasks <- c()
    
# Calculate Cronbach's Alpha and drop Tasks  -----------------------------------------------------------------------
    
  # Define functions ---
    
      # Determine tasks to drop (with item-total correlation less than 0.3)
        determine_dropped_tasks <- function(scoring){
          # find names of tasks that need to be dropped
          dropped_tasks2 <- c()
          for (a in 1:ncol(scoring)){
            if(item_corr[[a]] <= 0.3){
              dropped_tasks2[a] <- colnames(scoring[a])
            }
          }
          # delete NA values from list
          dropped_tasks <- c()
          b = 1
          if(!is.null(dropped_tasks2)){ # excluding empty values
            for(a in 1:length(dropped_tasks2)){
              if(!is.na(dropped_tasks2[a]))
              {
                dropped_tasks[b] <- dropped_tasks2[a]
                b <- b+1
              }
            }
          }
          # return result of function
          return(dropped_tasks)
        }
  
  # LOCr and LNVr ---
    
      itemanalysis <- psych::alpha(locationtasks_cat_r, check.keys=FALSE, warnings=FALSE)
    # cronbach's alpha
      alpha_locr <- itemanalysis$total$raw_alpha
    # item-total correlation (Trennschärfe)
      item_corr <- itemanalysis$item.stats$r.cor
      item_corr_locr <- item_corr
    # drop tasks
      dropped_tasks_locr <- determine_dropped_tasks(locationtasks_cat_r)
    # drop tasks by item-total correlation value
      locationtasks_cat_r_drop <- locationtasks_cat_r
      for (a in 1:12){
        if(item_corr[[a]] <= 0.3){
          locationtasks_cat_r_drop <- dplyr::select(locationtasks_cat_r, -colnames(locationtasks_cat_r[a]))
        }
      }
    # repeat item analysis after dropping tasks
      itemanalysis_drop <- psych::alpha(locationtasks_cat_r_drop, check.keys=FALSE, warnings=FALSE)
      alpha_locr_drop <- itemanalysis_drop$total$raw_alpha
  
  # LOCc and LNVc ---
      
    itemanalysis <- psych::alpha(locationtasks_cat_c, check.keys=FALSE, warnings=FALSE)
    # cronbach's alpha
      alpha_locc <- itemanalysis$total$raw_alpha
    # item-total correlation (Trennschärfe)
      item_corr <- itemanalysis$item.stats$r.cor
      item_corr_locc <- item_corr
    # drop tasks
      dropped_tasks_locc <- determine_dropped_tasks(locationtasks_cat_c)
    # drop tasks by item-total correlation value
      locationtasks_cat_c_drop <- locationtasks_cat_c
      for (a in 1:12){
        if(item_corr[[a]] <= 0.3){
          locationtasks_cat_c_drop <- dplyr::select(locationtasks_cat_c, -colnames(locationtasks_cat_c[a]))
        }
      }
    # repeat item analysis after dropping tasks
      itemanalysis_drop <- psych::alpha(locationtasks_cat_c_drop, check.keys=FALSE, warnings=FALSE)
      alpha_locc_drop <- itemanalysis_drop$total$raw_alpha
    # store dropped tasks
      d1 <- 1
      d2 <- 1
      if(!is.null(dropped_tasks_locc)){
        while(!is.na(dropped_tasks_locc[d1])){
          all_dropped_tasks[d2] <- dropped_tasks_locc[d1]
          d1 <- d1+1
          d2 <- d2+1
        }
      }
    
  # DM and DA ---
    
    itemanalysis <- psych::alpha(vdirectiontasks_cat_r, check.keys=FALSE, warnings=FALSE)
    # cronbach's alpha
      alpha_dir <- itemanalysis$total$raw_alpha
    # item-total correlation (Trennschärfe)
      item_corr <- itemanalysis$item.stats$r.cor
      item_corr_dir <- item_corr
    # drop tasks
      dropped_tasks_dir <- determine_dropped_tasks(vdirectiontasks_cat_r)
    # drop tasks by item-total correlation value
      vdirectiontasks_cat_r_drop <- vdirectiontasks_cat_r
      for (a in 1:8){
        if(item_corr[[a]] <= 0.3){
          vdirectiontasks_cat_r_drop <- dplyr::select(vdirectiontasks_cat_r, -colnames(vdirectiontasks_cat_r[a]))
        }
      }
    # repeat item analysis after dropping tasks
      itemanalysis_drop <- psych::alpha(vdirectiontasks_cat_r_drop, check.keys=FALSE, warnings=FALSE)
      alpha_dir_drop <- itemanalysis_drop$total$raw_alpha
    
  # DMc and DAc ---
      
    itemanalysis <- psych::alpha(vdirectiontasks_cat_c, check.keys=FALSE, warnings=FALSE)
    # cronbach's alpha
      alpha_dirc <- itemanalysis$total$raw_alpha
    # item-total correlation (Trennschärfe)
      item_corr <- itemanalysis$item.stats$r.cor
      item_corr_dirc <- item_corr
    # drop tasks
      dropped_tasks_dirc <- determine_dropped_tasks(vdirectiontasks_cat_c)
    # drop tasks by item-total correlation value
      vdirectiontasks_cat_c_drop <- vdirectiontasks_cat_c
      for (a in 1:4){
        if(item_corr[[a]] <= 0.3){
          vdirectiontasks_cat_c_drop <- dplyr::select(vdirectiontasks_cat_c, -colnames(vdirectiontasks_cat_c[a]))
        }
      }
    # repeat item analysis after dropping tasks
      itemanalysis_drop <- psych::alpha(vdirectiontasks_cat_c_drop, check.keys=FALSE, warnings=FALSE)
      alpha_dirc_drop <- itemanalysis_drop$total$raw_alpha
    # store dropped tasks
      d1 <- 1
      if(!is.null(dropped_tasks_dirc)){
        while(!is.na(dropped_tasks_dirc[d1])){
          all_dropped_tasks[d2] <- dropped_tasks_dirc[d1]
          d1 <- d1+1
          d2 <- d2+1
        }
      }
      
  # All tasks ---
      
      itemanalysis <- psych::alpha(tasks_all, check.keys=FALSE, warnings=FALSE)
      # cronbach's alpha
      alpha_all <- itemanalysis$total$raw_alpha
      # item-total correlation (Trennschärfe)
      item_corr <- itemanalysis$item.stats$r.cor
      item_corr_all <- item_corr
      # drop tasks
      dropped_tasks_all <- determine_dropped_tasks(tasks_all)
      # drop tasks by item-total correlation value
      tasks_all_drop <- tasks_all
      for (a in 1:8){
        if(item_corr[[a]] <= 0.3){
          tasks_all_drop <- dplyr::select(tasks_all, -colnames(tasks_all[a]))
        }
      }
      # repeat item analysis after dropping tasks
      itemanalysis_drop <- psych::alpha(tasks_all_drop, check.keys=FALSE, warnings=FALSE)
      alpha_all_drop <- itemanalysis_drop$total$raw_alpha
      
  # All tasks c ---
      
      itemanalysis <- psych::alpha(tasks_all_c, check.keys=FALSE, warnings=FALSE)
      # cronbach's alpha
      alpha_allc <- itemanalysis$total$raw_alpha
      # item-total correlation (Trennschärfe)
      item_corr <- itemanalysis$item.stats$r.cor
      item_corr_allc <- item_corr
      # drop tasks
      dropped_tasks_allc <- determine_dropped_tasks(tasks_all_c)
      # drop tasks by item-total correlation value
      tasks_allc_drop <- tasks_all_c
      for (a in 1:8){
        if(item_corr[[a]] <= 0.3){
          tasks_allc_drop <- dplyr::select(tasks_all_c, -colnames(tasks_all_c[a]))
        }
      }
      # repeat item analysis after dropping tasks
      itemanalysis_drop_c <- psych::alpha(tasks_allc_drop, check.keys=FALSE, warnings=FALSE)
      alpha_allc_drop <- itemanalysis_drop$total$raw_alpha
      
  