# Collect and Calculate Task Performance Data for Direction Tasks

# Create Data.Frame for Task Performance of each Participant  --------------------------------------------------------------

vdirectiontasks_raw <- data.frame(matrix(ncol=16, nrow=participants))
colnames(vdirectiontasks_raw) <- c("participant","time","duration","totalRoute",
                                   # DM = four mark-view-direction tasks (see definition of variables below)
                                   "DM1","DM2","DM3","DM4",
                                   # DA = four adopt-view-direction tasks (see definition of variables below)
                                   "DA1","daturn1",
                                   "DA2","daturn2",
                                   "DA3","daturn3",
                                   "DA4","daturn4")

# Fill Data.Frame with Data  -------------------------------------------------------------------------------------------

vdirectiontasks_raw$participant <- logfile$players
vdirectiontasks_raw$time <- strftime(logfile$start)
vdirectiontasks_raw$duration <- logfile$duration
vdirectiontasks_raw$totalRoute <- logfile$totalRoute

for(n in 1:participants){
  
    # Filter for Data
  
    temp_vdirtasks <- dplyr::filter(logfile$events[[n]], tasktype == "theme-direction")
  
    # DM (Mark-View-Direction Tasks)
    temp_markvdirtasks <- dplyr::filter(temp_vdirtasks, answertype == "MAP_DIRECTION", !is.na(angle_real_click), eventtype != "ON_MAP_CLICKED")
    temp_markvdirtasks <- dplyr::select(temp_markvdirtasks, -answertype, -angle_marked_real)
    
    # DA (Adopt-View-Direction Tasks)
    temp_adoptvdirtasks <- dplyr::filter(temp_vdirtasks, eventtype == "ON_MAP_CLICKED" | eventtype == "ON_OK_CLICKED" & questiontype != "MAP_DIRECTION")
    temp_adoptvdirtasks <- dplyr::filter(temp_adoptvdirtasks, answertype == "DIRECTION", eventtype != "ON_MAP_CLICKED")
    temp_adoptvdirtasks <- dplyr::select(temp_adoptvdirtasks, -tasktype, -eventtype, -questiontype, -taskdefinition, -dir_marked, -dir_real, 
                                         -dir_click, -longitude_marked, -latitude_marked, -longitude_real, 
                                         -latitude_real, -longitude_click, -latitude_click, -distance_marked_real, -distance_real_click, 
                                         -accuracy, -pannings, -zoomings, -answertype, -angle_real_click)
    
    # Calculate DA Sum of Turnings Data
    
      # Filter events for INIT_TASK and ON_OK_CLICKED Events to find the timestamps
      temp_adoptvdirtasksturnings <- dplyr::filter(logfile$events[[n]], questiontype=="MAP_DIRECTION_MARKER" & (eventtype =="ON_OK_CLICKED" | eventtype =="INIT_TASK"))
      # Get waypointdata of the participant n
      waypoints_for_sumofturnings <- logfile$waypoints[[n]]
      # Note rownumbers in an extra column
      waypoints_for_sumofturnings  [,"rownumber"] <- 0
      waypoints_for_sumofturnings$rownumber <- as.numeric(row.names(waypoints_for_sumofturnings))
      
      # Calculate "duration" between INIT_GAME -> INIT_TASK and INIT_GAME -> ON_OK_CLICKED for each task
      temp_adoptvdirtasksturnings [,"duration"] <- 0
      d <- 1
      for (d in 1: nrow(temp_adoptvdirtasksturnings)){
        temp_adoptvdirtasksturnings$duration[[d]] <- as.numeric(difftime(strptime(paste(temp_adoptvdirtasksturnings$timestamp[[d]]),"%Y-%m-%d %H:%M:%S"),
                                                                         strptime(paste(waypoints_for_sumofturnings$timestamp[[1]]),"%Y-%m-%d %H:%M:%S"),units="sec"))
      }
      
      # Calculate sum of turnings for each task
      temp_adoptvdirtasks [,"sumofturnings"] <- NA
      # four tasks
      for (d in 1: (nrow(temp_adoptvdirtasksturnings)/2)){
        # every second line 1, 3, 5, 7, ....
        e <- 2*d-1
        # filter for waypoints for current task by duration 
        waypoints_for_sumofturnings_temp <- dplyr::filter(waypoints_for_sumofturnings,waypoints_for_sumofturnings$rownumber>as.numeric(temp_adoptvdirtasksturnings$duration[[e]]) 
                                                          & waypoints_for_sumofturnings$rownumber<as.numeric(temp_adoptvdirtasksturnings$duration[[e+1]]))
        # temporal calculated sum of turning angles
        sumofturnings_part <- 0
        # sum of all turning angles
        sumofturnings <- 0
        
        a <- 1
        # for all lines of the waypointdata from 1 up to the last line minus one
        while (a < (nrow(waypoints_for_sumofturnings_temp-1))){
          # get first angle
          firstangle <- waypoints_for_sumofturnings_temp[[5]][[a]]
          # go to next line of the  data
          a <- a+1
          # for all lines of the data from current line up to the last line
          for (a in a: (nrow(waypoints_for_sumofturnings_temp))){
            # get second angle
            secondangle <- waypoints_for_sumofturnings_temp[[5]][[a]]
            # deviation between both angles
            angle <- abs(firstangle - secondangle)
            if (angle < 180){
              # case difference < 180
              sumofturnings_part <- angle
            } else {
              # case difference > 180
              sumofturnings_part <- 360 - angle
            }
            # get sumofturnings
            sumofturnings <- sumofturnings + sumofturnings_part
            break # break the whole for-loop and go on with the while loop
          }
        }
        temp_adoptvdirtasks$sumofturnings[[d]] <- sumofturnings
      }
      
  # Fill Data.Frame with Data   
    
    vdirectiontasks_raw$DM1[[n]] <- temp_markvdirtasks$angle_real_click[[1]]
    vdirectiontasks_raw$DM2[[n]] <- temp_markvdirtasks$angle_real_click[[2]]
    vdirectiontasks_raw$DM3[[n]] <- temp_markvdirtasks$angle_real_click[[3]]
    vdirectiontasks_raw$DM4[[n]] <- temp_markvdirtasks$angle_real_click[[4]]
    
    vdirectiontasks_raw$DA1[[n]] <- temp_adoptvdirtasks$angle_marked_real[[1]]
    vdirectiontasks_raw$DA2[[n]] <- temp_adoptvdirtasks$angle_marked_real[[2]]
    vdirectiontasks_raw$DA3[[n]] <- temp_adoptvdirtasks$angle_marked_real[[3]]
    vdirectiontasks_raw$DA4[[n]] <- temp_adoptvdirtasks$angle_marked_real[[4]]
    
    vdirectiontasks_raw$daturn1[[n]] <- temp_adoptvdirtasks$sumofturnings[[1]]
    vdirectiontasks_raw$daturn2[[n]] <- temp_adoptvdirtasks$sumofturnings[[2]]
    vdirectiontasks_raw$daturn3[[n]] <- temp_adoptvdirtasks$sumofturnings[[3]]
    vdirectiontasks_raw$daturn4[[n]] <- temp_adoptvdirtasks$sumofturnings[[4]]
    
}
