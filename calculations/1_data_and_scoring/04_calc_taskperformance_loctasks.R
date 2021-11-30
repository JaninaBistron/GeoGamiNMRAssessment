# Collect and Calculate Task Performance Data for Location Tasks

# Create Data.Frame for Task Performance of each Participant ---------------------------------------------------------------

locationtasks_raw <- data.frame(matrix(ncol=58, nrow=participants))
colnames(locationtasks_raw) <- c("participant","time","duration","totalRoute",
                                 # LOC = six self-location tasks (see definition of variables below)
                                 "LOC1","LOC1r","locac1","locpan1","loczoom1",
                                 "LOC2","LOC2r","locac2","locpan2","loczoom2",
                                 "LOC3","LOC3r","locac3","locpan3","loczoom3",
                                 "LOC4","LOC4r","locac4","locpan4","loczoom4",
                                 "LOC5","LOC5r","locac5","locpan5","loczoom5",
                                 "LOC6","LOC6r","locac6","locpan6","loczoom6",
                                 # LNV = six navigation-to-a-flag tasks (see definition of variables below)
                                 "LNV1","LNV1r","lnvac1","lnvroute1",
                                 "LNV2","LNV2r","lnvac2","lnvroute2",
                                 "LNV3","LNV3r","lnvac3","lnvroute3",
                                 "LNV4","LNV4r","lnvac4","lnvroute4",
                                 "LNV5","LNV5r","lnvac5","lnvroute5",
                                 "LNV6","LNV6r","lnvac6","lnvroute6")

# Fill Data.Frame with Data  ------------------------------------------------------------------------------------------

locationtasks_raw$participant <- logfile$players
locationtasks_raw$time <- strftime(logfile$start)
locationtasks_raw$duration <- logfile$duration
locationtasks_raw$totalRoute <- logfile$totalRoute

for(n in 1:participants){

  # Filter for Data

  temp_loctasks <- dplyr::filter(logfile$events[[n]], tasktype == "theme-loc" | tasktype == "nav-flag")
  temp_loctasks <- dplyr::filter(temp_loctasks, eventtype == "ON_MAP_CLICKED" | eventtype == "ON_OK_CLICKED")
  temp_loctasks <- dplyr::select(temp_loctasks, -questiontype, -answertype, -taskdefinition, -dir_marked, -dir_real,
                                 -dir_click, -angle_marked_real, -angle_real_click, -longitude_marked,
                                 -latitude_marked, -longitude_real, -latitude_real, -longitude_click, -latitude_click)
  # LOC (Self-Location-Tasks)
  temp_selfloctasks <- dplyr::filter(temp_loctasks, tasktype == "theme-loc", !is.na(distance_real_click)) 
  # delete empty rows (OriGami bug: sometimes multiple OK-clicks are registered, but just one of them contains distance_real_click values)
  temp_selfloctasks <- dplyr::select(temp_selfloctasks, -distance_marked_real, -tasktype)
  temp_selfloctasks <- dplyr::filter(temp_selfloctasks, eventtype == "ON_OK_CLICKED")
  temp_selfloctasks <- dplyr::select(temp_selfloctasks, -eventtype)
  # LNV (Navigation-to-a-Flag Tasks)
  temp_navflagtasks <- dplyr::filter(temp_loctasks, tasktype == "nav-flag" & eventtype == "ON_OK_CLICKED")
  temp_navflagtasks <- dplyr::select(temp_navflagtasks, -distance_real_click, -tasktype, -eventtype,
                                     -pannings, -zoomings)

  # Calculate LNV Route Length Data

    # Filter events for INIT_TASK and ON_OK_CLICKED events to find the needed timestamps
    temp_navflagtasksroute <- dplyr::filter(logfile$events[[n]], tasktype=="nav-flag" & (eventtype =="ON_OK_CLICKED" | eventtype =="INIT_TASK"))
    # Get waypointdata of the participant n
    waypoints_for_route <- logfile$waypoints[[n]]
    # Note rownumbers in an extra column
    waypoints_for_route  [,"rownumber"] <- 0
    waypoints_for_route$rownumber <- as.numeric(row.names(waypoints_for_route))
    # Calculate "duration" between INIT_GAME -> INIT_TASK and INIT_GAME -> ON_OK_CLICKED for each task
    temp_navflagtasksroute [,"duration"] <- 0
    for (d in 1: nrow(temp_navflagtasksroute)){
      temp_navflagtasksroute$duration[[d]] <- as.numeric(difftime(strptime(paste(temp_navflagtasksroute$timestamp[[d]]),
                                                                           "%Y-%m-%d %H:%M:%S"),strptime(paste(waypoints_for_route$timestamp[[1]]),"%Y-%m-%d %H:%M:%S"),units="sec"))
    }
    # Calculate route length for each task
    temp_navflagtasks [,"route"] <- NA
    for (d in 1: (nrow(temp_navflagtasksroute)/2)){
      # every second line (1, 3, 5, 7, ....)
      e <- 2*d-1
      # filter for waypoints of current task by duration
      waypoints_for_route_temp <- dplyr::filter(waypoints_for_route, waypoints_for_route$rownumber>as.numeric(temp_navflagtasksroute$duration[[e]])
                                                & waypoints_for_route$rownumber<as.numeric(temp_navflagtasksroute$duration[[e+1]]))
      # get route length of current task
      temp_navflagtasks$route[[d]] <- sum(waypoints_for_route_temp$speed)
    }

  # Fill Data.Frame with Data

    locationtasks_raw$LOC1[[n]] <- temp_selfloctasks$distance_real_click[[1]]
    locationtasks_raw$LOC2[[n]] <- temp_selfloctasks$distance_real_click[[2]]
    locationtasks_raw$LOC3[[n]] <- temp_selfloctasks$distance_real_click[[3]]
    locationtasks_raw$LOC4[[n]] <- temp_selfloctasks$distance_real_click[[4]]
    locationtasks_raw$LOC5[[n]] <- temp_selfloctasks$distance_real_click[[5]]
    locationtasks_raw$LOC6[[n]] <- temp_selfloctasks$distance_real_click[[6]]

    locationtasks_raw$locac1[[n]] <- temp_selfloctasks$accuracy[[1]]
    locationtasks_raw$locac2[[n]] <- temp_selfloctasks$accuracy[[2]]
    locationtasks_raw$locac3[[n]] <- temp_selfloctasks$accuracy[[3]]
    locationtasks_raw$locac4[[n]] <- temp_selfloctasks$accuracy[[4]]
    locationtasks_raw$locac5[[n]] <- temp_selfloctasks$accuracy[[5]]
    locationtasks_raw$locac6[[n]] <- temp_selfloctasks$accuracy[[6]]

    locationtasks_raw$locpan1[[n]] <- temp_selfloctasks$pannings[[1]]
    locationtasks_raw$locpan2[[n]] <- temp_selfloctasks$pannings[[2]]
    locationtasks_raw$locpan3[[n]] <- temp_selfloctasks$pannings[[3]]
    locationtasks_raw$locpan4[[n]] <- temp_selfloctasks$pannings[[4]]
    locationtasks_raw$locpan5[[n]] <- temp_selfloctasks$pannings[[5]]
    locationtasks_raw$locpan6[[n]] <- temp_selfloctasks$pannings[[6]]

    locationtasks_raw$loczoom1[[n]] <- temp_selfloctasks$zoomings[[1]]
    locationtasks_raw$loczoom2[[n]] <- temp_selfloctasks$zoomings[[2]]
    locationtasks_raw$loczoom3[[n]] <- temp_selfloctasks$zoomings[[3]]
    locationtasks_raw$loczoom4[[n]] <- temp_selfloctasks$zoomings[[4]]
    locationtasks_raw$loczoom5[[n]] <- temp_selfloctasks$zoomings[[5]]
    locationtasks_raw$loczoom6[[n]] <- temp_selfloctasks$zoomings[[6]]

    locationtasks_raw$LNV1[[n]] <- temp_navflagtasks$distance_marked_real[[1]]
    locationtasks_raw$LNV2[[n]] <- temp_navflagtasks$distance_marked_real[[2]]
    locationtasks_raw$LNV3[[n]] <- temp_navflagtasks$distance_marked_real[[3]]
    locationtasks_raw$LNV4[[n]] <- temp_navflagtasks$distance_marked_real[[4]]
    locationtasks_raw$LNV5[[n]] <- temp_navflagtasks$distance_marked_real[[5]]
    locationtasks_raw$LNV6[[n]] <- temp_navflagtasks$distance_marked_real[[6]]

    locationtasks_raw$lnvac1[[n]] <- temp_navflagtasks$accuracy[[1]]
    locationtasks_raw$lnvac2[[n]] <- temp_navflagtasks$accuracy[[2]]
    locationtasks_raw$lnvac3[[n]] <- temp_navflagtasks$accuracy[[3]]
    locationtasks_raw$lnvac4[[n]] <- temp_navflagtasks$accuracy[[4]]
    locationtasks_raw$lnvac5[[n]] <- temp_navflagtasks$accuracy[[5]]
    locationtasks_raw$lnvac6[[n]] <- temp_navflagtasks$accuracy[[6]]

    locationtasks_raw$lnvroute1[[n]] <- temp_navflagtasks$route[[1]]
    locationtasks_raw$lnvroute2[[n]] <- temp_navflagtasks$route[[2]]
    locationtasks_raw$lnvroute3[[n]] <- temp_navflagtasks$route[[3]]
    locationtasks_raw$lnvroute4[[n]] <- temp_navflagtasks$route[[4]]
    locationtasks_raw$lnvroute5[[n]] <- temp_navflagtasks$route[[5]]
    locationtasks_raw$lnvroute6[[n]] <- temp_navflagtasks$route[[6]]

  # Calculate Difference between Distances and GPS accuracy
    
    # LOCr
    for(a in 1:6){
      if ((locationtasks_raw[[5*a]][[n]] - locationtasks_raw[[2+5*a]][[n]]) < 0){
        locationtasks_raw[[1+5*a]][[n]] <- 0 # no negativ values!
      } else {
        locationtasks_raw[[1+5*a]][[n]] <- locationtasks_raw[[5*a]][[n]] - locationtasks_raw[[2+5*a]][[n]]
      }
    }
    # LNVr
    for(a in 1:6){
      if ((locationtasks_raw[[31+4*a]][[n]] - locationtasks_raw[[33+4*a]][[n]]) < 0){
        locationtasks_raw[[32+4*a]][[n]] <- 0 # no negativ values!
      } else {
        locationtasks_raw[[32+4*a]][[n]] <- locationtasks_raw[[31+4*a]][[n]] - locationtasks_raw[[33+4*a]][[n]]
      }
    }
    
}
