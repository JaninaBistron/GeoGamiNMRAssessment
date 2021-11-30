# Collect and Calculate Event Data

for(n in 1:participants){
  
  temp <- logfile$events[[n]]
  
  ## basic data  ------------------------------------------------------------------------------------------------------
  
  timestamp <- temp$timestamp
  eventtype <- temp$type
  
  if(!is.null(temp$task$id)){
    task_id <- temp$task$id
    tasktype <- temp$task$type
    questiontype <- temp$task$question$type
    answertype <- temp$task$answer$type
    taskdefinition <- temp$task$question$text
  } else {
    task_id <- temp$task.id
    tasktype <- temp$task.type
    questiontype <- temp$task.question.type
    answertype <- temp$task.answer.type
    taskdefinition <- temp$task.question.text
  }
  
  # get number of events
  
  events <- nrow(temp)
  
  # get further event data
  
  if(!is.null(temp$position$coords$longitude)){
    longitude_real <- temp$position$coords$longitude # location of the participant
    latitude_real <- temp$position$coords$latitude # location of the participant
    accuracy <- temp$position$coords$accuracy # gps accuracy (in m)
    pannings <- temp$interaction$panCount # amount of pannings
    zoomings <- temp$interaction$zoomCount # amount of zoomings
    longitude_click <- temp$clickPosition$longitude # location marked by the participant (on the map)
    latitude_click <- temp$clickPosition$latitude # location marked by the participant (on the map)
  } else {
    longitude_real <- temp$position.coords.longitude
    latitude_real <- temp$position.coords.latitude
    accuracy <- temp$position.coords.accuracy
    pannings <- temp$interaction.panCount
    zoomings <- temp$interaction.zoomCount
    longitude_click <- temp$clickPosition.longitude
    latitude_click <- temp$clickPosition.latitude
  }
  
  longitude_marked <- c() # location of the "correct" task solution (task settings)
  latitude_marked <- c() # location of the "correct" task solution (task settings)
  
  ## Location Tasks   --------------------------------------------------------------------------------------------------
  
  # get location marked by the participant (on the map) - longitude_click & latitude_click
  
  longitude_click2 <- c()
  
  if(!is.null(temp$answer$clickPosition)){
    
    for(i in 1:events) {
      if (is.null(temp$answer$clickPosition[[i]][1])){
        longitude_click2[[i]] <- NA
      }
      else {
        longitude_click2[[i]] <- temp$answer$clickPosition[[i]][1]
      }
    }
    
    latitude_click2 <- c()
    for(i in 1:events){
      if (is.null(temp$answer$clickPosition[[i]][2])){
        latitude_click2[[i]] <- NA
      }
      else{
        latitude_click2[[i]] <- temp$answer$clickPosition[[i]][2]
      }
    }
  } else {
    
    for(i in 1:events) {
      if (is.null(temp$answer.clickPosition[[i]][1])){
        longitude_click2[[i]] <- NA
      }
      else {
        longitude_click2[[i]] <- temp$answer.clickPosition[[i]][1]
      }
    }
    
    latitude_click2 <- c()
    for(i in 1:events){
      if (is.null(temp$answer.clickPosition[[i]][2])){
        latitude_click2[[i]] <- NA
      }
      else{
        latitude_click2[[i]] <- temp$answer.clickPosition[[i]][2]
      }
    }
  }
  
  # combine two columns of data with longitude_click & latitude_click
  
  for(i in 1:events){
    if (is.na(longitude_click[i])){
      longitude_click[i] <- longitude_click2[i]
    }
  }
  for(i in 1:events){
    if (is.na(latitude_click[i])){
      latitude_click[i] <- latitude_click2[i]
    }
  }
  
  # get location of the "correct" solution/task settings - longitude_marked & latitude_marked
  
  if(!is.null(temp$task$answer$position$geometry$coordinates)){
    
    for(i in 1:events){
      if (is.null(temp$task$answer$position$geometry$coordinates[[i]][1])){
        longitude_marked[[i]] <- NA
      } else {
        longitude_marked[[i]] <- temp$task$answer$position$geometry$coordinates[[i]][1]
      }
    }
    for(i in 1:events){
      if (is.null(temp$task$answer$position$geometry$coordinates[[i]][2])){
        latitude_marked[[i]] <- NA
      } else {
        latitude_marked[[i]] <- temp$task$answer$position$geometry$coordinates[[i]][2]
      }
    }
  } else {
    
    for(i in 1:events){
      if (is.null(temp$task.answer.position.geometry.coordinates[[i]][1])){
        longitude_marked[[i]] <- NA
      } else {
        longitude_marked[[i]] <- temp$task.answer.position.geometry.coordinates[[i]][1]
      }
    }
    for(i in 1:events){
      if (is.null(temp$task.answer.position.geometry.coordinates[[i]][2])){
        latitude_marked[[i]] <- NA
      } else {
        latitude_marked[[i]] <- temp$task.answer.position.geometry.coordinates[[i]][2]
      }
    }
  }
  
  # get distances (between "correct" solution/task settings and participant's solution)
  
      # create dataframes with longlat data
      
      longlat_real <- data.frame(longitude_real=unlist(longitude_real),latitude_real=unlist(latitude_real))
      longlat_click <- data.frame(longitude_click=unlist(longitude_click),latitude_click=unlist(latitude_click))
      longlat_marked <- data.frame(longitude_marked=unlist(longitude_marked),latitude_marked=unlist(latitude_marked))
      
      # create new dataframes for calculated distances
     
      distance_real_click <- data.frame(matrix(nrow=events, ncol=1))
      colnames(distance_real_click) <- c("distance_real_click")
      distance_marked_real <- data.frame(matrix(nrow=events, ncol=1))
      colnames(distance_marked_real) <- c("distance_marked_real")
      
      # create new matrices for spDistsN1 function
      
      longlat_real_m <- matrix(ncol=2)
      longlat_click_m <- matrix(ncol=2)
      longlat_marked_m <- matrix(ncol=2)
      
      # distance calculation: marked location (task settings) vs. participants' real location
      
      for(i in 1:events){
        if (is.na(longlat_marked[[1]][i])){
          distance_marked_real[[1]][i] <- NA
        } else {
          # matrix for spDistsN1 (distance) function:
          longlat_marked_m[1,1] <- data.matrix(longlat_marked[[1]][i])
          longlat_marked_m[1,2] <- data.matrix(longlat_marked[[2]][i])
          longlat_real_m[1,1] <- data.matrix(longlat_real[[1]][i])
          longlat_real_m[1,2] <- data.matrix(longlat_real[[2]][i])
          # calculate distances with long-lat-data:
          distance_marked_real[[1]][i] <- sp::spDistsN1(longlat_marked_m,longlat_real_m,longlat=TRUE)*1000
        }
      }
      
      # distance calculation: clicked location (by the participant) vs. participants' real location
      
      for(i in 1:events){
        if (is.na(longlat_click[[1]][i])){
          distance_real_click[[1]][i] <- NA
        } else {
          # matrix for spDistsN1 (distance) function:
          longlat_click_m[1,1] <- data.matrix(longlat_click[[1]][i])
          longlat_click_m[1,2] <- data.matrix(longlat_click[[2]][i])
          longlat_real_m[1,1] <- data.matrix(longlat_real[[1]][i])
          longlat_real_m[1,2] <- data.matrix(longlat_real[[2]][i])
          # calculate distances with long-lat-data:
          distance_real_click[[1]][i] <- sp::spDistsN1(longlat_click_m,longlat_real_m,longlat=TRUE)*1000
        }
      }
  
      
  ## Direction Tasks   -------------------------------------------------------------------------------------------------
  
  # get directions
  
  if(n>1){rm(dir_marked)} # need to be empty in the beginning
  if(!is.null(temp$task$question$direction$bearing)){
    dir_marked <- temp$task$question$direction$bearing # dir_marked = "correct" view direction (task settings)
  } else {
    dir_marked <- temp$task.question.direction.bearing
  }
  if(n>1){rm(dir_click)} # need to be empty in the beginning
  dir_click <- temp$clickDirection # dir_click = view direction marked by the participant (on the map)
  dir_real <- temp$compassHeading # dir_real = view direction of the participant
  
  # combining two columns of data with dir_click
  
  if(!is.null(temp$answer$clickDirection)){
    dir_click2 <- temp$answer$clickDirection
  } else {
    dir_click2 <- temp$answer.clickDirection
  }
  
  for(i in 1:events){
    if(!is.null(dir_click)){
      if (is.na(dir_click[i])){
        dir_click[i] <- dir_click2[i]
      }
    }
  }
  
  # create new dataframes for angle calculation
  
  angle_real_click <- data.frame(matrix(nrow=events, ncol=1))
  colnames(angle_real_click) <- c("angle_real_click")
  angle_marked_real <- data.frame(matrix(nrow=events, ncol=1))
  colnames(angle_marked_real) <- c("angle_marked_real")
  
  # angle calculation: marked angle (task settings) vs. participants' real view direction
  
  for(i in 1:events){
    if(!is.null(dir_marked)){
      if (is.na(dir_marked[i])){
        # case: no entry
        angle_marked_real[[1]][i] <- NA
      } else {
        # case: entry
        angle <- abs(dir_marked[i] - dir_real[i])
        if (angle < 180){
          # case: difference < 180
          angle_marked_real [[1]][i] <- angle
        } else {
          # case: difference > 180
          angle_marked_real [[1]][i] <- 360 - angle
        }
      }
    }
  }
  # if there is no direction data -> empty list
  if(is.null(dir_marked)){
    dir_marked <- data.frame(matrix(nrow=events, ncol=1))
  }
  
  # angle calculation: participants' view direction vs. participants' clicked angle
  
  for(i in 1:events){
    if(!is.null(dir_click)){
      if (is.na(dir_click[i])){
        # case: no entry
        angle_real_click[[1]][i] <- NA
      } else {
        # case: entry
        angle <- abs(dir_click[i] - dir_real[i])
        if (angle < 180){
          # case: difference < 180
          angle_real_click [[1]][i] <- angle
        } else {
          # case: difference > 180
          angle_real_click [[1]][i] <- 360 - angle
        }
      }
    }
  }
  # if there is no direction data
  if(is.null(dir_click)){
    dir_click <- data.frame(matrix(nrow=events, ncol=1))
  }
  
  ## combine data in one file   ---------------------------------------------------------------------------------------
  
  logfile$events[[n]] <- data.frame(task_id,timestamp,eventtype,tasktype,questiontype,answertype,taskdefinition,
                                    longitude_marked=unlist(longitude_marked),
                                    latitude_marked=unlist(latitude_marked),longitude_real=unlist(longitude_real),
                                    latitude_real=unlist(latitude_real),accuracy,
                                    longitude_click=unlist(longitude_click),latitude_click=unlist(latitude_click),
                                    distance_marked_real,
                                    distance_real_click,pannings,zoomings,
                                    dir_marked=unlist(dir_marked),dir_real=unlist(dir_real),dir_click=unlist(dir_click),
                                    angle_marked_real,angle_real_click)
}