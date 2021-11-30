# Collect and Calculate Waypoint Data

for(n in 1:participants) { 
  temp <- logfile$waypoints[[n]]
  
  # get timestamp, location, and compass data
  timestamp <- temp$timestamp
  compass <- temp$compassHeading
  
  if(!is.null(temp$position$coords$latitude)){
    latitude <- temp$position$coords$latitude
    longitude <- temp$position$coords$longitude
    accuracy <- temp$position$coords$accuracy
    speed <- temp$position$coords$speed
  } else {
    latitude <- temp$position.coords.latitude
    longitude <- temp$position.coords.longitude
    accuracy <- temp$position.coords.accuracy
    speed <- temp$position.coords.speed
  }
  
  # store data in logfile
  logfile$waypoints[[n]] <- data.frame(timestamp,latitude,longitude,accuracy,compass,speed)
}