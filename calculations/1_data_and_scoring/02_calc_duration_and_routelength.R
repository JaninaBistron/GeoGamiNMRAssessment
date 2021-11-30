# Calculate Total Game Duration and Route Length

# calculate total game duration -----------------------------------------------------------------------------------------

logfile [,"duration"] <- 0
logfile[,7] <- as.numeric(difftime(strptime(paste(logfile[,6]),"%Y-%m-%d %H:%M:%S"),
                                   strptime(paste(logfile[,5]),"%Y-%m-%d %H:%M:%S")))
logfile[,7] <- format(round(logfile[,7], 0), nsmall = 0)

# calculate total route length ------------------------------------------------------------------------------------------

for (n in 1:participants){
  logfile$totalRoute[[n]] <-   format(round(sum(logfile[[1]][[n]]$speed),0), nsmall=0)
}