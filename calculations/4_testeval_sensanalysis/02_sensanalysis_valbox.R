# Sensitivity Analysis - Shiny Dashboard: Value Box Color and Icon

valueboxcolor <- function(x) {
  if (format(round(x,2), nsmall=2) >= 0.7){
    return("green")
  }
  else{
    return("yellow")
  }
}
valueboxicon <- function(x) {
  if (format(round(x,2), nsmall=2) >= 0.9){
    return("grin-stars")
  }
  else if(format(round(x,2), nsmall=2) >= 0.8 & format(round(x,2), nsmall=2) < 0.9){
    return("grin")
  }
  else if(format(round(x,2), nsmall=2) >= 0.7 & format(round(x,2), nsmall=2) < 0.8){
    return("smile")
  }
  else{
    return("meh")
  }
}