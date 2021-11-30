# Sensitivity Analysis - Repeat Evaluation for custom thresholds

# choose location -------------------------------------------------------------------------------------------------------
if(input$location=="Senden"){
  load("results/senden/raw_data.Rda")
  load("results/senden/taskperformance_loc.Rda")
  load("results/senden/taskperformance_dir.Rda")
  location <- input$location
  participants <- nrow(logfile)
} else {
  if(input$location=="Duelmen"){
    load("results/duelmen/raw_data.Rda")
    load("results/duelmen/taskperformance_loc.Rda")
    load("results/duelmen/taskperformance_dir.Rda")
    location <- input$location
    participants <- nrow(logfile)
  } else {}
}

# define input thresholds ------------------------------------------------------------------------------------------------
LOClimit <- input$loc
locpanlimit <- input$locpan
loczoomlimit <- input$loczoom
LNVlimit <- input$lnv
lnvroutefactor <- input$lnvroute
DMlimit <- input$dm
DAlimit <- input$da
daturnlimit <- input$daturn

# load code --------------------------------------------------------------------------------------------------------------
source("calculations/1_data_and_scoring/06_score_loctasks.R", encoding="utf-8", local = TRUE)
source("calculations/1_data_and_scoring/07_score_loc.R", encoding="utf-8", local = TRUE)
source("calculations/1_data_and_scoring/08_score_lnv.R", encoding="utf-8", local = TRUE)
source("calculations/1_data_and_scoring/09_score_dirtasks.R", encoding="utf-8", local = TRUE)
source("calculations/1_data_and_scoring/10_score_dm.R", encoding="utf-8", local = TRUE)
source("calculations/1_data_and_scoring/11_score_da.R", encoding="utf-8", local = TRUE)
source("calculations/2_testeval_items_reliability/01_cronbachsalpha_itemtotal.R", encoding="utf-8", local = TRUE)
source("calculations/2_testeval_items_reliability/02_taskdifficulty.R", encoding="utf-8", local = TRUE)
source("calculations/2_testeval_items_reliability/03_splithalfreliability.R", encoding="utf-8", local = TRUE)
source("calculations/3_testeval_scoredistrib_validity/01_totalscores.R", encoding="utf-8", local = TRUE)
source("calculations/3_testeval_scoredistrib_validity/02_validity.R", encoding="utf-8", local = TRUE)
