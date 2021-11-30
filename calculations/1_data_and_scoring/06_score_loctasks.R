# Create Dataframe for Location Tasks Scoring

locationtasks_cat <- data.frame(matrix(ncol=44, nrow=participants))
colnames(locationtasks_cat) <- c("participant","time",
                                 "LOC1r","locpan1","loczoom1",
                                 "LOC2r","locpan2","loczoom2",
                                 "LOC3r","locpan3","loczoom3",
                                 "LOC4r","locpan4","loczoom4",
                                 "LOC5r","locpan5","loczoom5",
                                 "LOC6r","locpan6","loczoom6",
                                 "LNV1r","lnvroute1",
                                 "LNV2r","lnvroute2",
                                 "LNV3r","lnvroute3",
                                 "LNV4r","lnvroute4",
                                 "LNV5r","lnvroute5",
                                 "LNV6r","lnvroute6",
                                 "LOC1c","LOC2c","LOC3c","LOC4c","LOC5c","LOC6c",
                                 "LNV1c","LNV2c","LNV3c","LNV4c","LNV5c","LNV6c")