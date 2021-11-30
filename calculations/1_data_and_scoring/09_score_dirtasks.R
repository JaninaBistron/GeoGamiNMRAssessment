# Create Dataframe for Direction Task Scoring

vdirectiontasks_cat <- data.frame(matrix(ncol=22, nrow=participants))
colnames(vdirectiontasks_cat) <- c("participant_","time_",
                                   "DM1","DM2","DM3","DM4",
                                   "DA1","daturn1",
                                   "DA2","daturn2",
                                   "DA3","daturn3",
                                   "DA4","daturn4",
                                   "DM1c","DM2c","DM3c","DM4c",
                                   "DA1c","DA2c","DA3c","DA4c")