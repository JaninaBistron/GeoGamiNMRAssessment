# Calculate Total Scorings

# data.frame for total score
totalscore <- data.frame(matrix(ncol=6, nrow=participants))
colnames(totalscore) <- c("sumloc_r", "sumdir", "sumtotal_r", "sumloc_c", "sumdir_c", "sumtotal_c")

# data.frame for total score after dropping tasks
totalscore_drop <- data.frame(matrix(ncol=3, nrow=participants))
colnames(totalscore_drop) <- c("sumloc_c", "sumdir_c", "sumtotal_c")

# fill columns for total scoring
  # without confidence values
  totalscore$sumloc_r <- locationtasks_cat$LOC1r + locationtasks_cat$LOC2r + locationtasks_cat$LOC3r + locationtasks_cat$LOC4r + locationtasks_cat$LOC5r + locationtasks_cat$LOC6r + locationtasks_cat$LNV1r + locationtasks_cat$LNV2r + locationtasks_cat$LNV3r + locationtasks_cat$LNV4r + locationtasks_cat$LNV5r + locationtasks_cat$LNV6r
  totalscore$sumdir <- vdirectiontasks_cat$DA1 + vdirectiontasks_cat$DA2 + vdirectiontasks_cat$DA3 + vdirectiontasks_cat$DA4 + vdirectiontasks_cat$DM1 + vdirectiontasks_cat$DM2 + vdirectiontasks_cat$DM3 + vdirectiontasks_cat$DM4
  totalscore$sumtotal_r <- totalscore$sumloc_r + totalscore$sumdir
  # with confidence values
  totalscore$sumloc_c <- locationtasks_cat$LOC1c + locationtasks_cat$LOC2c + locationtasks_cat$LOC3c + locationtasks_cat$LOC4c + locationtasks_cat$LOC5c + locationtasks_cat$LOC6c + locationtasks_cat$LNV1c + locationtasks_cat$LNV2c + locationtasks_cat$LNV3c + locationtasks_cat$LNV4c + locationtasks_cat$LNV5c + locationtasks_cat$LNV6c
  totalscore$sumdir_c <- vdirectiontasks_cat$DA1c + vdirectiontasks_cat$DA2c + vdirectiontasks_cat$DA3c + vdirectiontasks_cat$DA4c + vdirectiontasks_cat$DM1c + vdirectiontasks_cat$DM2c + vdirectiontasks_cat$DM3c + vdirectiontasks_cat$DM4c
  totalscore$sumtotal_c <- totalscore$sumloc_c + totalscore$sumdir_c

# fill columns for total scoring after dropping tasks
  # with confidence values
  totalscore_drop$sumloc_c <- testhalfscores_droppedtasks$LcT1 + testhalfscores_droppedtasks$LcT2
  totalscore_drop$sumdir_c <- testhalfscores_droppedtasks$DTc1 + testhalfscores_droppedtasks$DTc2
  totalscore_drop$sumtotal_c <- totalscore_drop$sumloc_c + totalscore_drop$sumdir_c