## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  
#  library(SimmonsResearchR)
#  
#  # varlist is the variable list to test
#  res1 <- diff_compare(dat1=data.baseline,
#                       dat2=data.primary,
#                       wgt1=PERS_WGT,
#                       wgt2=PERS_WGT,
#                       varlist=varlist,
#                       segment="SEGID1")
#  
#  res2 <- diff_compare(dat1=data.primary,
#                       dat2=data.president.election,
#                       wgt1=PERS_WGT,
#                       wgt2=PERS_WGT,
#                       varlist=varlist,
#                       segment="SEGID2")
#  
#  res3 <- diff_compare(dat1=data.president.election,
#                       dat2=data.post.election,
#                       wgt1=PERS_WGT,
#                       wgt2=PERS_WGT,
#                       varlist=varlist,
#                       segment="SEGID3")
#  
#  # combine SEGID1, SEGID2 and SEGID3, and save to spreadsheet "output.xlsx""
#  combine_comparison_results("output.xlsx", res1, res2, res3)
#  
#  

