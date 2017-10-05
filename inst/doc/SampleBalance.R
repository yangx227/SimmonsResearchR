## ---- eval = FALSE-------------------------------------------------------
#  # load the libraries
#  library(haven)
#  library(dplyr)
#  library(openxlsx)
#  library(SimmonsResearchR)

## ---- eval = FALSE-------------------------------------------------------
#  
#  # read the data
#  DatIn <- read_sav("W50123 AdultMasterDemoFile SB v2.sav") %>%
#    filter(WAVE_ID %in% c('1516', '1616'))
#  
#  # read targ file which is a spreadsheet contains targ information
#  targ <- read.xlsx("targ.xlsx")
#  
#  # subset the data
#  work1.new <- DatIn %>%
#    filter(group==1,
#           dma %in% c(501, 504, 505, 506, 510, 511, 524, 528, 602, 618, 623, 641, 803, 807))
#  
#  work2.new <- DatIn %>%
#    filter(group==3,
#           dma %in% c(501, 504, 505, 506, 510, 511, 524, 528, 602, 618, 623, 641, 803, 807))
#  
#  work1.wgt <- work1.new[["DESIGN_WGT"]] /2
#  work2.wgt <- work2.new[["DESIGN_WGT"]] /2
#  
#  # Initialize sample balance, save targ information to plan text file
#  targs.list <- sample_balance_init(data=DatIn, targ=targ, out="targ.txt")
#  
#  # sample balancing using 6 times mean cap, and save diagnosis information to out1.xlsx
#  sb1 <- sample_blance(data=work1.new,
#                       ID="BOOK_ID",
#                       targstr=targs.list[[1]],
#                       dweights=work1.wgt,
#                       cap=T,
#                       typeofcap=1,
#                       capval=6,
#                       floor=T,
#                       floorval=50,
#                       eps=.001,
#                       rounding=F,
#                       klimit=F,
#                       klimitval=Inf,
#                       out="out1.xlsx")
#  
#  # sample balancing using 2 times std cap, and save diagnosis information to out2.xlsx
#  sb2 <- sample_blance(data=work2.new,
#                       ID="BOOK_ID",
#                       targstr=targs.list[[2]],
#                       dweights=work2.wgt,
#                       cap=T,
#                       typeofcap=2,
#                       capval=2,
#                       floor=T,
#                       floorval=50,
#                       eps=.001,
#                       rounding=F,
#                       klimit=F,
#                       klimitval=Inf,
#                       out="out2.xlsx")
#  
#  # Get new capped weights from the 2 sample balancing modules
#  cap_wgt1<-sb1[[2]]
#  cap_wgt2<-sb2[[2]]
#  

## ----results='asis', echo = FALSE----------------------------------------
 library(SimmonsResearchR)

 data(targ)

 knitr::kable(targ, caption = "Example of the Targ Spreadsheet") 

