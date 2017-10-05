
Compare2MeansZ <- function(dat1=dat1, dat2=dat2, wgt1=wgt2, wgt2=wgt2, varlist=varlist, ratediff = 0.05) {
  
  iter <- 1
  
  for (cvar in varlist) { 
    result1 <- as.data.frame(tapply(wgt1, dat1[[cvar]], sum, na.rm=TRUE)/sum(wgt1, na.rm=TRUE))
    rown1 <- rownames(result1)
    result1 <- as.data.frame(result1[rownames(result1)!="NaN",])
    names(result1) <- "Rate1"
    result1$Varname <- paste0(cvar, "_", rown1)
    
    result2 <- as.data.frame(tapply(wgt2, dat2[[cvar]], sum, na.rm=TRUE)/sum(wgt2, na.rm=TRUE))
    rown2 <- rownames(result2)
    result2 <- as.data.frame(result2[rownames(result2)!="NaN",])
    names(result2) <- "Rate2"
    result2$Varname <- paste0(cvar, "_", rown2)  
    
    result3 <- as.data.frame(tapply(rep(1,nrow(dat1)), dat1[[cvar]], sum, na.rm=TRUE))
    rown3 <- rownames(result3)
    result3 <- as.data.frame(result3[rownames(result3)!="NaN",])
    names(result3) <- "Count1"
    result3$Varname <- paste0(cvar, "_", rown3)
    
    result4 <- as.data.frame(tapply(rep(1,nrow(dat2)), dat2[[cvar]], sum, na.rm=TRUE))
    rown4 <- rownames(result4)
    result4 <- as.data.frame(result4[rownames(result4)!="NaN",])
    names(result4) <- "Count2"
    result4$Varname <- paste0(cvar, "_", rown4)
    
    result5 <- as.data.frame(tapply(wgt1, dat1[[cvar]], sum, na.rm=TRUE))
    rown5 <- rownames(result5)
    result5 <- as.data.frame(result5[rownames(result5)!="NaN",])
    names(result5) <- "Weighted_Intab1"
    result5$Varname <- paste0(cvar, "_", rown5)
    
    result6 <- as.data.frame(tapply(wgt2, dat2[[cvar]], sum, na.rm=TRUE))
    rown6 <- rownames(result6)
    result6 <- as.data.frame(result6[rownames(result6)!="NaN",])
    names(result6) <- "Weighted_Intab2"
    result6$Varname <- paste0(cvar, "_", rown6)
    
    rlabel <- as.data.frame(tapply(wgt1, dat1[[cvar]], sum, na.rm=TRUE))
    rlabel <- as.data.frame(rlabel[rownames(rlabel)!="NaN",])
    names(rlabel) <- "Temp"
    
    vlabel <- attributes(dat1[[cvar]])$label
    vValue <- attributes(dat1[[cvar]])$labels
    rlabel$Varname <- paste0(cvar, "_", rown1)
    
    cValue <- vValue[vValue %in% c(rownames(result1))]
    
    rlabel$Label <- paste0(vlabel, ': ', c(rownames(result1)))
    #    rlabel$Label <- paste0(vlabel, ': ', names(cValue))
    
    
    rlabel$Temp <- NULL
    
    #TTest
    w1 <- wgt1[!is.na(dat1[[cvar]])]
    w2 <- wgt2[!is.na(dat2[[cvar]])]
    
    x <- dat1[[cvar]][!is.na(dat1[[cvar]])]
    y <- dat2[[cvar]][!is.na(dat2[[cvar]])]
    
    t.result <- wtd.t.test(x=x, y=y, weight=w1, weighty=w2, samedata = FALSE)
    
    WMean1 <- weighted.mean(dat1[[cvar]], wgt1, na.rm = TRUE)
    WMean2 <- weighted.mean(dat2[[cvar]], wgt2, na.rm = TRUE)
    
    resultTmp <- data.frame(Varname=cvar, 
                            WMean1=WMean1,
                            WMean2=WMean2,
                            TValue=t.result$coefficients[[1]],
                            TTest.Pvalue=t.result$coefficients[[3]], 
                            TSig05=ifelse(t.result$coefficients[[3]]<=0.05,1,0),
                            TSig01=ifelse(t.result$coefficients[[3]]<=0.01,1,0)
    )
    
    if (iter == 1) {
      rf1 <- result1
      rf2 <- result2
      rf3 <- result3
      rf4 <- result4
      rf5 <- result5
      rf6 <- result6
      
      rl  <- rlabel      
      t.test.result <-  resultTmp
      
      iter <- iter + 1
    } else {
      rf1 <- dplyr::bind_rows(rf1, result1)
      rf2 <- dplyr::bind_rows(rf2, result2)
      rf3 <- dplyr::bind_rows(rf3, result3)
      rf4 <- dplyr::bind_rows(rf4, result4)
      rf5 <- dplyr::bind_rows(rf5, result5)
      rf6 <- dplyr::bind_rows(rf6, result6)
      
      rl <-  dplyr::bind_rows(rl, rlabel)
      t.test.result <- dplyr::bind_rows(t.test.result, resultTmp)
    }
    
    iter <- iter + 1
  }      

  rf <- dplyr::inner_join(rf1,rf2)
  rf <- dplyr::inner_join(rf, rf3)
  rf <- dplyr::inner_join(rf, rf4)
  rf <- dplyr::inner_join(rf, rf5)
  rf <- dplyr::inner_join(rf, rf6)
  
  rf <- dplyr::inner_join(rf, rl)
  
  TotalCount1 <- nrow(dat1)
  TotalCount2 <- nrow(dat2)
  
  rf <- tbl_df(rf) %>%
    mutate(p = (Count1+Count2)/(TotalCount1+TotalCount2),
           ZValue = (Rate1-Rate2)/sqrt(p*(1-p)*(1/TotalCount1+1/TotalCount2)),
           ZSig05 = ifelse(abs(ZValue)>1.96,1,0),
           Diff   = Rate2 - Rate1,
           DiffSq = Diff*Diff,
           DiffRate = abs(Diff)/Rate1)
  
  
  # 2 criterials, may be changed
  rf <- rf %>%
    mutate(DiffFlag = ifelse(abs(DiffRate) >= ratediff & abs(Diff) >= 0.01, 1, 0)) %>%
    select(Varname, Label, Count1, Count2, Weighted_Intab1, Weighted_Intab2, Rate1, Rate2, Diff, DiffRate, DiffFlag, ZValue, ZSig05) %>%
    rename(Intab1_Count = Count1, 
           Intab2_Count = Count2,
           Intab1_Rate = Rate1,
           Intab2_Rate = Rate2)

  vc <- str_split(rf$Varname,"_")
  for (k in 1:length(vc)) {
    vc[[k]] <- unlist(vc[[k]][-length(vc[[k]])])
    vc[[k]] <- str_c(vc[[k]], collapse="_")
  } 
  
  vc1 <- data.frame(Varname= rf$Varname , 
                    vn1 =  unlist(vc),
                    stringsAsFactors = FALSE)
  
  vc2 <- vc1 %>% 
    mutate(vn2=ifelse(vn1==lag(vn1),NA,vn1))
  vc2$vn2[1] <- vc2$vn1[1]
  
  vc3 <- left_join(vc2, t.test.result, by=c("vn2" = "Varname")) %>%
    select(Varname, TValue, TTest.Pvalue, TSig05)
  
  rf <- left_join(rf, vc3) %>%
    select(Varname, Label, Intab1_Count, Intab2_Count,Weighted_Intab1,Weighted_Intab2,
           Intab1_Rate,Intab2_Rate,Diff,DiffRate,DiffFlag,
           ZValue, ZSig05,
           TValue, TTest.Pvalue, TSig05)
  
  Intab1TotalCnt <- sum(rf$Intab1_Count, na.rm = TRUE)
  Intab2TotalCnt <- sum(rf$Intab2_Count, na.rm = TRUE)
  
  Intab1TotalWgt <- sum(rf$Weighted_Intab1, na.rm = TRUE)
  Intab2TotalWgt <- sum(rf$Weighted_Intab2, na.rm = TRUE)
  
  Intab1TotalCnt <- nrow(dat1)
  Intab2TotalCnt <- nrow(dat2)
  
  Intab1TotalWgt <- sum(wgt1, na.rm = TRUE)
  Intab2TotalWgt <- sum(wgt2, na.rm = TRUE)
  
  DiffFlagCnt   <- sum(rf$DiffFlag, na.rm = TRUE)
  DiffFlagPct <- DiffFlagCnt / nrow(rf)
  
  TotalZSig05Flag <- sum(rf$ZSig05, na.rm = TRUE)
  TotalZSig05FlagPct <- TotalZSig05Flag / nrow(rf)
  
  TotalTSig05Flag <- sum(rf$TSig05, na.rm = TRUE)
  TotalTSig05FlagPct <- TotalTSig05Flag / sum(!is.na(rf$TSig05))
  
  Stat <- data.frame(Intab1TotalCnt=Intab1TotalCnt,
                     Intab2TotalCnt=Intab2TotalCnt,
                     Intab1TotalWgt=Intab1TotalWgt,
                     Intab2TotalWgt=Intab2TotalWgt, 
                     DiffFlagCnt=DiffFlagCnt, 
                     DiffFlagPct = DiffFlagPct,
                     TotalZSig05Flag = TotalZSig05Flag,
                     TotalZSig05FlagPct = TotalZSig05FlagPct,
                     TotalTSig05Flag = TotalTSig05Flag,
                     TotalTSig05FlagPct = TotalTSig05FlagPct              
  )
  
  #proportional rate diff sig
  rf.sig1 <- rf %>%
    filter(DiffFlag==1)
  
  sig.tmp1 <- rf[1,]
  for (i in 1:ncol(sig.tmp1)) {
    sig.tmp1[[i]] <- NA
  }
  sig.tmp1$Varname <- "EMPTY"
  
  rf.sig1 <- as.data.frame(rbind(sig.tmp1, rf.sig1)) %>%
    tbl_df() %>%
    select(-TValue, -TTest.Pvalue, -TSig05)
  
  #Z-test sig
  rf.sig2 <- rf %>%
    filter(ZSig05==1)
  
  sig.tmp2 <- rf[1,]
  for (i in 1:ncol(sig.tmp2)) {
    sig.tmp2[[i]] <- NA
  }
  sig.tmp2$Varname <- "EMPTY"
  
  rf.sig2 <- as.data.frame(rbind(sig.tmp2, rf.sig2)) %>%
    tbl_df() %>%
    select(-TValue, -TTest.Pvalue, -TSig05)
  
  return(list(rf = as.data.frame(rf), rf.sig1 = as.data.frame(rf.sig1), rf.sig2 = as.data.frame(rf.sig2), Stat = Stat))
  
}

Compare2MeansT <- function(dat1=dat1, dat2=dat2, wgt1=wgt2, wgt2=wgt2, varlist=varlist, ratediff = 0.05) {
  
  iter <- 1
  
  for (cvar in varlist) { 
    
    result1 <- as.data.frame(sum(wgt1*dat1[[cvar]], na.rm = TRUE)/sum(wgt1, na.rm = TRUE))
    names(result1) <- "Mean1"
    result1$Varname <- paste0(cvar)

    result2 <- as.data.frame(sum(wgt2*dat2[[cvar]], na.rm = TRUE)/sum(wgt2, na.rm = TRUE)) 
    names(result2) <- "Mean2"
    result2$Varname <- paste0(cvar)

    result3 <- as.data.frame(sum(!is.na(wgt1)))
    names(result3) <- "Count1"
    result3$Varname <- paste0(cvar)
    
    result4 <- as.data.frame(sum(!is.na(wgt2)))
    names(result4) <- "Count2"
    result4$Varname <- paste0(cvar)
  
    result5 <- as.data.frame(sum(wgt1, na.rm = TRUE))
    names(result5) <- "Weighted_Intab1"
    result5$Varname <- paste0(cvar)
  
    result6 <- as.data.frame(sum(wgt2, na.rm = TRUE))
    names(result6) <- "Weighted_Intab2"
    result6$Varname <- paste0(cvar)

    if (is.null(attributes(dat1[[cvar]])$label)) {
      rlabel <- data.frame(Label="", Varname = paste0(cvar))
    } else {
      rlabel <- data.frame(Label=attributes(dat1[[cvar]])$label, Varname = paste0(cvar))
    }
    
    #TTest
    w1 <- wgt1[!is.na(dat1[[cvar]])]
    w2 <- wgt2[!is.na(dat2[[cvar]])]
    
    x <- dat1[[cvar]][!is.na(dat1[[cvar]])]
    y <- dat2[[cvar]][!is.na(dat2[[cvar]])]
    
    t.result <- weights::wtd.t.test(x=x, y=y, weight=w1, weighty=w2, samedata = FALSE)
    
    WMean1 <- weighted.mean(dat1[[cvar]], wgt1, na.rm = TRUE)
    WMean2 <- weighted.mean(dat2[[cvar]], wgt2, na.rm = TRUE)
    
    resultTmp <- data.frame(Varname=cvar, 
                            WMean1=WMean1,
                            WMean2=WMean2,
                            TValue=t.result$coefficients[[1]],
                            TTest.Pvalue=t.result$coefficients[[3]], 
                            TSig05=ifelse(t.result$coefficients[[3]]<=0.05,1,0)
    )
    
    if (iter == 1) {
      rf1 <- result1
      rf2 <- result2
      rf3 <- result3
      rf4 <- result4
      rf5 <- result5
      rf6 <- result6 
      
      rl  <- rlabel
      
      t.test.result <-   resultTmp
      
      iter <- iter + 1
    } else {
      rf1 <- rbind(rf1, result1)
      rf2 <- rbind(rf2, result2)
      rf3 <- rbind(rf3, result3)
      rf4 <- rbind(rf4, result4)
      rf5 <- rbind(rf5, result5)
      rf6 <- rbind(rf6, result6) 
      
      rl <-  rbind(rl, rlabel)
      
      t.test.result <- dplyr::bind_rows(t.test.result, resultTmp)
      
    }
  }  
  
  rf <- dplyr::inner_join(rf1,rf2)
  rf <- dplyr::inner_join(rf, rf3)
  rf <- dplyr::inner_join(rf, rf4)
  rf <- dplyr::inner_join(rf, rf5)
  rf <- dplyr::inner_join(rf, rf6)
  rf <- dplyr::inner_join(rf, rl)
  rf <- dplyr::inner_join(rf, t.test.result)
  
  rf <- tbl_df(rf) %>%
    mutate(Mean1 = WMean1,
           Mean2 = WMean2,
           Diff   = Mean2 - Mean1,
           DiffSq = Diff*Diff,
           DiffRate = abs(Diff)/Mean1)
  
  rf <- rf %>%
    mutate(DiffFlag = ifelse(abs(DiffRate) >= ratediff, 1, 0)) %>%
    select(Varname, Label, Count1, Count2, Weighted_Intab1, Weighted_Intab2, Mean1, Mean2, Diff, DiffRate, DiffFlag, 
           TValue, TTest.Pvalue, TSig05) %>%
    rename(Intab1_Count = Count1, 
           Intab2_Count = Count2,
           Intab1_Mean = Mean1,
           Intab2_Mean = Mean2)
  
  rf <- rf %>% select(Varname, Label, Intab1_Count, Intab2_Count, Weighted_Intab1, Weighted_Intab2, Intab1_Mean, Intab2_Mean, Diff, DiffRate, DiffFlag, 
                      TValue, TTest.Pvalue, TSig05)
  
  Intab1TotalCnt <- sum(rf$Intab1_Count, na.rm = TRUE)
  Intab2TotalCnt <- sum(rf$Intab2_Count, na.rm = TRUE)
  
  Intab1TotalWgt <- sum(rf$Weighted_Intab1, na.rm = TRUE)
  Intab2TotalWgt <- sum(rf$Weighted_Intab2, na.rm = TRUE)
  
  Intab1TotalCnt <- nrow(dat1)
  Intab2TotalCnt <- nrow(dat2)
  
  Intab1TotalWgt <- sum(wgt1, na.rm = TRUE)
  Intab2TotalWgt <- sum(wgt2, na.rm = TRUE)
  
  DiffFlagCnt   <- sum(rf$DiffFlag, na.rm = TRUE)
  DiffFlagPct <- DiffFlagCnt / nrow(rf)
  
  TotalTSig05Flag <- sum(rf$TSig05, na.rm = TRUE)
  TotalTSig05FlagPct <- TotalTSig05Flag / sum(!is.na(rf$TSig05))
  
  NumOfVariables <- length(varlist)
  
  Stat <- data.frame(Intab1TotalCnt=Intab1TotalCnt,
                     Intab2TotalCnt=Intab2TotalCnt,
                     Intab1TotalWgt=Intab1TotalWgt,
                     Intab2TotalWgt=Intab2TotalWgt,
                     NumOfVariables=NumOfVariables,
                     DiffFlagCnt=DiffFlagCnt, 
                     DiffFlagPct = DiffFlagPct,
                     TotalTSig05Flag = TotalTSig05Flag,
                     TotalTSig05FlagPct = TotalTSig05FlagPct                  
  )
  
  #T-test sig
  rf.sig3 <- rf %>%
    filter(TSig05==1) 
  
  sig.tmp3 <- rf[1,]
  for (i in 1:ncol(sig.tmp3)) {
    sig.tmp3[[i]] <- NA
  }
  sig.tmp3$Varname <- "EMPTY"
  
  rf.sig3 <- as.data.frame(rbind(sig.tmp3, rf.sig3)) %>%
    tbl_df()
  
  return(list(rf = as.data.frame(rf), rf.sig3 = as.data.frame(rf.sig3), Stat = Stat))
  
}

#' Compare two datasets based on relative changes, ztest and ttest.
#'
#' @param dat1 dataset 1.
#' @param dat2 dataset 2.
#' @param wgt1 weight of dataset1.
#' @param wgt2 weight of dataset2.
#' @param varlist variable list to be compared. All variables must exsit in both datasets.
#' @param segment tag to the comparison which is used to identify different comparisons.
#' @param ratediff threshold of relative change (percent) for binary variables. All variables will be categoried to dummy variables for comparison at different levels. Default value: 0.05.
#' @return The return list contains \code{comparison results}, and \code{segment identifier}.
#' @export
#' @examples
#' res <- diff_compare(dat1=data.baseline, 
#'                     dat2=data.primary, 
#'                     wgt1=PERS_WGT,
#'                     wgt2=PERS_WGT, 
#'                     varlist=varlist, 
#'                     segment="SEGID1")
#'
diff_compare <- function(dat1=dat1, dat2=dat2, wgt1=wgt1, wgt2=wgt2, varlist=fvarlist, segment=segment, ratediff = 0.05) {
  
  require(dplyr)
  
  old <- options()
  options(warn = -1)
  
  on.exit(options(old))
  
  wgt1 <- enquo(wgt1)
  wgt2 <- enquo(wgt2)
  
  wgt1.tmp <- dat1 %>% select(!!wgt1)
  wgt2.tmp <- dat2 %>% select(!!wgt2)
  
  wgt1.tmp <- as.numeric(wgt1.tmp[[1]])
  wgt2.tmp <- as.numeric(wgt2.tmp[[1]])
  
  resultz <- Compare2MeansZ(dat1=dat1, dat2=dat2, wgt1=wgt1.tmp, wgt2=wgt2.tmp, varlist=varlist, ratediff = 0.05)
  resultt <- Compare2MeansT(dat1=dat1, dat2=dat2, wgt1=wgt1.tmp, wgt2=wgt2.tmp, varlist=varlist, ratediff = 0.05)

  return(Reformat(list(res.total.1=resultz, res.total.2=resultt), segment))
}

Reformat <- function(res, segment) {
  
  stat.tmp1 <- res$res.total.1$Stat
  stat.tmp2 <- res$res.total.1$Stat
  
  for (i in 1:ncol(stat.tmp1)) {
    stat.tmp1[[i]] <- NA
    stat.tmp2[[i]] <- NA
  }  
  
  
  res.summary1 <- rbind(stat.tmp1,
                        stat.tmp2,
                        res$res.total.1$Stat) %>%
    tbl_df() %>%
    select(-TotalTSig05Flag, -TotalTSig05FlagPct) %>%
    rename(LevelsDiffFlagCnt=DiffFlagCnt, LevelsDiffFlagPct=DiffFlagPct)     
  
  stat.tmp1 <- res$res.total.2$Stat
  stat.tmp2 <- res$res.total.2$Stat
  
  for (i in 1:ncol(stat.tmp1)) {
    stat.tmp1[[i]] <- NA
    stat.tmp2[[i]] <- NA
  }
  
  res.summary2 <- rbind(stat.tmp1,
                        stat.tmp2,
                        res$res.total.2$Stat  ) %>%
    tbl_df() %>%
    select(NumOfVariables, DiffFlagCnt, DiffFlagPct, TotalTSig05Flag, TotalTSig05FlagPct) %>%
    rename(VariableDiffFlagCnt=DiffFlagCnt, VariableDiffFlagPct=DiffFlagPct)
  
  res.summary <- dplyr::bind_cols(res.summary1, res.summary2) %>%
    select(Intab1TotalCnt, Intab2TotalCnt, Intab1TotalWgt, Intab2TotalWgt, 
           LevelsDiffFlagCnt, LevelsDiffFlagPct, 
           TotalZSig05Flag, TotalZSig05FlagPct,
           NumOfVariables, VariableDiffFlagCnt, VariableDiffFlagPct, TotalTSig05Flag, TotalTSig05FlagPct)
  
  rownames(res.summary) <- c(paste0("Intab1 = ", res$wgt1[2]),
                             paste0("Intab2 = ", res$wgt2[2]),
                             "Total")
  
  # Add one more role to Total dataframe
  total.added1 <- res$res.total.1$rf[1,]
  total.added2 <- res$res.total.2$rf[1,]
  
  for (i in 3:ncol(total.added1)) {
    total.added1[i] <- NA
  }
  for (i in 3:ncol(total.added2)) {
    total.added2[i] <- NA
  }
  
  total.added1$Varname <- "Total"
  total.added1$Label <- ""
  total.added1$Intab1_Count <- res$res.total.1$Stat$Intab1TotalCnt
  total.added1$Intab2_Count <- res$res.total.1$Stat$Intab2TotalCnt
  total.added1$Weighted_Intab1 <- res$res.total.1$Stat$Intab1TotalWgt
  total.added1$Weighted_Intab2 <- res$res.total.1$Stat$Intab2TotalWgt
  
  total.added2$Varname <- "Total"
  total.added2$Label <- ""
  total.added2$Intab1_Count <- res$res.total.2$Stat$Intab1TotalCnt
  total.added2$Intab2_Count <- res$res.total.2$Stat$Intab2TotalCnt
  total.added2$Weighted_Intab1 <- res$res.total.2$Stat$Intab1TotalWgt
  total.added2$Weighted_Intab2 <- res$res.total.2$Stat$Intab2TotalWgt
  
  res$res.total.1$rf <- dplyr::bind_rows(total.added1, res$res.total.1$rf)  
  res$res.total.2$rf <- dplyr::bind_rows(total.added2, res$res.total.2$rf)  
  
  # Remove unnecessary columns
  res$res.total.1$rf <- Remove1(res$res.total.1$rf) 
  
  #res$res.total.1$rf.sig1$Varname <- "TOTAL"
  #res$res.total.1$rf.sig1$Varname[1] <- " "
  
  
  rf.sig1 <- res$res.total.1$rf.sig1
  
  #res$res.total.1$rf.sig2$Varname <- "TOTAL"
  #res$res.total.1$rf.sig2$Varname[1] <- " "
  
  rf.sig2 <- res$res.total.1$rf.sig2
  #res$res.total.2$rf.sig3$Varname <- "TOTAL"
  
  #  res$res.total.2$rf.sig3$Varname[1] <- " "
  rf.sig3 <- res$res.total.2$rf.sig3
  
  if (is.null(segment)) segment <- character()
  
  l <- list(Summary=as.data.frame(res.summary),
            Total1=as.data.frame(res$res.total.1$rf), 
            Total2=as.data.frame(res$res.total.2$rf), 
            BinVar.Sig=as.data.frame(rf.sig1),
            BinVarZTest.Sig=as.data.frame(rf.sig2),
            VarTTest.Sig=as.data.frame(rf.sig3),
            segment=segment)
  
  names(l) <- c(paste0(segment, '_', "Summary"), 
                paste0(segment, '_', "Total1"),
                paste0(segment, '_', "Total2"),
                paste0(segment, '_', "BinVar.Sig"),
                paste0(segment, '_', "BinVarZTest.Sig"),
                paste0(segment, '_', "VarTTest.Sig"),
                "segment")
  return(l)
}

#' Combine several comparison results into one excel spreadsheet.
#'
#' @param filename Output spreadsheet file name.
#' @param ... Results from \code{diff_compare}. The function can accept as many comparing results as possible, but at least 2.
#' @return No return value.
#' @export
#' @examples
#' res1 <- diff_compare(dat1=data.baseline, dat2=data.primary, wgt1=PERS_WGT, wgt2=PERS_WGT, varlist=varlist, segment="SEGID1")
#' res2 <- diff_compare(dat1=data.baseline, dat2=data.president.election, wgt1=PERS_WGT, wgt2=PERS_WGT, varlist=varlist, segment="SEGID2")
#' res3 <- diff_compare(dat1=data.baseline, dat2=data.post.election, wgt1=PERS_WGT, wgt2=PERS_WGT, varlist=varlist, segment="SEGID3")
#' combine_comparison_results("CompareOutput.xlsx", res1, res2, res3)
#' 
combine_comparison_results <- function(filename, ...) {
  
  old <- options()
  options(warn = -1)
  
  on.exit(options(old))
  
  dots <- list(...)
  ndots <- length(dots)
  
  l <- list()
  l[[1]] <- data.frame()
  k <- 2
  
  namelist <- character()
  seglist <- character()
  
  for (i in seq_along(dots)) {
    if (!is.null(dots[[i]])) {
      
      dots1 <- dots[[i]]      
      seglist <- c(seglist, dots1[[length(dots1)]])
      
      # exclude the last element of the list - segment
      dots1 <- dots1[1:(length(dots1)-1)]
      
      for (j in seq_along(dots1)) {
        
        if (!is.null(dots1[[j]])) {
          if (j==1) {
            tmp <- slice(dots1[[j]],3)
            if (k==2) sf <- tmp
            else sf <- dplyr::bind_rows(sf, tmp)
            
            }
          
          l[[k]] <- (dots1[[j]])
          k <- k + 1
          
        }
      }
      
      namelist <- c(namelist, names(dots1))
    }
  }
  
  segment <- data.frame(segment=seglist)
  sf <- dplyr::bind_cols(segment, sf)
  
  l[[1]] <- sf
  names(l) <- c("Summary", namelist)
  
  openxlsx::write.xlsx(l, 
                       file=filename, 
                       asTable=TRUE, 
                       freezePane=TRUE, 
                       colNames = TRUE, 
                       rowNames=TRUE, 
                       firstRow=TRUE, 
                       colWidths="auto")
  
}


Remove1 <- function(res) {
  
  res$TValue <- NULL
  res$TTest.Pvalue <- NULL
  res$TSig05 <- NULL
  res$TSig01 <- NULL
  
  return(res)
}