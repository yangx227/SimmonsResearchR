
find_mismatch_vars <- function(str1, str2) {
  
  vlist <- list()
  for (cvar in str1) {
    if (!(cvar %in% str2)) {
      vlist[length(vlist)+1] <- cvar
    }
  }
  
  if (length(vlist)==0) {
    return(NULL)
  } else {
    return(unlist(vlist))
  }
  
}

#' Initialize sample balancing process.
#'
#' @param data Data.
#' @param targ An excel spreadsheet which contains targs information.
#' @param out Output targs into plain text file. If NULL, no output.
#' @return The targlist.
#' @examples
#' data <- read_sav("OnlineWgts.sav")
#' targ <- read.xlsx("targ.xlsx")
#' 
#' results <- sample_balance_init(data=data, targ=targ, out="targs.txt")
#' @export
sample_balance_init <- function(data=data, targ=targ, out=NULL) {
  
  # check if the variables are in the data
  targ.var <- (distinct(targ["Variable"]))$Variable
  
  vars <- find_mismatch_vars(targ.var, names(data))
  if (!is.null(vars)) {
    warning("The following variable(s) are not in the data: ", vars)
  }
  
  targ.num <- n_distinct(targ$Targ)
  targ.list <- list()
  
  for (iter in 1:targ.num) {
    
    targ.tmp <- filter(targ, Targ==iter)
    varlist <- (distinct(targ.tmp["Variable"])$Variable)
    
    llabel <- list()
    lvalue <- list()
    lname  <- list()
    lvars  <- list()
    
    for (pos in seq_along(varlist)) {
      targ.pos <- filter(targ.tmp, Variable==varlist[pos])
      tname <- distinct(select(targ.pos, Name))
      
      llabel[[pos]] <- targ.pos[["Label"]]
      lvalue[[pos]] <- targ.pos[["Value"]]
      lname[[pos]]  <- tname[["Name"]]
      lvars[[pos]]  <- varlist[pos]
      
    }
    
    lname <- unlist(lname)
    lvars <- unlist(lvars)
    
    targs <- lvalue
    names(targs) <- lname
    
    for(i in 1:length(lname)) {
      names(targs[[i]])<-llabel[[i]]
    }
    
    targ.list[[iter]] <- list(targs, lvars)
  }
  
  if (!is.null(out)) {
    sink(file=out)
    
    for (i in 1:length(targ.list)) {
      cat("Targs", i, ":\n\n")
      print(targ.list[[i]][[1]])
      cat("\n\n\n")
    }
    
    sink()
  }
  
  return(targ.list)
}

#' Sample balancing processing with a k-factor.
#'
#' @param data Data to be sample balanced.
#' @param ID Unique ID of the dataset.
#' @param targstr Targ structure returned by the function \code{sample_balance_init}.
#' @param dweights Weight in the data. Default: rep(1,nrow(data))
#' @param cap Cap flag. If yes, a cap will be applied in sample balancing process. Default: True.
#' @param typeofcap Type of the cap. if 1, then cap by n*mean; if 2, then cap by mean +/-n*sd. n is assigned by capval. Default: 1 (mean cap).
#' @param capval The cap value. If mean cap is used, during each iteration, when new weights are larger than \code{capvalue*mean}, they will be adjusted back to \code{capvalue*mean}. If std cap is used, during each iteration, when new weights are larger than \code{mean+capvalue*std(weight)}, they will be adjusted back to \code{mean+capvalue*std(weight)}. when new weights are smaller than \code{mean-capvalue*std(weight)}, they will be adjusted up to \code{mean-capvalue*std(weight)}. Default: 6.
#' @param floor Floor flag. Indicate if a floor value will be used in sample balancing process. Default: False. When no cap used, this option is recommended to be TRUE.
#' @param floorval Floor value.  During each iteration, when new weights are smaller than the floorvalue, they will be adjusted up to floorvalue. Default: -Inf.
#' @param eps Episilon. The threshold used in iteration algorithm. Algorithm converges when the diff is less than eps. Default: 0.01
#' @param rounding Round flag. Indicate if the weights will be rounded in sample balancing process. Default: FALSE.
#' @param klimit Klimit flag. Indicate if a klimit value will be used in sample balancing process. Default: FALSE.
#' @param klimitval Klimit value. When Klimit value is applied, before iteration, two new vector will be created: \code{maxneww=dweights*klimitval} and \code{minneww=dweights/klimitval}. During each iteration, when new weights are larger than maxneww, they will be adjusted back to maxneww, when new weights are lower than minneww, they will be adjusted up to minneww. This option works similarly as the Cap option. Default: -Inf. 
#' @param maxiter Max number of iteration in sample balancing process. Default: 20000.
#' @param random Randomization flag. If yes, data will be randimized first before sample balancing process. Default: FALSE.
#' @param out The filename of the spreadsheet which contains the output information of sample balancing. The file is saved in current working directory. If NULL, no output file is generated.
#' @return Return a list with 2 elements. The first element contains sample balancing information such as convergence, iterations, DEFF, capval, stat efficiency et al. The second element is a dataframe with two columns, ID and new balanced (capped) weights.
#' @export
#' @examples
#' data <- read_sav("OnlineWgts.sav")
#' targ <- read.xlsx("targ.xlsx")
#' 
#' targstr <- sample_balance_init(data=data, targ=targ)
#' 
#' # Cap using 6*means
#' results.sb <- sample_blance(data = data,
#'                             ID = "BOOK_ID",
#'                             targstr = targstr,
#'                             dweights = wgt,
#'                             cap = T,
#'                             typeofcap = 1,
#'                             capval = 6,
#'                             floor = T,
#'                             floorval = 50,
#'                             eps = .01,
#'                             rounding = F,
#'                             klimit = F,
#'                             klimitval = Inf,
#'                             out = "out.xlsx")
#'
sample_balance <- function(data = data,
                          ID = ID,
                          targstr = targstr,
                          dweights = rep(1,nrow(data)),
                          cap = T,
                          typeofcap = 1, 
                          capval = 6, 
                          floor = F,
                          floorval = -Inf,
                          eps = 0.01,
                          rounding = F,
                          klimit = F,
                          klimitval = Inf,
                          maxiter = 20000,
                          random = FALSE,
                          out = NULL){
  
  targs <- targstr[[1]]
  cvars <- targstr[[2]]
  
  if (random) {
    data <- data[sample(nrow(data)),]
  }
  data.id <- data[[ID]]
  
  # check 1: number of targets = number of variables
  #if(length(data) != length(targs))
  #  stop(paste("CK1: Number of columns in data,",length(data)," not equal to number of variables in targets,",length(targs),".",sep=""))
  
  # reorder the SB variables in the order of targs
  data <- select(data, cvars) %>% as.data.frame()
  
  # check 2: Number of levels for targets ne length of SB variable
  for(i in 1:length(targs))
  {
    if(length(unique(data[,i]))!= length(targs[[i]]))
      stop(paste("CK2: Number of unique values in balancing variable# ",i," not equal to number of variables target classes.",sep=""))
  }
  
  # check 3: sum of targets for each variable are equivalent
  if(!prod(sum(targs[[1]])==unlist(lapply(targs,sum))))
    stop("CK3: Sum of targets is not equivalent for variables.",i)
  
  # check 4: every class is populated for every variable 
  for(i in 1:length(targs))
  {
    if(length(min(data[,i]):max(data[,i]))!=length(unique(data[,i])))
      stop(paste("In variable ",i," there are not observed data for each value in the range of the data.",sep=""))
    if(min(data[,i])!=1)
      stop(paste("CK4: In variable ",i," the first class is not represented by a value of 1...NEED a 1.",sep=""))
  }
  
  # check 4: cap type
  if(cap==T){
    if (!(typeofcap==1 | typeofcap==2 )) {
      warning("Cap used, but typeofcap not in (1,2), 1-mean, 2-std \n")
      return(NULL)
    }
  }
  curw<-rep(0,nrow(data))
  neww<-dweights
  maxneww<-dweights*klimitval
  minneww<-dweights/klimitval
  counter<-0
  
  capvalnew <- capval
  
  if (cap == T) {
    if (typeofcap==1) { #use capval*mean
      capvalnew <- capval*mean(dweights,na.rm=TRUE)
      
      if(rounding)
        capvalnew<-round(capvalnew)
      
    } else {
      if (typeofcap==2) { #use capval*sd
        capvalsd <- sd(dweights,na.rm=TRUE)
        capvalmean <- mean(dweights,na.rm=TRUE)
        capvalupper <- capvalmean+capval*capvalsd
        capvallower <- capvalmean-capval*capvalsd
        
        if(rounding) {
          capvalupper<-round(capvalupper)
          capvallower<-round(capvallower)
        }
        
        capvalnew <- capvalupper
      }
      # else do nothing
    }
  }
  
  #  if(rounding)
  #    capval<-round(capval)
  cat(sum(abs(curw-neww)),fill=T)
  
  while( (sum(abs(curw-neww))>eps)&(counter<maxiter))
  {
    cat(counter,sum(abs(curw-neww)),fill=T,sep=",")
    curw<-neww
    counter<-counter+1
    for(i in 1:length(targs))
      for(j in 1:length(targs[[i]]))
      {
        cellsum<-sum(neww*(data[,i]==j))
        if(rounding) 
          neww[data[,i]==j]<-round(targs[[i]][j]*neww[data[,i]==j]/cellsum)
        else 
          neww[data[,i]==j]<-targs[[i]][j]*neww[data[,i]==j]/cellsum
        
        if (klimit) 
        {
          neww[neww > maxneww]<-maxneww[neww > maxneww]
          neww[neww < minneww]<-minneww[neww < minneww]
        }
        
        if(cap) {
          if (typeofcap==1) 
            neww[neww>capvalnew]<-capvalnew
          
          if (typeofcap==2){
            neww[neww>capvalupper]<-capvalupper   
            neww[neww<capvallower]<-capvallower   
          }
        }
        if(floor)
          neww[neww<floorval]<-floorval
      }#close j
  }#close while
  
  sumabsdiff<-0
  for(i in 1:length(targs))
    for(j in 1:length(targs[[i]]))
      sumabsdiff<-sumabsdiff + abs(targs[[i]][j]-sum(neww*(data[,i]==j)))
  
  results<-list(neww,counter,sum(abs(curw-neww)),eps,floorval,capvalnew,klimitval,sumabsdiff)
  names(results)<-c("BALANCED_WEIGHT","ITERATIONS","ITERATION_DIFFERENCE","EPS","FLOORVAL","CAPVAL","KLIMITVAL","SUM_ABS_DIFF")
  
  if(!floor)
    results$FLOORVAL<--Inf
  
  if(!cap)
    results$CAPVAL<-Inf
  else results$CAPVAL<-capvalnew
  
  if(!klimit)
    results$KLIMITVCAL<-Inf
  
  #  return(results)
  
  datalist1<-results[[1]]
  iterations1<-results[[2]]
  converge1<-results[[3]]<results[[4]]
  names(datalist1)<-"BALANCED_WEIGHT"
  
  diag <- (weight_diag(data,
                       dweights,
                       datalist1,
                       targs,
                       results$FLOORVAL,
                       results$CAPVAL,
                       results$KLIMITVAL,
                       results$SUM_ABS_DIFF,
                       iterations1,
                       converge1))

  data.new <- data.frame(data.id, datalist1)
  names(data.new) <- c(ID, "Capped_WGT")
  
  diag <- list(diag, data.new)
  
  if (!is.null(out)) {
    cat("\nSave diagnosis to", out, ".\n")
    
    diag.new <- try(create_diag_output(diag))
    
    require(openxlsx)
    
    hs <- createStyle(textDecoration = "BOLD", fontName = "Arial", fontColour = "#FFFFFF", fontSize=10,
                      fgFill = "#4F80BD")
    
    write.xlsx(diag.new, out, asTable = FALSE, rowNames = FALSE, colNames = TRUE, colWidths = "auto", overwrite = TRUE,
               borders = "rows", firstRow = TRUE, headerStyle = hs)
    
  }
  
  return(diag)
}

weight_diag <-function(balvars,dweight,balweight,targs,floor,ceiling,kfactor,sum_abs_diff,iterations, convergence)
{
  
  eff_stat <- function(wt) {
    # Josephine.EFF
    V <- sd(wt, na.rm=TRUE)/mean(wt, na.rm=TRUE)
    eff=100/(1+V^2)
    
    return(eff)
  }
  
  #returns a list of length (#vars + 1)
  #each element corresponds to diagnostics for each variable, then the last is an overall/total summary
  result_list<-list(NULL)
  
  mininputcontrol<-Inf
  maxinputcontrol<--Inf
  
  for(i in 1:length(targs))
  {
    t.data<-matrix(-1,nrow=8,ncol=length(targs[[i]]))
    
    # get counts
    t.data[1,]<-tapply(balweight,balvars[,i],length)
    
    # get targs
    t.data[2,]<-targs[[i]]
    
    # get weights
    t.data[3,]<-tapply(balweight,balvars[,i],sum)
    
    ### Generate deff estimate by cell for initial design weights ###
    t.data[4,]<-1+((tapply(dweight,balvars[,i],sd))/(tapply(dweight,balvars[,i],mean)))**2
    
    ### Generate deff estimate by cell for balanced weights ###
    t.data[5,]<-1+((tapply(balweight,balvars[,i],sd))/(tapply(balweight,balvars[,i],mean)))**2
    
    ### Generate ratio between median of the balanced weights & median of the design weights at the cell levvel ###
    t.data[6,]<-tapply(balweight,balvars[,i],median)/(tapply(dweight,balvars[,i],median))
    
    ### Generate control input Ratio ###
    t.data[7,]<-tapply(balweight,balvars[,i],sum)/targs[[i]]
    
    ### Efficient Stat
    t.data[8,] <- tapply(balweight, balvars[,i],eff_stat)
    
    mininputcontrol<-min(mininputcontrol,min(t.data[7,]))
    maxinputcontrol<-max(maxinputcontrol,max(t.data[7,]))
    
    t.data<-as.data.frame(t.data)
    
    dimnames(t.data)[[1]]<-c("SAMPLE_COUNT","TARGET","BAL_WEIGHT","DEFF_4_DWEIGHT","DEFF_4_BALWGT","RATIO_OF_MEDIAN_BALWGT_2_DWGT","CONTROL_INPUT_RATIO", "STAT_EFFICIENCY")
    
    dimnames(t.data)[[2]]<-names(targs[[i]])
    
    result_list[[i]]<-t.data
  }
  
  # make total-level summary
  t.data<-matrix(-1,nrow=21,ncol=1)
  
  # Relative convergence
  t.data[1,1]<-convergence
  
  # Iterations
  t.data[2,1]<-iterations
  
  # SAMPLE COUNT
  
  t.data[3,1]<-nrow(balvars)
  
  # TARGET pop
  t.data[4,1]<-sum(targs[[1]])
  
  tmp<-lapply(targs,sum)
  if(min(unlist(tmp)) != max(unlist(tmp)))
    warning("TARGETS DO NOT ALL SUM UP TO THE SAME VALUE")
  
  # BALANCED TOTAL
  t.data[5,1]<-sum(balweight)
  
  # SUM OF ABSOLUTE MARGINAL DIFFERENCES
  t.data[6,1]<-sum_abs_diff
  
  #PERCENT MARGINAL DEVIATION
  t.data[7,1]<-sum_abs_diff/sum(targs[[1]])
  
  # min control input ratio
  t.data[8,1]<-mininputcontrol
  
  # max control input ratio
  t.data[9,1]<-maxinputcontrol
  
  # Generate overall Deff estimate for design weights ###
  t.data[10,1]<-1+(sd(dweight)/mean(dweight))**2
  
  # Generate overall Deff estimate for balanced weights ###
  t.data[11,1]<-1+(sd(balweight)/mean(balweight))**2
  
  #Generate the minimum and maximum ratio between balanced and design weights ###
  t.data[12,1]<-min(balweight/dweight)
  t.data[13,1]<-max(balweight/dweight)
  
  #floor value
  t.data[14,1]<-floor
  
  # how many times you hit the floor
  t.data[15,1]<-sum(balweight<=floor)
  
  #cap
  t.data[16,1]<-ceiling
  
  # how many times you hit the ceiling
  t.data[17,1]<-sum(balweight>=ceiling)
  
  # k-factor
  t.data[18,1]<-kfactor
  
  # how many times you hit the kfactor_floor of dweights
  t.data[19,1]<- sum((balweight/dweight)<=(1/kfactor))
  
  # how many times you hit the kfactor_ceiling of dweights
  t.data[20,1]<- sum((balweight/dweight)>=(kfactor))
  
  # efficient stat
  t.data[21,1]<-eff_stat(balweight)
  
  t.data<-as.data.frame(t.data)
  
  row.names(t.data)<-c("RELATIVE_CONVERGENCE", "ITERATIONS","SAMPLE_COUNT","TARGET","BALANCED_WEIGHT","SUM_OF_ABSOLUTE_MARGINAL_DIFFERENCES","PERCENT_MARGINAL_DEVIATION",
                       "CONTROL_INPUT_RATIO_MIN", "CONTROL_INPUT_RATIO_MAX","DEFF_FOR_DWEIGHT", "DEFF_FOR_BALWEIGHT", "MIN_RATIO_BALWGT_DIVBY_DWGT", "MAX_RATIO_BALWGT_DIVBY_DWGT",
                       "FLOOR_VALUE","FLOOR_COUNT","CAP_VALUE","CAPPED_COUNT","KFACTOR","KFACTOR_FLOOR_COUNT","KFACTOR_CAP_COUNT","TOT_STAT_EFFICIENCY")
  
  result_list[[i+1]]<-t.data
  names(result_list)<-c(names(targs),"OVERALL")
  return(result_list)
}

create_diag_output <- function(diag) {
  
  if (is.null(diag)) 
    return(NULL)
  
  diag1 <- diag[[1]]
  
  for (i in 1:(length(diag1)-1)) {
    
    diag2 <- diag1[[i]]
    
    diag3 <- as.data.frame(t(diag2))
    
    diag4 <- diag3[1,]
    for (j in 1:length(diag4)) diag4[,j] <- NA
    diag5 <- diag4
    
    diag3$Var <- rownames(diag3)
    
    diag4$Var <- paste0("Total (",names(diag1[i]),")")
    diag4[[1]] <- sum(diag3[,1])
    diag4[[2]] <- sum(diag3[,2])
    diag4[[3]] <- sum(diag3[,3])
    
    diag5$Var <- " "
    
    if (i==1) {
      final <- bind_rows(diag3,diag4)
    } else {
      final <- bind_rows(final, diag5, diag3, diag4)
    }
  }
  
  final <- select(final, Var, everything())
  
  diag6 <- diag1[["OVERALL"]]
  diag6$Var <- rownames(diag6)
  diag6 <- select(diag6, Var, everything())
  names(diag6) <- c("VAR", "VALUE")
  
  diag.new <- list()
  diag.new[[1]] <- final
  diag.new[[2]] <- diag6
  
  names(diag.new) <- c("DIAG", "OVERALL")
  
  return(diag.new)  
}

