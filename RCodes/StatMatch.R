
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
    cvars <- c()
    
    tvars <- unlist(vlist)
    for (i in 1:length(tvars)) {
      cvars <- paste0(cvars, tvars[[i]], sep=",")
    }
    cvars <- stringr::str_sub(cvars, 1, (stringr::str_length(cvars)-1))
    
    return(cvars)
  }
  
}

stat_match_init <- function(data.rec = data.rec, data.don = data.don, rec.id = rec.id, don.id = don.id, match.vars = match.vars, don.class = NULL, by.var = NULL, method = method, verbose = TRUE, parallel = parallel, ...) {
  
  if (is.null(data.rec) | nrow(data.rec) == 0) {
    warning("Recipient data is null! ")
    return(NULL)
  }
  
  if (is.null(data.don) | nrow(data.don) == 0) {
    warning("Donor data is null!")
    return(NULL)
  }
  
  rec.vars <- names(data.rec)
  don.vars <- names(data.rec)
  
  para.list <- list(...)
  
  # check if rec.id in data.rec
  if (!(rec.id %in% rec.vars)) {
    warning(rec.id, " is not in recipient data!")
    return(NULL)
  }
  
  # check if don.id in data.don
  if (!(don.id %in% don.vars)) {
    warning(don.id, " is not in donor data!")
    return(NULL)
  }
  
  # check if match variables in both recipent and donor data
  if (is.null(match.vars)) {
    warning("No matching variables!")
    return(NULL)
    
  } else {
    if (!all(match.vars %in% rec.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(match.vars, rec.vars), " are not in recipient data!")
      return(NULL)
    }
    
    if (!all(match.vars %in% don.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(match.vars, don.vars), " are not in donor data!")
      return(NULL)
    }
    
    rec.svars <- match.vars
    don.svars <- match.vars
    
  }
  
  # check if don.class in both recipient and donor data
  if (!is.null(don.class)) {
    
    if (!all(don.class %in% rec.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(don.class, rec.vars), " are not in recipient data!")
      return(NULL)
    }
    
    if (!all(don.class %in% don.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(don.class, don.vars), " are not in donor data!")
      return(NULL)
    }
    
    rec.svars <- c(rec.svars, don.class)
    don.svars <- c(don.svars, don.class)
  }  
  
  # check if there is don.class2, if yes, are all the variable in recipient and donor data?
  if (!is.null(para.list$don.class2)) don.class2 <- para.list$don.class2
  else don.class2 <- NULL
  
  if (!is.null(don.class2)) {
    
    if (!all(don.class2 %in% rec.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(don.class2, rec.vars), " are not in recipient data!")
      return(NULL)
    }
    
    if (!all(don.class2 %in% don.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(don.class2, don.vars), " are not in donor data!")
      return(NULL)
    }
    
    rec.svars <- c(rec.svars, don.class2)
    don.svars <- c(don.svars, don.class2)
  } 
  
  # check if there is don.class3, if yes, are all the variable in recipient and donor data?
  if (!is.null(para.list$don.class3)) don.class3 <- para.list$don.class3
  else don.class3 <- NULL
  
  if (!is.null(don.class3)) {
    
    if (!all(don.class3 %in% rec.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(don.class3, rec.vars), " are not in recipient data!")
      return(NULL)
    }
    
    if (!all(don.class3 %in% don.vars)) {
      warning("Match variable(s) ", find_mismatch_vars(don.class3, don.vars), " are not in donor data!")
      return(NULL)
    }
    
    rec.svars <- c(rec.svars, don.class3)
    don.svars <- c(don.svars, don.class3)
  }   
  
  # check if by.var in both recipient data
  if (!is.null(by.var)) {
    
    if (!all(by.var %in% rec.vars)) {
      warning("by.var variable ", find_mismatch_vars(by.var, rec.vars), " is not in recipient data!")
      return(NULL)
    }
    
    rec.svars <- c(rec.svars, by.var)
  } 
  
  if (verbose) cat("Initialize stat matching ...\n\n")
  
  # parse the list(...)
  
  if (!is.null(para.list$k)) k <- para.list$k
  else {
    cat("k value not assigned, default (15) will be used.\n")
    k <- 15
  }
  
  if (!is.null(para.list$method)) method <- para.list$method
  else {
    cat("method not assigned, 'NNK.hotdeck' will be used. \n")
    method <- "NNK.hotdecks"
  }
  
  if (!is.null(para.list$dist.fun)) dist.fun <- para.list$dist.fun
  else {
    cat("dist.fun not assigned, 'Manhattan' distance will be used. \n")
    dist.fun <- "Manhattan"
  }
  
  if (!is.null(para.list$constrained)) {
    if (!is.logical(para.list$constrained)) {
      
      warning("constrained should be logical(TRUE/FALSE). \n")
      return(NULL)
    }
    constrained <- para.list$constrained
    
  } else {
    cat("constrained option not assigned, default (FALSE) will be used. \n")
    constrained <- FALSE
  }
  
  if (!is.null(para.list$constr.alg)) {
    if (!para.list$constr.alg %in% c("lpSolve", "hungarian")) {
      warning("constr.alg has to be either 'lpSolve' or 'hungarian'.\n")
      return(NULL)
    }
    
    constr.alg <- para.list$constr.alg
  } else {
    cat("constr.alg not assigned, 'lpSolve' algorithm will be used. \n")
    constr.alg <- "lpSolve"
  }
  
  if (!is.null(para.list$suppresswarning)) {
    if (!is.logical(para.list$suppresswarning)) {
      warning("suppresswarning should be logical(TRUE/FALSE). \n")
      return(NULL)
    } 
    suppresswarningFlag <- para.list$suppresswarning
  } else {
    suppresswarningFlag = TRUE
  }
  
  # all matching variables have to be numeric
  data.rec <- data.rec %>% dplyr::mutate_at(vars(match.vars), as.numeric)
  data.don <- data.don %>% dplyr::mutate_at(vars(match.vars), as.numeric)
  
  # subset data.rec and data.don, only keep rec.id, don.id, match.var, don.class
  
  data.rec <- data.rec %>%
    select(rec.id, rec.svars)
  
  data.don <- data.don %>%
    select(don.id, rec.svars)
  
  if (verbose) {
    cat("\nSummary:\n")
    cat("Recipient data size: (", nrow(data.rec), ",", ncol(data.rec), ")\n")
    cat("Donor data size: (", nrow(data.don), ",", ncol(data.don), ")\n")
    
    rec.don.ratio <- nrow(data.rec)/nrow(data.don)
    
    cat("The ratio (no. recs)/(no. dons):", rec.don.ratio, "\n")
    
    cat("constrained =", constrained, "\n")
    cat("constr.alg =", constr.alg, "\n")
    cat("k =", k, "\n")
    cat("method =", method, "\n")
    cat("dist.fun =", dist.fun, "\n")
    cat("Match variables:", match.vars, "\n")
    cat("Don.class:", don.class, "\n")
    cat("Don.class2:", don.class2, "\n")
    cat("Don.class3:", don.class3, "\n")
    cat("by.var:", by.var, "\n")
    cat("parallel:", parallel, "\n")
    cat("suppresswarningFlag:", suppresswarningFlag, "\n")
  }
  
  if (rec.don.ratio > k) {
    cat("The ratio (no. recs)/(no. dons) is larger than k value, recipient data will be truncated into the chunks.\n")
  }
  
  SMControlList <- list()
  
  if (!is.null(by.var)) {
    
    data.rec.tmp <- data.rec %>%
      select(by.var) %>%
      distinct() 
    data.rec.tmp$by.var.new <- rownames(data.rec.tmp)
    
    data.rec <- left_join(data.rec, data.rec.tmp, by=c(by.var))
    by.var.n <- n_distinct(data.rec$by.var.new)
    
    data.don <- left_join(data.don, data.rec.tmp, by=c(by.var))
    
    # what if MAG not in data.don?
    
    for (n in 1:by.var.n) {
      
      data.rec.tmp <- data.rec %>%
        filter(by.var.new == n)
      
      data.don.tmp <- data.don %>%
        filter(by.var.new == n)
      
      # what if data.don.tmp has 0 rows?
      # just sample nrow(data.rec.tmp) from data.don
      
      if (is.null(data.don.tmp) | nrow(data.don.tmp) == 0) {
        data.don.tmp <- dplyr::sample_n(data.don, size = min(nrow(data.don), nrow(data.rec.tmp)), replace = FALSE)
      }
      
      SMControlList[[length(SMControlList)+1]] <- list(data.rec = as.data.frame(data.rec.tmp),
                                                       data.don = as.data.frame(data.don.tmp),
                                                       rec.id = rec.id,
                                                       don.id = don.id,
                                                       match.vars = match.vars,
                                                       don.class = don.class,
                                                       don.class2 = don.class2,
                                                       don.class3 = don.class3,
                                                       constrained = constrained,
                                                       constr.alg = constr.alg,
                                                       k = k,
                                                       method = method,
                                                       dist.fun = dist.fun,
                                                       by.var = by.var,
                                                       verbose = verbose,
                                                       suppresswarningFlag = suppresswarningFlag)
      
    }
    
  } else {
    
    SMControlList[[1]] <- list(data.rec = as.data.frame(data.rec),
                               data.don = as.data.frame(data.don),
                               rec.id = rec.id,
                               don.id = don.id,
                               match.vars = match.vars,
                               don.class = don.class,
                               don.class2 = don.class2,
                               don.class3 = don.class3,
                               constrained = constrained,
                               constr.alg = constr.alg,
                               k = k,
                               method = method,
                               dist.fun = dist.fun,
                               by.var = NULL,
                               verbose = verbose,
                               suppresswarningFlag = suppresswarningFlag)
    
  }  
  
  return(SMControlList)
}


#' Statistical matching with StatMatch.
#'
#' @param data.rec Recipient data.This data frame must contain the variables (columns) that should be used, directly or indirectly, in the matching application. Missing values (NA) are allowed.
#' @param data.don Donor data. The variables (columns) involved, directly or indirectly, in the computation of distance must be the same and of the same type as those in data.rec.
#' @param rec.id Recipient id in recipient data.
#' @param don.id Donor id in donor data.
#' @param match.vars A character vector with the names of the matching variables (the columns in both the data frames) that have to be used to compute distances among records (rows) in data.rec and those in data.don. 
#' @param don.class A character vector with the names of the variables (columns in both the data frames) that have to be used to identify the donation classes. In this case the computation of distances is limited to those units of data.rec and data.doc that belong to the same donation class. The case of empty donation classes should be avoided.The variables chosen for the creation of the donation clasess should not contain missing values (NAs).
#' @param don.class2 The second level of don.class. If stat matching fails by using don.class, the function will rerun the stat match using don.class2 if provided. 
#' @param don.class3 The third level of don.class. If stat matching fails by using don.class and don.class2, the function will rerun the stat match using don.class3 if provided.
#' @param by.var A variable which segments both recipients and donors into the same groups, where the stat matching are conducted. By.var variable has to be the variable in both data.rec and data.don. If by.var is NULL, stat matching will run without grouping first. If by.var has many levels (for example > 20), parallel processing is recommended.
#' @param dist.fun Distance function. The following distances are allowed: "Manhattan" (aka "City block"; default), "Euclidean", "Mahalanobis","exact" or "exact matching", "Gower", "minimax" or one of the distance functions available in the package \code{proxy}.
#' @param k The number of times that a unit in data.don can be selected as a donor when constrained=TRUE. Default value is 15.
#' @param constrained Logical. When constrained=FALSE (default) each record in data.don can be used as a donor more than once. On the contrary, when constrained=TRUE each record in data.don can be used as a donor only k times. In this case, the set of donors is selected by solving an optimization problem, in order to minimize the overall matching distance. See description of the argument constr.alg for details. Set this option to FALSE if stat match keeps on failing.
#' @param constr.alg A string that has to be specified when constrained=TRUE. Two choices are available: “lpSolve” and “hungarian”. Note that Hungarian algorithm is faster and more efficient if compared to constr.alg="lpSolve" but it allows selecting a donor just once, i.e. k = 1 . Default: "LpSolve".
#' @param parallel Logical. When TRUE, stat matching runs in parallel way (call parLapply function in parallel package). Default FALSE.
#' @return A synthetic data frame after the statistical matching of two data sources. The data frame includes all the columns in data.rec plus the don.id in data.don and a variable called "MatchLevel". MatchLevel can be 1 (matched on don.class level), 2 (matched on don.class2 level), 3 (matched on don.class3 level) and 0 (stat matching unsuccessful).
#' @export
#' @examples
#' mag <- data(mag)
#'
#' rec <- mag %>% filter(type==1)
#' don <- mag %>% filter(type==0)
#' 
#' don.class  = c("age", "gender", "WhiteNH")
#' don.class2 = c("age", "gender")
#' don.class3 = c("gender")
#' 
#' X.mtc   =  c("NFAC1_2", "NFAC2_2", "NFAC3_2", "NFAC4_2", "NFAC5_2", "NFAC6_2", "NFAC7_2", "childhh", "agemid", "incmid", "ethnic", "maritalstat",
#'              "educat", "homestat", "employstat", "dvryes", "cabdsl")
#' 
#' out.nnd.c <- stat_match(data.rec = rec1,
#'                         data.don = don1,
#'                         rec.id = "BOOK_ID",
#'                         don.id = "RESPID", 
#'                         match.vars = X.mtc,
#'                         don.class  = group.v,
#'                         don.class2 = don.class2,
#'                         don.class3 = don.class3,
#'                         by.var = "MAG",
#'                         k = 50,
#'                         constrained = T))
#'
stat_match <- function(data.rec = data.rec, data.don = data.don, rec.id = rec.id, don.id = don.id, match.vars = match.vars, don.class = NULL, by.var = NULL, method = method, verbose = TRUE, parallel = FALSE, ...) {
  
  old <- options()
  options(warn = -1)
  
  on.exit(options(old))
  
  SMControlList <- stat_match_init(data.rec = data.rec, data.don = data.don, rec.id = rec.id, don.id = don.id, match.vars = match.vars, don.class = don.class, by.var = by.var, method = method, parallel = parallel, verbose = TRUE, ...)
  
  
  if(is.null(SMControlList)) {
    stop("Stat matching process aborted!")
  }
  
  cat("\nStart stat matching ... \n")
  
  if (parallel) {
    
    cluster <- parallel::makeCluster(parallel::detectCores()-1, "PSOCK")
    out <- parallel::parLapply(cluster, SMControlList, stat_match_nnd)
    parallel::stopCluster(cluster)
    
  } else {
    out <- lapply(SMControlList, stat_match_nnd)
  }
  
  
  
  
  final.out <- out[[1]]
  if (length(out) > 1) {
    for (i in 2:length(out)){
      final.out <- suppressWarnings(bind_rows(final.out, out[[i]]))
    }
  }
  
  remove.junk <- function() {
    junk <- dir(path=paste0(getwd()), pattern="tmp*") # ?dir
    file.remove(junk) # ?file.remove
  }
  
  #on.exit(remove.junk())
  
  return(final.out) 
}

stat_match_nnd <- function(SMControl=SMControl) {
  
  if (is.null(SMControl)) return(NULL)
  
  if (!is.null(SMControl$by.var)) {
    cat(SMControl$by.var,"=",unique(SMControl$data.rec[["by.var.new"]]),"\n")
  }

  #  tmp <- file(paste("test/tmp", nrow(SMControl$data.rec),".txt"), "w")
  tmp <- file(paste("tmp.txt"), "w")
  sink(tmp) 
  
  on.exit(sink(), add = TRUE)
  # on.exit(close(tmp))
  
  out.nnd.s <- NULL
  MatchLevel <- 0
  
  k <- SMControl$k
  
  if (SMControl$suppresswarningFlag) {
    try(suppressWarnings(out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                                             data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                                             don.class = SMControl$don.class, constrained = SMControl$constrained, 
                                                             constr.alg = SMControl$constr.alg, k = k)), silent = TRUE)
    
    # if fails, use don.class2
    if (!is.null(out.nnd.s)) {
      
      MatchLevel <- 1
      
    } else {
      
      if (!is.null(SMControl$don.class2)) {
        try(suppressWarnings(out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                                                 data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                                                 don.class = SMControl$don.class2, constrained = SMControl$constrained, 
                                                                 constr.alg = SMControl$constr.alg, k = k)), silent = TRUE)
      }
      
      # if fails again, use don.class2 without constrain
      if (!is.null(out.nnd.s)) {
        MatchLevel <- 2
      } else {
        
        if (!is.null(SMControl$don.class2)) {
          try(suppressWarnings(out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                                                   data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                                                   don.class = SMControl$don.class2, constrained = FALSE)), 
              silent = TRUE)
        }
        # if fails again, use don.class3 without constrain
        if (!is.null(out.nnd.s)) {
          MatchLevel <- 3
        } else {
          
          if (!is.null(SMControl$don.class3)) {
            try(suppressWarnings(out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                                                     data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                                                     don.class = SMControl$don.class3, constrained = FALSE)), 
                silent = TRUE)
          }
          
          if (!is.null(out.nnd.s)) {
            MatchLevel <- 4
          }
          
          
        }
        
      }
      
    }
  } else {
    
    try((out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                             data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                             don.class = SMControl$don.class, constrained = SMControl$constrained, 
                                             constr.alg = SMControl$constr.alg, k = k)), silent = TRUE)
    
    # if fails, use don.class2
    if (!is.null(out.nnd.s)) {
      
      MatchLevel <- 1
      
    } else {
      
      if (!is.null(SMControl$don.class2)) {
        try((out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                                 data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                                 don.class = SMControl$don.class2, constrained = SMControl$constrained, 
                                                 constr.alg = SMControl$constr.alg, k = k)), silent = TRUE)
      }
      
      # if fails again, use don.class2 without constrain
      if (!is.null(out.nnd.s)) {
        MatchLevel <- 2
      } else {
        
        if (!is.null(SMControl$don.class2)) {
          try((out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                                   data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                                   don.class = SMControl$don.class2, constrained = FALSE)), 
              silent = TRUE)
        }
        # if fails again, use don.class3 without constrain
        if (!is.null(out.nnd.s)) {
          MatchLevel <- 3
        } else {
          
          if (!is.null(SMControl$don.class3)) {
            try((out.nnd.s <- StatMatch::NND.hotdeck(data.rec = SMControl$data.rec, 
                                                     data.don = SMControl$data.don, match.vars = SMControl$match.vars, 
                                                     don.class = SMControl$don.class3, constrained = FALSE)), 
                silent = TRUE)
          }
          
          if (!is.null(out.nnd.s)) {
            MatchLevel <- 4
          }
          
        }
        
      }
      
    }
    
  }
  
  out.nnd.f <- StatMatch::create.fused(data.rec=SMControl$data.rec,
                                       data.don=SMControl$data.don,
                                       mtc.ids=out.nnd.s$mtc.ids,
                                       z.vars=SMControl$don.id)
  
  out.nnd.f$MatchLevel <- MatchLevel
  
  return(out.nnd.f)
  
}

#' Append assigned attributes in donor file to the fused data.
#'
#' @param data.fuse Fused data generated by function \code{stat_match}.
#' @param data.don Donor data.
#' @param by  Variables for table joining. Usually the donor_id plus by.var variable.
#' @param vars Variables from donor data which need to be appended.
#' @return A synthetic data frame includes all columns in data.fuse plus the columns of vars from data.don.
#' @export
#' @examples
#'
#' out <- donor_data_append(out, magtmp, by=c("donor_id", "MAG"), vars=c(paste0("AT",1:10)))
#'
append_donor_data <- function(data.fuse=data.fuse, data.don=data.don, by=by, vars=vars) {
  
  opt <- options()
  on.exit(options(opt))
  
  options(warn=-1)
  
  if (is.null(data.fuse)) {
    stop("data.fuse is null!")
  }
  
  if (is.null(data.don)) {
    stop("data.don is null!")
  }
  
  don.vars <- names(data.don)
  
  if (is.null(vars)) {
    warning("No appended variables!")
    return(NULL)
    
  } else {
    if (!all(vars %in% don.vars)) {
      warning("Variable(s) ", find_mismatch_vars(vars, don.vars), " are not in donor data!")
      return(NULL)
    }
  }
  
  don <- subset(data.don, select=c(by, vars))
  return(as.data.frame(left_join(data.fuse, don, by=c(by))))
}

