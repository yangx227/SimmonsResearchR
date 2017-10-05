

mi_impute_init <- function(data = data, ID = ID, m = m, rounded = rounded, parallel = parallel, verbose = verbose, ...) {
  
  if (is.null(data) | nrow(data) == 0) {
    warning("Data is null or row = 0 ! ")
    return(NULL)
  }  
  
  vname <- names(data)
  
  if (is.null(ID)) {
    warning("ID variable must be provided!")
    return(NULL)
  } else {
    # check if ID are in data
    if (!(ID %in% vname)) {
      warning("ID variable ", ID, " is not in the data!")
      return(NULL)
    }
  }
  
  para.list <- list(...)
  
  # parse the list(...)
  
  
  if (!is.null(para.list$imputed.vars)) {
    imputed.vars <- para.list$imputed.vars
    
    # check if imputed.vars are in data
    if (!all(imputed.vars %in% vname)) {
      warning("Imputed variable(s) ", find_mismatch_vars(imputed.vars, vname), " are not in the data!")
      return(NULL)
    }
    
  }
  else {
    imputed.vars <- NULL
  }
  
  if (!is.null(para.list$indep.vars)) {
    indep.vars <- para.list$indep.vars
    
    # check if indep.vars are in data
    if (!all(indep.vars %in% vname)) {
      warning("Independent variable(s) ", find_mismatch_vars(indep.vars, vname), " are not in the data!")
      return(NULL)
    }
    
  } else {
    indep.vars <- NULL
    
    # if there are imputed.vars, indep.vars cannot be null
    if (!is.null(imputed.vars)) {
      warning("No independent variable(s) assigned, please use option (indep.vars) to assign independent variable(s)! ")
      return(NULL)
    }
    
  }
  
  if (!is.null(para.list$indep.vars2)) {
    indep.vars2 <- para.list$indep.vars2
    
    # check if indep.vars are in data
    if (!all(indep.vars2 %in% vname)) {
      warning("Independent variable(s) ", find_mismatch_vars(indep.vars2, vname), " are not in the data!")
      return(NULL)
    }
    
  }  else {
    indep.vars2 <- NULL
  }
  
  if (!is.null(para.list$by.vars)) by.vars <- para.list$by.vars
  else {
    by.vars <- NULL
  }
  
  if (verbose) {
    cat("Amelia imputation initialize ...\n")
    
    if (!is.null(imputed.vars)) cat("Variables to be imputed: \n", imputed.vars, "\n\n")
    if (!is.null(indep.vars)) cat("Variables used to impute: \n", indep.vars, "\n\n")
    
    if (!is.null(by.vars)) cat("by.vars: \n", by.vars, "\n")
    
    cat("\nOptons:\n")
    cat("m = ", m, "\n")
    cat("parallel = ", parallel, "\n")
    cat("rounded = ", rounded, "\n\n")
    
  }
  
  MIControl <- list(ID = ID,
                    imputed.vars = imputed.vars,
                    indep.vars = indep.vars,
                    indep.vars2 = indep.vars2,
                    by.vars = by.vars,
                    m = m,
                    rounded = rounded, 
                    parallel = parallel, 
                    verbose = verbose)
  
  return(MIControl)
  
  
}

impute_default <- function(data = data, MIControl = MIControl) {
  
  data.tmp.id <- data %>% dplyr::select(MIControl$ID)
  data.tmp <- data %>% dplyr::select(dplyr::setdiff(names(data), MIControl$ID))
  
  data.tmp <- data.tmp %>% 
    dplyr::mutate_if(is.character, as.numeric)
  
  if (MIControl$parallel) {
    data.mi <- Amelia::amelia(data.tmp, m = MIControl$m, parallel = 'snow', ncpus = 3)
  } else {
    data.mi <- Amelia::amelia(data.tmp, m = MIControl$m)
  }
  
  maxv <- sapply(data.tmp, function(x) max(x, na.rm=TRUE))
  minv <- sapply(data.tmp, function(x) min(x, na.rm=TRUE))
  
  m <- MIControl$m
  
  # if the imputed values are below/above their min/max values, adjust the imputed values to be in the range
  for (j in 1:m) {
    
    im <- data.mi$imputations[[j]]
    
    im.name <- names(im)
    
    for (i in 1:ncol(im)) {
      s <- as.numeric(im[[i]])
      
      max.ind <- (s > maxv[i])
      s[which(max.ind)] <- maxv[i]
      
      min.ind <- (s < minv[i])
      s[which(min.ind)] <- minv[i]
      
      s <- data.frame(s)
      names(s) <- names(im[,i])
      
      if (i == 1) {
        imputed <- s
      } else {
        imputed <- cbind(imputed, s)
      }
      
    }
    
    names(imputed) <- im.name
    data.mi$imputations[[j]] <- imputed
  }
  
  data.mi.final <- (data.mi$imputations[[1]])
  for (i in 2:m) {
    data.mi.final <- (data.mi.final+data.mi$imputations[[i]])
  }
  # take the average of the imputed datasets
  data.mi.final <- data.mi.final/m     
  
  if (MIControl$rounded) data.mi.final <- round(data.mi.final)
  
  name.tmp <- names(data.mi.final)
  
  data.mi.final <- cbind(data.tmp.id, data.mi.final) %>%
    as.data.frame()
  
  names(data.mi.final) <- c(MIControl$ID, name.tmp)
  
  return(data.mi.final)
}


Impute_one_attr <- function(datOneAttr, indep.vars2 = indep.vars2, m = m, rounded = rounded, parallel = parallel, verbose = verbose) {
  
  # impute the 1st variable using the rest variables in the data frame
  
  # impute one by one is faster since we have to average and cal min/max which can all be vectorized
  # other we have to have a loop for all imputed vars
  
  cvar <- names(datOneAttr[,1])
  
  if (verbose) {
    cat(cvar, " ")
  }
  
  datTmp <- as.matrix(datOneAttr)
  
  # The 1st column is the variable to be imputed
  im.max <- max(datTmp[,1], na.rm = TRUE)
  im.min <- min(datTmp[,1], na.rm = TRUE)
  
  if (m == 1) ncpus <- 1
  else ncpus <- 3
  
  # Run by parallel or not
  if (parallel) {
    im.tmp.imputed <- Amelia::amelia(datTmp, m=m, p2s=0, boot.type = "none", parallel = 'snow', ncpus = ncpus)
  } else {
    im.tmp.imputed <- Amelia::amelia(datTmp, m=m, p2s=0, boot.type = "none")
  }
  
  # If Amelia fails, first we use additional independent variables for imputation
  # If still fails,  return the original data without imputation
  
  if (im.tmp.imputed$code != 1) {
    
    if (is.null(indep.vars2)) {
      message("Amelia imputation fails, please add additional independent variables!")
      message("Variable ", cvar, " will not be imputed!")
      
      im.out <- data.frame(datTmp[,1])
      names(im.out) <- cvar
      
      return(im.out)
      
    } else {
      
      datTmp <- as.matrix(datOneAttr[, names(datOneAttr) %in% c(cvar, indep.vars2)])
      
      if (verbose) 
        message("Re-impute using additional independent variables!")
      
      # Run by parallel or not
      if (parallel) {
        im.tmp.imputed <- Amelia::amelia(datTmp, m=m, p2s=0, boot.type = "none", parallel = 'snow', ncpus = ncpus)
      } else {
        im.tmp.imputed <- Amelia::amelia(datTmp, m=m, p2s=0, boot.type = "none")
      }
      
      # If still fails
      if (im.tmp.imputed$code != 1) {
        message("Amelia imputation fails!")
        message("Variable ", cvar, " will not be imputed!")
        
        im.out <- data.frame(datTmp[,1])
        names(im.out) <- cvar
        
        return(im.out)
      } else {
        if (verbose) cat("Success!\n")
      }
    }
  }  
  
  
  for (i in 1:m) {
    im.tmp <- im.tmp.imputed$imputations[[i]][,1]
    
    if (i==1) im <- im.tmp
    else {
      im <- im + im.tmp
    }
  }
  
  im <- im / m  
  
  im[im > im.max] <- im.max
  im[im < im.min] <- im.min
  
  if (rounded) {
    im <- round(im, digits=0)
  }
  
  im.out <- data.frame(im)
  names(im.out) <- cvar
  
  return(im.out)
}

Impute_attrs <- function(datAttrs, imputed.vars = imputed.vars, indep.vars = indep.vars, indep.vars2 = indep.vars2, m = m, rounded = rounded, parallel = parallel, verbose = verbose) {
  
  iter <- 1
  
  for (cvar in c(imputed.vars)) {
    
    if (verbose) {
      if (iter==1) cat("  processing variables: ")
    }
    
    datAttrs.tmp <- datAttrs[, names(datAttrs) %in% c(cvar, indep.vars)] %>%
      dplyr::select(cvar, everything())
    
    if (sum(is.na(datAttrs.tmp[,1])) == 0) {
      
      if (verbose) {
        cat("There is no missing data for ", cvar, ", skip this variable! \n")
      }
      
      cvar.im <- datAttrs.tmp[,1]
      names(cvar.im) <- cvar
    } else {
      cvar.im <- Impute_one_attr(datAttrs.tmp, 
                                 indep.vars2 = indep.vars2, 
                                 m = m, 
                                 parallel = parallel, 
                                 rounded = rounded, 
                                 verbose = verbose)
    }
    
    if (iter == 1) {
      im.final <- cvar.im
    }
    else {
      im.final <- suppressWarnings(bind_cols(im.final, cvar.im))
    }
    iter <- iter + 1
  }
  
  return(im.final)
  
}

impute_by <- function(data = data, ID = ID, imputed.vars = imputed.vars, indep.vars = indep.vars, indep.vars2 = NULL, m = 5, by.vars = by.vars, parallel = FALSE, rounded = TRUE, verbose = TRUE) {
  
  impute.final <- NULL
  
  bvar <- data %>% select(by.vars)
  by.varlist <- labels(table(bvar))[[1]]
  
  if (verbose) {
    cat("All segments (total =", length(by.varlist), "):\n")
    print(by.varlist)
  }
  
  iter <- 1
  for (v in c(by.varlist)) {
    
    if (verbose) {
      if (iter == 1)  cat("\nImpute by", by.vars, "...\n")
    }
    
    dattmp1 <- data %>% 
      dplyr::filter_at(vars(by.vars), any_vars(.==v))
    row1 <- nrow(dattmp1)
    
    dattmp1ID <- dattmp1 %>%
      select(ID, by.vars)
    
    dattmp2 <- dattmp1[,names(dattmp1) %in% indep.vars]
    dattmp3 <- dattmp2[complete.cases(dattmp2),] %>% tbl_df()
    row2 <- nrow(dattmp3)
    
    if (verbose) {
      cat(names(dattmp1ID[1,2]), "=", v, "\n")
    }
    
    if (row1>row2) {
      if (verbose) {
        message("There are ", row1-row2, " completed missing rows, dropped in imputation\n")
      }
      
      dattmp1 <- dattmp1[complete.cases(dattmp2), ] %>% dplyr::tbl_df()
      
      dattmp1ID <- dattmp1 %>%
        dplyr::select(ID, by.vars)     
    }
    
    impute.tmp <- suppressWarnings(Impute_attrs(dattmp1, 
                                                imputed.vars = imputed.vars, 
                                                indep.vars = indep.vars, 
                                                indep.vars2 = indep.vars2, 
                                                m = m, 
                                                parallel = parallel,
                                                rounded = rounded, 
                                                verbose = verbose))
    
    impute.tmp <- suppressWarnings(bind_cols(dattmp1ID, impute.tmp))
    
    if (verbose) cat("\n")
    
    if (iter == 1) {
      impute.final <- impute.tmp
    }
    else {
      impute.final <- suppressWarnings(bind_rows(impute.final, impute.tmp))
    }
    iter <- iter + 1
  }
  
  # reorder as data
  data.tmp <- dplyr::select(data, ID, by.vars)
  
  impute.final <- suppressWarnings(inner_join(data.tmp, impute.final, by = c(ID, by.vars)))
  
  return(impute.final)
}

#' Missing imputation with Amelia.
#'
#' @description Conduct missing imputation using Amelia algorithms.
#' @usage amelia_impute(data, ID, m, imputed.vars, indep.vars, ...)
#' @param data Input data to be imputed.
#' @param ID ID variable in the data.
#' @param imputed.vars Variables to be imputed.
#' @param indep.vars Independent variables which will be used for missing imputation. If NULL, function will use all the variables except ID in the data for the missing imputattion.
#' @param indep.vars2 Additional independent variables used for missing imputation. When imputation with \code{indep.vars} fails, additional independent variables will be used for second round imputation. Can be NULL.
#' @param m Number of times for imputation. Default value: 5. Can be 1.
#' @param by.var A grouping variable. If not NULL, the data will be segmented into many groups, and the missing imputations are done in each group, then the imputed data are combined. If NULL, the data will not be segmented before missing imputation.
#' @param parallel Logical. When TRUE, Amelia runs in a parallel way. Default FALSE.
#' @param rounded Logical. If the final imputed values are rounded to the nearest integer or not. Default value: TRUE.
#' @param verbose Logical. Default TRUE.
#' @export
#' @return A synthetic data frame with the imputed values. All the imputed values are the average of m imputed datasets.
#' @examples
#' data.im <- amelia_impute(data = data,
#'                          ID = "BOOK_ID",
#'                          imputed.vars = paste0("AT",1:33), 
#'                          indep.vars   = c(paste0("d",4:16)) 
#'                          indep.vars2  = c(paste0("d",4:6)),
#'                          m = 5)
#'
amelia_impute <- function(data = data, ID = ID, m = 5, rounded = TRUE, parallel = FALSE, verbose = TRUE, ...) {
  
  MIControl <- mi_impute_init(data = data, ID = ID, m = m, rounded = rounded, parallel = parallel, verbose = verbose, ...)
  
  if (!is.null(MIControl)) {
    
    if (is.null(MIControl$imputed.vars) & is.null(MIControl$indep.vars)) {
      # if no dependent & independent variables, we just use Amelia to do imputation directly
      
      return(impute_default(data = data, MIControl = MIControl))
      
    } else {
      
      if (!is.null(MIControl$imputed.vars) & !is.null(MIControl$indep.vars)) {
        
        if (is.null(MIControl$by.vars)) {
          
          datatmp <- mutate(data, segment = 1)
          datatmp$tmpID <- rownames(datatmp)
          
          imputed <- impute_by(data = datatmp, 
                               ID = "tmpID", 
                               imputed.vars = MIControl$imputed.vars, 
                               indep.vars = MIControl$indep.vars, 
                               indep.vars2 = MIControl$indep.vars2, 
                               by.vars = "segment", 
                               m = MIControl$m,
                               rounded = MIControl$rounded, 
                               parallel = MIControl$parallel,
                               verbose = MIControl$verbose) %>% select(imputed.vars)
          
          vname <- intersect(names(imputed), names(data))
          
          for (cvar in vname) 
            data[[cvar]] <- imputed[[cvar]]
          
          return(data)
          
        } else {
          
          imputed <- impute_by(data = data, 
                               ID = MIControl$ID, 
                               imputed.vars = MIControl$imputed.vars, 
                               indep.vars = MIControl$indep.vars, 
                               indep.vars2 = MIControl$indep.vars2, 
                               by.vars = MIControl$by.vars, 
                               m = MIControl$m,
                               rounded = MIControl$rounded, 
                               parallel = MIControl$parallel,
                               verbose = MIControl$verbose) %>% select(imputed.vars)
          
          vname <- intersect(names(imputed), names(data))
          
          for (cvar in vname) 
            data[[cvar]] <- imputed[[cvar]]
          
          return(data)
        }
        
        
      }
      
    }
    
  }
  
  return(NULL)  
}
