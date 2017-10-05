
remove_multi_collinearity <- function(data = data, cutoff = 0.7) {
  
  if (is.null(data)) return(NULL)
  
  cr <- cor(data)
  highCr <- caret::findCorrelation(cr, cutoff = cutoff)
  
  if (length(highCr)>0) {
    return(data[,-highCr])
  } else {
    return(data)
  }
  
}

generate_formula1 <- function(DV, IDVList) {
  
  stopifnot(!is.null(DV), !is.null(IDVList))
  
  idvlist_pasted <- character(0)
  
  for (i in 1:length(IDVList)) {
    idvlist_pasted <- paste0(idvlist_pasted, IDVList[i], sep='+')
  }
  idvlist_pasted <- stringr::str_sub(idvlist_pasted, 1, stringr::str_length(idvlist_pasted)-1)
  
  return(paste0(DV,'~',idvlist_pasted))
  
}

generate_reg_formula <- function(DVList = DVList, IDVList = IDVList) {
  
  stopifnot(!is.null(DVList), !is.null(IDVList))
  
  idvlist_pasted <- character(0)
  
  for (i in 1:length(IDVList)) {
    idvlist_pasted <- paste0(idvlist_pasted, IDVList[i], sep='+')
  }
  idvlist_pasted <- stringr::str_sub(idvlist_pasted, 1, stringr::str_length(idvlist_pasted)-1)
  
  model.formula <- NULL
  
  # Dependent variables
  for (i in 1:length(DVList)) {
    ModelID <- paste0('MOD',i)
    
    #formula1 <- paste0(DVList[i], '~', idvlist_pasted)
    formula1 <- idvlist_pasted
    
    model.formula.tmp <- data.frame(ModelID =ModelID,
                                    DV = DVList[i],
                                    ModelFormula = formula1)

    model.formula.tmp$ModelID <- as.character(model.formula.tmp$ModelID)
    model.formula.tmp$DV <- as.character(model.formula.tmp$DV)
    model.formula.tmp$ModelFormula <- as.character(model.formula.tmp$ModelFormula)
    
    model.formula[[length(model.formula)+1]] <- model.formula.tmp
  }  
  
  return(model.formula)
}

lrm_ind_model <- function(lr.formula = lr.formula, all = all, Included = Included) {
  
  if (is.null(lr.formula)) return(NULL)

  cat("Fitting ", lr.formula$ModelID,":", lr.formula$DV, "\n")
  
  formula1 <- as.formula(paste0(lr.formula$DV, '~', lr.formula$ModelFormula))
  
  lr <- NULL
  
  try(lr <- rms::lrm(formula1, data=ModelDataTmp, maxit = 50, tol = 1e-20), silent = TRUE)
  
  if (is.null(lr) | (lr$fail == TRUE)) {
    # use ridge regression and try one more
    try(lr <- rms::lrm(formula, data=MDataTmp, penalty = 5, maxit = 50, tol = 1e-20), silent = TRUE)
    
    if (is.null(lr) | (lr$fail == TRUE)) {
      message("Model fit: ", lr.formula$DV, " fails!")
      return(NULL)
    }
  }
  
  # Performs a slightly inefficient but numerically stable version of fast backward elimination on factors
  if (!all) {
    lrbw <- NULL
    if (is.null(Included)) {
      try(lrbw <- rms::fastbw(lr, rule="p", sls=0.05, type="individual"))
    } else {
      try(lrbw <- rms::fastbw(lr, rule="p", sls=0.05, type="individual", force=c(1:length(Included))))      
    }
    
    if (is.null(lrbw)) {
      message("Model variable selection: ", lr.formula$DV, " fails!")
      return(NULL)
    }
  
    final.vars <- lrbw$names.kept
    if (length(final.vars)>0) {
      formula1 <- as.formula(generate_formula1(lr.formula$DV, final.vars))
    
      lrf <- NULL
      lrf <- rms::lrm(formula1, data=ModelDataTmp, maxit = 50, tol = 1e-20)

      if (!is.null(lrf) & (lrf$fail == FALSE)) {
        s <- as.character(formula1)[3]
        s <- stringr::str_replace_all(s, "\n", "")
        s <- stringr::str_replace_all(s, fixed(" "), "")
        s <- unlist(strsplit(s, split='\\+'))
      
        return(list(DV = lr.formula$DV,
                    IDV = s,
                    lr = lrf))
      }
    }
  } else {
    s <- as.character(formula1)[3]
    s <- stringr::str_replace_all(s, "\n", "")
    s <- stringr::str_replace_all(s, fixed(" "), "")
    s <- unlist(strsplit(s, split='\\+'))
    
    return(list(DV = lr.formula$DV,
                IDV = s,
                lr = lr))
  }
    
  return(NULL)
}

lrm_model_init <- function(data = data, DVList = DVList, IDVList = IDVList, Included = Included, all = all) {

  stopifnot(!is.null(data), nrow(data)>0)
  
  vname <- names(data)
  
  # check if all DVList in the data
  if (is.null(DVList)) {
    warning("No dependent variables!")
    return(NULL)
    
  } else {
    if (!all(DVList %in% vname)) {
      warning("Dependent variable(s) ", find_mismatch_vars(DVList, vname), " are not in the data!")
      return(NULL)
    }
  }
  
  # check if all IDVList in the data
  if (is.null(IDVList)) {
    warning("No independent variables!")
    return(NULL)
    
  } else {
    if (!all(IDVList %in% vname)) {
      warning("Independent variable(s) ", find_mismatch_vars(IDVList, vname), " are not in the data!")
      return(NULL)
    }
  }  

  # check if all Included in the data
  if (!is.null(Included)) {
    if (!all(Included %in% vname)) {
      warning("Included (forced) independent variable(s) ", find_mismatch_vars(Included, vname), " are not in the data!")
      return(NULL)
    }
  } 
  
  # use completed cases
  data <- data[complete.cases(data),]
  
  DVData <- data %>%
    dplyr::select(DVList)
  
  IDVData <- data %>%
    dplyr::select(IDVList)
  
  # intersection between IDVList and Included?
  
  if (!all) {
    nzv <- caret::nearZeroVar(IDVData, freqCut = 99/1, uniqueCut = 2)
    if (length(nzv) > 0) IDVData <- IDVData[,-nzv]
  
    IDVData <- remove_multi_collinearity(IDVData, cutoff=0.7)
  }
  
  if (ncol(IDVData) == 0) {
    warning("No independent variable predictable!")
    return(NULL)
  }

  if (!is.null(Included)) {
    IDVList <- c(Included, names(IDVData))
    
    IDVData <- data %>%
      dplyr::select(IDVList)
  }
  
  IDVList <- names(IDVData)
  
  f <- generate_reg_formula(DVList=DVList,IDVList=IDVList)
  
  if (is.null(f)) return(NULL)
  
  return(list(model.data=cbind(DVData, IDVData), model.formula=f))
}

#' Logistic Regression Model
#' 
#' @description Fit binary logistic regression models using MLE or penalized MLE. All the dependent variables are modeled using selected independent variables automatically. Multi-colliearity is removed in the final models using correlation analysis.
#' @param data The data. It must contain the variables (columns) that should be used, directly or indirectly, in the modelling procedures. Missing values (NA) are allowed.
#' @param DVList Dependent variables list which are needed to be modelled.
#' @param IDVList Independent variables list used to model dependent variables.
#' @param Included A variable list whose elements are forced to be included in the models, no matter variable selection is conducted or not. Default: NULL.
#' @param all Logical. If TRUE, the function will not do variables selection, only model fitting will be done. Default: FALSE. 
#' @return A list data structure which contains model results.
#' @export
#' @examples
#' 
#' model.Psyco <- lrm_model(data=ModelBase.new, 
#'                          DVList=DVList,
#'                          IDVList=PsycoVars)
#' 
lrm_model <- function(data = data, DVList = DVList, IDVList = IDVList, Included = NULL, all = FALSE, parallel = FALSE) {
  
  old <- options()
  options(warn = -1)
  
  model.struct <- lrm_model_init(data=data, DVList=DVList, IDVList=IDVList, Included = Included, all = all)
  if (is.null(model.struct)) return(NULL)
  
  model.data <- model.struct$model.data
  model.formula <- model.struct$model.formula

  ModelDataTmp <<- model.data
  
  dd <<- rms::datadist(model.data)
  options(datadist="dd")
  
  on.exit(dd <<- NULL, add = TRUE)
  on.exit(ModelDataTmp <<- NULL, add = TRUE)
  on.exit(options(old))
  
  if (parallel) {
    cluster <- parallel::makeCluster(parallel::detectCores()-1, "PSOCK")
    out <- parallel::parLapply(cluster, model.formula, lrm_ind_model)
    parallel::stopCluster(cluster)
    
  } else {
    out <- lapply(model.formula, lrm_ind_model, all = all, Included = Included)
  }

  return(out)
}

lrm_models <- function(data = data, model.formula = model.formula, all = FALSE, Included = Included, parallel = FALSE) {
  
  ModelDataTmp <<- data
  
  dd <<- rms::datadist(data)
  options(datadist="dd")
  
  on.exit(dd <<- NULL, add = TRUE)
  on.exit(ModelDataTmp <<- NULL)
  
  if (parallel) {
    cluster <- parallel::makeCluster(parallel::detectCores()-1, "PSOCK")
    out <- parallel::parLapply(cluster, model.formula, lrm_ind_model)
    parallel::stopCluster(cluster)
    
  } else {
    out <- lapply(model.formula, lrm_ind_model, all = all, Included = Included)
  }
  
  return(out)
}

#' Combine Logistic Regression Models
#' 
#' @description Combine two logistic regression models generated by lrm_model. Read the models information and refit the new models based on the union of the variables in two models. 
#' @param data The data. It must contain the variables (columns) that should be used, directly or indirectly, in the modelling procedures. Missing values (NA) are allowed.
#' @param model1 The first model.
#' @param model2 The second model.
#' @param Included A variable list whose elements are forced to be included in the models, no matter variable selection is conducted or not. Default: NULL.
#' @return A list data structure which contains model results.
#' @export
#' @examples
#' 
#' model.Psyco1 <- lrm_model(data = ModelBase, DVList = DVList, IDVList = PsycoVars1)
#' model.Psyco2 <- lrm_model(data = ModelBase, DVList = DVList, IDVList = PsycoVars2)
#' 
#' #combine two Psyco models
#' model.Psyco <- combine_lrm_models(data=ModelBase, model1=model.Psyco1, model2 = model.Psyco2)
#' 
#' #combine two Psyco models, however, we force the demo variables to be in the final model.
#' #the demo variables have to be in the data already.
#' 
#' DemoVars <- c('Gender','respmar2','employ','incmid','parent','own','agemid','race1','race2','race3','educat1','educat2','educat3')
#' model.Psyco <- combine_lrm_models(data=ModelBase, model1=model.Psyco1, model2 = model.Psyco2, Included = DemoVars)
#' 
combine_lrm_models <- function(data = data, model1 = model1, model2 = model2, Included = NULL) {

  # two models should have the same DVs, and in the same order, check
  stopifnot(!is.null(data), !is.null(model1), !is.null(model2))
  
  old <- options()
  options(warn = -1)
  
  on.exit(options(old))
  
  if (!(length(model1)==length(model2))) {
    warning("The length of the two models are not eual, two models should have the same dependent variables!")
    return(NULL)
  }
  
  # check if all Included in the data
  if (!is.null(Included)) {
    vname <- names(data)
    
    if (!all(Included %in% vname)) {
      warning("Included (forced) independent variable(s) ", find_mismatch_vars(Included, vname), " are not in the data!")
      return(NULL)
    }
  } 
  
  model.formula <- NULL
  
  for (i in 1:length(model1)) {
    
    if (!(model1[[i]]$DV==model2[[i]]$DV)) {
      warning("The dependent variables in two models are not in the same order!")
      return(NULL)
    }
    
    DV <-  model1[[i]]$DV
    
    vars1 <- model1[[i]]$IDV
    vars2 <- model2[[i]]$IDV
    vars <- c(vars1, vars2)

    if (!is.null(Included)) {
      vars <- c(Included, vars)
      
      mtmp <- data %>% dplyr::slice(1) %>%
        dplyr::select(vars)
      
      vars <- names(mtmp)
    }
    
    idvlist_pasted <- character(0)

    if (length(vars)>0) {
      for (j in 1:length(vars)) {
        idvlist_pasted <- paste0(idvlist_pasted, vars[j], sep='+')
      }
      
      idvlist_pasted <- stringr::str_sub(idvlist_pasted, 1, stringr::str_length(idvlist_pasted)-1)
    }
    

    ModelID <- paste0('MOD',i)
    
    #formula1 <- paste0(DVList[i], '~', idvlist_pasted)
    formula1 <- idvlist_pasted
    
    model.formula.tmp <- data.frame(ModelID =ModelID,
                                    DV = DVList[i],
                                    ModelFormula = formula1)
    
    model.formula.tmp$ModelID <- as.character(model.formula.tmp$ModelID)
    model.formula.tmp$DV <- as.character(model.formula.tmp$DV)
    model.formula.tmp$ModelFormula <- as.character(model.formula.tmp$ModelFormula)
    
    model.formula[[length(model.formula)+1]] <- model.formula.tmp
  }
  
  return(lrm_models(data = data, model.formula = model.formula, Included = Included))
  
}

#' Model Deployment: Score the new data based on the model.
#' 
#' @description Based on the models built, score a new data. Probability will be calculated as the model scores.
#' @param model The model generated by lrm_model.
#' @param newdata A new data to be scored
#' @param ID The ID variable in the data.
#' @param file The file name to save the model probabilities and the segments.
#' @return .
#' @export
#' @examples
#' 
#' model.score <- score_new_data(model = model.final, newdata = newdata, ID = "BOOK_ID", file = "modelscores.xlsx") 
#' 
score_new_data <- function(model = model, newdata = newdata, ID = ID, file = file) {
  
  old <- options()
  options(warn = -1)
  
  # need to make sure all the predictors are in the new data
  dd <- rms::datadist(newdata)
  options(datadist="dd")
  
  on.exit(options(old))
  
  pred.id <- select(newdata, ID)
  pred.out <- NULL
  
  for (i in 1:length(model)) {
    
    DV <- model[[i]]$DV
    lr <- model[[i]]$lr
    
    pred <- predict(lr, newdata=newdata, type="fitted")
    
    # if the scores are missing, replace with 0
    pred[is.na(pred)] <- 0
    
    pred.out.tmp <- data.frame(pred)
    names(pred.out.tmp) <- DV

    if (i==1) pred.out <- pred.out.tmp
    else pred.out <- cbind(pred.out, pred.out.tmp)

  }
  
  pred.out.final <- cbind(pred.id, pred.out)
  
  if (!is.null(file)) {
    l <- list(PROB=pred.out.final)
    
    openxlsx::write.xlsx(l, 
                         file=file, 
                         asTable=FALSE, 
                         freezePane=TRUE, 
                         colNames = TRUE, 
                         rowNames=FALSE, 
                         firstRow=TRUE, 
                         colWidths="auto")
  }
  
  return(pred.out.final)
}

#' Model Deployment: Score model data.
#' 
#' @description Based on the models built, score the modeling base. Probability will be calculated, and the new segments will be generated based on the model scores. Segment has 3 levels. 0: prospects. 1: selected prospects. 2: users. Selected prospected is non users and whose model scores are high than the cutoff value.
#' @param model The model generated by lrm_model.
#' @param newdata The modeling data used to build the model.
#' @param ID The ID variable in the data.
#' @param cutoff The cutoff percent used to create the segment. 
#' @param file The file name to save the model probabilities and the segments.
#' @return .
#' @export
#' @examples
#' 
#' model.final.score <- score_model_data(model = model.final, newdata = ModelBase, ID = "BOOK_ID", cutoff = 0.1, file = "modelscores.xlsx") 
#' 
score_model_data <- function(model = model, newdata = newdata, ID = ID, cutoff = 0.5, file = file) {

  old <- options()
  options(warn = -1)
  
  # need to make sure all the predictors are in the new data
  dd <- rms::datadist(newdata)
  options(datadist="dd")

  on.exit(options(old))
  
  pred.id <- select(newdata, ID)
  pred.out <- NULL
  
  for (i in 1:length(model)) {
    
    DV <- model[[i]]$DV
    lr <- model[[i]]$lr
    
    pred <- predict(lr, newdata=newdata, type="fitted")
    
    # if the scores are missing, replace with 0
    pred[is.na(pred)] <- 0
    cutoff <- ifelse(cutoff < 0, 0, ifelse(cutoff > 1, 1, cutoff))
    cutvalue <- quantile(pred, 1-cutoff)
    
    pred.out.tmp <- cbind(newdata[[DV]], data.frame(PROB=pred))
    names(pred.out.tmp) <- c("DV", "PROB")
    
    pred.out.tmp <- pred.out.tmp %>%
      dplyr::mutate(segment = ifelse(DV==1, 2, 
                              ifelse(DV==0 & PROB>=cutvalue, 1, 0)))

    pred.out.tmp1 <- dplyr::select(pred.out.tmp, PROB)
    names(pred.out.tmp1) <- DV 
    
    pred.out.tmp2 <- dplyr::select(pred.out.tmp, segment)
    names(pred.out.tmp2) <- DV 
    
    pred.out.tmp3 <- dplyr::select(pred.out.tmp, DV)
    names(pred.out.tmp3) <- DV     
    
    if (i==1) {
      pred.out1 <- pred.out.tmp1
      pred.out2 <- pred.out.tmp2
      pred.out3 <- pred.out.tmp3
    } else {
      pred.out1 <- cbind(pred.out1, pred.out.tmp1)
      pred.out2 <- cbind(pred.out2, pred.out.tmp2)
      pred.out3 <- cbind(pred.out3, pred.out.tmp3)
    }
  }
  
  pred.out.final1 <- cbind(pred.id, pred.out1)
  pred.out.final2 <- cbind(pred.id, pred.out2)
  pred.out.final3 <- cbind(pred.id, pred.out3)
  
  l <- NULL
  if (!is.null(file)) {
    l <- list(PROB=pred.out.final1,
              SEGMENT=pred.out.final2,
              DependentVariable=pred.out.final3)
    
    openxlsx::write.xlsx(l, 
                         file=file, 
                         asTable=FALSE, 
                         freezePane=TRUE, 
                         colNames = TRUE, 
                         rowNames=FALSE, 
                         firstRow=TRUE, 
                         colWidths="auto")
  }
  
  return(l)
}

#' Save models to spreadsheet.
#' 
#' @description Save models information including statistics (R2, AUC), odds, pvalue and vif to excel spreadsheet.
#' @param out The file name in excel spreadsheet format (.xlsx).
#' @return no return values
#' @export
#' @examples
#' 
#' save_model_excel(model.final, out="Model.xlsx")
#' 
save_model_excel <- function(model.out = model.out, out = out) {
  
  if (is.null(model.out)) return(NULL)
  
  old <- options()
  options(warn = -1)
  
  on.exit(options(old))
  
  for (i in 1:length(model.out)) {
    
    lrf <- model.out[[i]]$lr
      
    R2 <- lrf$stats["R2"]
    AUC <- lrf$stats["C"]
    VIF <- rms::vif(lrf)
    odds <- exp(lrf$coefficients)
    pval <- anova(lrf)[,3] 
    
    s <- model.out[[i]]$IDV
    s <- stringr::str_replace_all(s, "\n", "")
    s <- stringr::str_replace_all(s, fixed(" "), "")
    vars <- unlist(strsplit(s, split='\\+'))
    
    VIF1 <- VIF
    odds1 <- odds
    pval1 <- pval
    vars1 <- vars
    
    ModelStatsTmp <- cbind.data.frame(DV = model.out[[i]]$DV, 
                                      R2 = R2, 
                                      AUC = AUC,
                                      stringsAsFactors = FALSE)
    
    #VIF
    VIF <- data.frame(VIF=VIF, vname=seq(1:length(VIF))-1)
    
    VIF <- VIF %>%
      tidyr::spread(key=vname,value=VIF)
    colnames(VIF) <- paste0('VIF', colnames(VIF))
    
    ModelVIFTmp <- cbind.data.frame(DV = model.out[[i]]$DV, 
                                    VIF = VIF,
                                    stringsAsFactors = FALSE)
    names(ModelVIFTmp) <- c("DV", paste0("VIF",1:length(VIF1)))
    
    #odds
    odds <- data.frame(odds=odds, vname=seq(1:length(odds))-1)
    
    odds <- odds %>%
      tidyr::spread(key=vname,value=odds)
    colnames(odds) <- paste0('Odds', colnames(odds))
    
    ModelOddsTmp <- cbind.data.frame(DV = model.out[[i]]$DV, 
                                     odds,
                                     stringsAsFactors = FALSE)
    names(ModelOddsTmp) <- c("DV", paste0("ODDS",1:length(odds1)))
    
    #pval
    pval <- data.frame(pval=pval, vname=seq(1:length(pval))) %>%
      dplyr::filter(vname<length(pval))
    
    pval <- pval %>%
      tidyr::spread(key=vname,value=pval)
    colnames(pval) <- paste0('pval', colnames(pval))    
    
    ModelPValTmp <- cbind.data.frame(DV = model.out[[i]]$DV, 
                                     pval,
                                     stringsAsFactors = FALSE) 
    names(ModelPValTmp) <- c("DV", paste0("PVALUE",1:(length(pval1)-1)))   
    
    #variables
    vars <- data.frame(Name=vars, vname=seq(1:length(pval)))
    
    vars <- vars %>%
      tidyr::spread(key=vname,value=Name)
    colnames(vars) <- paste0('VName', colnames(vars))
    
    ModelVarsTmp <- cbind.data.frame(DV = model.out[[i]]$DV, 
                                     vars,
                                     stringsAsFactors = FALSE)
    names(ModelVarsTmp) <- c("DV", paste0("VAR",1:(length(vars1))))   
   
    if (i == 1) {
      ModelStats <- ModelStatsTmp
      ModelOdds  <- ModelOddsTmp
      ModelPVal  <- ModelPValTmp
      ModelVIF  <- ModelVIFTmp
      
      ModelVars  <- ModelVarsTmp
      
    } else {
      ModelStats <- bind_rows(ModelStats, ModelStatsTmp)
      ModelOdds <- bind_rows(ModelOdds, ModelOddsTmp)
      ModelPVal <- bind_rows(ModelPVal, ModelPValTmp)
      ModelVIF  <- bind_rows(ModelVIF, ModelVIFTmp)
      ModelVars <- bind_rows(ModelVars, ModelVarsTmp)
    }
  }
  
  if (!is.null(out)) {
    l <- list(Stats = ModelStats,
              Variable = ModelVars,
              Odds = ModelOdds,
              PValue = ModelPVal,
              VIF = ModelVIF
    )
    
    openxlsx::write.xlsx(l, 
                         file=out, 
                         asTable=TRUE, 
                         freezePane=TRUE, 
                         colNames = TRUE, 
                         rowNames=FALSE, 
                         firstRow=TRUE, 
                         colWidths="auto")
  }
  
}
