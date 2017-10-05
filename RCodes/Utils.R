
#' Append attributes of the common variables in both source and target datasets, from the target to the source.
#'
#' @param data1 Target dataset.
#' @param data2 Source dataset.
#' @return The source dataset with attributes of the common variables appended from the target dataset. 
#' @examples
#' data1 <- read_sav("example1.sav")
#' data2 <- read_sav("example2.sav")
#' data1 <- attributes_append(data1 = data1, data2 = data2)
#' @export
#
attributes_append <- function(data1=data1, data2=data2) {
  
  int_vars <- intersect(names(data1), names(data2))
  
  for (cvar in int_vars) {
    attributes(data1[[cvar]]) <- attributes(data2[[cvar]])
  }
  
  return(data1)
}

kwtable <- function(x) {
  ttable <- addmargins(table(x))
  
  if (is.null(attributes(x)$labels)) {
    message("Variable has no labels set yet.")
    return(ttable)
  }
  
  if (length(names(ttable)) == length(c(names(attributes(x)$labels),'Sum'))) {
    names(ttable) <- c(names(attributes(x)$labels),'Sum')  
  } else {
    message("The # of the variable's levels is not the same as the # of the variable's lablel, please check if there are all missing on some levels.")
    cat("Variable label:\n")
    print(names(attributes(x)$labels))
    cat("# of current levels: ", length(names(ttable))-1, "\n\n")
  }
  return(ttable)
}

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

dummy_recode <- function(x, sep=sep, drop=drop) {
  
  x.new <- dummies::dummy.data.frame(x, sep=sep)
  
  if (drop) {
    m <- colMeans(x.new)
    min.index <- which.min(m)
    
    return(dplyr::select(x.new,-min.index))
  } else {
    return(x.new)
  }
}

#' Creation of dummy variables.
#' @description Create dummy variables for the factor variables in the dataset. The function is based on the functions in R 'dummies' package.
#' @param data An object such as a data.frame or matrix that has colnames.
#' @param sep For the names of the created dummy variables, sep is the character used between the variable name and the value. Default: "_".
#' @param drop Logical. Whether to drop one level with smallest mean value. This is very useful to create dummy variables for regression since we can at most keep n-1 levels of a variable in the model. Default: False.
#' @param all Logical. Whether to return columns that are not dummy classes. The default is FALSE and not returns all classes. Non dummy classes are dropped
#' @return A synthetic data frame with the dummy variables. If all is TRUE, columns that are not dummy classes will be returned as well. If drop is TRUE, only (n-1) dummy variables will be created for one factor variable.
#' @export
#' @examples
#' mag <- data(mag)
#' 
#' var.factor <- c("NFAC1_2", "NFAC2_2", "NFAC3_2", "NFAC4_2", "NFAC5_2", "NFAC6_2", "NFAC7_2", "childhh", "ethnic", "maritalstat",
#'                 "educat", "homestat", "employstat", "dvryes", "cabdsl")
#' 
#' var.num <- c("agemid", "incmid")
#' 
#' #coerce var.factor to factor and var.num to numeric
#' mag1 <- mag %>%
#'   dplyr::mutate_at(vars(var.factor), as.factor) %>%
#'   dplyr::mutate_at(vars(var.num), as.numeric)
#'   
#' #only works on factors
#' results <- dummy_recodes(mag1, drop=TRUE, all=FALSE)
#' 
dummy_recodes <- function(data=data, sep="_", drop=FALSE, all=FALSE) {
  
  stopifnot(!is.null(data), nrow(data)>0, ncol(data)>0)
  
  cname <- names(data)
  
  if (is.null(cname)) {
    warning("data has to be a data frame or a matrix!")
    return(NULL)
  }
  
  # select character or factor variables
  data.tmp <- data %>%
    select_if(is.factor)
  
  cname <- names(data.tmp)
  if (is.null(cname) | length(cname) == 0) {
    message("The data does not contain any factor variables!")
    return(NULL)
  }
  
  
  for (cvar in seq_along(cname)) {
    x <- as.data.frame(data.tmp[[cname[cvar]]])
    names(x) <- cname[cvar]
    
    rnew <- dummy_recode(x, sep=sep, drop=drop)
    
    if (cvar == 1) results <- rnew
    else results <- cbind(results, rnew)
  }
  
  if (all) {
    data.tmp <- subset(data, select=setdiff(names(data), cname))
    
    return(as.data.frame(cbind(data.tmp, results)))
  } else {
    return(as.data.frame(results))
  }
  
}

smodel <- function(DV=DV, IDVList=IDVList) {
  
  # DV is a data frame with 1 column DV
  # MBase is a data frame with all IDV columns
  
  #IDVList <- names(MBase)
  
  # generate formula
  for (i in 1:length(IDVList)) {
    if (i==1) formula1 <- IDVList[1]
    else formula1 <- paste0(formula1, '+', IDVList[i])
  }
  
  cat(DV, " ")
  
  formula2 <- as.formula(paste0(DV, "~", formula1))
  
  try(lr <- rms::ols(formula2, data=datatmp, tol = 1e-10), silent = TRUE)
  if (!is.null(lr)) try(lrbw <- rms::fastbw(lr, rule="p", sls=0.05, type="individual"))
  
  if (!is.null(lrbw)) {
    FinalVars <- c(lrbw$names.kept)
    FinalVars <- data.frame(Variable=FinalVars)
    
    return(list(FinalVars, DV))
  }
  
  return(NULL)
}

#' The choice of the optimal matching variables based on OLS.
#' @description Use OLS to select the optimal subset of the matching variables. The variables which appear more times in the OLS models are considered as more important variables for statistical matching. Multicollinearity is checked for all the final variables.
#' @param data An object such as a data.frame or matrix that has colnames of dependents/independents variables.
#' @param DVList The dependent variables for model variables selection. It should be the variables need to be fused into the recipent data from the donor data.
#' @param IDVList The independent variables for model variables selection. It should be the original matching variables need to be redundented. 
#' @param out Output filename. The output file is in spreadsheet format, the file name should have a spreadsheet file extension (.xlsx). If ignored, no spreadsheet output will be generated.
#' @return A synthetic data frame with the variables freqency in OLS models. 
#' @export
#' @examples
#' 
#' #define dependent variables, which are the variables need to be fused from the donor data
#' DVList <- names(mag %>% select(starts_with("AT")))
#' 
#' #define match variables, some need to be the factors to generate dummy variables
#' match.var =  c("NFAC1_2", "NFAC2_2", "NFAC3_2", "NFAC4_2", "NFAC5_2", "NFAC6_2", "NFAC7_2", "childhh", "agemid", "incmid", "ethnic", "maritalstat",
#'                "educat", "homestat", "employstat", "dvryes", "cabdsl")
#'                
#' match.var.factor <- c("NFAC1_2", "NFAC2_2", "NFAC3_2", "NFAC4_2", "NFAC5_2", "NFAC6_2", "NFAC7_2", "childhh", "ethnic", "maritalstat",
#'                       "educat", "homestat", "employstat", "dvryes", "cabdsl")
#' match.var.num <- c("agemid", "incmid")
#' 
#' #only keep dependent and independent variables
#' don <- mag %>%
#'   mutate_at(vars(match.var.factor), as.factor) %>%
#'   mutate_at(vars(match.var.num), as.numeric) %>%
#'   mutate_at(vars(DVList), as.numeric) %>%
#'   select(DVList, match.var.factor, match.var.num)
#'   
#' #generate dummy variables
#' don.new <- dummy_recodes(don, drop=TRUE, all=TRUE)
#' 
#' IDVListData <- don.new %>%
#'   select(setdiff(names(don.new), c(DVList))) %>%
#'   slice(1)
#'
#' #match variable redunction   
#' results <- vars_redun(data=don.new, DVList = DVList, IDVList = names(IDVListData), out="results.xlsx")
#' 
vars_redun_ols <- function(data=data, DVList = DVList, IDVList = IDVList, out = NULL) {
  
  stopifnot(!is.null(data), nrow(data)>0, ncol(data)>0)
  stopifnot(length(DVList)>0, length(IDVList)>0)
  
  # make sure all the DV/IDV are in the data
  dnames <- names(data)
  
  if (!all(DVList %in% dnames)) {
    warning("Dependent variable(s) ", find_mismatch_vars(DVList, dnames), " are not in data!")
    return(NULL)
  }
  
  if (!all(IDVList %in% dnames)) {
    warning("Independent variable(s) ", find_mismatch_vars(IDVList, dnames), " are not in data!")
    return(NULL)
  }
  
  IDVData <- data %>%
    select(IDVList)
  
  cr <- cor(IDVData)
  highCr <- caret::findCorrelation(cr, cutoff=0.70)
  
  if (length(highCr)>0) {
    cat("Variable(s) ", names(IDVData[1,highCr]), " are excluded because of multicolinearity!\n")
    IDVList <- names(IDVData[,-highCr])
  }
  
  datatmp <<- data
  on.exit(datatmp <<- NULL, add = TRUE)
  on.exit(dd <<- NULL)
  
  dd <<- rms::datadist(datatmp)
  options(datadist="dd")
  
  cat("Modeling dependent variables:", "\n")
  results <- lapply(DVList, smodel, IDVList = IDVList)
  
  final <- NULL
  for (i in 1:length(results)) {
    if (i==1) final <- results[[i]][[1]]
    else final <- suppressWarnings(bind_rows(final, results[[i]][[1]]))
  }
  
  final <- final %>%
    group_by(Variable) %>%
    summarise(Freq=n()) %>%
    arrange(desc(Freq))
  
  if (!is.null(out)) {
    
    cat("\nSave variables selection to", out, ".\n")
    openxlsx::write.xlsx(final, 
                         file=out, 
                         asTable=TRUE, 
                         freezePane=TRUE, 
                         colNames = TRUE, 
                         rowNames=TRUE, 
                         firstRow=TRUE, 
                         colWidths="auto")
  }
  return(as.data.frame(final))
}
