## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE------------------------------------------------------

library(dplyr)
library(Amelia)
library(SimmonsResearchR)


## ------------------------------------------------------------------------

data(magdwt)
summary(magdwt)


## ------------------------------------------------------------------------

imputed1 <- amelia_impute(data = magdwt, ID = "RESPID", m = 5, rounded = TRUE)


## ------------------------------------------------------------------------

imputed.vars <- paste0("AT", 1:10)
indep.vars <- c(paste0("d",4:6), paste0("d28_",10:26))

imputed2 <- amelia_impute(data = magdwt, ID = "RESPID", m = 5, rounded = TRUE, imputed.vars = imputed.vars, indep.vars = indep.vars)


## ------------------------------------------------------------------------

imputed.vars <- paste0("AT", 1:10)
indep.vars <- c(paste0("d",4:6), paste0("d28_",10:26))
indep.vars2 <- c(paste0("d",4:6))

#if MI fails, rerun using variables in indep.vars2
#grouping the obs into segments using MAG variable before IM
imputed2 <- amelia_impute(data = (magdwt %>% filter(MAG<=10)), ID = "RESPID", m = 5, rounded = TRUE, imputed.vars = imputed.vars, indep.vars = indep.vars, indep.vars2 = indep.vars2, by.vars = "MAG")


