## ---- message=FALSE------------------------------------------------------

library(dplyr)
library(StatMatch)
library(dummies)
library(rms)
library(SimmonsResearchR)


## ------------------------------------------------------------------------

data(mag)

# select MAG vehicles (MAG<=10) for testing
rec <- mag %>% filter(type==1, MAG<=10)
don <- mag %>% filter(type==0, MAG<=10)


## ------------------------------------------------------------------------

match.vars   = c("NFAC1_2", "NFAC2_2", "NFAC3_2", "NFAC4_2", 
                 "childhh","agemid", "incmid", "ethnic", 
                 "maritalstat", "educat", "homestat", 
                 "employstat", "dvryes", "cabdsl")


## ------------------------------------------------------------------------

DVList <- names(mag %>% select(starts_with("AT")))
DVList
      

## ------------------------------------------------------------------------

match.var.factor <- c("NFAC1_2", "NFAC2_2", "NFAC3_2", "NFAC4_2", "NFAC5_2", "NFAC6_2", "NFAC7_2", "childhh", "ethnic", "maritalstat",                       "educat", "homestat", "employstat", "dvryes", "cabdsl")

match.var.num <- c("agemid", "incmid")

#change the types of the matching variables, only factors will be used to generate dummy variables
don <- don %>%
  mutate_at(vars(match.var.factor), as.factor) %>%
  mutate_at(vars(match.var.num), as.numeric) %>%
  mutate_at(vars(DVList), as.numeric) %>%
  select(DVList, match.var.factor, match.var.num)
    

## ------------------------------------------------------------------------

don.new <- dummy_recodes(don, drop=TRUE, all=TRUE)

#output the variables for OLS
IDVListData <- don.new %>%
  select(setdiff(names(don.new), c(DVList))) %>%
  slice(1)

IDVList <- names(IDVListData)
IDVList


## ------------------------------------------------------------------------

results <- vars_redun_ols(data=don.new, DVList = DVList, IDVList = IDVList)

knitr::kable(results, caption = "Variables Importance") 
      

## ------------------------------------------------------------------------

match.vars   = c("NFAC1_2", "NFAC2_2", "NFAC3_2", "NFAC4_2", 
                 "childhh","agemid", "maritalstat", "employstat", "dvryes")


## ------------------------------------------------------------------------

rec <- mag %>% filter(type==1, MAG<=10)
don <- mag %>% filter(type==0, MAG<=10)

system.time(out.nnd.c <- stat_match(data.rec=rec,
                         data.don=don,
                         rec.id="BOOK_ID",
                         don.id="RESPID", 
                         match.vars=match.vars,
                         don.class=c("age", "gender", "WhiteNH"),
                         k = 50,
                         constrained=T))

table(out.nnd.c$MatchLevel)
      

## ------------------------------------------------------------------------

group.v = c("age", "gender", "WhiteNH")
group.v2 = c("age", "gender")
group.v3 = c("gender")

system.time(out.nnd.c <- stat_match(data.rec=rec,
                         data.don=don,
                         rec.id="BOOK_ID",
                         don.id="RESPID", 
                         match.vars=match.vars,
                         don.class=group.v,
                         don.class2=group.v2,
                         don.class3=group.v3,
                         k = 50,
                         constrained=T))

table(out.nnd.c$MatchLevel)     


## ------------------------------------------------------------------------

system.time(out.nnd.c <- stat_match(data.rec=rec,
                         data.don=don,
                         rec.id="BOOK_ID",
                         don.id="RESPID", 
                         match.vars=match.vars,
                         don.class=group.v,
                         don.class2=group.v2,
                         don.class3=group.v3,
                         by.var = "MAG",
                         k = 50,
                         constrained=T))

table(out.nnd.c$MatchLevel)
      

## ------------------------------------------------------------------------

out <- append_donor_data(data.fuse=out.nnd.c, data.don=don, by=c("RESPID","MAG"), vars=DVList)
glimpse(out.nnd.c)


