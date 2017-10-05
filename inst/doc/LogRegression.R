## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE------------------------------------------------------

library(dplyr)
library(tidyr)
library(rms)
library(stringr)
library(SimmonsResearchR)


## ------------------------------------------------------------------------

data(modeldata)

#There are 5 dependent variables
DVList <- c("B_AUTOMOTIVE_GENS_MAKE_NETS_10459_Any_ChevroletGeo", 
            "B_AUTOMOTIVE_GENS_MAKE_NETS_10459_Any_Ford", 
            "B_AUTOMOTIVE_GENS_MAKE_NETS_10459_Any_Honda", 
            "B_AUTOMOTIVE_GENS_MAKE_NETS_10459_Any_Nissan", 
            "B_AUTOMOTIVE_GENS_MAKE_NETS_10459_Any_Toyota")


## ------------------------------------------------------------------------

DemoVars <- c('Gender','respmar2','employ','incmid','parent','own','agemid','race1',
              'race2','race3','educat1','educat2','educat3')

model.demo <- lrm_model(data=modeldata, DVList=DVList, IDVList=DemoVars, all=TRUE)


## ---- eval = FALSE-------------------------------------------------------
#  
#  save_models_excel(model.demo, out="demo.xlsx")
#  

## ------------------------------------------------------------------------

DTVars <- names(modeldata %>% slice(1) %>%
                  select(starts_with("DT")))

length(DTVars)

model.dt <- lrm_model(data=modeldata, DVList=DVList, IDVList=DTVars)


## ---- message=FALSE------------------------------------------------------
PsycoVars <- names(modeldata %>% slice(1) %>%
                     select(Apparel_5605_1:Views_7650_78))
length(PsycoVars)

# divide 570 Psycovars into 3 groups
PsycoVars1 <- PsycoVars[1:200]
PsycoVars2 <- PsycoVars[201:400]
PsycoVars3 <- PsycoVars[401:570]

model.Psyco1 <- lrm_model(data=modeldata, DVList=DVList,IDVList=PsycoVars1)
model.Psyco2 <- lrm_model(data=modeldata, DVList=DVList,IDVList=PsycoVars2)
model.Psyco3 <- lrm_model(data=modeldata, DVList=DVList,IDVList=PsycoVars3)


## ---- message=FALSE------------------------------------------------------

model.Psyco12 <- combine_lrm_models(data=modeldata, model.Psyco1, model.Psyco2)
model.Psyco123 <- combine_lrm_models(data=modeldata, model.Psyco12, model.Psyco3)


## ------------------------------------------------------------------------

model.final <- combine_lrm_models(data=modeldata, model.dt, model.Psyco123, Included=DemoVars)


## ---- eval = FALSE-------------------------------------------------------
#  
#  save_models_excel(model.final, out="modelfinal.xlsx")
#  

## ---- eval = FALSE-------------------------------------------------------
#  model.final.score <- score_model_data(model = model.final, newdata = modeldata, ID = "BOOK_ID", cutoff = 0.1, file = "modelscores.xlsx")
#  

