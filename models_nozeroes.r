##############################
## ICE - family stability 
## time series models for batch of outcomes

# rm(list=ls()); gc()
# 
# library(tidyverse)
# library(haven)
# 
# setwd("U:/fc-deportation")
# 
# source("models_read.r")

################################################
####### models
################################################
###### create list of identical models for puma, afcars



# puma_models_0<-lapply(puma_model_data,
#                     function(x){
#                       lm(log(outcome) ~
#                            race * 
#                            (year:c287_active +
#                            factor(state) +
#                            factor(year) + 
#                            factor(state) * year),
#                              data=x)
#                     })
#   

puma_models_st<-lapply(puma_model_data,
                      function(x){
                        lm(log(outcome) ~ race * (
                          factor(c287time_minus34) + 
                            factor(c287time_minus12) + 
                            factor(c287time_0) + 
                            factor(c287time_plus12) + 
                            factor(c287time_plus34) + 
                            factor(c287time_plus56) + 
                            year * factor(state) + # state slope
                            factor(year) + # national year dummy
                            factor(state)),  # state intercepts
                          data=x%>%
                            filter(outcome>0))
                      })

puma_models_2<-lapply(puma_model_data,
                      function(x){
                        lm(log(outcome) ~ race * (
                            factor(c287time_minus34) +
                              factor(c287time_minus12) +
                              factor(c287time_0) +
                              factor(c287time_plus12) +
                              factor(c287time_plus34) +
                              factor(c287time_plus56) +
                              year * factor(fips) + # state slope
                              factor(year) + # national year dummy
                              factor(fips)),  # state intercepts
                          data=x%>%
                            filter(outcome>0))
                      })

puma_models_3<-lapply(puma_model_data,
                      function(x){
                        lm(log(outcome) ~ race * (
                          factor(c287time_minus34) +
                            factor(c287time_minus12) +
                            factor(c287time_0) +
                            factor(c287time_plus12) +
                            factor(c287time_plus34) +
                            factor(c287time_plus56) +
                            year * fips + # state slope
                            factor(year) + # national year dummy
                            factor(fips)),  # state intercepts
                          data=x%>%
                            filter(c287_everapplied == TRUE)%>%
                            filter(outcome>0))
                      })
rm(pop_child); rm(cnty287g); rm(pop); rm(pop_child); rm(pop_puma); rm(temp); rm(puma_temp)
save.image("models_nozeroes.RData")

#lapply(puma_models_st, BIC); lapply(puma_models_1, BIC); lapply(puma_models_2, BIC)

#source("models_vis.r")