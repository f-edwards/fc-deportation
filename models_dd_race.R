##############################
## ICE - family stability
## time series models for batch of outcomes

rm(list=ls()); gc()

library(tidyverse)
library(haven)

setwd("U:/fc-deportation")

source("models_read_race.r")

################################################
####### models
################################################
###### create list of identical models for puma, afcars

puma_models<-list()
for(i in 1:length(puma_model_data)){
  puma_models[[i]]<-list()
  puma_models[[i]][["black"]]<-lm(sqrt(black) ~
                                    factor(fips) + 
                                    factor(years_to_active),
  data=puma_model_data[[i]])
}
  
  
  lapply(puma_model_data,
                      function(x){
                        lm(sqrt(outcome) ~
                             race*(
                               factor(fips) + 
                                 factor(years_to_active) +
                                 factor(c287_everactive)*factor(years_to_active)),
                          data=x)
                      })

### rob se sandbox
# library(sandwich)
# vc_rob<-vcovCL(puma_models[[1]], cluster = puma_model_data[[1]]$fips)
# se<-sqrt(diag(vc_rob))
# weird results, mucccch lower standard errors
# hist(summary(puma_models[[1]])$coefficients[,2]/se)

puma_models_nozeroes<-lapply(puma_model_data,
                             function(x){
                               lm(sqrt(outcome) ~
                                    race*(
                                      factor(fips) + 
                                        factor(years_to_active) +
                                        factor(c287_everactive)*factor(years_to_active)),
                                  data=x%>%
                                    filter(outcome!=0))
                             })

puma_models_trend<-lapply(puma_model_data,
                    function(x){
                      lm(sqrt(outcome) ~
                           race*(
                             factor(fips) * year +
                               factor(years_to_active) +
                               factor(c287_everactive)*factor(years_to_active)),
                         data=x)
                    })



puma_models_nozeroes_trend<-lapply(puma_model_data,
                          function(x){
                            lm(sqrt(outcome) ~
                                 race*(
                                   factor(fips) * year +
                                     factor(years_to_active) +
                                     factor(c287_everactive)*factor(years_to_active)),
                               data=x%>%
                                 filter(outcome!=0))
                          })