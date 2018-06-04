##############################
## ICE - family stability
## time series models for batch of outcomes

rm(list=ls()); gc()

library(tidyverse)
library(sandwich)

setwd("U:/fc-deportation")

source("models_read.r")

################################################
####### models
################################################
###### create list of identical models for puma, afcars


#### run on everapplied
#### mess with sample size inclusion
#### what explains - 
### 1) dad's being deported, family responses. 
### 2)families are selectively moving, poor complex families left
#### treatment intensity ()
#### controls: mobility patterns
#### spillover for migration hypothesis

### unauth share, pct hisp, hisp income, migration
### treatment intesnity - agreement type taskforce/jail 
### deportations
### late adopters vs early adopters, regions
### changes in employment


### with and w/o linear, foster care models
### use google trends data to get at changes in search related to deportation / ice

puma_models<-lapply(puma_model_data,
                    function(x){
                      lm((outcome) ~ - 1 +
                           race:years_to_active+
                           race:factor(year) + 
                           race:factor(fips),
                         data=x)
                    })

# puma_models_trend<-lapply(puma_model_data,
#                           function(x){
#                             lm((outcome) ~ - 1 +
#                                  race:years_to_active+
#                                  race:factor(year) + 
#                                  race:factor(fips) + 
#                                  race:factor(fips):year,
#                                data=x)
#                           })
# 
# puma_models_everapplied<-lapply(puma_model_data,
#                     function(x){
#                       lm((outcome) ~ - 1 +
#                            race:years_to_active+
#                            race:factor(year) + 
#                            race:factor(fips),
#                          data=x%>%
#                            filter(c287_everapplied==TRUE))
#                     })
# 
# puma_models_trend_everapplied<-lapply(puma_model_data,
#                           function(x){
#                             lm((outcome) ~ - 1 +
#                                  race:years_to_active+
#                                  race:factor(year) + 
#                                  race:factor(fips) + 
#                                  race:factor(fips):year,
#                                data=x%>%
#                                  filter(c287_everapplied==TRUE))
#                           })

afcars_models<-lapply(afcars_model_data,
                    function(x){
                      lm((outcome) ~ - 1 +
                           factor(age_group) +
                           race:years_to_active +
                           race:factor(year) + 
                           race:factor(fips),
                         data=x)
                    })

# afcars_models_trend<-lapply(afcars_model_data,
#                           function(x){
#                             lm((outcome) ~ - 1 +
#                                  factor(age_group) +
#                                  race:years_to_active+
#                                  race:factor(year) + 
#                                  race:factor(fips) + 
#                                  race:factor(fips):year,
#                                data=x)
#                           })
# 
# afcars_models_everapplied<-lapply(afcars_model_data,
#                                 function(x){
#                                   lm((outcome) ~ - 1 +
#                                        factor(age_group) +
#                                        race:years_to_active+
#                                        race:factor(year) + 
#                                        race:factor(fips),
#                                      data=x%>%
#                                        filter(c287_everapplied==TRUE))
#                                 })
# 
# afcars_models_trend_everapplied<-lapply(afcars_model_data,
#                                       function(x){
#                                         lm((outcome) ~ - 1 +
#                                              factor(age_group) +
#                                              race:years_to_active+
#                                              race:factor(year) + 
#                                              race:factor(fips) + 
#                                              race:factor(fips):year,
#                                            data=x%>%
#                                              filter(c287_everapplied==TRUE))
#                                       })

#### pull out coefs, compute robust standard errors
### 


save.image("linear_models.RData")