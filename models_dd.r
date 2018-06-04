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


### do a hand prediction on these...
### I think the plm models are working right...

### rob se sandbox
# library(sandwich)
# vc_rob<-vcovCL(puma_models[[1]], cluster = puma_model_data[[1]]$fips)
# se<-sqrt(diag(vc_rob))
# weird results, mucccch lower standard errors
# hist(summary(puma_models[[1]])$coefficients[,2]/se)
# 
# puma_models_nozeroes<-lapply(puma_model_data,
#                              function(x){
#                                lm(sqrt(outcome) ~
#                                     race*(
#                                       factor(fips) + 
#                                         factor(years_to_active) +
#                                         factor(c287_everactive)*factor(years_to_active)),
#                                   data=x%>%
#                                     filter(outcome!=0))
#                              })
# 

#### ADD 

# puma_models<-lapply(puma_model_data,
#                     function(x){
#                       lm((outcome) ~
#                            race*(
#                              factor(fips) +
#                                factor(years_to_active) +
#                                factor(year)),
#                          data=x)
#                     })

# library(sandwich)
# rob_vc<-vcovCL(puma_models[[1]], cluster = puma_model_data[[1]]$fips)
# se<-sqrt(diag(rob_vc))
# 
# x<-puma_models[[1]]

test_dat<-puma_model_data[[1]]


test<-lm((outcome) ~ - 1 +
          race:years_to_active+
          race:factor(year) + 
          race:factor(fips),
         data=test_dat)


### compare estimated se's
vc<-list("standard" = vcov(test),
         "basic" = sandwich(test),
         "CL-1" = vcovCL(test, cluster = test_dat[, c("fips")], type = "HC1", fix=TRUE),
         "CL-2" = vcovCL(test, cluster = test_dat[, c("fips", "race")], type = "HC1", fix=TRUE),
         #"PC" = vcovPC(test, cluster = test_dat[, c("fips", "race")], order.by = test_dat$year, fix=TRUE),
         "HC" = vcovHC(test, type="HC1"))
 
         
se<-function(vcov)sapply(vcov, function(x)round(sqrt(diag(x)),3))
head(se(vc))

# 
# puma_models_trend<-lapply(puma_model_data,
#                           function(x){
#                             lm((outcome) ~
#                                  race*(
#                                    factor(fips) * year +
#                                      factor(years_to_active) +
#                                      factor(year)),
#                                data=x)
#                           })
# 
# 
# 
# 
# puma_models_nozeroes_trend<-lapply(puma_model_data,
#                           function(x){
#                             lm(sqrt(outcome) ~
#                                  race*(
#                                    factor(fips) * year +
#                                      factor(years_to_active) +
#                                      factor(c287_everactive)*factor(years_to_active)),
#                                data=x%>%
#                                  filter(outcome!=0))
#                           })