##############################
## ICE - family stability
## time series models for batch of outcomes

rm(list=ls()); gc()

library(tidyverse)
library(haven)
librar

setwd("U:/fc-deportation")

source("models_read.r")

################################################
####### models
################################################
###### create list of identical models for puma, afcars

x<-puma_model_data[[3]]


test<-lm((outcome) ~
     race*(
       factor(fips) * year +
         factor(years_to_active) +
         factor(year)),
   data=x)


test<-lm((outcome) ~
           race*(
             factor(fips)+
               factor(years_to_active) +
               factor(year)),
         data=x)


puma_models_plm<-plm(sqrt(outcome) ~ 
                    (c287_active + 
                      minus_4 +
                      minus_3 +
                      minus_2 +
                      minus_1 +
                      plus_0 +
                      plus_1 +
                      plus_2 +
                      plus_3 +
                      plus_4 +
                      plus_5 +
                      plus_6 +
                      plus_7),
                data=x%>%filter(race=="hispanic"),
                index=c("fips", "year"), model = "within",
                effect = "twoways")




var<-puma_outcomes[i]
newdatDIFF<-make_pred(puma_models[[i]])
plot_out[[i]]<-ggplot(newdatDIFF,
                      aes(x=years_to_active, y = pred))+
  geom_point()+
  geom_hline(yintercept=0, lty=2)+
  geom_vline(xintercept=0, lty=2)+
  facet_wrap(~race)+
  ylab(var)





                     
### rob se sandbox
library(sandwich)
vc_rob<-vcovCL(puma_models, cluster = x$fips)
se<-sqrt(diag(vc_rob))
# weird results, mucccch lower standard errors
# hist(summary(puma_models[[1]])$coefficients[,2]/se)

## general d in d
## outcome ~ c287_everactive + 

puma_models<-lapply(puma_model_data,
                      function(x){
                        lm(sqrt(outcome) ~
                             race*(c287_active + 
                               factor(fips) + 
                                 factor(years_to_active)),
                          data=x)
                      })



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