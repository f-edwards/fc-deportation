##############################
## ICE - family stability
## time series models for batch of outcomes

rm(list=ls()); gc()

library(tidyverse)
library(haven)

setwd("U:/fc-deportation")

source("models_read.r")

################################################
####### models
################################################
###### create list of identical models for puma, afcars



# puma_models_0<-lapply(puma_model_data,
#                     function(x){
#                       lm(sqrt(outcome) ~
#                            race * 
#                            (year:c287_active +
#                            factor(state) +
#                            factor(year) + 
#                            factor(state) * year),
#                              data=x)
#                     })
#   

# puma_models_st<-lapply(puma_model_data,
#                        function(x){
#                          lm(sqrt(outcome) ~ race * (
#                            factor(c287time_minus34) + 
#                              factor(c287time_minus12) + 
#                              factor(c287time_0) + 
#                              factor(c287time_plus12) + 
#                              factor(c287time_plus34) + 
#                              factor(c287time_plus56) + 
#                              year * factor(state) + # state slope
#                              factor(year) + # national year dummy
#                              factor(state)),  # state intercepts
#                            data=x)
#                        })

d_d_alt<-lm(sqrt(outcome) ~
                   race*(
                     factor(fips) +
                       factor(years_to_active) +
                       factor(c287_everactive)*factor(years_to_active)
                   ),
  data=puma_model_data[[3]])

var<-"cpgkid"
make_pred<-function(model){
  race<-unique(puma_long$race)
  fips<-4013
  state<-"4"
  years_to_active<- -3:5
  newdat<-data.frame(expand.grid(race, fips, years_to_active, c287_everactive))
  names(newdat)<-c("race", "fips", "years_to_active", "c287_everactive")
  ### make never implement scenario, everactive false, years_to_active==-1
  newdat_false<-expand.grid(race, fips, -1, FALSE)
  names(newdat_false)<-c("race", "fips", "years_to_active", "c287_everactive")
  ### make df with rows equal to newdat
  newdat_false<-map_df(seq_len(length(years_to_active)), ~newdat_false)
  #### predict for each time group, for never implement
  predTRUE<-predict(model, newdat)
  predFALSE<-predict(model, newdat_false)
  predDIFF<-predTRUE-predFALSE
  newdatDIFF<-newdat
  newdatDIFF$pred<-predDIFF
  return(newdatDIFF)
}

ggplot(newdatDIFF,
  aes(x=years_to_active, y = pred))+
  geom_point()+
  geom_hline(yintercept=0, lty=2)+
  geom_vline(xintercept=0, lty=2)+
  #geom_ribbon(alpha = 0.6) + 
  facet_wrap(~race)+
  ylab(var)+
  ggtitle("287g application counties, county + year FEs and slopes")

# puma_models_2<-lapply(puma_model_data,
#                       function(x){
#                         lm(sqrt(outcome) ~ race * (
#                           factor(c287time_minus34) +
#                             factor(c287time_minus12) +
#                             factor(c287time_0) +
#                             factor(c287time_plus12) +
#                             factor(c287time_plus34) +
#                             factor(c287time_plus56) +
#                             year * factor(fips) + # state slope
#                             factor(year) + # national year dummy
#                             factor(fips)),  # state intercepts
#                           data=x)
#                       })
# 
# puma_models_3<-lapply(puma_model_data,
#                       function(x){
#                         lm(sqrt(outcome) ~ race * (
#                           factor(c287time_minus34) +
#                             factor(c287time_minus12) +
#                             factor(c287time_0) +
#                             factor(c287time_plus12) +
#                             factor(c287time_plus34) +
#                             factor(c287time_plus56) +
#                             year * fips + # state slope
#                             factor(year) + # national year dummy
#                             factor(fips)),  # state intercepts
#                           data=x%>%
#                             filter(c287_everapplied == TRUE))
                      })
rm(pop_child); rm(cnty287g); rm(pop); rm(pop_child); rm(pop_puma); rm(temp); rm(puma_temp)
save.image("models_zeroes.RData")

#lapply(puma_models_st, BIC); lapply(puma_models_1, BIC); lapply(puma_models_2, BIC)

#source("models_vis.r")