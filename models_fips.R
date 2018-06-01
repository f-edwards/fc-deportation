##############################
## ICE - family stability 
## time series models for batch of outcomes

rm(list=ls()); gc()

library(tidyverse)
library(haven)

setwd("U:/fc-deportation")

pop<-read_csv("./data/seer_reduced.csv")

#### to merge onto pums data for more stable denominators
pop_child<-pop%>%
  filter(age_group!="adult")%>%
  group_by(year, fips, race)%>%
  summarise(cpop = sum(pop))

################# for entries by first entry, by incapacitation
### lots of missing data on incap_index

cnty287g<-read_dta("./data/county287g.dta")
cnty287g$countyid<-as.numeric(cnty287g$countyid)
cnty287g$year<-as.numeric(cnty287g$year)
cnty287g$c287active<-as.logical(cnty287g$c287active)
cnty287g<-cnty287g%>%
  rename(fips=countyid)%>%
  mutate(c287_everapplied=TRUE)%>%
  select(-countyname)

#### drop the x year format, will fix that for the outcomes we've got
#### to expand the pre-treatment TS

cnty287g<-cnty287g%>%
  select(-year, -c287active)%>%
  distinct()

#######################################
## read and format puma data

  pop_puma<-read_dta("./data/countykids00to16.dta")
  pop_puma<-pop_puma%>%
    select(countyid, countyname, year, cukid_hisp,
           cukid_nhb, cukid_nhw,
           ckhhsize_nhw, ckhhsize_nhb, ckhhsize_hisp,
           ckhhfams_nhw, ckhhfams_nhb, ckhhfams_hisp,
           cgkid_nhw, cgkid_nhb, cgkid_hisp,
           cklivesmom_nhw, cklivesmom_nhb, cklivesmom_hisp,
           cknolivespar_nhw, cknolivespar_nhb, cknolivespar_hisp)
  pop_puma$fips<-as.numeric(pop_puma$countyid)
  pop_puma$year<-as.integer(pop_puma$year)
  ### exclude cases where sampling unit by race is <30
  ### only filter on hispanic sample for now, filtering on black sample 
  ### drastically cuts included counties
  pop_puma<-pop_puma%>%
    filter(cukid_hisp>=30)
  
  #### drop years with no cnty287g data
  pop_puma<-pop_puma%>%
    filter(year<=2012)
  
  vars<-c("ckhhsize",
          "ckhhfams",
          "cgkid",
          "cklivesmom",
          "cknolivespar")
  
  ### convert to long by race
  puma_temp<-list()
  for(i in 1:length(vars)){
    index<-grep(vars[i], names(pop_puma))
    temp<-pop_puma[,c(1:3, index)]
    temp<-temp%>%
      gather(key = race, value = value, -countyid, -countyname, -year)
    names(temp)[5]<-vars[i]
    temp$race<-substr(temp$race, nchar(temp$race)-3, nchar(temp$race))
    temp$race<-recode(temp$race, 
                      "_nhb" = "black",
                      "_nhw" = "white, nh",
                      "hisp" = "hispanic")
    puma_temp[[i]]<-temp
  }
  
  puma_long<-puma_temp[[1]]%>%
    left_join(puma_temp[[2]])%>%
    left_join(puma_temp[[3]])%>%
    left_join(puma_temp[[4]])%>%
    left_join(puma_temp[[5]])

  puma_long<-puma_long%>%
    rename(fips = countyid)%>%
    mutate(fips = as.numeric(fips))%>%
    left_join(cnty287g)%>%
    mutate(c287_everapplied = ifelse(is.na(c287_everapplied), "FALSE", 
                                     c287_everapplied),
           c287_active = ifelse(c287start_yr>=year, "TRUE", "FALSE"),
           c287_active = ifelse(is.na(c287start_yr), "FALSE", c287_active))
  
  puma_long<-puma_long%>%
    left_join(pop_child)%>%
    filter(!(is.na(cpop)))
  
  puma_long<-puma_long%>%
    mutate(cgkid = cgkid / cpop,
           cklivesmom = cklivesmom / cpop,
           cknolivespar = cknolivespar / cpop)
  
  ### pull out state
  puma_long<-puma_long%>%
    mutate(state = factor(ifelse(nchar(fips)==4, substr(fips, 1, 1),
                                 substr(fips, 1, 2))))
  
  ### make ever approved, time to implement variables
  ### for comparison of time to implement to never active,
  ### add 287everactive dummy
  puma_long<-puma_long%>%
    mutate(c287_everactive = !(is.na(c287start_yr)),
           years_to_active = year - c287start_yr,
           c287time_minus34 = ifelse(is.na(c287start_yr), FALSE, years_to_active < -2 & years_to_active>-5),
           c287time_minus12 = ifelse(is.na(c287start_yr), FALSE, years_to_active < 0 & years_to_active>-3),
           c287time_0 = ifelse(is.na(c287start_yr), FALSE, years_to_active == 0),
           c287time_plus12 = ifelse(is.na(c287start_yr), FALSE, years_to_active > 0 & years_to_active <3),
           c287time_plus34 = ifelse(is.na(c287start_yr), FALSE, years_to_active > 2 & years_to_active < 5),
           c287time_plus56 = ifelse(is.na(c287start_yr), FALSE, years_to_active > 4 & years_to_active <7))
  
  puma_outcomes<-c("ckhhsize",
                   "ckhhfams",
                   "cgkid",
                   "cklivesmom",
                   "cknolivespar")
  
  puma_model_data<-list()
  
  for(i in 1:length(puma_outcomes)){
    names_temp<-c("fips","state", "year", "race", 
                  "c287_active", "c287_everapplied", "c287_everactive",
                  "c287time_minus34","c287time_minus12", "c287time_0", "c287time_plus12",
                  "c287time_plus34","c287time_plus56", 
                  puma_outcomes[i])
    puma_model_data[[i]]<-puma_long%>%
      select(one_of(names_temp))
    names(puma_model_data)[i]<-puma_outcomes[i]
    names(puma_model_data[[i]])[length(names(puma_model_data[[i]]))]<-"outcome"
    ### for log transforms
    puma_model_data[[i]]$outcome<-puma_model_data[[i]]$outcome + 0.0001
  }
#######################################
## read and format afcars data
#### read in imputed afcars data
  files<-list.files("./imputations")
  imputeds<-paste("./imputations/", files[grep("cnty", files)], sep="")
  
  afcars<-read_csv(imputeds[1])
  
  for(i in 2:length(imputeds)){
    temp<-read.csv(imputeds[i])
    afcars<-afcars%>%
      rbind(temp)
  }
  
  ### remove over 18s
  afcars<-afcars%>%
    filter(age_group!="adult")

  afcars<-afcars%>%
    filter(.imp==1)%>% ### for development, change later to !=0
    filter(race!="other")%>%
    rename(fips = fipscode)
  
  ### fill in zeroes
  afcars<-afcars%>%
    complete(.imp, year, race, age_group, nesting(state, fips), fill=list(entered = 0, abuse = 0,
                                                           first_entry = 0, reun_exit = 0,
                                                           incar = 0, incap_other = 0,
                                                           neglect = 0, caseload = 0))
   
  #### join to 287 and make time vars
  afcars<-afcars%>%
    left_join(cnty287g)%>%
    mutate(c287_everapplied = ifelse(is.na(c287_everapplied), FALSE,
                  c287_everapplied),
           c287_active = ifelse(c287start_yr>=year, TRUE, FALSE),
           c287_active = ifelse(is.na(c287_active), FALSE, c287_active))%>%
    mutate(c287_everactive = !(is.na(c287start_yr)),
           years_to_active = year - c287start_yr,
           c287time_minus34 = ifelse(is.na(c287start_yr), FALSE, years_to_active < -2 & years_to_active>-5),
           c287time_minus12 = ifelse(is.na(c287start_yr), FALSE, years_to_active < 0 & years_to_active>-3),
           c287time_0 = ifelse(is.na(c287start_yr), FALSE, years_to_active == 0),
           c287time_plus12 = ifelse(is.na(c287start_yr), FALSE, years_to_active > 0 & years_to_active <3),
           c287time_plus34 = ifelse(is.na(c287start_yr), FALSE, years_to_active > 2 & years_to_active < 5),
           c287time_plus56 = ifelse(is.na(c287start_yr), FALSE, years_to_active > 4 & years_to_active <7))
  
  
  
  #### afcars moves all nyc cases to manhattan, make correction in seer
  pop_mod<-pop%>%
    mutate(fips = ifelse(fips %in% c(36005, 36047, 36061, 36081, 36085), 36061, fips))%>%
    group_by(year, fips, race, age_group)%>%
    summarise(pop = sum(pop))
  
  afcars<-afcars%>%
    left_join(pop_mod)
  
  #### stop at 2012
  afcars<-afcars%>%
    filter(year<2013)
  
  ### convert to per capita
  afcars<-afcars%>%
    mutate(entered = entered / pop,
           caseload = caseload / pop,
           reun_exit = reun_exit / pop,
           first_entry = first_entry / pop,
           abuse = abuse / pop,
           neglect = neglect / pop,
           incar = incar / pop,
           incap_other = incap_other / pop)
  
  afcars_outcomes<-c("entered",
                     "caseload",
                     "reun_exit",
                     "first_entry",
                     "abuse",
                     "neglect",
                     "incar",
                     "incap_other")
  
  afcars_model_data<-list()
  
  for(i in 1:length(afcars_outcomes)){
    names_temp<-c("fips","state", "year", "race", "age_group", 
                  "c287_active", "c287_everapplied", "c287_everactive",
                  "c287time_minus34","c287time_minus12", "c287time_0", "c287time_plus12",
                  "c287time_plus34","c287time_plus56", 
                  afcars_outcomes[i])
    
    afcars_model_data[[i]]<-afcars%>%
      select(one_of(names_temp))
    names(afcars_model_data)[i]<-afcars_outcomes[i]
    names(afcars_model_data[[i]])[length(names(afcars_model_data[[i]]))]<-"outcome"
    ### for log transforms
    afcars_model_data[[i]]$outcome<-afcars_model_data[[i]]$outcome + 0.000001
  }
###################### MISSINGS ON MERGE WITH SEER POP BECAUSE OF COURSE THERE ARE



#### fix mismatches later

# ### we lose some counties because of puma data
# table(cnty287g$FIPS%in%pop_puma$FIPS)
# unique(cnty287g[which(!(cnty287g$FIPS%in%pop_puma$FIPS)), "FIPS"])




################################################
####### models
################################################
###### create list of identical models for puma, afcars



puma_models_0<-lapply(puma_model_data,
                    function(x){
                      lm(log(outcome) ~ 
                           (year:c287_active) +
                           (race:c287_active) +
                           (year:race:c287_active) +
                           year * factor(fips) + 
                           factor(year) + 
                           factor(fips), 
                             data=x)
                    })

puma_models_1<-lapply(puma_model_data,
                      function(x){
                        lm(log(outcome) ~ race * (
                             factor(c287_everactive) + 
                             factor(c287time_minus34) + 
                             factor(c287time_minus12) + 
                             factor(c287time_0) + 
                             factor(c287time_plus12) + 
                             factor(c287time_plus34) + 
                             factor(c287time_plus56) + 
                             year * factor(fips) + # state slope
                             factor(year) + 
                               factor(fips)),  # state intercepts
                           data=x)
                      })

puma_models_2<-lapply(puma_model_data,
                      function(x){
                        lm(log(outcome) ~ race * (
                          factor(c287_everactive) + 
                            factor(c287time_minus34) + 
                            factor(c287time_minus12) + 
                            factor(c287time_0) + 
                            factor(c287time_plus12) + 
                            factor(c287time_plus34) + 
                            factor(c287time_plus56) + 
                            year * factor(fips) + # state slope
                            factor(year)),  # state intercepts
                          data=x%>%
                            filter(c287_everapplied == TRUE))
                      })



afcars_models_0<-lapply(afcars_model_data,
                        function(x){
                          lm(log(outcome) ~ 
                               (year:c287_active) +
                               (race:c287_active) +
                               (age_group:c287_active)+
                               (year:race:c287_active) +
                               year * factor(fips) + 
                               factor(year) +
                               factor(fips), 
                             data=x)
                        })

afcars_models_1<-lapply(afcars_model_data,
                      function(x){
                        lm(log(outcome) ~ race * ( age_group * (
                          factor(c287_everactive) + 
                            factor(c287time_minus34) + 
                            factor(c287time_minus12) + 
                            factor(c287time_0) + 
                            factor(c287time_plus12) + 
                            factor(c287time_plus34) + 
                            factor(c287time_plus56)) + 
                            year * factor(fips) + # state slope
                            factor(year) + # national year dummy
                            factor(fips)),  # state intercepts
                          data=x)
                      })

afcars_models_2<-lapply(afcars_model_data,
                        function(x){
                          lm(log(outcome) ~ race * ( age_group * (
                            factor(c287_everactive) + 
                              factor(c287time_minus34) + 
                              factor(c287time_minus12) + 
                              factor(c287time_0) + 
                              factor(c287time_plus12) + 
                              factor(c287time_plus34) + 
                              factor(c287time_plus56)) + 
                              year * factor(fips) + # state slope
                              factor(year) + # national year dummy
                              factor(fips)),  # state intercepts
                            data=x%>%
                              filter(c287_everapplied == TRUE))
                        })


####### visualize model results
## create new data with arbitrary fips
## to obtain average effect of 287g on each outcome
## obtain 287g effect for year, each race, for active, for everapplied
## start 287g in 2008
## for m0
for(i in 1:length(puma_models_0)){
  var<-puma_outcomes[i]
  year<-2005:2012
  race<-unique(puma_long$race)
  tf<-c("TRUE", "FALSE")
  fips<-4013
  state<-"4"
  
  newdat<-data.frame(expand.grid(fips, year, race, fips))
  names(newdat)<-c("fips", "year", "race", "state")
  #### make two scenarios, early 287 implementation
  #### no 287 implementation, difference preds, plot both
  newdatTRUE<-newdat%>%
    mutate(c287_active = ifelse(year <= 2008, FALSE, TRUE))%>%
    mutate(fips = factor(fips),
           c287_active = factor(c287_active))
  
  newdatFALSE<-newdat%>%
    mutate(c287_active = FALSE)%>%
    mutate(fips = factor(fips),
           c287_active = factor(c287_active))
  
  predTRUE<-exp(predict(puma_models_0[[i]], newdatTRUE))
  predFALSE<-exp(predict(puma_models_0[[i]], newdatFALSE))
  predDIFF<-predTRUE-predFALSE
  newdatDIFF<-newdatTRUE
  newdatDIFF$pred<-predDIFF
  
  newdatTRUE$pred<-predTRUE
  newdatTRUE$scen<-"Early 287(g)"
  newdatFALSE$pred<-predFALSE
  newdatFALSE$scen<-"No 287(g)"
  
  
  plot_dat<-rbind(newdatTRUE, newdatFALSE)
  
  filename<-paste("./images/puma_0_pred_fips", var, ".png", sep="")
  
  ggplot(plot_dat, 
         aes(x=year, y = pred, color=scen))+
    geom_line()+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(~race)+
    ylab(var)+
    ggtitle("predicted values for early 287g, no 287g")+
    ggsave(filename)
  
  filename<-paste("./images/puma_0_diff_fips", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=year, y = pred))+
    geom_line()+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(~race)+
    ylab(var)+
    ggtitle("difference from no 287g prediction with early implementation")+
    ggsave(filename)

}
######################################################
#### puma_models_1 by relative 287g implementation time

for(i in 1:length(puma_models_1)){
  var<-puma_outcomes[i]
  year<-2008
  race<-unique(puma_long$race)
  state<-"4"
  fips<-4013
  tf<-FALSE ### for all 7 dummies
  
  newdat<-data.frame(expand.grid(year, race, fips, 
                                 tf, tf, tf, tf, tf, tf, tf))
  names(newdat)<-c("year", "race", "state", "fips",
                   "c287_everactive", "c287time_minus34",
                   "c287time_minus12", "c287time_0", 
                   "c287time_plus12", "c287time_plus34", 
                   "c287time_plus56")
  
  time_index<-which(names(newdat)%in%c("c287time_minus34", "c287time_plus56"))
  #### create scenario at each time var (6) for each group
  newdat$c287_everactive<-TRUE
  newdat_template<-newdat
  for(j in time_index[1]:time_index[2]){
    newdat_temp<-newdat_template
    newdat_temp[, time_index[1]:time_index[2]]<-FALSE
    newdat_temp[, j]<-TRUE
    newdat<-rbind(newdat, newdat_temp)
  }
  
  newdat<-newdat[-c(1:3),]
  
  #### predict for each time group, for never implement
  
  predTRUE<-exp(predict(puma_models_1[[i]], newdat))
  newdat_false<-newdat_template%>%
    mutate(c287_everactive=FALSE)
  predFALSE<-exp(predict(puma_models_1[[i]], newdat_false))
  predFALSE<-rep(predFALSE, 6)
  predDIFF<-predTRUE-predFALSE
  
  newdatDIFF<-newdat
  newdatDIFF$pred<-predDIFF
  newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=3)
  
  filename<-paste("./images/all_event_fips", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=time, y = pred))+
    geom_point()+
    geom_hline(yintercept=0, lty=2)+
    geom_vline(xintercept=0, lty=2)+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(~race)+
    ylab(var)+
    ggtitle("change in outcome by time-to-287g active, all puma counties in model")+
    ggsave(filename)
  
}

######################################################
#### puma_models_1 by relative 287g implementation time

for(i in 1:length(puma_models_2)){
  var<-puma_outcomes[i]
  year<-2008
  race<-unique(puma_long$race)
  state<-"4"
  fips<-4013
  tf<-FALSE ### for all 7 dummies
  
  newdat<-data.frame(expand.grid(year, race, fips, 
                                 tf, tf, tf, tf, tf, tf, tf))
  names(newdat)<-c("year", "race", "state","fips",
                   "c287_everactive", "c287time_minus34",
                   "c287time_minus12", "c287time_0", 
                   "c287time_plus12", "c287time_plus34", 
                   "c287time_plus56")
  
  time_index<-which(names(newdat)%in%c("c287time_minus34", "c287time_plus56"))
  #### create scenario at each time var (6) for each group
  newdat$c287_everactive<-TRUE
  newdat_template<-newdat
  for(j in time_index[1]:time_index[2]){
    newdat_temp<-newdat_template
    newdat_temp[, time_index[1]:time_index[2]]<-FALSE
    newdat_temp[, j]<-TRUE
    newdat<-rbind(newdat, newdat_temp)
  }
  
  newdat<-newdat[-c(1:3),]
  
  #### predict for each time group, for never implement
  
  predTRUE<-exp(predict(puma_models_2[[i]], newdat))
  newdat_false<-newdat_template%>%
    mutate(c287_everactive=FALSE)
  predFALSE<-exp(predict(puma_models_2[[i]], newdat_false))
  predFALSE<-rep(predFALSE, 6)
  predDIFF<-predTRUE-predFALSE
  
  newdatDIFF<-newdat
  newdatDIFF$pred<-predDIFF
  newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=3)
  
  filename<-paste("./images/everapplied_event_fips", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=time, y = pred))+
    geom_point()+
    geom_hline(yintercept=0, lty=2)+
    geom_vline(xintercept=0, lty=2)+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(~race)+
    ylab(var)+
    ggtitle("change in outcome by time-to-287g active, everapplied counties")+
    ggsave(filename)
  
}


for(i in 1:length(afcars_models_0)){
  var<-afcars_outcomes[i]
  year<-2005:2012
  race<-unique(afcars$race)
  tf<-c("TRUE", "FALSE")
  state<-"1"
  age_group<-unique(afcars$age_group)
  
  newdat<-data.frame(expand.grid(year, race, fips, age_group))
  names(newdat)<-c("year", "race", "fips", "age_group")
  #### make two scenarios, early 287 implementation
  #### no 287 implementation, difference preds, plot both
  newdatTRUE<-newdat%>%
    mutate(c287_active = ifelse(year <= 2008, FALSE, TRUE))%>%
    mutate(fips = factor(fips))
  
  newdatFALSE<-newdat%>%
    mutate(c287_active = FALSE)%>%
    mutate(fips = factor(fips))
  
  predTRUE<-exp(predict(afcars_models_0[[i]], newdatTRUE))
  predFALSE<-exp(predict(afcars_models_0[[i]], newdatFALSE))
  predDIFF<-predTRUE-predFALSE
  newdatDIFF<-newdatTRUE
  newdatDIFF$pred<-predDIFF
  
  newdatTRUE$pred<-predTRUE
  newdatTRUE$scen<-"Early 287(g)"
  newdatFALSE$pred<-predFALSE
  newdatFALSE$scen<-"No 287(g)"
  
  
  plot_dat<-rbind(newdatTRUE, newdatFALSE)
  
  filename<-paste("./images/afcars_0_pred_fips", var, ".png", sep="")
  
  ggplot(plot_dat, 
         aes(x=year, y = pred, color=scen))+
    geom_line()+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(age_group~race)+
    ylab(var)+
    ggtitle("predicted values for early 287g, no 287g")+
    ggsave(filename)
  
  filename<-paste("./images/afcars_0_diff_fips", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=year, y = pred))+
    geom_line()+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(age_group~race)+
    ylab(var)+
    ggtitle("difference from no 287g prediction with early implementation")+
    ggsave(filename)
  
}

for(i in 1:length(afcars_models_1)){
  var<-afcars_outcomes[i]
  year<-2008
  race<-unique(afcars$race)
  state<-"1"
  age_group<-unique(afcars$age_group)
  tf<-FALSE ### for all 7 dummies
  
  newdat<-data.frame(expand.grid(year, race, fips, age_group,
                                 tf, tf, tf, tf, tf, tf, tf))
  names(newdat)<-c("year", "race", "fips","age_group",
                   "c287_everactive", "c287time_minus34",
                   "c287time_minus12", "c287time_0", 
                   "c287time_plus12", "c287time_plus34", 
                   "c287time_plus56")
  
  time_index<-which(names(newdat)%in%c("c287time_minus34", "c287time_plus56"))
  #### create scenario at each time var (6) for each group
  newdat$c287_everactive<-TRUE
  newdat_template<-newdat
  for(j in time_index[1]:time_index[2]){
    newdat_temp<-newdat_template
    newdat_temp[, time_index[1]:time_index[2]]<-FALSE
    newdat_temp[, j]<-TRUE
    newdat<-rbind(newdat, newdat_temp)
  }
  
  newdat<-newdat[-c(1:9),]
  
  #### predict for each time group, for never implement
  
  predTRUE<-exp(predict(afcars_models_1[[i]], newdat))
  newdat_false<-newdat_template%>%
    mutate(c287_everactive=FALSE)
  predFALSE<-exp(predict(afcars_models_1[[i]], newdat_false))
  predFALSE<-rep(predFALSE, 6)
  predDIFF<-predTRUE-predFALSE
  
  newdatDIFF<-newdat
  newdatDIFF$pred<-predDIFF
  newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=9)
  
  filename<-paste("./images/all_event_fips", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=time, y = pred))+
    geom_point()+
    geom_hline(yintercept=0, lty=2)+
    geom_vline(xintercept=0, lty=2)+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(age_group~race)+
    ylab(var)+
    ggtitle("change in outcome by time-to-287g active, all afcars counties in model")+
    ggsave(filename)
  
}

for(i in 1:length(afcars_models_2)){
  var<-afcars_outcomes[i]
  year<-2008
  race<-unique(afcars$race)
  state<-"1"
  age_group<-unique(afcars$age_group)
  tf<-FALSE ### for all 7 dummies
  
  newdat<-data.frame(expand.grid(year, race, fips, age_group,
                                 tf, tf, tf, tf, tf, tf, tf))
  names(newdat)<-c("year", "race", "fips","age_group",
                   "c287_everactive", "c287time_minus34",
                   "c287time_minus12", "c287time_0", 
                   "c287time_plus12", "c287time_plus34", 
                   "c287time_plus56")
  
  time_index<-which(names(newdat)%in%c("c287time_minus34", "c287time_plus56"))
  #### create scenario at each time var (6) for each group
  newdat$c287_everactive<-TRUE
  newdat_template<-newdat
  for(j in time_index[1]:time_index[2]){
    newdat_temp<-newdat_template
    newdat_temp[, time_index[1]:time_index[2]]<-FALSE
    newdat_temp[, j]<-TRUE
    newdat<-rbind(newdat, newdat_temp)
  }
  
  newdat<-newdat[-c(1:9),]
  
  #### predict for each time group, for never implement
  
  predTRUE<-exp(predict(afcars_models_2[[i]], newdat))
  newdat_false<-newdat_template%>%
    mutate(c287_everactive=FALSE)
  predFALSE<-exp(predict(afcars_models_2[[i]], newdat_false))
  predFALSE<-rep(predFALSE, 6)
  predDIFF<-predTRUE-predFALSE
  
  newdatDIFF<-newdat
  newdatDIFF$pred<-predDIFF
  newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=9)
  
  filename<-paste("./images/everapplied_event_fips", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=time, y = pred))+
    geom_point()+
    geom_hline(yintercept=0, lty=2)+
    geom_vline(xintercept=0, lty=2)+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(age_group~race)+
    ylab(var)+
    ggtitle("change in outcome by time-to-287g active, everapplied counties")+
    ggsave(filename)
  
}