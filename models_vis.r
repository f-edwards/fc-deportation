

######################################################
#### puma_models_1 by relative 287g implementation time

for(i in 1:length(puma_models_1)){
  var<-puma_outcomes[i]
  year<-2008
  race<-unique(puma_long$race)
  fips<-4013
  state<-"4"
  tf<-FALSE ### for all 7 dummies
  
  newdat<-data.frame(expand.grid(year, race, state, fips,
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
  
  filename<-paste("./images/all_event_FE_", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=time, y = pred))+
    geom_point()+
    geom_hline(yintercept=0, lty=2)+
    geom_vline(xintercept=0, lty=2)+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(~race)+
    ylab(var)+
    ggtitle("all puma counties, fips + year FE")+
    ggsave(filename)
  
}

######################################################
#### puma_models_1 by relative 287g implementation time

for(i in 1:length(puma_models_2)){
  var<-puma_outcomes[i]
  year<-2008
  race<-unique(puma_long$race)
  fips<-4013
  state<-"4"
  tf<-FALSE ### for all 7 dummies
  
  newdat<-data.frame(expand.grid(year, race, state, fips,
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
  
  predTRUE<-exp(predict(puma_models_2[[i]], newdat))
  newdat_false<-newdat_template%>%
    mutate(c287_everactive=FALSE)
  predFALSE<-exp(predict(puma_models_2[[i]], newdat_false))
  predFALSE<-rep(predFALSE, 6)
  predDIFF<-predTRUE-predFALSE
  
  newdatDIFF<-newdat
  newdatDIFF$pred<-predDIFF
  newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=3)
  
  filename<-paste("./images/all_event_slopes_", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=time, y = pred))+
    geom_point()+
    geom_hline(yintercept=0, lty=2)+
    geom_vline(xintercept=0, lty=2)+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(~race)+
    ylab(var)+
    ggtitle("all puma counties, fips + year FE, within-fips slope")+
    ggsave(filename)
  
}

for(i in 1:length(puma_models_3)){
  var<-puma_outcomes[i]
  year<-2008
  race<-unique(puma_long$race)
  state<-"4"
  tf<-FALSE ### for all 7 dummies
  
  newdat<-data.frame(expand.grid(year, race, state, fips,
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
  
  predTRUE<-exp(predict(puma_models_2[[i]], newdat))
  newdat_false<-newdat_template%>%
    mutate(c287_everactive=FALSE)
  predFALSE<-exp(predict(puma_models_2[[i]], newdat_false))
  predFALSE<-rep(predFALSE, 6)
  predDIFF<-predTRUE-predFALSE
  
  newdatDIFF<-newdat
  newdatDIFF$pred<-predDIFF
  newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=3)
  
  filename<-paste("./images/everapplied_event_slopes_", var, ".png", sep="")
  
  ggplot(newdatDIFF,
         aes(x=time, y = pred))+
    geom_point()+
    geom_hline(yintercept=0, lty=2)+
    geom_vline(xintercept=0, lty=2)+
    #geom_ribbon(alpha = 0.6) + 
    facet_wrap(~race)+
    ylab(var)+
    ggtitle("everapplied counties, fips + year FE, within-fips slope")+
    ggsave(filename)
  
}

# afcars_models_0<-lapply(afcars_model_data,
#                         function(x){
#                           lm(log(outcome) ~ 
#                                (year:c287_active) +
#                                (race:c287_active) +
#                                (age_group:c287_active)+
#                                (year:race:c287_active) +
#                                year * factor(state) + 
#                                factor(year) +
#                                factor(state), 
#                              data=x)
#                         })
# 
# afcars_models_1<-lapply(afcars_model_data,
#                       function(x){
#                         lm(log(outcome) ~ race * ( age_group * (
#                           factor(c287_everactive) + 
#                             factor(c287time_minus34) + 
#                             factor(c287time_minus12) + 
#                             factor(c287time_0) + 
#                             factor(c287time_plus12) + 
#                             factor(c287time_plus34) + 
#                             factor(c287time_plus56)) + 
#                             year * factor(state) + # state slope
#                             factor(year) + # national year dummy
#                             factor(state)),  # state intercepts
#                           data=x)
#                       })
# 
# afcars_models_2<-lapply(afcars_model_data,
#                         function(x){
#                           lm(log(outcome) ~ race * ( age_group * (
#                             factor(c287_everactive) + 
#                               factor(c287time_minus34) + 
#                               factor(c287time_minus12) + 
#                               factor(c287time_0) + 
#                               factor(c287time_plus12) + 
#                               factor(c287time_plus34) + 
#                               factor(c287time_plus56)) + 
#                               year * factor(state) + # state slope
#                               factor(year) + # national year dummy
#                               factor(state)),  # state intercepts
#                             data=x%>%
#                               filter(c287_everapplied == TRUE))
#                         })


####### visualize model results
## create new data with arbitrary fips
## to obtain average effect of 287g on each outcome
## obtain 287g effect for year, each race, for active, for everapplied
## start 287g in 2008
## for m0
# for(i in 1:length(puma_models_0)){
#   var<-puma_outcomes[i]
#   year<-2005:2012
#   race<-unique(puma_long$race)
#   tf<-c("TRUE", "FALSE")
#   fips<-4013
#   state<-"4"
#   
#   newdat<-data.frame(expand.grid(fips, year, race, state))
#   names(newdat)<-c("fips", "year", "race", "state")
#   #### make two scenarios, early 287 implementation
#   #### no 287 implementation, difference preds, plot both
#   newdatTRUE<-newdat%>%
#     mutate(c287_active = ifelse(year <= 2008, FALSE, TRUE))%>%
#     mutate(fips = factor(fips),
#            c287_active = factor(c287_active))
#   
#   newdatFALSE<-newdat%>%
#     mutate(c287_active = FALSE)%>%
#     mutate(fips = factor(fips),
#            c287_active = factor(c287_active))
#   
#   predTRUE<-exp(predict(puma_models_0[[i]], newdatTRUE))
#   predFALSE<-exp(predict(puma_models_0[[i]], newdatFALSE))
#   predDIFF<-predTRUE-predFALSE
#   newdatDIFF<-newdatTRUE
#   newdatDIFF$pred<-predDIFF
#   
#   newdatTRUE$pred<-predTRUE
#   newdatTRUE$scen<-"Early 287(g)"
#   newdatFALSE$pred<-predFALSE
#   newdatFALSE$scen<-"No 287(g)"
#   
#   
#   plot_dat<-rbind(newdatTRUE, newdatFALSE)
#   
#   # filename<-paste("./images/puma_0_pred_", var, ".png", sep="")
#   # 
#   # ggplot(plot_dat, 
#   #        aes(x=year, y = pred, color=scen))+
#   #   geom_line()+
#   #   #geom_ribbon(alpha = 0.6) + 
#   #   facet_wrap(~race)+
#   #   ylab(var)+
#   #   ggtitle("predicted values for early 287g, no 287g")+
#   #   ggsave(filename)
#   
#   filename<-paste("./images/puma_0_diff_", var, ".png", sep="")
#   
#   ggplot(newdatDIFF,
#          aes(x=year, y = pred))+
#     geom_line()+
#     #geom_ribbon(alpha = 0.6) + 
#     facet_wrap(~race)+
#     ylab(var)+
#     ggtitle("difference from no 287g prediction with early implementation")+
#     ggsave(filename)
# 
# }

# 
# for(i in 1:length(afcars_models_0)){
#   var<-afcars_outcomes[i]
#   year<-2005:2012
#   race<-unique(afcars$race)
#   tf<-c("TRUE", "FALSE")
#   state<-"1"
#   age_group<-unique(afcars$age_group)
#   
#   newdat<-data.frame(expand.grid(year, race, state, age_group))
#   names(newdat)<-c("year", "race", "state", "age_group")
#   #### make two scenarios, early 287 implementation
#   #### no 287 implementation, difference preds, plot both
#   newdatTRUE<-newdat%>%
#     mutate(c287_active = ifelse(year <= 2008, FALSE, TRUE))%>%
#     mutate(fips = factor(fips))
#   
#   newdatFALSE<-newdat%>%
#     mutate(c287_active = FALSE)%>%
#     mutate(fips = factor(fips))
#   
#   predTRUE<-exp(predict(afcars_models_0[[i]], newdatTRUE))
#   predFALSE<-exp(predict(afcars_models_0[[i]], newdatFALSE))
#   predDIFF<-predTRUE-predFALSE
#   newdatDIFF<-newdatTRUE
#   newdatDIFF$pred<-predDIFF
#   
#   newdatTRUE$pred<-predTRUE
#   newdatTRUE$scen<-"Early 287(g)"
#   newdatFALSE$pred<-predFALSE
#   newdatFALSE$scen<-"No 287(g)"
#   
#   
#   plot_dat<-rbind(newdatTRUE, newdatFALSE)
#   
#   filename<-paste("./images/afcars_0_pred_", var, ".png", sep="")
#   
#   ggplot(plot_dat, 
#          aes(x=year, y = pred, color=scen))+
#     geom_line()+
#     #geom_ribbon(alpha = 0.6) + 
#     facet_wrap(age_group~race)+
#     ylab(var)+
#     ggtitle("predicted values for early 287g, no 287g")+
#     ggsave(filename)
#   
#   filename<-paste("./images/afcars_0_diff_", var, ".png", sep="")
#   
#   ggplot(newdatDIFF,
#          aes(x=year, y = pred))+
#     geom_line()+
#     #geom_ribbon(alpha = 0.6) + 
#     facet_wrap(age_group~race)+
#     ylab(var)+
#     ggtitle("difference from no 287g prediction with early implementation")+
#     ggsave(filename)
#   
# }
# 
# for(i in 1:length(afcars_models_1)){
#   var<-afcars_outcomes[i]
#   year<-2008
#   race<-unique(afcars$race)
#   state<-"1"
#   age_group<-unique(afcars$age_group)
#   tf<-FALSE ### for all 7 dummies
#   
#   newdat<-data.frame(expand.grid(year, race, state, age_group,
#                                  tf, tf, tf, tf, tf, tf, tf))
#   names(newdat)<-c("year", "race", "state","age_group",
#                    "c287_everactive", "c287time_minus34",
#                    "c287time_minus12", "c287time_0", 
#                    "c287time_plus12", "c287time_plus34", 
#                    "c287time_plus56")
#   
#   time_index<-which(names(newdat)%in%c("c287time_minus34", "c287time_plus56"))
#   #### create scenario at each time var (6) for each group
#   newdat$c287_everactive<-TRUE
#   newdat_template<-newdat
#   for(j in time_index[1]:time_index[2]){
#     newdat_temp<-newdat_template
#     newdat_temp[, time_index[1]:time_index[2]]<-FALSE
#     newdat_temp[, j]<-TRUE
#     newdat<-rbind(newdat, newdat_temp)
#   }
#   
#   newdat<-newdat[-c(1:9),]
#   
#   #### predict for each time group, for never implement
#   
#   predTRUE<-exp(predict(afcars_models_1[[i]], newdat))
#   newdat_false<-newdat_template%>%
#     mutate(c287_everactive=FALSE)
#   predFALSE<-exp(predict(afcars_models_1[[i]], newdat_false))
#   predFALSE<-rep(predFALSE, 6)
#   predDIFF<-predTRUE-predFALSE
#   
#   newdatDIFF<-newdat
#   newdatDIFF$pred<-predDIFF
#   newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=9)
#   
#   filename<-paste("./images/all_event_", var, ".png", sep="")
#   
#   ggplot(newdatDIFF,
#          aes(x=time, y = pred))+
#     geom_point()+
#     geom_hline(yintercept=0, lty=2)+
#     geom_vline(xintercept=0, lty=2)+
#     #geom_ribbon(alpha = 0.6) + 
#     facet_wrap(age_group~race)+
#     ylab(var)+
#     ggtitle("change in outcome by time-to-287g active, all afcars counties in model")+
#     ggsave(filename)
#   
# }
# 
# for(i in 1:length(afcars_models_2)){
#   var<-afcars_outcomes[i]
#   year<-2008
#   race<-unique(afcars$race)
#   state<-"1"
#   age_group<-unique(afcars$age_group)
#   tf<-FALSE ### for all 7 dummies
#   
#   newdat<-data.frame(expand.grid(year, race, state, age_group,
#                                  tf, tf, tf, tf, tf, tf, tf))
#   names(newdat)<-c("year", "race", "state","age_group",
#                    "c287_everactive", "c287time_minus34",
#                    "c287time_minus12", "c287time_0", 
#                    "c287time_plus12", "c287time_plus34", 
#                    "c287time_plus56")
#   
#   time_index<-which(names(newdat)%in%c("c287time_minus34", "c287time_plus56"))
#   #### create scenario at each time var (6) for each group
#   newdat$c287_everactive<-TRUE
#   newdat_template<-newdat
#   for(j in time_index[1]:time_index[2]){
#     newdat_temp<-newdat_template
#     newdat_temp[, time_index[1]:time_index[2]]<-FALSE
#     newdat_temp[, j]<-TRUE
#     newdat<-rbind(newdat, newdat_temp)
#   }
#   
#   newdat<-newdat[-c(1:9),]
#   
#   #### predict for each time group, for never implement
#   
#   predTRUE<-exp(predict(afcars_models_2[[i]], newdat))
#   newdat_false<-newdat_template%>%
#     mutate(c287_everactive=FALSE)
#   predFALSE<-exp(predict(afcars_models_2[[i]], newdat_false))
#   predFALSE<-rep(predFALSE, 6)
#   predDIFF<-predTRUE-predFALSE
#   
#   newdatDIFF<-newdat
#   newdatDIFF$pred<-predDIFF
#   newdatDIFF$time<-rep(c(-4, -2, 0, 2, 4, 6), each=9)
#   
#   filename<-paste("./images/everapplied_event_", var, ".png", sep="")
#   
#   ggplot(newdatDIFF,
#          aes(x=time, y = pred))+
#     geom_point()+
#     geom_hline(yintercept=0, lty=2)+
#     geom_vline(xintercept=0, lty=2)+
#     #geom_ribbon(alpha = 0.6) + 
#     facet_wrap(age_group~race)+
#     ylab(var)+
#     ggtitle("change in outcome by time-to-287g active, everapplied counties")+
#     ggsave(filename)
#   
# }