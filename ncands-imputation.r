#.rs.restartR()
rm(list=ls())
gc()

setwd("Q:/NDACAN/NDACAN-RA/fre9/fc-deportation")

library(tidyverse)
library(mice)

set.seed(1)

ncands<-read_csv("./data/ncands-rptsrc-individual.csv", na="NA")

ncands<-ncands%>%
  filter(!(FIPS%in%c(72005, 72013, 72021, 72025, 
                     72031, 72057, 72069, 72097, 72113, 72127, 72999)))

recode_FIPS<-function(x){
  #### THESE ARE FIPS-YEARS IN AFCARS NOT MATCHED IN SEER POP
  ### weld county CO: FIPS 8123, enters pop data in 2002, 08914 prior
  x$FIPS<-ifelse((x$FIPS==8123)&(x$year<2002), 
                 8914, x$FIPS)
  ### Boulder county CO: FIPS 8013, enters pop data in 2002, 08912 prior
  x$FIPS<-ifelse((x$FIPS==8013)&(x$year<2002), 
                 8912, x$FIPS)
  ### Adams county CO: FIPS 8001, enters pop data in 2002, 8911 prior
  x$FIPS<-ifelse((x$FIPS==8001)&(x$year<2002), 
                 8911, x$FIPS)
  ### Jefferson county CO: FIPS 8059, enteres pop data in 2002, 8913 prior
  x$FIPS<-ifelse((x$FIPS==8059)&(x$year<2002), 
                 8913, x$FIPS)
  ### Bedford city, bedford county, VA: FIPS 51515, 51019, mapped to 51917
  x$FIPS<-ifelse((x$FIPS==51515)|(x$FIPS==51019), 
                 51917, x$FIPS)
  ### Clifton Forge, VA: FIPS 51560 not included in SEER, mapped to 51005
  x$FIPS<-ifelse(x$FIPS==51560, 
                 51005, x$FIPS)
  ### Oglala Lakota County, SD: FIPS 46102, not remapped in SEER, mapped to 46113
  x$FIPS<-ifelse(x$FIPS==46102, 
                 46113, x$FIPS)
  
  ### Wrangell Petersburg Census Area, AK: FIPS 02280, mapped in SEER after 2013 to 2275
  x$FIPS<-ifelse((x$FIPS==2280)&(x$year>2013), 
                 2275, x$FIPS)
  ### Prince of Wales Census Area, AK: FIPS 02201, mapped in SEER after 2013 to 2130
  x$FIPS<-ifelse((x$FIPS==2201)&(x$year>2013), 
                 2130, x$FIPS)
  ### Kusilvak census area, AK: FIPS 2158, not mapped in SEER, recode to 2270
  x$FIPS<-ifelse(x$FIPS==2158, 
                 2270, x$FIPS)
  return(x)
}

ncands<-recode_FIPS(ncands)

ncands$ethnicity[which(ncands$ethnicity=="unable")]<-NA
gc()

pop<-read_csv("./data/seer-child-pop-2000-2016.csv")
pop<-pop%>%
  select(-adult_pop)%>%
  spread(HISORGIN, child_pop)%>%
  rename(child_hispanic=hispanic, child_nonhispanic=`non-hispanic`)%>%
  mutate(child_hispanic = log(child_hispanic),
         child_nonhispanic = log(child_nonhispanic))

ncands<-ncands%>%
  rename(year = subyr,
         FIPS = RptFIPS,
         HISORGIN = ethnicity)%>%
  mutate(FIPS=as.numeric(FIPS))

### multilevel imputation using mice
### https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html
# fips_samp<-sample(sample(unique(ncands$FIPS)), 100, replace=F)
# 
# samp<-ncands%>%
#   filter(FIPS%in%fips_samp)
# 
# samp<-samp[sample(1:nrow(samp), 100000, replace=F), ]
# samp<-samp%>%
#   mutate(FIPS=as.numeric(FIPS))

merge<-left_join(ncands, pop)
merge<-merge%>%
  mutate(report_source = factor(report_source),
         HISORGIN = factor(HISORGIN))
########################## DEAL WITH FAILED MATCHES, SUB STATE AVG COUNTY POP FOR 999s
z<-which(is.na(merge$state))
failed_full<-merge[z,]
failed<-unique(merge[z, "FIPS"])
failed$state_fips<-ifelse(nchar(failed$FIPS)==4, substr(failed$FIPS, 1, 1),
                     substr(failed$FIPS, 1, 2))
library(maps)
data(state.fips)
state.fips<-state.fips%>%
  rename(state_fips=fips)%>%
  select(state_fips, abb)%>%
  mutate(state_fips=as.character(state_fips))%>%
  distinct()
state.fips<-rbind(state.fips, c(2, "AK"))
state.fips<-rbind(state.fips, c(15, "HI"))

failed<-left_join(failed, state.fips)
failed<-failed%>%
  select(FIPS, abb)%>%
  rename(state=abb)

corrected<-merge%>%
  filter(FIPS%in%failed$FIPS)%>%
  select(-state)%>%
  left_join(failed)%>%### add in state-level county average log child pop data as way to improve imputations for NCANDS missing counties
  select(-child_hispanic, -child_nonhispanic)%>%
  left_join(
    pop%>%
      group_by(state, year)%>%
      summarise(child_hispanic=mean(child_hispanic, na.rm=TRUE),
                child_nonhispanic = mean(child_nonhispanic, na.rm=TRUE)))

merge<-merge%>%
  filter(!(FIPS%in%failed$FIPS))%>%
  bind_rows(corrected)%>%
  mutate(child_hispanic=ifelse(is.na(child_hispanic)|(child_hispanic==0), 
                               1, child_hispanic))
  

states<-unique(merge$state)
#states<-states[-which(is.na(states))]
years<-unique(merge$year)
gc()

#### make imputations and output

gc()

cnty<-list()

for(i in 8:length(states)){
  #for(j in 1:length(years)){
    print(states[[i]])
    #print(years[[j]])
    dat_temp<-merge%>%filter(state == states[i] 
                             #year == years [j]
                             )
    ini<-mice(dat_temp, m=1, maxit = 0)
    pred<-ini$pred
    pred[, "FIPS"]<-0
    # pred["child_hispanic", ]<-0
    # pred["child_nonhispanic", ]<-0
    imp_out<-mice(dat_temp, predictorMatrix = pred)
    dat_temp<-mice::complete(imp_out, "long", 
                             include=TRUE)
    cnty[[i]]<-dat_temp%>%
                  group_by(.imp, FIPS, year, HISORGIN, report_source, state)%>%
                  summarise(count=n())%>%
                  spread(report_source, count, fill=0, sep="_")%>%
                  ungroup()
    
    if("report_source_NA"%in%names(cnty)){      
    cnty[[i]]<-cnty[[i]]%>%
      dplyr::select(-report_source_NA)}
    
    write_csv(cnty[[i]], paste("./data/ncands_cnty_imp", i, ".csv", sep=""))
    
    gc()   
  #}
}



save.image("mice-ncands.Rdata")
# AFCARS<-AFCARS.imp$imputations[[1]]
# rm(AFCARS.imp)
gc()
# 
# ### complete imputation data
# imp_dat_out<-mice::complete(imp_out[[1]], "long")
# 
# for(j in 1:length(imp_out)){
#   imp_dat_out<-rbind(imp_dat_out,
#                      mice::complete(imp_out[[j]], "long"))
# }
# 
# imp_dat_out<-imp_dat_out%>%
#   arrange(.imp)
# 
# 
# # write data
# out<-rbind(ncands%>%
#              mutate(.imp = "0"),
#            imp_dat_out%>%
#              select(-.id))
# 
# write_csv(out, "ncands_imputed_state_fe.csv")

q(save="no")
