rm(list=ls())
gc()
library(tidyverse)
library(haven)
library(maps)
source("functions.r")
setwd("./data")
data(state.fips)
state.fips<-state.fips%>%
  rename(STATE=fips,
         stname=abb)%>%
  mutate(stname = as.character(stname))%>%
  select(STATE, stname)
state.fips<-rbind(state.fips,
                  c(2, "AK"), c(15, "HI"))%>%
  distinct()
state.fips$STATE<-as.integer(state.fips$STATE)



########################################################################
###### READ AFCARS IMPUTATION DATA 
########################################################################
# AFCARS<-read_csv("afcars_imputed_state_fe.csv")
# 
# ##################################################################
# ###### FORMAT IMPUTATED AFCARS DATA FOR EDA
# ##################################################################
# cnty_entries<-AFCARS%>%
#   group_by(.imp, STATE, FIPSCODE, FY, HISORGIN)%>%
#   summarise(entries_first = as.integer(sum(first_entry, na.rm=TRUE)),
#             entries_first_nonab = as.integer(sum(first_entry * (abuse==0),na.rm=TRUE)),
#             cl = n(),
#             cl_not_abuse = as.integer(sum(abuse==0, na.rm=TRUE)),
#             reun_exit = as.integer(sum(reun_exit, na.rm=TRUE)))%>%
#   ungroup()
# write_csv(cnty_entries, "afcars_imputed_county_sums.csv")
afcars<-read_csv("afcars_imputed_county_sums.csv")
##############################################################################
###### READ/TRANSFORM NCANDS IMPUTED DATA
##############################################################################
files<-list.files()
files_ncands<-files[grep("ncands_cnty", files)]

# for(i in 1:length(files_ncands)){
#   filename<-files_ncands[i]
#   names<-read_csv(filename, n_max = 1)
#   if("report_source_NA"%in%names(names)){
#     temp<-read_csv(filename,
#                    col_types = "dddccdddd")
#     temp<-temp%>%
#       select(-report_source_NA)
#   }else{
#     temp<-read_csv(filename,
#                    col_types = "dddccddd")
#   }
#   
#   write.csv(temp, 
#             file=paste("./fixed/",filename,sep=""), 
#             row.names=FALSE)
# }

ncands<-read_csv(files_ncands[1])
# if("report_source_NA"%in%names(ncands)){
#   ncands<-ncands%>%
#     select(-report_source_NA)
# }
for(i in 2:length(files_ncands)){
  ncands_temp<-read_csv(files_ncands[i])
  # if("report_source_NA"%in%names(ncands_temp)){
  #   ncands_temp<-ncands_temp%>%
  #     select(-report_source_NA)
  # }
  ncands<-rbind(ncands, ncands_temp)
}

ncands<-ncands%>%
  rename(stname=state)%>%
  left_join(state.fips)
########################################################################
###### READ/TRANSFORM S-COMM DATA
########################################################################
scomm<-read_csv("./countySComm.csv")
scomm<-scomm%>%
  rename(stname=State,
         countyname=Area_Name)%>%
  mutate(FIPS=as.integer(FIPS))

#### need to cut based on month, right? going to round to next year if month>=10
#### rounds on 3 months, seems conservative
scomm<-scomm%>%
  mutate(scomm_yr_round = ifelse(Scomm_mo>=10, 
                                 Scomm_yr + 1, 
                                 Scomm_yr))
##############################################################################
###### READ/TRANSFORM 287(G) DATA
##############################################################################
cnty287g<-read_dta("./county287g.dta")
cnty287g<-cnty287g%>%
  rename(FIPS=countyid)%>%
  mutate(FIPS=as.numeric(FIPS),
         year=as.numeric(year),
         c287active = as.logical(c287active),
         c287deny = as.logical(c287deny),
         c287deny_outcome = as.character(c287deny_outcome),
         c287deny_yr = as.numeric(c287deny_yr),
         c287start_yr = as.numeric(c287start_yr),
         c287_identify = as.numeric(c287_identify))%>%
  mutate(c287_ever_applied = TRUE)%>%
  select(-countyname)
##############################################################################
###### READ/TRANSFORM SEER CHILD POPULATION DATA
##############################################################################
### use AFCARS for state fips crosswalk w/county fips
pop<-read_csv("./seer-child-pop-2000-2016.csv")
pop<-pop%>%
  rename(stname=state)%>%
  left_join(state.fips)%>%
  filter(STATE!=72)
##############################################################################
###### CHECK ON MATCHES BY FIPS for all data, RECODE to match SEER pop
###############################################################################
# # https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
# following suggestiuons on recoding here
# https://seer.cancer.gov/seerstat/variables/countyattribs/ruralurban.html
##########################
## AFCARS
##########################
# afcars_fipsyr<-unique(paste(afcars$FIPS, afcars$year))
# pop_fipsyr<-unique(paste(pop$FIPS, pop$year))
# z<-afcars_fipsyr[which(!(afcars_fipsyr%in%pop_fipsyr))]
afcars<-afcars%>%
  rename(FIPS=FIPSCODE,
         year=FY)
afcars<-recode_FIPS(afcars)
##########################
## NCANDS
##########################
# ncands_fipsyr<-unique(paste(ncands$FIPS, ncands$year))
# pop_fipsyr<-unique(paste(pop$FIPS, pop$year))
# z<-ncands_fipsyr[which(!(ncands_fipsyr%in%pop_fipsyr))]
### just 999s and nonexistent LA codes ;eft
### FIPS ALREADY RECODED
ncands<-recode_FIPS(ncands)
###########################
## SCOMM, 287g
###########################
# #z<-scomm[which(!(scomm$FIPS)%in%pop$FIPS), "FIPS"]
scomm<-recode_FIPS(scomm%>%mutate(year=0))%>%select(-year)
# cnty287g_fipsyr<-unique(paste(cnty287g$FIPS, cnty287g$year))
# z<-cnty287g_fipsyr[which(!(cnty287g_fipsyr%in%pop_fipsyr))]
cnty287g<-recode_FIPS(cnty287g)

##########################################################
## JOIN on POP, rep for .imp, then create indicators for reporting state AFCARS/NCANDS
##########################################################
#### generate reported indicators for afcars / ncands
reported_a<-afcars%>%
              select(STATE, year)%>%
              mutate(reported_afcars=TRUE)%>%
              distinct()

reported_n<-ncands%>%
  select(STATE, year)%>%
  mutate(reported_ncands=TRUE)%>%
  distinct()

reported<-full_join(reported_a, reported_n)%>%
  complete(STATE, year)%>%
  mutate(reported_afcars=ifelse(is.na(reported_afcars),
                                F,
                                T),
         reported_ncands=ifelse(is.na(reported_ncands),
                                F,
                                T))
# add in entries for the 999s in pop
fails<-which(!(ncands$FIPS%in%pop$FIPS))
adds<-ncands[fails,]

### create dummy data frame for pop
dummy<-adds%>%
  filter(.imp==1)%>%
  mutate(child_pop=NA,
         adult_pop=NA)%>%
  select(-report_source_community, - report_source_law_enf, - report_source_other,
         -.imp)

pop<-bind_rows(pop, dummy)

pop<-pop%>%
  left_join(reported)
##### repeat pop for each .imp, so I can right join on pop for counties with no cases
pop<-pop%>%
  mutate(.imp=0)%>%
  bind_rows(pop%>%
              mutate(.imp=1))%>%
  bind_rows(pop%>%
              mutate(.imp=2))%>%
  bind_rows(pop%>%
              mutate(.imp=3))%>%
  bind_rows(pop%>%
              mutate(.imp=4))%>%
  bind_rows(pop%>%
              mutate(.imp=5))
#full join on all counties 2000 - 2016
dat_out<-pop%>%
  left_join(afcars)%>%
  left_join(ncands)
# transform false NA's in imputed data
dat_out<-dat_out%>%
  mutate(entries_first = ifelse((.imp>0)&
                  (reported_afcars==TRUE)&
                  (is.na(entries_first)),
                  0, entries_first),
         entries_first_nonab = ifelse((.imp>0)&
                                        (reported_afcars==TRUE)&
                                        (is.na(entries_first_nonab)),
                                      0, entries_first_nonab),
         cl = ifelse((.imp>0)&
                       (reported_afcars==TRUE)&
                       (is.na(cl)),
                     0, cl),
         cl_not_abuse = ifelse((.imp>0)&
                       (reported_afcars==TRUE)&
                       (is.na(cl_not_abuse)),
                     0, cl_not_abuse),
         reun_exit = ifelse((.imp>0)&
                        (reported_afcars==TRUE)&
                        (is.na(reun_exit)),
                      0, reun_exit),
         report_source_community = ifelse((.imp>0)&
                       (reported_ncands==TRUE)&
                       (is.na(report_source_community)),
                     0, report_source_community),
         report_source_law_enf = ifelse((.imp>0)&
                       (reported_ncands==TRUE)&
                       (is.na(report_source_law_enf)),
                     0, report_source_law_enf),
         report_source_other = ifelse((.imp>0)&
                       (reported_ncands==TRUE)&
                       (is.na(report_source_other)),
                     0, report_source_other))
              
dat_out<-dat_out%>%
  left_join(scomm)%>%
  left_join(cnty287g%>%
              select(-c287_ever_applied))%>%
  left_join(cnty287g%>%
              select(FIPS, c287_ever_applied)%>%
              distinct())%>%
  mutate(c287_ever_applied = ifelse(is.na(c287_ever_applied),
                                          FALSE,
                                          c287_ever_applied))%>%
  select(.imp, year, STATE, stname, FIPS, HISORGIN, everything())

write_csv(dat_out, "merged_data.csv")