rm(list=ls())
gc()
library(tidyverse)
library(haven)
library(maps)
setwd("./data")


data(state.fips)
state.fips<-state.fips%>%
  rename(STATE=fips,
         stname=abb)%>%
  select(STATE, stname)
state.fips<-rbind(state.fips,
                  c(2, "AK"), c(15, "HI"))%>%
  distinct()
state.fips$STATE<-as.integer(state.fips$STATE)

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
  ### Wade Hampton Census Area, AK: FIPS 2158, not in SEER, remap to 2270
  x$FIPS<-ifelse((x$FIPS==2158), 
                 2270, x$FIPS)
  return(x)
}


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

ncands<-read_csv(files_ncands[1])
if("report_source_NA"%in%names(ncands)){
  ncands<-ncands%>%
    select(-report_source_NA)
}
for(i in 2:length(files_ncands)){
  ncands_temp<-read_csv(files_ncands[i])
  if("report_source_NA"%in%names(ncands_temp)){
    ncands_temp<-ncands_temp%>%
      select(-report_source_NA)
  }
  ncands<-rbind(ncands, ncands_temp)
}
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
  mutate(FIPS=as.numeric(FIPS))%>%
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
afcars_fipsyr<-unique(paste(afcars$FIPS, afcars$year))
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
#z<-scomm[which(!(scomm$FIPS)%in%pop$FIPS), "FIPS"]
scomm<-recode_FIPS(scomm%>%mutate(year=0))%>%select(-year)
cnty287g_fipsyr<-unique(paste(cnty287g$FIPS, cnty287g$year))
z<-cnty287g_fipsyr[which(!(cnty287g_fipsyr%in%pop_fipsyr))]
cnty287g<-recode_FIPS(cnty287g)

##########################################################
## JOIN on POP, rep for .imp, then create indicators for reporting state AFCARS/NCANDS
##########################################################
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

dat_out<-pop%>%
  left_join(afcars)%>%
  left_join(afcars%>%
              select(STATE, year)%>%
              mutate(afcars_reported = TRUE))%>%
  mutate(afcars_reported = ifelse(is.na(afcars_reported),
                                  FALSE,
                                  afcars_reported))
              


# county_index<-pop%>%
#   select(year, FIPS, stname)%>%
#   distinct()%>%
#   left_join(ncands_dat%>%
#               filter(.imp==1)%>%
#               select(year, stname)%>%
#               mutate(reported=TRUE)%>%
#               distinct())%>%
#   filter(reported == TRUE)%>%
#   select(-reported)
# 
# test<-ncands_dat%>%
#   left_join(county_index%>%
#   left_join(ncands_dat)
# ncands_merge<-left_join(pop, 
#                 ncands_dat)%>%
#   left_join(ncands_dat%>%
#               filter(.imp==1)%>%
#               select(year, stname)%>%
#               mutate(reported=TRUE)%>%
#               distinct())%>%
#   filter(reported == TRUE)

