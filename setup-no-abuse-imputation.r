rm(list=ls())
gc()
library(tidyverse)
library(readr)
library(haven)
library(Amelia)
library(maps)

########################################################################
###### READ AFCARS IMPUTATION DATA
########################################################################

################# CREATE FIRST ENTRIES, NON-ABUSE REMREASON
#### BRING IN IMPUTED DATA
### set imputation n variable
n_imp<-3
### set up data as list
AFCARS<-read_csv(paste("afcars-imp", 1, ".csv", sep=""))
AFCARS<-AFCARS%>%
  mutate(data=paste("imp1"))
for(i in 2:n_imp){
  temp<-read_csv(paste("afcars-imp", i, ".csv", sep=""))
  temp<-temp%>%
    mutate(data=paste("imp", i, sep=""))
  AFCARS<-bind_rows(AFCARS, temp)
  rm(temp); gc()
}
AFCARS<-AFCARS%>%
  select(-X1)

##################################################################
###### FORMAT IMPUTATED AFCARS DATA FOR EDA
##################################################################

cnty_entries<-AFCARS%>%
  group_by(STATE, FIPSCODE, FY, HISORGIN, data)%>%
  summarise(entries = as.integer(sum(first_entry * (abuse==0))),
            not_abuse = as.integer(sum(abuse==0)),
            reun_exit = as.integer(sum(reun_exit)))%>%
  ungroup()

temp<-cnty_entries%>%
  group_by(STATE, FIPSCODE, FY, HISORGIN)%>%
  summarise(entries_min=min(entries),
            entries_max=max(entries),
            not_abuse_min=min(not_abuse),
            not_abuse_max=max(not_abuse),
            reun_exit_min=min(reun_exit),
            reun_exit_max=max(reun_exit))%>%
  ungroup()

cnty_entries<-left_join(cnty_entries%>%
                  filter(data=="imp1")%>%
                  select(-data,
                         -not_abuse,
                         -entries,
                         -reun_exit), 
                  temp)%>%
  rename(FIPS=FIPSCODE)

natl_entries<-AFCARS%>%
  group_by(FY, HISORGIN, data)%>%
  summarise(entries = as.integer(sum(first_entry * (abuse==0))),
            not_abuse = as.integer(sum(abuse==0)),
            reun_exit = as.integer(sum(reun_exit)))%>%
  ungroup()

temp<-natl_entries%>%
  group_by(FY, HISORGIN)%>%
  summarise(entries_min=min(entries),
            entries_max=max(entries),
            not_abuse_min=min(not_abuse),
            not_abuse_max=max(not_abuse),
            reun_exit_min=min(reun_exit),
            reun_exit_max=max(reun_exit))%>%
  ungroup()

natl_entries<-left_join(natl_entries%>%
                          filter(data=="imp1")%>%
                          select(-data, 
                                 -not_abuse,
                                 -entries,
                                 -reun_exit), 
                        temp)


state_entries<-AFCARS%>%
  group_by(STATE, FY, HISORGIN, data)%>%
  summarise(entries = as.integer(sum(first_entry * (abuse==0))),
            not_abuse = as.integer(sum(abuse==0)),
            reun_exit = as.integer(sum(reun_exit)))%>%
  ungroup()

temp<-state_entries%>%
  group_by(STATE, FY, HISORGIN)%>%
  summarise(entries_min=min(entries),
            entries_max=max(entries),
            not_abuse_min=min(not_abuse),
            not_abuse_max=max(not_abuse),
            reun_exit_min=min(reun_exit),
            reun_exit_max=max(reun_exit))%>%
  ungroup()

state_entries<-left_join(state_entries%>%
                          filter(data=="imp1")%>%
                          select(-data,
                                 -not_abuse,
                                 -entries,
                                 -reun_exit), 
                        temp)


##### IMPUTATION VISUALS
# ggplot(natl_entries, 
#        aes(x=FY, fill=HISORGIN,
#            ymin = entries_min,
#             ymax = entries_max))+
#   geom_ribbon()+
#   geom_line(aes(y=(entries_min+entries_max)/2, x=FY, col=HISORGIN))
# 
# ggplot(natl_entries, 
#        aes(x=FY, fill=HISORGIN,
#            ymin = not_abuse_min,
#            ymax = not_abuse_max))+
#   geom_ribbon()+
#   geom_line(aes(y=(not_abuse_min+not_abuse_max)/2, x=FY, col=HISORGIN))
# 
# ggplot(natl_entries, 
#        aes(x=FY, fill=HISORGIN,
#            ymin = reun_exit_min,
#            ymax = reun_exit_max))+
#   geom_ribbon()+
#   geom_line(aes(y=(reun_exit_min+reun_exit_max)/2, x=FY, col=HISORGIN))
# 
# ggplot(state_entries, 
#        aes(x=FY, fill=HISORGIN,
#            ymin = entries_min,
#            ymax = entries_max))+
#   geom_ribbon()+
#   geom_line(aes(y=(entries_min+entries_max)/2, x=FY, col=HISORGIN))+
#   facet_wrap(~STATE)
### make imputation bounds

s_comm<-read_csv("countySComm.csv")
s_comm<-s_comm%>%
  mutate(FIPS=as.numeric(FIPS),
         s_comm_active=TRUE,
         year=Scomm_yr)

#### want to have an active = T time series variable
#### so loop over counties, create new entries for each year between Scomm_yr[i] 
#### and max(Scomm_yr)
max_yr<-max(s_comm$Scomm_yr)
for(i in 1:nrow(s_comm)){
  diff<-max_yr - s_comm$Scomm_yr[i]
  if(diff!=0){
    new_rows<-s_comm[i,]
    temp<-new_rows
    for(j in 1:diff){
      temp$year<-temp$year+1
      new_rows<-bind_rows(new_rows, temp)
    }
  }
  new_rows<-new_rows[-1,]
  s_comm<-bind_rows(s_comm, new_rows)
}

s_comm<-s_comm%>%
  arrange(FIPS)

cnty_yr<-expand.grid(unique(s_comm$FIPS), 
                     unique(s_comm$Scomm_yr))

cnty_yr<-cnty_yr%>%
  rename(FIPS=Var1,
         year=Var2)

s_comm_full<-left_join(cnty_yr,
                       s_comm)%>%
  arrange(FIPS)%>%
  select(-State, -Area_Name)%>%
  mutate(s_comm_active=ifelse(is.na(s_comm_active), FALSE, s_comm_active))

### START HERE - MAX SCOMM_MO AND SCOMM_YR CONSISTENT FOR ALL YEARS

cnty287g<-read_dta("county287g.dta")
cnty287g$countyid<-as.numeric(cnty287g$countyid)
cnty287g$year<-as.numeric(cnty287g$year)
cnty287g$c287active<-as.logical(cnty287g$c287active)
cnty287g<-cnty287g%>%
  rename(FIPS=countyid)%>%
  mutate(c287_everapplied=TRUE)

### join on all with ever application
#dat<-left_join(cnty_entries, cnty287g)

cnty_entries<-cnty_entries%>%
  filter(STATE!=72)%>%
  mutate(FIPS=ifelse(FIPS==9, NA, FIPS))

state_entries<-state_entries%>%
  filter(STATE!=72)

### get abbreviations for plotting
data(state.fips)
state.fips<-state.fips%>%
  rename(STATE=fips,
         stname=abb)%>%
  select(STATE, stname)
state.fips<-rbind(state.fips, 
                  c(2, "AK"), c(15, "HI"))%>%
  distinct()
state.fips$STATE<-as.integer(state.fips$STATE)
cnty_entries<-cnty_entries%>% 
  left_join(state.fips)
state_entries<-state_entries%>%
  left_join(state.fips)

### use AFCARS for state fips crosswalk w/county fips
pop<-pop%>%
  rename(stname=state)%>%
  left_join(state.fips)

#### read s comm data from Matt
scomm<-read_csv("countySComm.csv")
scomm<-scomm%>%
  rename(stname=State,
         countyname=Area_Name)%>%
  mutate(FIPS=as.integer(FIPS))

### failed matches: use pop as reference
z<-scomm[- which(scomm$FIPS%in%pop$FIPS), "FIPS"]
z1<-cnty_entries[- which(cnty_entries$FIPS%in%pop$FIPS), "FIPS"]
failed<-rbind(z, z1)%>%
  distinct()

#### DROP FOR NOW, come back and fix these. mapping available at
#### DROPPED FIPS ARE 2158 (<-2270), 46102, 51019, NA, 51515, 51560
# https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf

recode_FIPS<-function(x){
  x$FIPS<-ifelse(x$FIPS == 2270, 
                 2158,
                 ifelse(x$FIPS == 46113,
                        46102,
                        ifelse(x$FIPS == 2201,
                               2130,
                               ifelse(x$FIPS == 2280,
                                      2195,
                                      x$FIPS))))
  return(x)
}

dat<-recode_FIPS(dat); pop<-recode_FIPS(pop); scomm<-recode_FIPS(scomm); cnty287g<-recode_FIPS(cnty287g)

ncands<-read_csv("ncands-comm-reports.csv", na="NULL")
ncands_complete<-expand.grid(unique(ncands$subyr), 
                             unique(ncands$RptFIPS), 
                             unique(ncands$cethn))%>%
  rename(subyr=Var1, RptFIPS=Var2, cethn=Var3)
### join on full list to include zeroes
ncands_complete<-full_join(ncands_complete, ncands)%>%
  mutate(total_reports = ifelse(is.na(total_reports), 0, total_reports),
         comm_report = ifelse(is.na(comm_report), 0, comm_report))
### mutate to character variables
ncands_complete<-ncands_complete%>%
  mutate()

#### merge pop onto the county, state, natl files
