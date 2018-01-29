rm(list=ls())
gc()
library(tidyverse)
library(readr)
library(haven)
library(Amelia)
library(maps)

########################################################################
###### READ AFCARS IMPUTATION DATA - NEED TO ADD CASELOADS FOR REUN_EXIT DENOMINATOR
########################################################################

################# CREATE FIRST ENTRIES, NON-ABUSE REMREASON
#### BRING IN IMPUTED DATA
### set imputation n variable
n_imp<-3
### set up data as list
AFCARS<-read_csv(paste("./data/afcars-imp", 1, ".csv", sep=""))
AFCARS<-AFCARS%>%
  mutate(data=paste("imp1"))
for(i in 2:n_imp){
  temp<-read_csv(paste("./data/afcars-imp", i, ".csv", sep=""))
  temp<-temp%>%
    mutate(data=paste("imp", i, sep=""))
  AFCARS<-bind_rows(AFCARS, temp)
  rm(temp); gc()
}
AFCARS<-AFCARS%>%
  select(-X1)

### Drop Puerto Rico
AFCARS<-AFCARS%>%
  filter(STATE!=72)

##################################################################
###### FORMAT IMPUTATED AFCARS DATA FOR EDA
##################################################################

cnty_entries<-AFCARS%>%
  group_by(STATE, FIPSCODE, FY, HISORGIN, data)%>%
  summarise(entries = as.integer(sum(first_entry * (abuse==0))),
            not_abuse = as.integer(sum(abuse==0)),
            reun_exit = as.integer(sum(reun_exit)))%>%
  ungroup()%>%
  rename(FIPS=FIPSCODE)

##### FOR FILLING IN ZERO HISPANIC ENTRY COUNTIES
##### FIRST ID ALL COUNTIES WITH ANY NON-HISPANIC ENTRIES
##### THEN SET NEW DATA WITH ZERO FOR ALL CATS FOR HISPANIC KIDS
##### BECAUSE THESE DATA ARE IMPUTED, NO TRULY MISSING CASES HERE

non_hisp<-cnty_entries%>%
  filter(HISORGIN == "non-hispanic")%>%
  select(FY, STATE, FIPS, stname, data)%>%
  distinct()

hisp<-cnty_entries%>%
  filter(HISORGIN == "hispanic")%>%
  select(FY, STATE, FIPS, stname, data)%>%
  distinct()

zeroes<-anti_join(non_hisp, hisp)

zeroes<-zeroes%>%
  mutate(entries = 0, not_abuse = 0, reun_exit = 0,
         HISORGIN = "hispanic")

cnty_entries<-cnty_entries%>%
  bind_rows(zeroes)%>%
  arrange(data, HISORGIN, FIPS, FY)


natl_entries<-AFCARS%>%
  group_by(FY, HISORGIN, data)%>%
  summarise(entries = as.integer(sum(first_entry * (abuse==0))),
            not_abuse = as.integer(sum(abuse==0)),
            reun_exit = as.integer(sum(reun_exit)))%>%
  ungroup()

state_entries<-AFCARS%>%
  group_by(STATE, FY, HISORGIN, data)%>%
  summarise(entries = as.integer(sum(first_entry * (abuse==0))),
            not_abuse = as.integer(sum(abuse==0)),
            reun_exit = as.integer(sum(reun_exit)))%>%
  ungroup()

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

cnty_entries<-cnty_entries%>%
  rename(year = FY)

state_entries<-state_entries%>%
  rename(year = FY)

natl_entries<-natl_entries%>%
  rename(year = FY)


########################################################################
###### READ/TRANSFORM S-COMM DATA
########################################################################
scomm<-read_csv("./data/countySComm.csv")
scomm<-scomm%>%
  rename(stname=State,
         countyname=Area_Name)%>%
  mutate(FIPS=as.integer(FIPS))%>%
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

##############################################################################
###### READ/TRANSFORM 287(G) DATA
##############################################################################

cnty287g<-read_dta("./data/county287g.dta")
cnty287g$countyid<-as.numeric(cnty287g$countyid)
cnty287g$year<-as.numeric(cnty287g$year)
cnty287g$c287active<-as.logical(cnty287g$c287active)
cnty287g<-cnty287g%>%
  rename(FIPS=countyid)%>%
  mutate(c287_everapplied=TRUE)


##############################################################################
###### READ/TRANSFORM SEER CHILD POPULATION DATA
##############################################################################
### use AFCARS for state fips crosswalk w/county fips
pop<-read_csv("./data/seer-child-pop-2000-2016.csv")
pop<-pop%>%
  rename(stname=state)%>%
  left_join(state.fips)%>%
  filter(STATE!=72)

##############################################################################
###### READ/TRANSFORM NCANDS MALTREATMENT REPORTING DATA
##############################################################################

ncands<-read_csv("./data/ncands-comm-reports.csv", na="NULL")
ncands_complete<-expand.grid(unique(ncands$subyr), 
                             unique(ncands$RptFIPS), 
                             unique(ncands$cethn))%>%
  rename(subyr=Var1, RptFIPS=Var2, cethn=Var3)
### join on full list to include zeroes, missings
ncands_complete<-full_join(ncands_complete, ncands)
### zeroes are those where cethn = 2 is present, but 1 isn't


## NA is only zero if the county has other reports for the year
# 
#   mutate(total_reports = ifelse(is.na(total_reports), 0, total_reports),
#          comm_report = ifelse(is.na(comm_report), 0, comm_report))
### mutate to character variables
ncands_complete<-ncands_complete%>%
  mutate(RptFIPS = as.integer(RptFIPS))%>%
  mutate(HISORGIN = ifelse(cethn == 1, "hispanic", 
                           ifelse(cethn == 2, "non-hispanic",
                                  "missing")))

##############################################################################
###### CHECK ON MATCHES BY FIPS, RECODE
##############################################################################
### failed matches: use pop as reference
z<-scomm[- which(scomm$FIPS%in%pop$FIPS), "FIPS"]
z1<-cnty_entries[- which(cnty_entries$FIPS%in%pop$FIPS), "FIPS"]%>%distinct()
z2<-ncands_complete[- which(ncands_complete$FIPS%in%pop$FIPS)]%>%distinct()
failed<-rbind(z, z1, z2)%>%
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

##############################################################################
###### MERGE DATA 
##############################################################################

#### COUNTY DATA 
cnty_dat<-cnty_entries%>%
  left_join(pop%>%
              select(-adult_pop))
  
state_dat<-  