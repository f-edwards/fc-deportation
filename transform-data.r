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

##### FOR DEVELOPMENT#
AFCARS<-AFCARS%>%
  filter(data=="imp1")
######################


cnty_entries<-AFCARS%>%
  group_by(STATE, FIPSCODE, FY, HISORGIN, data)%>%
  summarise(entries = as.integer(sum(first_entry * (abuse==0))),
            not_abuse = as.integer(sum(abuse==0)),
            reun_exit = as.integer(sum(reun_exit)),
            caseload = n())%>%
  ungroup()%>%
  rename(FIPS=FIPSCODE)

##### FOR FILLING IN ZERO HISPANIC ENTRY COUNTIES
##### FIRST ID ALL COUNTIES WITH ANY NON-HISPANIC ENTRIES
##### THEN SET NEW DATA WITH ZERO FOR ALL CATS FOR HISPANIC KIDS
##### BECAUSE THESE DATA ARE IMPUTED, NO TRULY MISSING CASES HERE

non_hisp<-cnty_entries%>%
  filter(HISORGIN == "non-hispanic")%>%
  select(FY, STATE, FIPS, data)%>%
  distinct()

hisp<-cnty_entries%>%
  filter(HISORGIN == "hispanic")%>%
  select(FY, STATE, FIPS, data)%>%
  distinct()

zeroes<-anti_join(non_hisp, hisp)

zeroes<-zeroes%>%
  mutate(entries = 0, not_abuse = 0, reun_exit = 0, caseload = 0,
         HISORGIN = "hispanic")

cnty_entries<-cnty_entries%>%
  bind_rows(zeroes)%>%
  arrange(data, HISORGIN, FIPS, FY)


natl_entries<-AFCARS%>%
  group_by(FY, HISORGIN, data)%>%
  summarise(entries = sum(first_entry * (abuse==0)),
            not_abuse = sum(abuse==0),
            reun_exit = sum(reun_exit),
            caseload = n())%>%
  ungroup()

state_entries<-AFCARS%>%
  group_by(STATE, FY, HISORGIN, data)%>%
  summarise(entries = sum(first_entry * (abuse==0)),
            not_abuse = sum(abuse==0),
            reun_exit = sum(reun_exit),
            caseload = n())%>%
  ungroup()

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
  mutate(FIPS=as.integer(FIPS))

#### need to cut based on month, right? going to round to next year if month>=10
#### rounds on 3 months, seems conservative
scomm<-scomm%>%
  mutate(scomm_yr_round = ifelse(Scomm_mo>=10, 
                                 Scomm_yr + 1, 
                                 Scomm_yr))


cnty_yr<-expand.grid(unique(scomm$FIPS), 
                     unique(2000:2016))

cnty_yr<-cnty_yr%>%
  rename(FIPS=Var1,
         year=Var2)

scomm_full<-left_join(cnty_yr,
                       scomm)

scomm_full<-scomm_full%>%
  mutate(scomm_active = (year >= scomm_yr_round))


##############################################################################
###### READ/TRANSFORM 287(G) DATA
##############################################################################

cnty287g<-read_dta("./data/county287g.dta")
cnty287g<-cnty287g%>%
  rename(FIPS=countyid)%>%
  mutate(FIPS=as.numeric(FIPS))%>%
  mutate(c287_ever_applied = TRUE)%>%
  select(-countyname)

ever_applied<-cnty287g%>%
  select(FIPS, c287_ever_applied)%>%
  distinct()

cnty_yr<-expand.grid(FIPS = unique(scomm$FIPS), 
                     year = unique(cnty287g$year))

full_set<-cnty_yr%>%
  left_join(ever_applied)%>%
  mutate(c287_ever_applied = ifelse(is.na(c287_ever_applied), FALSE, TRUE))

cnty287g_full<-left_join(full_set, 
                      cnty287g)

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

ncands<-ncands%>%
  mutate(FIPS = as.integer(RptFIPS))%>%
  mutate(HISORGIN = ifelse(cethn == 1, "hispanic", 
                           ifelse(cethn == 2, "non-hispanic",
                                  "missing")))%>%
  mutate(HISORGIN = ifelse(is.na(HISORGIN), "missing", HISORGIN))%>%
  select( -RptFIPS, -cethn)%>%
  rename(year = subyr)%>%
  group_by(FIPS, year, HISORGIN)%>% ### to merge missing categories
  summarise(total_reports = sum(total_reports),
            comm_report = sum(comm_report),
            missing_rptsrc = sum(missing_report))

ncands_complete<-expand.grid(year = unique(ncands$year), 
                             FIPS = unique(ncands$FIPS), 
                             HISORGIN = unique(ncands$HISORGIN))
### join on full list to include zeroes, missings
ncands_complete<-full_join(ncands_complete, ncands)
### zeroes are those where HISORGIN = 2, total_reports is !=NA, but 1 ==NA

ncands_hisp_na<-ncands_complete%>%
  select(year, FIPS, HISORGIN, total_reports)%>%
  filter(is.na(total_reports))%>%
  filter(HISORGIN==1)
  
ncands_non_hisp_na<-ncands_complete%>%
  select(year, FIPS, HISORGIN, total_reports)%>%
  filter(is.na(total_reports))%>%
  filter(HISORGIN==2)
  
##### then anti join, set report variables to zero - 
### want ncands_hisp_na - ncands_non_hisp_na

ncands_zeroes<-anti_join(ncands_hisp_na, ncands_non_hisp_na)%>%
  mutate(zero=TRUE)%>%
  select(-total_reports)

ncands_out<-left_join(ncands_complete, ncands_zeroes)%>%
  mutate(zero = ifelse(is.na(zero), FALSE, zero),
         total_reports = ifelse(zero == TRUE, 0, total_reports),
         comm_report = ifelse(zero==TRUE, 0, comm_report),
         missing_rptsrc = ifelse(is.na(missing_rptsrc), 0, missing_rptsrc))%>%
  select(-zero)

### use this for state totals later
ncands_out$STATE<-as.integer(with(ncands_out,
                              ifelse(nchar(FIPS)==4, substr(FIPS, 1, 1),
                                     ifelse(nchar(FIPS)==5, substr(FIPS, 1, 2), NA))))

ncands_out<-ncands_out%>%
  filter(STATE!=72)

### check for 999s - there are many
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

ncands_cnty<-ncands_out%>%
  filter(substrRight(FIPS, 3)!="999")


# ##############################################################################
# ###### CHECK ON MATCHES BY FIPS, RECODE
# ##############################################################################
# ### failed matches: use pop as reference
# z<-scomm[- which(scomm$FIPS%in%pop$FIPS), "FIPS"]
# z1<-cnty_entries[- which(cnty_entries$FIPS%in%pop$FIPS), "FIPS"]%>%distinct()
# z2<-ncands_complete[- which(ncands_complete$FIPS%in%pop$FIPS)]%>%distinct()
# failed<-rbind(z, z1, z2)%>%
#   distinct()
# 
# #### DROP FOR NOW, come back and fix these. mapping available at
# #### DROPPED FIPS ARE 2158 (<-2270), 46102, 51019, NA, 51515, 51560
# # https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
# 
# recode_FIPS<-function(x){
#   x$FIPS<-ifelse(x$FIPS == 2270,
#                  2158,
#                  ifelse(x$FIPS == 46113,
#                         46102,
#                         ifelse(x$FIPS == 2201,
#                                2130,
#                                ifelse(x$FIPS == 2280,
#                                       2195,
#                                       x$FIPS))))
#   return(x)
# }

#cnty_entries<-recode_FIPS(cnty_entries)

#z<-test[which(is.na(test$child_pop)), ]

##############################################################################
###### MERGE DATA - IGNORING FAILED MATCHES FOR NOW....
##############################################################################

#### COUNTY DATA 
cnty_entries<-cnty_entries%>%
  left_join(pop%>%
              select(-adult_pop))

state_entries<-state_entries%>%
  left_join(pop%>%
              group_by(STATE, year, HISORGIN)%>%
            summarise(child_pop=sum(child_pop)))

natl_entries<-natl_entries%>%
  left_join(pop%>%
              group_by(year, HISORGIN)%>%
              summarise(child_pop = sum(child_pop)))

ncands_cnty<-ncands_cnty%>%
  left_join(pop%>%
              select(-adult_pop))

ncands_st<-ncands_out%>%
  group_by(STATE, year, HISORGIN)%>%
  summarise(total_report = sum(total_reports, na.rm=TRUE),
            comm_report = sum(comm_report, na.rm=TRUE),
            missing_rptsrc = sum(missing_rptsrc, na.rm=TRUE))%>%
  mutate(total_report = ifelse(total_report == 0, NA, total_report),
         comm_report = ifelse(comm_report == 0, NA, comm_report))%>%
  left_join(pop%>%
              group_by(STATE, year, HISORGIN)%>%
              summarise(child_pop=sum(child_pop)))

ncands_natl<-ncands_st%>%
  filter(!(is.na(total_report)))%>%
  group_by(year, HISORGIN)%>%
  summarise(total_report = sum(total_report),
            comm_report = sum(comm_report, na.rm=TRUE),
            child_pop = sum(child_pop))
