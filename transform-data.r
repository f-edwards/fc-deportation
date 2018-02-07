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

########################################################################
###### READ AFCARS IMPUTATION DATA - NEED TO ADD CASELOADS FOR REUN_EXIT DENOMINATOR
########################################################################

################# CREATE FIRST ENTRIES, NON-ABUSE REMREASON
#### BRING IN IMPUTED DATA
### set imputation n variable
AFCARS<-read_csv("afcars_imputed_state_fe.csv")

##################################################################
###### FORMAT IMPUTATED AFCARS DATA FOR EDA
##################################################################

######################
# 
# 

cnty_entries<-AFCARS%>%
  group_by(.imp, STATE, FIPSCODE, FY, HISORGIN)%>%
  summarise(entries_first = as.integer(sum(first_entry, na.rm=TRUE)),
            entries_first_nonab = as.integer(sum(first_entry * (abuse==0),na.rm=TRUE)),
            cl = n(),
            cl_not_abuse = as.integer(sum(abuse==0, na.rm=TRUE)),
            reun_exit = as.integer(sum(reun_exit, na.rm=TRUE)))%>%
  ungroup()


write_csv(cnty_entries, "afcars_imputed_county_sums.csv")

 
# cnty_entries<-cnty_entries%>% 
#   left_join(state.fips)
# state_entries<-state_entries%>%
#   left_join(state.fips)
# 
# cnty_entries<-cnty_entries%>%
#   rename(year = FY)
# 
# state_entries<-state_entries%>%
#   rename(year = FY)
# 
# natl_entries<-natl_entries%>%
#   rename(year = FY)

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

cnty287g<-read_dta("./county287g.dta")
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
pop<-read_csv("./seer-child-pop-2000-2016.csv")
pop<-pop%>%
  rename(stname=state)%>%
  left_join(state.fips)%>%
  filter(STATE!=72)

# ##############################################################################
# ###### CHECK ON MATCHES BY FIPS, RECODE
# # ##############################################################################
afcars_fipsyr<-unique(paste(AFCARS$FIPS, AFCARS$year))
pop_fipsyr<-unique(paste(pop$FIPS, pop$year))

z<-afcars_fipsyr[which(!(afcars_fipsyr%in%pop_fipsyr))]
# # https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
# following suggestiuons on recoding here
# https://seer.cancer.gov/seerstat/variables/countyattribs/ruralurban.html
AFCARS<-AFCARS%>%
  rename(FIPS=FIPSCODE,
         year=FY)

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
  return(x)
}

AFCARS<-recode_FIPS(AFCARS)

##############################################################################
###### READ/TRANSFORM NCANDS MALTREATMENT REPORTING DATA
##############################################################################
<<<<<<< HEAD
files<-list.files()
files_ncands<-files[grep("ncands_cnty", files)]

ncands_dat<-read_csv(files_ncands[1])
for(i in 1:length(files_ncands)){
  ncands_temp<-read_csv(files_ncands[i],
                        col_types = "nnnccddd")
  if("report_source_NA"%in%names(ncands_temp)){
    ncands_temp<-ncands_temp%>%
      select(-report_source_NA)
  }
  ncands_dat<-rbind(ncands_dat, ncands_temp)
=======


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
>>>>>>> d99b9a80bf670a62b47dba211edce5dd27ab61b6
}

ncands_imp<-ncands_dat%>%filter(.imp!=0)

ncands_dat<-ncands_dat%>%
  rename(stname=state)%>%
  left_join(pop)

test<-ncands_dat%>%
  left_join(scomm)

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
