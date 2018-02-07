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

##############################################################################
###### READ/TRANSFORM NCANDS MALTREATMENT REPORTING DATA
##############################################################################
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
