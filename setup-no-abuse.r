load("AFCARS.Rdata")

#### for dealing with missingness: two strategies:
#### individual-level imputation OR drop counties with NA on abuse



################# for entries by first entry, by incapacitation
### lots of missing data on incap_index
###
cnty_entries<-AFCARS%>%
  group_by(STATE, FIPSCODE, FY, HISORGIN, abuse, first_entry)%>%
  filter(Entered==1)%>%
  summarise(entries = sum(Entered))%>%
  rename(year=FY, FIPS=FIPSCODE)%>%
  ungroup()




cnty_entries<-cnty_entries%>%
  left_join(AFCARS%>%
              group_by(STATE, FIPSCODE, FY, HISORGIN, abuse, first_entry)%>%
              count()%>%
              rename("caseload"=n)%>%
              rename(year=FY, FIPS=FIPSCODE)%>%
              ungroup())

##### check against kids count data. MATCHES ON TX
# test<-cnty_entries%>%
#   group_by(STATE, year, HISORGIN)%>%
#   summarise(entries=sum(entries)) 

cnty287g<-read_dta("county287g.dta")
cnty287g$countyid<-as.numeric(cnty287g$countyid)
cnty287g$year<-as.numeric(cnty287g$year)
cnty287g$c287active<-as.logical(cnty287g$c287active)
cnty287g<-cnty287g%>%
  rename(FIPS=countyid)%>%
  mutate(c287_everapplied=TRUE)

### join on all with ever application
#dat<-left_join(cnty_entries, cnty287g)

dat<-cnty_entries%>%
  filter(STATE!=72)%>%
  mutate(FIPS=ifelse(FIPS==9, NA, FIPS))

data(state.fips)

state.fips<-state.fips%>%
  rename(STATE=fips,
         stname=abb)%>%
  select(STATE, stname)

state.fips<-rbind(state.fips, 
                  c(2, "AK"), c(15, "HI"))%>%
  distinct()

state.fips$STATE<-as.integer(state.fips$STATE)

dat<-dat%>% ### get abbreviations for plotting
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
z1<-dat[- which(dat$FIPS%in%pop$FIPS), "FIPS"]
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

#dat<-dat[-which(dat$FIPS%in%failed$FIPS),]

#z2<-which(!(scomm$FIPS%in%dat$FIPS))
