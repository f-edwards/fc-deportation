rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(readr)

AFCARS<-fread("afcars-deport00-15.csv")
AFCARS$HISORGIN<-ifelse(AFCARS$HISORGIN==1, "hispanic",
                        ifelse((AFCARS$HISORGIN==2)||(AFCARS$HISORGIN==0), "non-hispanic",
                               ifelse(AFCARS$HISORGIN==3, NA,
                                      AFCARS$HISORGIN)))

# state_year_entries<-AFCARS%>%
#   group_by(STATE, FY, HISORGIN)%>%
#   filter(Entered==1)%>%
#   summarise(n=n()) #### this matches kids count for TX

#### Make binary vars for parental incapaciation, for first entries
AFCARS$incap_index<-(AFCARS$PRTSJAIL==1)|(AFCARS$ABANDMNT==1)|(AFCARS$RELINQSH==1)

AFCARS$first_entry<-(AFCARS$Entered==1)&(AFCARS$TotalRem==1)

pop<-read_fwf("us.1990_2015.singleages.adjusted.txt", 
              col_types = "iciiiiiii",
              fwf_widths(c(4,2,5, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "FIPS", "registry", "race", "hispanic", "sex", "age", "pop")))
pop<-left_join(pop%>%
                 filter(year>=2005)%>%
                 filter(year<=2010)%>%
                 filter(age<=18)%>%
                 group_by(year, FIPS, hispanic)%>%
                 summarise(child_pop=sum(pop)),
               pop%>%
                 filter(year>=2005)%>%
                 filter(year<=2010)%>%
                 filter(age>18)%>%
                 group_by(year, FIPS, hispanic)%>%
                 summarise(adult_pop=sum(pop)))

pop<-pop%>%
  rename(HISORGIN=hispanic)%>%
  mutate(HISORGIN=ifelse(HISORGIN==1, "hispanic",
                ifelse(HISORGIN==0, "non-hispanic",
                       NA)))

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
          
save.image("AFCARS.Rdata")