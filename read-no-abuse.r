rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(readr)
library(Amelia)

AFCARS<-fread("afcars-deport00-16.csv", na.strings="NULL")

AFCARS<-AFCARS%>%
  filter(!(is.na(FIPSCODE)), FIPSCODE!=9)

AFCARS$HISORGIN<-ifelse(AFCARS$HISORGIN==3, NA,
                        ifelse((AFCARS$HISORGIN==2)|(AFCARS$HISORGIN==0), "non-hispanic",
                               ifelse(AFCARS$HISORGIN==1, "hispanic",
                                      AFCARS$HISORGIN)))

gc()

### Filter out cases with any cat not relevant, should give upper bound on incap removals 
### abuse_index<- [PHYABUSE, SEXABUSE, AAPARENT, DAPARENT, AACHILD, DACHILD, CHILDIS, PRTSDIED]

AFCARS$abuse<-with(AFCARS, (PHYABUSE==1) | 
                          (SEXABUSE==1) | 
                          (AAPARENT==1) | 
                          (DAPARENT==1) | 
                          (AACHILD==1) | 
                          (DACHILD==1) | 
                          (CHILDIS==1) | 
                          (PRTSDIED==1))

AFCARS$first_entry<- (AFCARS$Entered==1) &
  (AFCARS$TotalRem==1)

AFCARS$reun_exit<-ifelse(AFCARS$DISREASN==1, 
                         TRUE, 
                         ifelse((AFCARS$DISREASN==99) & (AFCARS$Exited == 1), 
                                NA,
                                FALSE))

AFCARS<-AFCARS%>%
  select(FY, STATE, FIPSCODE, 
         SEX, HISORGIN, TotalRem, 
         AgeAtEnd, Exited, Served,
         abuse, first_entry, reun_exit)

# AFCARS.imp<-amelia(AFCARS, m = 1, 
#                    p2s = 1, idvars = c("STATE", "FIPSCODE"),
#                    noms = c("SEX", "HISORGIN",
#                             "Exited"))
# gc()
# 
# AFCARS<-AFCARS.imp$imputations[[1]]

pop<-read_fwf("us.1990_2015.singleages.adjusted.txt", 
              col_types = "iciiiiiii",
              fwf_widths(c(4,2,5, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "FIPS", "registry", 
                           "race", "hispanic", "sex", "age", "pop")))

pop<-pop%>%
  filter(year>1999)

state.index<-pop%>%
  select(state, FIPS)%>%
  distinct()

pop<-left_join(pop%>%
                 filter(age<18)%>%
                 group_by(year, FIPS, hispanic)%>%
                 summarise(child_pop=sum(pop)),
               pop%>%
                 filter(age>=18)%>%
                 group_by(year, FIPS, hispanic)%>%
                 summarise(adult_pop=sum(pop)))

pop<-pop%>%
  rename(HISORGIN=hispanic)%>%
  mutate(HISORGIN=ifelse(HISORGIN==1, "hispanic",
                ifelse(HISORGIN==0, "non-hispanic",
                       NA)))
  
pop<-pop%>%
  left_join(state.index)

pop<-pop%>%
  filter(FIPS!=99999)

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
          
save.image("AFCARS-no-abuse.Rdata")