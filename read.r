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
#create county for latinx kid population, non-latinx

gc()