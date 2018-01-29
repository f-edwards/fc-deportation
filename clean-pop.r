rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(readr)
pop<-read_fwf("us.1990_2016.singleages.adjusted.txt", 
              col_types = "iciiiiiii",
              fwf_widths(c(4,2,5, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "FIPS", "registry", "race", "hispanic", "sex", "age", "pop")))

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

write_csv(pop, "seer-child-pop-2000-2016.csv")