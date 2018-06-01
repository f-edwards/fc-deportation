#.rs.restartR()
rm(list=ls())
gc()
#install.packages(c("tidyverse", "mice", "haven", "pan"))
library(tidyverse)
library(mice)
library(haven)
setwd("U:/fc-deportation")


###### merge imputations

files<-paste("./imputations/", list.files("./imputations"), sep="")
imputeds<-paste(files[grep(".RData", files)], sep="")

for(i in 1:length(imputeds)){
  print(i)
  load(imputeds[i])
  data<-mice::complete(imp.out, 
                       action = "long", 
                       include = TRUE)
  
  names(data)<-tolower(names(data))
  data<-data%>%
    mutate(age_group = ifelse(ageatend<6, "0-5", 
                              ifelse(ageatend<13, "6-12",
                                     ifelse(ageatend<19, "13-18",
                                            "adult"))))%>%
    mutate(race = ifelse(hisorgin == "hispanic", "hispanic",
                         ifelse(blkafram==1, "black",
                                ifelse(white==1, "white, nh", 
                                       "other"))))%>%
    select(.imp, year, state, fipscode, race, age_group,
           entered, abuse, first_entry, reun_exit, incar, incap_other,
           neglect)
  
  #### set up rem_reason measures to be conditional on entry, don't want caseloads there
  data<-data%>%
    mutate(incar = entered * incar,
           incap_other = entered * incap_other,
           neglect = entered * neglect,
           abuse = entered * abuse)

  #filename<-paste("./imputations/imputed_afcars", i, ".csv", sep="")
  #write_csv(data, filename)
  cnty<-data%>%
    group_by(.imp, year, state, fipscode, race, age_group)%>%
    summarise_all(sum, na.rm=TRUE)%>%
    left_join(data%>%
                group_by(.imp, year, state, fipscode, race, age_group)%>%
                summarise(caseload=n()))
  
  filename<-paste("./imputations/imputed_afcars_cnty", i, ".csv", sep="")
  write_csv(cnty, filename)
}

pop<-read_fwf("./data/us.1990_2016.singleages.adjusted.txt", 
              col_types = "iciiiiiii",
              fwf_widths(c(4,2,5, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "FIPS", "registry", "race", "hispanic", "sex", "age", "pop")))

pop<-pop%>%
  filter(year>1999)%>%
  mutate(race = ifelse(race == 2, "black",
                        ifelse(race==1 & hispanic ==0, "white, nh",
                                  ifelse(hispanic==1, "hispanic",
                                                   "other"))))%>%
  mutate(age_group = ifelse(age<6, "0-5", 
                            ifelse(age<13, "6-12", 
                                   ifelse(age<=18, "13-18", "adult"))))%>%
  select(-registry, -hispanic)

pop<-pop%>%
  rename(fips=FIPS)%>%
  group_by(year, fips, race, age_group)%>%
  summarise(pop = sum(pop))

write_csv(pop, "./data/seer_reduced.csv")