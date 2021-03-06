##############################
## ICE - family stability 
## time series models for batch of outcomes

rm(list=ls()); gc()

library(tidyverse)
library(haven)

setwd("U:/fc-deportation")


pop<-read_csv("./data/seer_reduced.csv")

#### to merge onto pums data for more stable denominators
pop_child<-pop%>%
  filter(age_group!="adult")%>%
  group_by(year, fips, race)%>%
  summarise(cpop = sum(pop))

################# for entries by first entry, by incapacitation
### lots of missing data on incap_index

cnty287g<-read_dta("./data/county287g.dta")
cnty287g$countyid<-as.numeric(cnty287g$countyid)
cnty287g$year<-as.numeric(cnty287g$year)
cnty287g$c287active<-as.logical(cnty287g$c287active)
cnty287g<-cnty287g%>%
  rename(fips=countyid)%>%
  mutate(c287_everapplied=TRUE)%>%
  select(-countyname)

#### drop the x year format, will fix that for the outcomes we've got
#### to expand the pre-treatment TS

cnty287g<-cnty287g%>%
  select(-year, -c287active, -c287_identify)%>%
  distinct()

#######################################
## read and format puma data

pop_puma<-read_dta("./data/countykids00to16.dta")
pop_puma<-pop_puma%>%
  select(countyid, countyname, year, cukid_hisp,
         cukid_nhb, cukid_nhw,
         ckhhsize_nhw, ckhhsize_nhb, ckhhsize_hisp,
         ckhhfams_nhw, ckhhfams_nhb, ckhhfams_hisp,
         cpgkid_nhw, cpgkid_nhb, cpgkid_hisp,
         cpklivesmom_nhw, cpklivesmom_nhb, cpklivesmom_hisp,
         cpknolivespar_nhw, cpknolivespar_nhb, cpknolivespar_hisp,
         cpklivessinmom_nhw, cpklivessinmom_hisp, cpklivessinmom_nhb,
         cpmixgen_hisp, cpmixgen_nhb, cpmixgen_nhw,
         cpklivessinpar_hisp,cpklivessinpar_nhb,cpklivessinpar_nhw,
         cpknparhead_hisp,cpknparhead_nhw,cpknparhead_nhb,
         cpknfamhead_hisp,cpknfamhead_nhw,cpknfamhead_nhb,
         cpunauth, cphisp, cpimmig)
pop_puma$fips<-as.numeric(pop_puma$countyid)
pop_puma$year<-as.integer(pop_puma$year)
### exclude cases where sampling unit by race is <30
### only filter on hispanic sample for now, filtering on black sample 
### drastically cuts included counties
pop_puma<-pop_puma

#### DROP CUKID LESS THAN 30 FOR ALL GROUPS (ESP BLACK)

#### drop years with no cnty287g data
pop_puma<-pop_puma%>%
  filter(year<=2014)

vars<-c("ckhhsize",
        "ckhhfams",
        "cpgkid",
        "cpklivesmom",
        "cpknolivespar",
        "cpklivessinmom",
        "cpklivessinpar",
        "cpknparhead",
        "cpknfamhead",
        "cpmixgen",
        "cukid")

### convert to long by race
puma_temp<-list()
for(i in 1:length(vars)){
  index<-grep(vars[i], names(pop_puma))
  temp<-pop_puma[,c(1:3, index)]
  temp<-temp%>%
    gather(key = race, value = value, -countyid, -countyname, -year)
  names(temp)[5]<-vars[i]
  temp$race<-substr(temp$race, nchar(temp$race)-3, nchar(temp$race))
  temp$race<-recode(temp$race, 
                    "_nhb" = "black",
                    "_nhw" = "white, nh",
                    "hisp" = "hispanic")
  puma_temp[[i]]<-temp
}

puma_long<-puma_temp[[1]]%>%
  left_join(puma_temp[[2]])%>%
  left_join(puma_temp[[3]])%>%
  left_join(puma_temp[[4]])%>%
  left_join(puma_temp[[5]])%>%
  left_join(puma_temp[[6]])%>%
  left_join(puma_temp[[7]])%>%
  left_join(puma_temp[[8]])%>%
  left_join(puma_temp[[9]])%>%
  left_join(puma_temp[[10]])%>%
  left_join(puma_temp[[11]])%>%
  left_join(pop_puma%>%
              select(countyid, year, cpunauth, cphisp, cpimmig))%>%
  filter(cukid>30)### drop cases with v small samples

puma_long<-puma_long%>%
  rename(fips = countyid)%>%
  mutate(fips = as.numeric(fips))%>%
  left_join(cnty287g)%>%
  mutate(c287_everapplied = ifelse(is.na(c287_everapplied), "FALSE", 
                                   c287_everapplied),
         c287_active = ifelse(c287start_yr>=year, "TRUE", "FALSE"),
         c287_active = ifelse(is.na(c287start_yr), "FALSE", c287_active))

### pull out state
puma_long<-puma_long%>%
  mutate(state = factor(ifelse(nchar(fips)==4, substr(fips, 1, 1),
                               substr(fips, 1, 2))))

### make ever approved, time to implement variables
### for comparison of time to implement to never active,
### add 287everactive dummy
puma_long<-puma_long%>%
  mutate(c287_everactive = !(is.na(c287start_yr)),
         years_to_active = year - c287start_yr,
         years_to_active = ifelse(is.na(years_to_active), 0, years_to_active))
### top and bottom code based on distribution of obseved
puma_long<-puma_long%>%
         mutate(years_to_active = ifelse(years_to_active< -4 , -4, years_to_active),
         years_to_active = ifelse(years_to_active> 6, 6, years_to_active))

puma_outcomes<-vars[1:10] # don't want cukid in there

puma_model_data<-list()

for(i in 1:length(puma_outcomes)){
  names_temp<-c("fips","state", "year", "race", 
                "years_to_active",
                "cpunauth", "cphisp",
                "c287_active", "c287_everapplied", "c287_everactive", 
                puma_outcomes[i])
  puma_model_data[[i]]<-puma_long%>%
    select(one_of(names_temp))
  names(puma_model_data)[i]<-puma_outcomes[i]
  names(puma_model_data[[i]])[length(names(puma_model_data[[i]]))]<-"outcome"
  puma_model_data[[i]]<-puma_model_data[[i]]%>%
    filter(!(is.na(outcome)))
  puma_model_data[[i]]$years_to_active<-factor(puma_model_data[[i]]$years_to_active,
                                   levels = c(0, -4, -3, -2, -1,  1, 2, 3, 4, 5, 6))
  puma_model_data[[i]]$race<-factor(puma_model_data[[i]]$race)
  puma_model_data[[i]]$race<-relevel(puma_model_data[[i]]$race, ref="white, nh")
  ### make relative time dummies
}




#######################################
## read and format afcars data
#### read in imputed afcars data

files<-list.files("./imputations")
imputeds<-paste("./imputations/", files[grep("imputed_", files)], sep="")

afcars<-read_csv(imputeds[1])

for(i in 2:length(imputeds)){
  temp<-read.csv(imputeds[i])
  afcars<-afcars%>%
    rbind(temp)
}

### remove over 18s
afcars<-afcars%>%
  filter(age_group!="adult")

afcars<-afcars%>%
  filter(.imp==1)%>% ### for development, change later to !=0
  filter(race!="other")%>%
  rename(fips = fipscode)

### fill in zeroes
afcars<-afcars%>%
  complete(.imp, year, race, age_group, 
           nesting(state, fips), 
           fill=list(entered = 0, abuse = 0,
                     first_entry = 0, reun_exit = 0,
                     incar = 0, incap_other = 0,
                     neglect = 0, caseload = 0))

#### afcars moves all nyc cases to manhattan, make correction in seer
pop_mod<-pop%>%
  mutate(fips = ifelse(fips %in% c(36005, 36047, 36061, 36081, 36085), 36061, fips))%>%
  group_by(year, fips, race, age_group)%>%
  summarise(pop = sum(pop))

afcars<-afcars%>%
  left_join(pop_mod)

### convert to per capita
afcars<-afcars%>%
  mutate(entered = entered / pop * 1000,
         caseload = caseload / pop * 1000,
         reun_exit_pct = reun_exit / caseload * 100,
         first_entry = first_entry / pop * 1000,
         abuse = abuse / pop * 1000,
         neglect = neglect / pop * 1000,
         incar = incar / pop * 1000,
         incap_other = incap_other / pop * 1000)


#### join to 287 and make time vars
afcars<-afcars%>%
  left_join(cnty287g)%>%
  mutate(c287_everapplied = ifelse(is.na(c287_everapplied), "FALSE", 
                                   c287_everapplied),
         c287_active = ifelse(c287start_yr>=year, "TRUE", "FALSE"),
         c287_active = ifelse(is.na(c287start_yr), "FALSE", c287_active))

### make ever approved, time to implement variables
### for comparison of time to implement to never active,
### add 287everactive dummy
afcars<-afcars%>%
  mutate(c287_everactive = !(is.na(c287start_yr)),
         years_to_active = year - c287start_yr,
         years_to_active = ifelse(is.na(years_to_active), 0, years_to_active))
### top and bottom code based on distribution of obseved
afcars<-afcars%>%
  mutate(years_to_active = ifelse(years_to_active< -4 , -4, years_to_active),
         years_to_active = ifelse(years_to_active> 6, 6, years_to_active))

afcars_outcomes<-c("entered",
                   "caseload",
                   "reun_exit",
                   "first_entry",
                   "abuse",
                   "neglect",
                   "incar",
                   "incap_other")

afcars_model_data<-list()

for(i in 1:length(afcars_outcomes)){
  names_temp<-c("fips","state", "year", 
                "race", "age_group",
                "years_to_active",
                "c287_active", "c287_everapplied", 
                "c287_everactive",
                afcars_outcomes[i])

  afcars_model_data[[i]]<-afcars%>%
    select(one_of(names_temp))
  names(afcars_model_data)[i]<-afcars_outcomes[i]
  names(afcars_model_data[[i]])[length(names(afcars_model_data[[i]]))]<-"outcome"
  afcars_model_data[[i]]<-afcars_model_data[[i]]%>%
    filter(!(is.na(outcome)))
  afcars_model_data[[i]]$years_to_active<-factor(afcars_model_data[[i]]$years_to_active,
                                               levels = c(0, -4, -3, -2, -1,  1, 2, 3, 4, 5, 6))
  afcars_model_data[[i]]$race<-factor(afcars_model_data[[i]]$race)
  afcars_model_data[[i]]$race<-relevel(afcars_model_data[[i]]$race, ref="white, nh")
  
}
##################### MISSINGS ON MERGE WITH SEER POP BECAUSE OF COURSE THERE ARE



