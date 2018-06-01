#.rs.restartR()
rm(list=ls())
gc()
#install.packages(c("tidyverse", "mice", "haven", "pan"))
library(tidyverse)
library(mice)
library(haven)
setwd("U:/fc-deportation")
source("functions.r")

AFCARS<-read_csv("./data/afcars-deport00-16-race-remdt.csv", na=c("NULL", 99))

AFCARS<-AFCARS%>%
  filter(!(is.na(FIPSCODE)), FIPSCODE!=9)%>%
  filter(STATE!=72)

AFCARS$HISORGIN<-ifelse(AFCARS$HISORGIN==3, NA,
                        ifelse((AFCARS$HISORGIN==2)|(AFCARS$HISORGIN==0), "non-hispanic",
                               ifelse(AFCARS$HISORGIN==1, "hispanic",
                                      AFCARS$HISORGIN)))

AFCARS$SEX<-ifelse(AFCARS$SEX == 1, "Male",
                   ifelse(AFCARS$SEX == 2, "Female",
                          NA))

AFCARS$abuse<-with(AFCARS, (PHYABUSE==1) |
                     (SEXABUSE==1))

AFCARS$incar<-with(AFCARS, (!abuse) &
                   (PRTSJAIL==1))


AFCARS$incap_other<-with(AFCARS, (!abuse)&(
                           (NOCOPE==1) |
                           (ABANDMNT==1) |
                           (RELINQSH==1)))

AFCARS$NEGLECT<-AFCARS$NEGLECT==1

AFCARS$first_entry<- (AFCARS$Entered==1) &
  (AFCARS$TotalRem==1) ### Entered is never missing, NA&FALSE = FALSE

AFCARS$reun_exit<-ifelse(AFCARS$DISREASN==1 & AFCARS$Exited ==1,
                         TRUE,
                         ifelse(is.na(AFCARS$DISREASN) & (AFCARS$Exited == 1),
                                NA,
                                FALSE))

AFCARS<-AFCARS%>%
  mutate(BLKAFRAM = factor(BLKAFRAM),
         WHITE = factor(WHITE))

AFCARS<-AFCARS%>%
  select(FY, STATE, FIPSCODE, BLKAFRAM, WHITE,
         HISORGIN, AgeAtEnd, SEX, Entered, abuse, first_entry, reun_exit, incar, incap_other, NEGLECT, LatRemDt)
gc()

AFCARS<-recode_FIPS(AFCARS)

###### BRING IN MATT'S COUNTY DATA
pop_puma<-read_dta("./data/countykids00to16.dta")
pop_puma$FIPSCODE<-as.numeric(pop_puma$countyid)
############## selecting county-year level variables for imputation, going for many to improve prediction
pop_imputation_data<-pop_puma%>%
  select(FIPSCODE, year, cpop, cpnhw, cpnhb, cphisp, cpimmig,
         cppoor, cpgkid, cpkhsg_own)
#### FOR IMPUTATION CONSTRUCT INTERPOLATIONS FOR 2001 - 2004 on pop cats

counties<-unique(pop_imputation_data$FIPSCODE)
missings<-data.frame("FIPSCODE" = rep(counties, 4),
                     "year"= c(rep(2001, length(counties)),
                               rep(2002, length(counties)),
                               rep(2003, length(counties)),
                               rep(2004, length(counties))))

for(i in counties){ ### linear interpolation for imputation models
  #print(i)
  start<-pop_imputation_data[which((pop_imputation_data$FIPSCODE==i) &
                                     (pop_imputation_data$year==2000)), ]
  end<-pop_imputation_data[which((pop_imputation_data$FIPSCODE==i) &
                                   (pop_imputation_data$year==2005)), ]
  if(nrow(start)>0 & nrow(end)>0){
    output<-bind_rows(start, start, start, start)
    output$year<-2001:2004
  for(j in 3:ncol(pop_imputation_data)){
    slope<-(end[j] - start[j])/4
    interps<-slope[[1]] * seq(1, 4, 1) + start[[j]]
    output[j]<-interps
  }
    pop_imputation_data<-bind_rows(pop_imputation_data, output)
  }
}

pop_imputation_data$FIPSCODE<-ifelse(pop_imputation_data$FIPSCODE==36999,
                            36061,
                            pop_imputation_data$FIPSCODE)

AFCARS_merge<-left_join(AFCARS%>%
                          rename(year = FY),
                        pop_imputation_data)%>%
  filter(!(is.na(cpop)))

divisions<-read.csv("./data/divisions.csv")

AFCARS_merge<-AFCARS_merge%>%
  left_join(divisions)

#### ALL MATCH EXCEPT NYC COUNTIES, 36005, 36047, 36081, 36085:
#### merge all counts into 36061 (Manhattan) where FC cases are coded
#####################################################################################################################
### multilevel imputation using mice
### https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html
#####################################################################################################################
#
# AFCARS_merge$STATE<-as.integer(AFCARS_merge$STATE)
# AFCARS_merge$FIPSCODE<-factor(AFCARS_merge$FIPSCODE)

### for the amount of data we've got, multilevel lim n->inf = FE, just do FE
AFCARS_merge$STATE<-factor(AFCARS_merge$STATE)
AFCARS_merge$FIPSCODE<-factor(AFCARS_merge$FIPSCODE)
AFCARS_merge$HISORGIN<-factor(AFCARS_merge$HISORGIN)
AFCARS_merge$SEX<-factor(AFCARS_merge$SEX)
AFCARS_merge$STATE<-factor(AFCARS_merge$STATE)
AFCARS_merge$FIPSCODE<-factor(AFCARS_merge$FIPSCODE)
AFCARS_merge$year<-as.integer(AFCARS_merge$year)
### sqrt transform for regression fitting
AFCARS_merge<-AFCARS_merge%>%
  mutate_at(vars(cpop, cpnhw, cpnhb, cphisp, 
                 cpimmig, cppoor, cpgkid, cpkhsg_own), sqrt)

### sqrt transform all pop variables for model fitting

ini<-mice(AFCARS_merge[1:100,], m=1, maxit = 0, seed = 1)
#### SET PREDICTOR MATRIX - -2 = ID random intercept, 1 = fixed effect, 2 = random effect
pred<-ini$pred
pred["HISORGIN",]<-1
pred["WHITE",]<-1
pred["BLKAFRAM",]<-1
pred["AgeAtEnd",]<-1
pred["SEX", ]<-1
pred["abuse", ]<-1
pred["first_entry", ]<-1
pred["incar", ]<-1
pred["incap_other", ]<-1
pred["NEGLECT", ]<-1
pred["reun_exit", ]<-1
pred[, "DIVISION"]<-0
pred[, "LatRemDt"]<-0
pred["LatRemDt",]<-0
diag(pred)<-0
#### SET IMPUTATION METHODS
meth<-ini$method
meth[4:15]<-"logreg"
meth[7]<-"pmm"
rm(ini)
rm(AFCARS)
rm(pop_imputation_data)
rm(pop_puma)
gc()

#### RUN TEST DATA IN PARALLEL
#### PARALLEL IS NUTSO ON THE RAM - NOT VIABLE
divisions<-unique(AFCARS_merge$DIVISION)
for(i in 7:length(divisions)){

  dat_temp<-AFCARS_merge%>%
    filter(DIVISION==divisions[i])

  print(divisions[i])
  print(nrow(dat_temp))
  print(summary(dat_temp$HISORGIN))
  print(summary(dat_temp$abuse))
  print(summary(dat_temp$first_entry))
  print(summary(dat_temp$reun_exit))

  imp.out<-mice(dat_temp,
           method = meth,
           pred = pred,
           m=5,
           maxit = 15,
           seed = 1)

  filename<-paste("./imputations/imputation", i, ".RData", sep="")
  save.image(filename)
  rm(imp.out)
  gc()
}


# q(save="no")

###### merge imputations

files<-paste("./imputations/", list.files("./imputations"), sep="")

for(i in 1:length(files)){
  print(i)
  load(files[i])
  data<-mice::complete(imp.out, 
                       action = "long", 
                       include = TRUE)
  filename<-paste("./imputations/imputed_afcars", i, ".csv", sep="")
  write_csv(data, filename)
}


