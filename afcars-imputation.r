#.rs.restartR()
rm(list=ls())
gc()
library(tidyverse, lib.loc="./library")
library(mice, lib.loc="./library")
library(haven, lib.loc="./library")
library(pan, lib.loc="./library")
source("functions.r")

AFCARS<-read_csv("./data/afcars-deport00-16.csv", na=c("NULL", 99))

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
                     (SEXABUSE==1) | 
                     (AAPARENT==1) | 
                     (DAPARENT==1) | 
                     (AACHILD==1) | 
                     (DACHILD==1) | 
                     (CHILDIS==1) | 
                     (PRTSDIED==1))


AFCARS$incar<-with(AFCARS,
                   (PRTSJAIL==1))


AFCARS$incap_broad<-with(AFCARS,
                         (PRTSJAIL==1) |
                           (NOCOPE==1) |
                           (ABANDMNT==1) |
                           (RELINQSH==1))

AFCARS$first_entry<- (AFCARS$Entered==1) &
  (AFCARS$TotalRem==1) ### Entered is never missing, NA&FALSE = FALSE

AFCARS$reun_exit<-ifelse(AFCARS$DISREASN==1 & AFCARS$Exited ==1, 
                         TRUE, 
                         ifelse(is.na(AFCARS$DISREASN) & (AFCARS$Exited == 1), 
                                NA,
                                FALSE))

AFCARS<-AFCARS%>%
  select(FY, STATE, FIPSCODE, SEX,
         HISORGIN, AgeAtEnd, abuse, first_entry, reun_exit)
gc()

AFCARS<-recode_FIPS(AFCARS)

###### BRING IN MATT'S COUNTY DATA
pop_puma<-read_dta("./data/countykids00to16.dta")
pop_puma$FIPSCODE<-as.numeric(pop_puma$countyid)
############## selecting county-year level variables for imputation, going for many to improve prediction
pop_imputation_data<-pop_puma%>%
  select(FIPSCODE, year, cpop, cpnhw, cpnhb, cphisp, cpimmig, cpnoncit,
         cppoor, cpvpoor, cpworking, cped_lths, cpgkid, cpkhsg_own,
         cpmarfam, cphhsnap, chhsize, chhinc)
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
                                   (pop_imputation_data$year==2007)), ]
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

#### ALL MATCH EXCEPT NYC COUNTIES, 36005, 36047, 36081, 36085: 
#### merge all counts into 36061 (Manhattan) where FC cases are coded
#####################################################################################################################
### multilevel imputation using mice
### https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html
#####################################################################################################################
# 
AFCARS_merge$STATE<-factor(AFCARS_merge$STATE)
AFCARS_merge$FIPSCODE<-factor(AFCARS_merge$FIPSCODE)
AFCARS_merge$HISORGIN<-factor(AFCARS_merge$HISORGIN)
AFCARS_merge$SEX<-factor(AFCARS_merge$SEX)

samp<-sample(1:nrow(AFCARS_merge), 10000, replace=F)
AFCARS.test<-AFCARS_merge[samp,]
ini<-mice(AFCARS.test, m=1, maxit = 0)
#### SET PREDICTOR MATRIX - -2 = ID random intercept, 1 = fixed effect, 2 = random effect
pred<-ini$pred
pred[which(pred[, "FIPSCODE"]==1),"FIPSCODE"]<- 0
pred["AgeAtEnd",]<-0
pred[which(pred[, "STATE"]==1),"STATE"]<- -2
AFCARS.test$STATE<-as.integer(AFCARS.test$STATE)
pred[which(pred==1)]<-2

#### SET IMPUTATION METHODS
meth<-ini$method
meth[["AgeAtEnd"]]<-""
meth[which(meth=="logreg")]<-"2l.bin"

#### RUN TEST DATA
imps<-mice(AFCARS.test, 
           method = meth, 
           pred = pred,
           maxit = 1,
           m=1)