#.rs.restartR()
rm(list=ls())
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(readr)
library(mice)
library(pan)

setwd("Y:/NDACAN/NDACAN-RA/fre9/fc-deportation")
AFCARS<-read_csv("./data/afcars-deport00-16.csv", na="NULL")

AFCARS<-AFCARS%>%
  filter(!(is.na(FIPSCODE)), FIPSCODE!=9)

AFCARS$HISORGIN<-ifelse(AFCARS$HISORGIN==3, NA,
                        ifelse((AFCARS$HISORGIN==2)|(AFCARS$HISORGIN==0), "non-hispanic",
                               ifelse(AFCARS$HISORGIN==1, "hispanic",
                                      AFCARS$HISORGIN)))
gc()

### Filter out cases with any cat not relevant, should give upper bound on incap removals 
### abuse _index<- [PHYABUSE, SEXABUSE, AAPARENT, DAPARENT, AACHILD, DACHILD, CHILDIS, PRTSDIED]
AFCARS$abuse<-with(AFCARS, (PHYABUSE==1) | 
                          (SEXABUSE==1) | 
                          (AAPARENT==1) | 
                          (DAPARENT==1) | 
                          (AACHILD==1) | 
                          (DACHILD==1) | 
                          (CHILDIS==1) | 
                          (PRTSDIED==1))

AFCARS$first_entry<- (AFCARS$Entered==1) &
  (AFCARS$TotalRem==1) ### Entered is never missing, NA&FALSE = FALSE

AFCARS$reun_exit<-ifelse(AFCARS$DISREASN==1, 
                         TRUE, 
                         ifelse((AFCARS$DISREASN==99) & (AFCARS$Exited == 1), 
                                NA,
                                FALSE))

AFCARS<-AFCARS%>%
  select(FY, STATE, FIPSCODE,
         HISORGIN, AgeAtEnd, abuse, first_entry, reun_exit)
gc()
### multilevel imputation using mice
### https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html
# AFCARS.imp<-amelia(AFCARS, m = 3,
#                    p2s = 1, idvars = c("STATE", "FIPSCODE"),
#                    noms = c("HISORGIN"))

samp<-sample(1:nrow(AFCARS), 1000, replace=F)
AFCARS.test<-AFCARS[samp,]

ind.clust<-2
ini<-mice(AFCARS.test, m=1, maxit = 0)
pred<-ini$pred
pred["HISORGIN", ]<-c(2, -2, 2, 0, 2, 2, 2, 2)
pred["abuse", ]<-c(2, -2, 2, 2, 2, 0, 2, 2)
pred["first_entry", ]<-c(2, -2, 2, 2, 2, 2, 0, 2)
pred["reun_exit", ]<-c(2, -2, 2, 2, 2, 2, 2, 0)
method<-c("", "", "", "2l.bin", "", "2l.bin",  "2l.bin",  "2l.bin")
#rm(ini)
gc()
imp_test<-mice(AFCARS.test, pred=pred, method = method, print=TRUE)
gc()
save.image("mice-test.Rdata")

# AFCARS<-AFCARS.imp$imputations[[1]]
# rm(AFCARS.imp)
gc()

save.image("AFCARS-no-abuse-imputation.Rdata")

write.amelia(AFCARS.imp, separate = TRUE, file.stem = "afcars-imp",
             extension = ".csv")
