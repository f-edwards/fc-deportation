#.rs.restartR()
rm(list=ls())
gc()

library(mice)
library(pan)
library(tidyverse)

set.seed(1)

ncands<-read_csv("./data/ncands-comm-reports-individual.csv", na="NULL")

pop<-read_csv("./data/seer-child-pop-2000-2016.csv")
pop<-pop%>%
  select(-adult_pop, -state)%>%
  spread(HISORGIN, child_pop)%>%
  rename(child_hispanic=hispanic, child_nonhispanic=`non-hispanic`)

ncands$cethn<-factor(ncands$cethn, levels=c(1, 2, 3, 9),
                     labels = c("hispanic", 
                                "non-hispanic",
                                "unable",
                                "missing"))

ncands$cethn[which((ncands$cethn=="unable")|(ncands$cethn=="missing"))]<-NA
gc()
ncands$cethn<-factor(as.character(ncands$cethn))
gc()

ncands$comm_report[which(ncands$miss_report==1)]<-NA
ncands<-ncands%>%
  select(-miss_report)
gc()

ncands$comm_report<-ncands$comm_report==1

ncands<-ncands%>%
  rename(year = subyr,
         FIPS = RptFIPS,
         HISORGIN = cethn)%>%
  mutate(FIPS=as.numeric(FIPS))

ncands_test<-ncands%>%filter(year ==2014)

### multilevel imputation using mice
### https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html
# fips_samp<-sample(sample(unique(ncands$FIPS)), 100, replace=F)
# 
# samp<-ncands%>%
#   filter(FIPS%in%fips_samp)
# 
# samp<-samp[sample(1:nrow(samp), 100000, replace=F), ]
# samp<-samp%>%
#   mutate(FIPS=as.numeric(FIPS))

merge<-left_join(ncands_test, pop)%>%
  filter(!(is.na(child_hispanic)))

merge$FIPS<-factor(merge$FIPS)
ini<-mice(merge, m=1, maxit = 0)
pred<-ini$pred
#pred["cethn",]<-c(1, -2, 0, 1)
#meth<-c("", "", "2l.norm", "")

gc()
imp_test<-mice(merge, 
               pred=pred, 
               #method = meth, 
               print=TRUE, 
               m=1,
               maxit=1)
gc()

save.image("mice-test.Rdata")
q(save="no")
# AFCARS<-AFCARS.imp$imputations[[1]]
# rm(AFCARS.imp)
gc()
