#.rs.restartR()
rm(list=ls())
gc()

setwd("R:/Project/NCANDS/fc-deportation")
.libPaths("./library")

library(tidyverse)
library(mice)

set.seed(1)

ncands<-read_csv("./data/ncands-rptsrc-individual.csv", na="NA")

ncands$ethnicity[which(ncands$ethnicity=="unable")]<-NA
gc()

pop<-read_csv("./data/seer-child-pop-2000-2016.csv")
pop<-pop%>%
  select(-adult_pop)%>%
  spread(HISORGIN, child_pop)%>%
  rename(child_hispanic=hispanic, child_nonhispanic=`non-hispanic`)%>%
  mutate(child_hispanic = log(child_hispanic),
         child_nonhispanic = log(child_nonhispanic))

ncands<-ncands%>%
  rename(year = subyr,
         FIPS = RptFIPS,
         HISORGIN = ethnicity)%>%
  mutate(FIPS=as.numeric(FIPS))

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

merge<-left_join(ncands, pop)
merge<-merge%>%
  mutate(report_source = factor(report_source),
         HISORGIN = factor(HISORGIN))

states<-unique(merge$state)
years<-unique(merge$year)
gc()

#### make imputations and output

dat_temp<-merge%>%filter(state == states[1] 
                         #year == years [j]
)
ini<-mice(dat_temp, m=1, maxit = 0)
pred<-ini$pred
pred[, "FIPS"]<-0
imp_out<-mice(dat_temp, predictorMatrix = pred)
name<-paste("ncands_imp", index, ".csv", sep="")
dat_temp<-mice::complete(imp_out, "long", include=TRUE)
cnty<-dat_temp%>%
  group_by(.imp, FIPS, year, HISORGIN, report_source, state)%>%
  summarise(count=n())%>%
  spread(report_source, count, fill=0, sep="_")%>%
  ungroup()

cnty<-cnty%>%
  dplyr::select(-report_source_NA)

gc()

for(i in 2:length(states)){
  #for(j in 1:length(years)){
    print(states[[i]])
    #print(years[[j]])
    dat_temp<-merge%>%filter(state == states[i] 
                             #year == years [j]
                             )
    ini<-mice(dat_temp, m=1, maxit = 0)
    pred<-ini$pred
    pred[, "FIPS"]<-0
    imp_out<-mice(dat_temp, predictorMatrix = pred)
    dat_temp<-mice::complete(imp_out, "long", include=TRUE)
    cnty<-dat_temp%>%
      group_by(.imp, FIPS, year, HISORGIN, report_source, state)%>%
      summarise(count=n())%>%
      spread(report_source, count, fill=0, sep="_")%>%
      ungroup()
    
    cnty<-cnty%>%
      dplyr::select(-report_source_NA)

    gc()   
  #}
}

write_csv(cnty, "./ncands_cnty_imp.csv")


save.image("mice-ncands.Rdata")
# AFCARS<-AFCARS.imp$imputations[[1]]
# rm(AFCARS.imp)
gc()
# 
# ### complete imputation data
# imp_dat_out<-mice::complete(imp_out[[1]], "long")
# 
# for(j in 1:length(imp_out)){
#   imp_dat_out<-rbind(imp_dat_out,
#                      mice::complete(imp_out[[j]], "long"))
# }
# 
# imp_dat_out<-imp_dat_out%>%
#   arrange(.imp)
# 
# 
# # write data
# out<-rbind(ncands%>%
#              mutate(.imp = "0"),
#            imp_dat_out%>%
#              select(-.id))
# 
# write_csv(out, "ncands_imputed_state_fe.csv")

q(save="no")
