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
source("functions.r")
#setwd("Y:/NDACAN/NDACAN-RA/fre9/fc-deportation")

pop<-read_csv("./data/seer-child-pop-2000-2016.csv")


pop<-pop%>%
  select(-adult_pop, -state)%>%
  spread(HISORGIN, child_pop)%>%
  rename(child_hispanic=hispanic, child_nonhispanic=`non-hispanic`,
         FIPSCODE=FIPS,
         FY=year)

AFCARS<-read_csv("./data/afcars-deport00-16.csv", na="NULL")


AFCARS<-AFCARS%>%
  filter(!(is.na(FIPSCODE)), FIPSCODE!=9)%>%
  filter(STATE!=72)

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



AFCARS<-recode_FIPS(AFCARS)

AFCARS<-left_join(AFCARS, pop)

z<-AFCARS[which(is.na(AFCARS$child_nonhispanic)), ]
table(z$FIPSCODE, z$FY)

#### a few to fix

AFCARS<-AFCARS%>%
  mutate(child_hispanic=log(child_hispanic),
         child_nonhispanic=log(child_nonhispanic))

AFCARS$STATE<-factor(AFCARS$STATE)
AFCARS$HISORGIN<-factor(AFCARS$HISORGIN)
### multilevel imputation using mice
### https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html
# AFCARS.imp<-amelia(AFCARS, m = 3,
#                    p2s = 1, idvars = c("STATE", "FIPSCODE"),
#                    noms = c("HISORGIN"))

# samp<-sample(1:nrow(AFCARS), 10000, replace=F)
# AFCARS.test<-AFCARS[samp,]
# 
# ind.clust<-2
# pred<-ini$pred
#pred[, "STATE"]<-rep(0, nrow(pred))
# pred["HISORGIN", ]<-c(1, -2, 0, 0, 1, 1, 1, 1, 1, 1)
# pred["abuse", ]<-c(2, -2, 2, 2, 2, 0, 2, 2)
# pred["first_entry", ]<-c(2, -2, 2, 2, 2, 2, 0, 2)
# pred["reun_exit", ]<-c(2, -2, 2, 2, 2, 2, 2, 0)
# method<-c("", "", "", "2l.bin", "", "2l.bin",  "2l.bin",  "2l.bin")
#rm(ini)
#AFCARS.5m<-AFCARS[sample(1:nrow(AFCARS), 5000000, replace=F),]
states<-unique(AFCARS$STATE)
imp_out<-list()



for (i in 1:length(states)){
  
  AFCARS_state<-AFCARS%>%filter(STATE==states[i])
  ini<-mice(AFCARS_state, m=1, maxit = 0)
  ### IS USING FIPS WITHIN STATE, WILL BE CHALLENGING FOR TX, CA, GA, OTHER HIGH N COUNTY STATES
  ### TRY WITHOUT HERE, DO SOME EXPERIMENTS WITH IT FOR SOME STATES
  pred<-ini$predictorMatrix
  pred[, "FIPSCODE"]<-rep(0, nrow(pred))
  
  gc()
  imp_out[[i]]<-mice(AFCARS_state, 
                 pred=pred, 
                 #method = method, 
                 #m=1,
                 #maxit=1,
                 print=TRUE)
  
}

imp_dat_out<-mice::complete(imp_out[[1]], "long")

for(j in 1:length(states)){
  imp_dat_out<-rbind(imp_dat_out,
                         mice::complete(imp_out[[j]], "long"))
}

imp_dat_out<-imp_dat_out%>%
  arrange(.imp)

save.image("mice-test.Rdata")
gc()
q(save="no")


# write data
out<-rbind(AFCARS%>%
              mutate(.imp = "0"),
            imp_dat_out%>%
              select(-.id))

write_csv(out, "afcars_imputed_state_fe.csv")

dat<-read_csv("./afcars_imputed_state_fe.csv")

cnty<-dat_temp%>%
  group_by(.imp, FIPS, year, HISORGIN, report_source, state)%>%
  summarise(count=n())%>%
  spread(report_source, count, fill=0, sep="_")%>%
  ungroup()

# ### diagnostic visuals
# 
# diag<-rbind(AFCARS%>%
#               mutate(.imp = "6"),
#             imp_dat_out%>%
#               select(-.id))
# 
# 
# natl<-diag%>%
#   group_by(.imp, FY)%>%
#   summarise(mean_abuse = mean(abuse, na.rm=TRUE),
#             sd_abuse = sd(abuse, na.rm=TRUE),
#             mean_first_entry = mean(first_entry, na.rm=TRUE),
#             sd_first_entry = sd(first_entry, na.rm=TRUE),
#             mean_reun_exit = mean(reun_exit, na.rm=TRUE),
#             sd_reun_exit = sd(reun_exit, na.rm=TRUE),
#             mean_hisorgin = mean(HISORGIN == "hispanic", na.rm=TRUE),
#             sd_hisorgin = sd(HISORGIN == "hispanic", na.rm=TRUE))
# 
# ggplot(natl, aes(y=mean_abuse, x=FY, col=.imp))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse,
#     #     ymax = mean_abuse + 2*sd_abuse),
#     alpha = 0.8)+
#   geom_line(aes(y=sd_abuse,  col=.imp), lty=2, alpha =0.8)+
#   ylab("mean abuse, solid, sd abuse, dashed")
#   ggsave("natl_abuse_imp.png")
#   
# ggplot(natl, aes(y=mean_first_entry, x=FY, col=.imp))+
#     geom_line(#aes(ymin = mean_first_entry - 2* sd_first_entry,
#       #     ymax = mean_first_entry + 2*sd_first_entry),
#       alpha = 0.8)+
#     geom_line(aes(y=sd_first_entry,  col=.imp), lty=2, alpha =0.8)+
#     ylab("mean first_entry, solid, sd first_entry, dashed")+
#   ggsave("natl_first_entry_imp.png")
#   
# ggplot(natl, aes(y=mean_reun_exit, x=FY, col=.imp))+
#     geom_line(#aes(ymin = mean_reun_exit - 2* sd_reun_exit,
#       #     ymax = mean_reun_exit + 2*sd_reun_exit),
#       alpha = 0.8)+
#     geom_line(aes(y=sd_reun_exit,  col=.imp), lty=2, alpha =0.8)+
#     ylab("mean reun_exit, solid, sd reun_exit, dashed")+
#   ggsave("natl_reun_exit_imp.png")
#   
# ggplot(natl, aes(y=mean_hisorgin, x=FY, col=.imp))+
#     geom_line(#aes(ymin = mean_hisorgin - 2* sd_hisorgin,
#       #     ymax = mean_hisorgin + 2*sd_hisorgin),
#       alpha = 0.8)+
#     geom_line(aes(y=sd_hisorgin,  col=.imp), lty=2, alpha =0.8)+
#     ylab("mean hisorgin, solid, sd hisorgin, dashed")+
#   ggsave("natl_hisorgin_imp.png")
# 
# state<-diag%>%
#   group_by(.imp, FY, STATE)%>%
#   summarise(mean_abuse = mean(abuse, na.rm=TRUE),
#             sd_abuse = sd(abuse, na.rm=TRUE),
#             mean_first_entry = mean(first_entry, na.rm=TRUE),
#             sd_first_entry = sd(first_entry, na.rm=TRUE),
#             mean_reun_exit = mean(reun_exit, na.rm=TRUE),
#             sd_reun_exit = sd(reun_exit, na.rm=TRUE),
#             mean_hisorgin = mean(HISORGIN == "hispanic", na.rm=TRUE),
#             sd_hisorgin = sd(HISORGIN == "hispanic", na.rm=TRUE))
# 
# ggplot(state, aes(y=mean_abuse, x=FY, col=.imp))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse,
#     #     ymax = mean_abuse + 2*sd_abuse),
#     alpha = 0.8)+
#   geom_line(aes(y=sd_abuse,  col=.imp), lty=2, alpha =0.8)+
#   ylab("mean abuse, solid, sd abuse, dashed")+
#   facet_wrap(~STATE)+
# ggsave("state_abuse_imp.png")
# 
# ggplot(state, aes(y=mean_first_entry, x=FY, col=.imp))+
#   geom_line(#aes(ymin = mean_first_entry - 2* sd_first_entry,
#     #     ymax = mean_first_entry + 2*sd_first_entry),
#     alpha = 0.8)+
#   geom_line(aes(y=sd_first_entry,  col=.imp), lty=2, alpha =0.8)+
#   ylab("mean first_entry, solid, sd first_entry, dashed")+
#   facet_wrap(~STATE)+
#   ggsave("state_first_entry_imp.png")
# 
# ggplot(state, aes(y=mean_reun_exit, x=FY, col=.imp))+
#   geom_line(#aes(ymin = mean_reun_exit - 2* sd_reun_exit,
#     #     ymax = mean_reun_exit + 2*sd_reun_exit),
#     alpha = 0.8)+
#   geom_line(aes(y=sd_reun_exit,  col=.imp), lty=2, alpha =0.8)+
#   ylab("mean reun_exit, solid, sd reun_exit, dashed")+
#   facet_wrap(~STATE)+
#   ggsave("state_reun_exit_imp.png")
# 
# ggplot(state, aes(y=mean_hisorgin, x=FY, col=.imp))+
#   geom_line(#aes(ymin = mean_hisorgin - 2* sd_hisorgin,
#     #     ymax = mean_hisorgin + 2*sd_hisorgin),
#     alpha = 0.8)+
#   geom_line(aes(y=sd_hisorgin,  col=.imp), lty=2, alpha =0.8)+
#   ylab("mean hisorgin, solid, sd hisorgin, dashed")+
#   facet_wrap(~STATE)+
#   ggsave("state_hisorgin_imp.png")