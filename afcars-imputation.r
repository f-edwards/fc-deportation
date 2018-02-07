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

AFCARS<-left_join(AFCARS, pop)
AFCARS<-AFCARS%>%
  filter(!is.na(child_hispanic))

AFCARS$STATE<-factor(AFCARS$STATE)
AFCARS$HISORGIN<-factor(AFCARS$HISORGIN)
### multilevel imputation using mice
### https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html
# AFCARS.imp<-amelia(AFCARS, m = 3,
#                    p2s = 1, idvars = c("STATE", "FIPSCODE"),
#                    noms = c("HISORGIN"))

samp<-sample(1:nrow(AFCARS), 10000, replace=F)
AFCARS.test<-AFCARS[samp,]

ind.clust<-2
ini<-mice(AFCARS.test, m=1, maxit = 0)
pred<-ini$pred
#pred[, "STATE"]<-rep(0, nrow(pred))
# pred["HISORGIN", ]<-c(1, -2, 0, 0, 1, 1, 1, 1, 1, 1)
# pred["abuse", ]<-c(2, -2, 2, 2, 2, 0, 2, 2)
# pred["first_entry", ]<-c(2, -2, 2, 2, 2, 2, 0, 2)
# pred["reun_exit", ]<-c(2, -2, 2, 2, 2, 2, 2, 0)
# method<-c("", "", "", "2l.bin", "", "2l.bin",  "2l.bin",  "2l.bin")
#rm(ini)
#AFCARS.5m<-AFCARS[sample(1:nrow(AFCARS), 5000000, replace=F),]
gc()
imp_test<-mice(AFCARS, 
               pred=pred, 
               #method = method, 
               #m=1,
               #maxit=1,
               print=TRUE)
gc()
save.image("mice-test.Rdata")

# AFCARS<-AFCARS.imp$imputations[[1]]
# rm(AFCARS.imp)
gc()

# save.image("AFCARS-no-abuse-imputation.Rdata")
# 
# write.amelia(AFCARS.imp, separate = TRUE, file.stem = "afcars-imp",
#              extension = ".csv")

# ### diagnostic visuals
# 
# diag<-bind_rows(AFCARS%>%mutate(abuse = as.numeric(abuse),
#                                 first_entry = as.numeric(first_entry),
#                                 reun_exit = as.numeric(reun_exit),
#                                 data="orig"),
#                 AFCARS.imp$imputations[[1]]%>%mutate(data="imp1"),
#                 AFCARS.imp$imputations[[2]]%>%mutate(data="imp2"),
#                 AFCARS.imp$imputations[[3]]%>%mutate(data="imp3"))
# 
# rm(AFCARS.imp)
# rm(AFCARS)
# gc()
# 
# natl<-diag%>%
#   group_by(data, FY)%>%
#   summarise(mean_abuse = mean(abuse, na.rm=TRUE),
#             sd_abuse = sd(abuse, na.rm=TRUE),
#             mean_first_entry = mean(first_entry, na.rm=TRUE),
#             sd_first_entry = sd(first_entry, na.rm=TRUE),
#             mean_reun_exit = mean(reun_exit, na.rm=TRUE),
#             sd_reun_exit = sd(reun_exit, na.rm=TRUE))
# 
# ggplot(natl, aes(y=mean_abuse, x=FY, col=data))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse, 
#     #     ymax = mean_abuse + 2*sd_abuse),
#     alpha = 0.8)+
#   ggsave("natl_abuse_imp.png")
# 
# ggplot(natl, aes(y=mean_first_entry, x=FY, col=data))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse, 
#     #     ymax = mean_abuse + 2*sd_abuse),
#     alpha = 0.8)+
#   ggsave("natl_first_imp.png")
# 
# ggplot(natl, aes(y=mean_reun_exit, x=FY, col=data))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse, 
#     #     ymax = mean_abuse + 2*sd_abuse),
#     alpha = 0.8)+
#   ggsave("natl_reun_imp.png")
# 
# state<-diag%>%
#   group_by(data, STATE, FY)%>%
#   summarise(mean_abuse = mean(abuse, na.rm=TRUE),
#             sd_abuse = sd(abuse, na.rm=TRUE),
#             mean_first_entry = mean(first_entry, na.rm=TRUE),
#             sd_first_entry = sd(first_entry, na.rm=TRUE),
#             mean_reun_exit = mean(reun_exit, na.rm=TRUE),
#             sd_reun_exit = sd(reun_exit, na.rm=TRUE))
# 
# ggplot(state, aes(y=mean_abuse, x=FY, col=data))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse, 
#              #     ymax = mean_abuse + 2*sd_abuse),
#               alpha = 0.8)+
#   facet_wrap(~STATE)+
#   ggsave("state_abuse_imp.png")
# 
# ggplot(state, aes(y=mean_first_entry, x=FY, col=data))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse, 
#     #     ymax = mean_abuse + 2*sd_abuse),
#     alpha = 0.8)+
#   facet_wrap(~STATE)+
#   ggsave("state_first_imp.png")
# 
# ggplot(state, aes(y=mean_reun_exit, x=FY, col=data))+
#   geom_line(#aes(ymin = mean_abuse - 2* sd_abuse, 
#     #     ymax = mean_abuse + 2*sd_abuse),
#     alpha = 0.8)+
#   facet_wrap(~STATE)+
#   ggsave("state_reun_imp.png")