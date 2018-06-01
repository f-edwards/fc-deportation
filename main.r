# source("read.r") #### to read in data files if starting from scratch
rm(list=ls())
gc()
library(tidyverse)
library(haven)
setwd("U:/fc-deportation")
### hypotheses: 1) Deportation -> more first entries
###               1a) Deportation -> more first entries, incapacitation cause
###               1b) Deportation -> more total entries
###             2) Deportation -> higher caseloads
###               2a) Deportation -> fewer reunification exits
###             3) Deportation -> lower community reporting

#### read in imputed afcars data
files<-list.files("./data")
imputeds<-paste("./data/", files[grep("cnty", files)], sep="")

afcars<-read_csv(imputeds[1])

for(i in 2:length(imputeds)){
  temp<-read.csv(imputeds[i])
  afcars<-afcars%>%
    rbind(temp)
}

### sort out observed, convert to wide by hisorgin
afcars<-afcars%>%
  filter(.imp!=0)%>%
  rename(fips = fipscode)
names(afcars)<-tolower(names(afcars))
afcars$hisorgin<-recode(afcars$hisorgin,
                        "hispanic"="h",
                        "non-hispanic"="nh")
afcars_long<-afcars
afcars_out<-afcars_long%>%select(.imp, state, fips, year)%>%
  distinct()
index<-6:13
for(i in index){
  temp<-afcars_long[c(1:5, i)]
  var<-names(afcars_long)[i]
  temp<-temp%>%
    spread(hisorgin, eval(var))
  names(temp)[5:6]<-paste(var, names(temp[5:6]), sep="_")
  ### spread generates na for unobserved
  temp[is.na(temp)]<-0
  afcars_out<-afcars_out%>%
    left_join(temp)
}

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

pop_puma<-read_dta("./data/countykids00to16.dta")
pop_puma<-pop_puma%>%
  select(countyid, countyname, year, cpop, ckid, chisp, ckid_hisp, 
         cimmig, cnoncit, cimmig_hisp, cnoncit_hisp,
         cphisp, cpnhw, cpimmig, cpnoncit,
         cppov_nhw, cppov_hisp, cpknolivespar_hisp, cpknolivespar_nhw,
         cunauth, cuahisp, cpunauth, cpfbunauth, cpuahisp, cpfbuahisp)
pop_puma$fips<-as.numeric(pop_puma$countyid)
pop_puma$year<-as.integer(pop_puma$year)

### join on all with ever application
dat<-left_join(afcars_out, pop_puma)

########################################################
## Evaluate fc time series
dat<-dat%>%
  mutate(ckid_nhisp=ckid - ckid_hisp)


  
              

# ### we lose some counties because of puma data
# table(cnty287g$FIPS%in%pop_puma$FIPS)
# unique(cnty287g[which(!(cnty287g$FIPS%in%pop_puma$FIPS)), "FIPS"])
dat<-left_join(dat, cnty287g)



write_csv(dat, "./data/dat_merge.csv")

#################
## filter only cases with 287g apps


# ## line plot on entries pc by year by ever applied, by denied, by active
# 
# TS1<-ggplot(dat%>%
#               group_by(c287active, year)%>%
#               summarise(entry_rate_h=sum(first_entry_h) / sum(ckid_hisp),
#                         entry_rate_nh=sum(first_entry_nh)/sum(ckid - ckid_hisp)), 
#             aes(x=year, y=entry_rate_h * 1000, col=c287active))+
#   geom_line()+
#   xlab("Year")+
#   ylab("Latino foster care entries per 1,000 children")
#   
# # ## scatter on entries pc vs detentions pc
# # 
# scatter1<-ggplot(dat%>%
#                    filter(c287active==TRUE),
#                  aes(x=detention_rate * 1000, y=hisp_first_ent_rate*1000))+
#   geom_point()+
#   xlab("287g detentions per 1,000 Latino adults")+
#   ylab("Latino foster care entries per 1,000 children")
# 
# ## do compare on FC rates for high vs not-high missing counties on latFC entries
# test<-dat%>%
#   filter(non_all_entries>0)%>%
#   mutate(prop.missing=miss_all_entries/(hisp_all_entries+non_all_entries+miss_all_entries))%>%
#   mutate(above_mean=prop.missing>mean(prop.missing),
#          above_90=prop.missing>quantile(prop.missing, 0.90))%>%
#   mutate(hisp_ent_rate=hisp_all_entries/hisp_child_pop)
# 
# t1<-test%>%
#   group_by(above_mean)%>%
#   summarise(hisp_ent_rate=mean(hisp_ent_rate), 
#             hisp_child_pop=mean(hisp_child_pop))
# 
# t2<-test%>%
#   group_by(above_90)%>%
#   summarise(hisp_ent_rate=mean(hisp_ent_rate), 
#             hisp_child_pop=mean(hisp_child_pop))
# 
# t.test(hisp_ent_rate~above_mean, data=test)
# t.test(hisp_ent_rate~above_90, data=test)
# ## weird - high missing counties have high entry rates, much higher mean entry rate than low-missing counties at mean threshold


### EVENT PLOTS
### produce event study graph - center time at 287gActive
##### assume continuation of pre-treatment time trend

#### weird, some showing up with 0 ckid_hisp
#### which(dat$first_entry_h>0 & dat$ckid_hisp==0)
### dropping for now

dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  filter(.imp==1)%>% ### change this for full spec
  mutate(caseload_h = caseload_h / (ckid_hisp+1) * 1000)%>%
  select(fips, year, c287start_yr, caseload_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))

##################
## de trending
# fit county intercept, random slope for all non-287 active counties
# then predict lines for active counties, measure diff?
library(lme4)
rs0<-lmer(sqrt(caseload_h) ~ 1 + year + 
            (year|fips),
          dat=dat_rate%>%
            filter(c287active==FALSE))
###### pull out only 287 active county-years for prediction
###### all fips are in, new years
new_data<-dat_rate
ranefs<-ranef(rs0)$fips
ranefs$fips<-row.names(ranefs)
names(ranefs)<-c("int", "slope", "fips")
#### add FEs to RE estimates
ranefs$int <- ranefs$int + fixef(rs0)[1]
ranefs$slope<-ranefs$slope + fixef(rs0)[2]
#### bind REs to new data
dat_detrend<-left_join(dat_rate, ranefs%>%
                      mutate(fips = as.numeric(fips)))

dat_detrend$pred<-(dat_detrend$int + dat_detrend$slope * dat_detrend$year)^2

ggplot(dat_detrend,
       aes(x = caseload_h, y=pred))+
  geom_point()+
  geom_abline(slope=1)+
  facet_wrap(~c287active)

dat_detrend$caseload_h_detrend<-dat_detrend$caseload_h - dat_detrend$pred
qplot(dat_detrend$caseload_h_detrend)
qplot(dat_detrend%>%filter(c287active==TRUE)%>%select(caseload_h_detrend))


ESplot<-dat_detrend%>%
  mutate(ES_time=(year+2000)-c287start_yr)

ESplot_sum<-ESplot%>%
  ungroup()%>%
  group_by(ES_time)%>%
  summarise(mean=mean(caseload_h_detrend))

ES287cl_detrend<-ggplot(ESplot_sum%>%
                filter(!(is.na(ES_time))& ES_time>-5), 
              aes(x=ES_time, y=mean))+
  geom_line()+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Diff from predicted")+
  xlab("Years from 287g implementation")+
  ggtitle("De-trended data using year|fips model on c287active==FALSE")+
  ggsave("es287cl_detrend.png")

########################## OBSERVED EVENT PLOTS

ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  ungroup()%>%
  group_by(ES_time)%>%
  summarise(mean=mean(caseload_h))
ES287cl<-ggplot(ESplot_sum%>%
                filter(!(is.na(ES_time))& ES_time>-5), 
              aes(x=ES_time, y=mean))+
  geom_line()+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care caseloads")+
  xlab("Years from 287g implementation")+
  ggtitle("Observed data using year|fips model on c287active==FALSE")+
  ggsave("es287cl.png")

#### entries
dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  mutate(entered_h = entered_h / (ckid_hisp+1) * 1000)%>%
  select(.imp, fips, year, c287start_yr, entered_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))
ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  group_by(ES_time, .imp)%>%
  summarise(median=median(entered_h),
            upper = quantile(entered_h, 0.95),
            lower = quantile(entered_h, 0.05))
ES287cl<-ggplot(ESplot_sum%>%
                  filter(!(is.na(ES_time))& ES_time>-5), 
                aes(x=ES_time, y=median, group= .imp))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care entries")+
  xlab("Years from 287g implementation")+
  ggtitle("entries")+
  ggsave("es287entered.png")

#### abuse
dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  mutate(abuse_h = abuse_h / (ckid_hisp+1) * 1000)%>%
  select(.imp, fips, year, c287start_yr, abuse_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))
ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  group_by(ES_time, .imp)%>%
  summarise(median=median(abuse_h),
            upper = quantile(abuse_h, 0.95),
            lower = quantile(abuse_h, 0.05))
ES287cl<-ggplot(ESplot_sum%>%
                  filter(!(is.na(ES_time))& ES_time>-5), 
                aes(x=ES_time, y=median, group= .imp))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care entries")+
  xlab("Years from 287g implementation")+
  ggtitle("abuse")+
  ggsave("es287abuse.png")

#### first_entry
dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  mutate(first_entry_h = first_entry_h / (ckid_hisp+1) * 1000)%>%
  select(.imp, fips, year, c287start_yr, first_entry_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))
ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  group_by(ES_time, .imp)%>%
  summarise(median=median(first_entry_h),
            upper = quantile(first_entry_h, 0.95),
            lower = quantile(first_entry_h, 0.05))
ES287cl<-ggplot(ESplot_sum%>%
                  filter(!(is.na(ES_time))& ES_time>-5), 
                aes(x=ES_time, y=median, group= .imp))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care entries")+
  xlab("Years from 287g implementation")+
  ggtitle("first_entry")+
  ggsave("es287first_entry.png")


#### reun_exit
dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  mutate(reun_exit_h = reun_exit_h / (caseload_h+1) * 1000)%>%
  select(.imp, fips, year, c287start_yr, reun_exit_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))
ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  group_by(ES_time, .imp)%>%
  summarise(median=median(reun_exit_h),
            upper = quantile(reun_exit_h, 0.95),
            lower = quantile(reun_exit_h, 0.05))
ES287cl<-ggplot(ESplot_sum%>%
                  filter(!(is.na(ES_time))& ES_time>-5), 
                aes(x=ES_time, y=median, group= .imp))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care entries")+
  xlab("Years from 287g implementation")+
  ggtitle("reun_exit")+
  ggsave("es287reun_exit.png")


#### incar
dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  mutate(incar_h = incar_h / (ckid_hisp+1) * 1000)%>%
  select(.imp, fips, year, c287start_yr, incar_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))
ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  group_by(ES_time, .imp)%>%
  summarise(median=median(incar_h),
            upper = quantile(incar_h, 0.95),
            lower = quantile(incar_h, 0.05))
ES287cl<-ggplot(ESplot_sum%>%
                  filter(!(is.na(ES_time))& ES_time>-5), 
                aes(x=ES_time, y=median, group= .imp))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care entries")+
  xlab("Years from 287g implementation")+
  ggtitle("incar")+
  ggsave("es287incar.png")


#### incap_broad
dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  mutate(incap_broad_h = incap_broad_h / (ckid_hisp+1) * 1000)%>%
  select(.imp, fips, year, c287start_yr, incap_broad_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))
ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  group_by(ES_time, .imp)%>%
  summarise(median=median(incap_broad_h),
            upper = quantile(incap_broad_h, 0.95),
            lower = quantile(incap_broad_h, 0.05))
ES287cl<-ggplot(ESplot_sum%>%
                  filter(!(is.na(ES_time))& ES_time>-5), 
                aes(x=ES_time, y=median, group= .imp))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care entries")+
  xlab("Years from 287g implementation")+
  ggtitle("incap_broad")+
  ggsave("es287incap_broad.png")


#### neglect
dat_rate<-dat%>%
  filter(ckid_hisp>0)%>%
  mutate(neglect_h = neglect_h / (ckid_hisp+1) * 1000)%>%
  select(.imp, fips, year, c287start_yr, neglect_h, c287active)%>%
  mutate(year = year - 2000)%>%
  mutate(c287active = ifelse(is.na(c287active), FALSE, c287active))
ESplot<-dat_rate%>%
  mutate(ES_time=(year+2000)-c287start_yr)
ESplot_sum<-ESplot%>%
  group_by(ES_time, .imp)%>%
  summarise(median=median(neglect_h),
            upper = quantile(neglect_h, 0.95),
            lower = quantile(neglect_h, 0.05))
ES287cl<-ggplot(ESplot_sum%>%
                  filter(!(is.na(ES_time))& ES_time>-5), 
                aes(x=ES_time, y=median, group= .imp))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_vline(xintercept = 0, lty=2)+
  ylab("Observed hispanic foster care entries")+
  xlab("Years from 287g implementation")+
  ggtitle("neglect")+
  ggsave("es287neglect.png")


dat_natl<-dat%>%
  group_by(.imp, year)%>%
  summarise(abuse_h = sum(abuse_h)/sum(ckid_hisp),
            first_entry_h = sum(first_entry_h)/sum(ckid_hisp),
            caseload_h = sum(caseload_h)/sum(ckid_hisp),
            abuse_nh = sum(abuse_nh)/sum(ckid_nhisp),
            first_entry_nh = sum(first_entry_nh)/sum(ckid_nhisp),
            caseload_nh = sum(caseload_nh)/sum(ckid_nhisp))



TS0<-ggplot(dat_natl,
            aes(x=year, group=.imp))+
  geom_line(aes(y=caseload_h), col=2)+
  geom_line(aes(y=caseload_nh), col=1)+
  ggtitle("National caseloads, hispanic in red")+
  ggsave("nationalTS.png")
