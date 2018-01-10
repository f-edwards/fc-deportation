# source("read.r") #### to read in data files if starting from scratch

library(data.table)
library(tidyverse)
library(haven)

### hypotheses: 1) Deportation -> more first entries
###             2) Deportation -> more total entries
###             3) Deportation -> fewer reunification exits
###             4) Deportation -> higher caseloads  
###             5) Deportation -> lower community reporting

#make ts for missingness by state
# TS<-dat%>%
#   group_by(STATE, FY)%>%
#   summarise(total.cases=n(),
#             prop.missing=sum(HISORGIN==3)/n(),
#             prop.lat=sum(HISORGIN==1)/n())
# 
# ts<-ggplot(TS, aes(x=FY, y=prop.missing))+
#   geom_line()+
#   facet_wrap(~STATE)
# 
# ggsave("c:/Users/fre9/Documents/ts.pdf", ts)

#make county level file - want child race/ethn, 1st entry, all entries, remrsn, rptsrc from ncands


################# for entries by first entry, by incapacitation
cntyFC<-AFCARS%>%
  group_by(FIPSCODE, FY, HISORGIN, incap_index, first_entry)%>%
  filter(Entered==1)%>%
  count()%>%
  rename("first_entries"=n)%>%
  left_join(AFCARS%>%
              group_by(FIPSCODE, FY, HISORGIN)%>%
              filter(Entered==1)%>%
              count()%>%
              rename("all_entries"=n))%>%
  rename(year=FY, FIPS=FIPSCODE)%>%
  ungroup()


# create all and first entries by hisorgin

cntyFC<-left_join(cntyFC%>%
                    select(-all_entries)%>%
                    spread(HISORGIN, first_entries, fill=0)%>%
                    rename(hisp_first_entries=hispanic, non_first_entries=`non-hispanic`, miss_first_entries=`<NA>`),
                  cntyFC%>%
                    select(-first_entries)%>%
                    spread(HISORGIN, all_entries, fill=0)%>%
                    rename(hisp_all_entries=hispanic, non_all_entries=`non-hispanic`, miss_all_entries=`<NA>`))

cntyFC<-left_join(cntyFC, incap_fc)
cntyFC[is.na(cntyFC$incap_hisp_entries), "incap_hisp_entries"]<-0

cnty287g<-read_dta("county287g.dta")
cnty287g$countyid<-as.numeric(cnty287g$countyid)
cnty287g$year<-as.numeric(cnty287g$year)
cnty287g$c287active<-as.logical(cnty287g$c287active)
cnty287g<-cnty287g%>%
  rename(FIPS=countyid)%>%
  select(-c287deny_outcome)

### join on all with ever application
dat<-right_join(cntyFC, cnty287g)
dat<-left_join(dat, pop_wide)

dat<-dat%>%
  mutate(hisp_all_ent_rate=hisp_all_entries/hisp_child_pop,
         hisp_first_ent_rate=hisp_first_entries/hisp_child_pop,
         prop.missing=miss_all_entries/(hisp_all_entries+non_all_entries+miss_all_entries),
         detention_rate=c287_identify/hisp_adult_pop)

## line plot on entries pc by year by ever applied, by denied, by active


TS1<-ggplot(dat%>%
              group_by(c287active, year)%>%
              summarise(hisp_first_ent_rate=mean(hisp_first_ent_rate, na.rm=TRUE)), 
            aes(x=year, y=hisp_first_ent_rate * 1000, col=c287active))+
  geom_line()+
  xlab("Year")+
  ylab("Latino foster care entries per 1,000 children")
  
## scatter on entries pc vs detentions pc

scatter1<-ggplot(dat%>%
                   filter(c287active==TRUE),
                 aes(x=detention_rate * 1000, y=hisp_first_ent_rate*1000))+
  geom_point()+
  xlab("287g detentions per 1,000 Latino adults")+
  ylab("Latino foster care entries per 1,000 children")

## do compare on FC rates for high vs not-high missing counties on latFC entries
test<-dat%>%
  filter(non_all_entries>0)%>%
  mutate(prop.missing=miss_all_entries/(hisp_all_entries+non_all_entries+miss_all_entries))%>%
  mutate(above_mean=prop.missing>mean(prop.missing),
         above_90=prop.missing>quantile(prop.missing, 0.90))%>%
  mutate(hisp_ent_rate=hisp_all_entries/hisp_child_pop)

t1<-test%>%
  group_by(above_mean)%>%
  summarise(hisp_ent_rate=mean(hisp_ent_rate), 
            hisp_child_pop=mean(hisp_child_pop))

t2<-test%>%
  group_by(above_90)%>%
  summarise(hisp_ent_rate=mean(hisp_ent_rate), 
            hisp_child_pop=mean(hisp_child_pop))

t.test(hisp_ent_rate~above_mean, data=test)
t.test(hisp_ent_rate~above_90, data=test)
## weird - high missing counties have high entry rates, much higher mean entry rate than low-missing counties at mean threshold


### EVENT PLOTS
### produce event study graph - center time at 287gActive
ESplot<-dat%>%
  filter(c287deny==0)%>%
  group_by(FIPS)%>%
  mutate(ES_time=year-c287start_yr)%>%
  mutate(entry_rate=hisp_first_entries/hisp_child_pop * 1000)

ESplot<-ESplot%>%
  ungroup()%>%
  group_by(ES_time)%>%
  summarise(mean=mean(entry_rate), lower=quantile(entry_rate, 0.25), upper=quantile(entry_rate, 0.75))%>%
  filter(ES_time>-4, ES_time<4)%>%
  filter(!(is.na(ES_time)))

ESline<-ggplot(ESplot, 
               aes(x=ES_time, y=mean))+
  geom_line()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  geom_vline(xintercept = 0, lty=2)+
  scale_x_continuous(breaks=c(-3, -2, -1, 0, 1, 2, 3))+
  coord_cartesian(ylim=c(0, 5))+
  ylab("Latino foster care entries per 1,000 children, 50 percent interval")+
  xlab("Years from 287g implementation")

ESplot<-dat%>%
  filter(c287deny==0)%>%
  group_by(FIPS)%>%
  mutate(ES_time=year-c287start_yr)%>%
  mutate(incap_rate=incap_hisp_entries/hisp_child_pop * 1000)

ESplot<-ESplot%>%
  ungroup()%>%
  group_by(ES_time)%>%
  summarise(mean=mean(incap_rate), lower=quantile(incap_rate, 0.25), upper=quantile(incap_rate, 0.75))%>%
  filter(ES_time>-4, ES_time<4)%>%
  filter(!(is.na(ES_time)))

ESincap<-ggplot(ESplot, 
               aes(x=ES_time, y=mean))+
  geom_line()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  geom_vline(xintercept = 0, lty=2)+
  scale_x_continuous(breaks=c(-3, -2, -1, 0, 1, 2, 3))+
  coord_cartesian(ylim=c(0, 1))+
  ylab("Latino incapacitation entries per 1,000 children, 50 percent interval")+
  xlab("Years from 287g implementation")

TSplot<-dat%>%
  group_by(FIPS)%>%
  mutate(entry_rate=hisp_all_entries/hisp_child_pop * 1000)%>%
  ungroup()%>%
  group_by(c287active)%>%
  summarise(mean=mean(entry_rate, na.rm=TRUE), lower=quantile(entry_rate, 0.25, na.rm=TRUE), upper=quantile(entry_rate, 0.75, na.rm=TRUE))

ggsave("event-plot.png", ESline)
ggsave("detention-fc.png", scatter1)
ggsave("ts1.png", TS1)
ggsave("incap-event-plot.png", ESincap)

missing<-ggplot(dat, 
                aes(x=year, y=prop.missing))+
  geom_line()+
  facet_wrap(~countyname)

ggsave("missing-by-county.pdf", missing, height=30, width=30)

reg1<-lm((hisp_all_ent_rate)~c287active + factor(FIPS) + factor(year), data=dat)