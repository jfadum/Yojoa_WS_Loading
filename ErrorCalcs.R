
#Required packages

library(tidyverse)
library(signal)
library(xts)
library(dygraphs) 
library(RcppRoll)
library(imputeTS)
library(ggthemes)
library(broom)
library(tsibble)
library(GGally)
library(tidyr)
library(lubridate)
library(readr)
library(plotly) 
library(ggthemes)



#Can't calculate error for Cacao and Yure, 
#but here we calculate the potential MAX contributions of the other four tributaries

nutes <- read_csv('13OCT21_Rivers_JFedits.csv') %>% 
  mutate(datetime = mdy_hms(paste(date,'12:00:00')),
         date = as.Date(datetime),
         tp = as.numeric(tp))

nh4<- nutes %>% 
  select(date, site, nh4_n, datetime) %>% 
  group_by(date, site) %>% 
  mutate(nh4_n= mean(nh4_n)) %>% 
  unique()

no3<- nutes %>% 
  select(date, site, no3_n, datetime) %>% 
  group_by(date, site) %>% 
  mutate(no3_n= mean(no3_n)) %>% 
  unique()  

tp<- nutes %>% 
  select(date, site, tp, datetime) %>% 
  group_by(date, site) %>% 
  mutate(tp= mean(tp)) %>% 
  unique() 

doc<- nutes %>% 
  select(date, site, doc, datetime) %>% 
  group_by(date, site) %>% 
  mutate(doc= mean(doc)) %>% 
  unique() 

tdn<- nutes %>% 
  select(date, site, tdn, datetime) %>% 
  group_by(date, site) %>% 
  mutate(tdn= mean(tdn)) %>% 
  unique() 

nutes<-cbind(nh4, no3[,3], tp[,3], doc[,3], tdn[,3])
#now that we only have one observation per day we can join ...

q_preds <- jumped_staff %>%
  rename(manual_stage = jumped_stage) %>%
  group_by(site) %>% 
  nest() %>%
  rename(sensor_data = data) %>%
  inner_join(q_mods,by = 'site') %>%
  mutate(preds = map2(q_mods,sensor_data,q.predict)) %>%
  select(-q_mods,-q_sum,-q_tidy,-data) %>%
  unnest(c(sensor_data,preds)) %>%
  mutate(across(ends_with('_q'), ~ ifelse(response == 'log',exp(.),.))) %>%
  dplyr::filter(datetime > mdy_hms('04/01/2019 00:00:00'),
                across(ends_with('_q'), ~ . < 10)) %>%
  mutate(date = as.Date(datetime)) 

sensor_qs <- q_preds %>%
  as_tibble() %>%
  group_by(date,site) %>%
  summarize(q = mean(max_q,na.rm=T))

sensor_qs<- as.data.frame(sensor_qs)
cacao_q<- read_csv("q_cacao_2019.csv")
cacao_q$date<- mdy(cacao_q$date)
cacao_q<- cacao_q %>% 
  mutate(site= "cacao") %>% 
  select(date, site, q) %>% 
  drop_na()

yure_q<- read_csv("q_yure_2019.csv")
yure_q$date<- mdy(yure_q$date)
yure_q<- yure_q %>% 
  mutate(site= "yure") %>% #add site so can combine data sets
  drop_na()


daily_q<- rbind(sensor_qs, cacao_q, yure_q)

complete <- daily_q %>%
  full_join(nutes %>% select(-site,-datetime)) %>%
  group_by(site) %>%
  mutate(interpolated = ifelse(is.na(doc),'Yes','No')) %>%
  mutate(across(c('nh4_n','no3_n','tp','doc','tdn'),
                ~na_interpolation(.))) %>%
  arrange(site,date) %>%
  dplyr::filter(date < ymd('2020-04-01'),
                date > ymd('2019-04-01')) %>% 
  filter(site != "Canal")


complete_long <- complete %>%
  pivot_longer(cols = nh4_n:tdn) %>% 
  mutate(month= month(date)) 

fluxes <- complete_long%>%
  #Convert Q from m3s to lpd
  mutate(q_lpd = q*1000*60*60*24) %>%
  #Convert nutes from mg/L to kg/l
  mutate(kgl = value/(1000*1000)) %>%
  #Convert to daily flux
  mutate(kg_day = q_lpd*kgl) %>% 
  drop_na()

max_flux<- fluxes %>%   #create new dataframe to easily trim dates and sum by month
  #  subset(date> "2019-04-01" & date < "2020-04-01") %>% 
  mutate(month=month(date))  %>% 
  select(site, name, kg_day, month) %>% 
  group_by(site,name, month) %>% 
  summarise(value=sum(kg_day))  

med_flux<-read_csv('med_calc_fluxes.csv') %>% 
  rename(med_load= value)


error_calc<- full_join(max_flux, med_flux, by= c("site", "name", "month")) %>% 
  mutate(percent_dif= ((value-med_load)/med_load)*100,
         absolute_dif= value-med_load)


N_P_aqua <- read_csv("N_P_aqua.csv")

N_aqua <- N_P_aqua %>% 
  rename(month= mes_2013,
         value= N_kg) %>% 
  mutate(site= "Aquaculture") %>% 
  select(site, value ) %>% 
  group_by(site) %>% 
  mutate(value=sum(value)) %>% 
  unique()

tdn <- max_flux %>% 
  dplyr::filter(name =="tdn") %>% 
  group_by(site) %>% 
  summarise(value=sum(value)) %>% 
  mutate(norm= c(1,.131,1,1,.934,.126),
         max_estimate= value/norm,
         dif= max_estimate-value) 

underestimate<- tdn %>% 
  mutate(value=sum(tdn$dif)) %>% 
  select(value) %>% 
  unique() %>% 
  mutate(site= "X estimate") %>% 
  select(site, value)


tdn<- tdn %>% 
  select(site, value)

N_loading<- rbind(tdn, N_aqua, underestimate)

N_loading<- N_loading%>% 
  arrange(desc(site)) %>%
  mutate(prop = value / sum(N_loading$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )



N_pie<-ggplot(N_loading, aes(x="", y=value, fill=site)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(title = "Annual N loading")+
  scale_fill_manual(values=c("red4","#35978F", "#8073AC", "#BF812D", "#8C510A","#01665E", "antiquewhite","#542788"))+
  theme_void()

N_pie


P_aqua <- N_P_aqua %>% 
  rename(month= mes_2013,
         value= P_kg) %>% 
  mutate(site= "Aquaculture") %>% 
  select(site, value ) %>% 
  group_by(site) %>% 
  mutate(value=sum(value))%>% 
  unique()

tp <- max_flux %>% 
  dplyr::filter(name =="tp") %>% 
  group_by(site) %>% 
  summarise(value=sum(value)) %>% 
  mutate(norm= c(1,.131,1,1,.934,.126),
         max_estimate= value/norm,
         dif= max_estimate-value) 

underestimate<- tp %>% 
  mutate(value=sum(tp$dif)) %>% 
  select(value) %>% 
  unique() %>% 
  mutate(site= "X estimate") %>% 
  select(site, value)


tp<- tp %>% 
  select(site, value)

P_loading<- rbind(tp, P_aqua, underestimate)

P_loading<- P_loading%>% 
  arrange(desc(site)) %>%
  mutate(prop = value / sum(P_loading$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )



P_pie<-ggplot(P_loading, aes(x="", y=value, fill=site)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(title = "Annual P loading")+
  scale_fill_manual(values=c("red4","#35978F", "#8073AC", "#BF812D", "#8C510A","#01665E", "antiquewhite","#542788"))+
  theme_void()

P_pie


#Calculating WS contributions using max modeled discharge only decreases aquaculture P contributions by ~1% and 
#N contributions by < 2%

