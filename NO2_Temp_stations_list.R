library(tidyverse)
library(lubridate)
url_Rdat<- "~/documents/Luftqualitaet/Daten/BW_Rdat/"
load(paste0(url_Rdat,"BW_stations_NO2_tbl.RData"))
BW_stations_NO2_tbl$station <-as_factor(BW_stations_NO2_tbl$station)
summary(BW_stations_NO2_tbl)
head(BW_stations_NO2_tbl)
levels(BW_stations_NO2_tbl$name)# 15 stations
load(file.path(url_Rdat,"FRI_tbl.RData"))
summary(FRI_tbl)
load(file.path(url_Rdat,"ALB_tbl.RData"))
summary(ALB_tbl)
load(file.path(url_Rdat,"BRN_tbl.RData"))
summary(BRN_tbl)
load(file.path(url_Rdat,"FRE_tbl.RData"))
summary(FRE_tbl)
load(file.path(url_Rdat,"HEID_tbl.RData"))
summary(HEID_tbl)
#Heilbronn
load(file.path(url_Rdat,"HEIL_tbl.RData"))
summary(HEIL_tbl)
#Eggenstein
load(file.path(url_Rdat,"EGG_tbl.RData"))
summary(EGG_tbl)
# Schwarzwald Süd
load(file.path(url_Rdat,"SWS.RData"))
SWS_tbl<-SWS_data$Sws.no2 %>% 
  filter(datetime>ymd_h("2001-02-21 15"))%>%
  left_join(SWS_data$Sws.temp)
# Odenwald
load(file.path(url_Rdat,"ODW.RData"))
ODW_data%>%summary()
ODW_tbl<-ODW_data$Odw.no2%>% left_join(ODW_data$Odw.temp)
# Ludwigsburg
load(file.path(url_Rdat,"LBG.RData"))
LBG_tbl<-LBG_data$Lbg.no2
# Reutlingen
load(file.path(url_Rdat,"RT.RData"))
RT_tbl<-RT_data$Rt.no2%>% left_join(RT_data$Rt.temp)
# Stgt Bad Cannstatt
load(file.path(url_Rdat,"STGT.RData"))
STGT_data%>% summary()
CAN_tbl<-STGT_data$Can.no2%>% left_join(STGT_data$Can.temp)
#Karlsruhe
load(file.path(url_Rdat,"KAR.RData"))
KAR_data$Kar.temp%>% head()
BW_NO2_Temp_list <- list(Alb= ALB_tbl,# Alb with 5 variables station, name,datetime,NO2,Temp
                    Brn= BRN_tbl,
                    Can= CAN_tbl,
                    Egg = EGG_tbl,
                    Frei = FRE_tbl,
                    Fri = FRI_tbl,
                    Heid =HEID_tbl,
                    Heil= HEIL_tbl,
                    Lbg= LBG_tbl,# Lbg ohne Temp variable
                    Odw= ODW_tbl,
                    Rt = RT_tbl,
                    Sws=SWS_tbl)

NO2_dat <-map(BW_list_tbl,function(x){ x$NO2})
str(NO2_dat)
NO2_dat[1]
sel_no2 =function(df){
  df$NO2
}
datx<- NO2_dat[[1]]%>% as.vector()
dat<-tibble (  x = 1:length(datx),
               y = datx)
head(dat)
ggplot(dat)+
  geom_histogram(mapping=aes(x=y),
                 bins = 50,
                 data = dat,
                 na.rm = T)
mean_no2 <- function(df){
  df<- na.omit(df)
  Dif = (quantile(df$NO2)[4]-quantile(df$NO2)[2])*1.5
  lo = quantile(df$NO2)[3]-Dif
  hi = quantile(df$NO2)[3]+ Dif
  df <- df%>% filter(df$NO2 > lo & df$NO2< hi)
  round(mean(df$NO2,na.rm=TRUE),1)
}
median_no2 <- function(df){
  median(df$NO2,na.rm = TRUE)
}
quntl_no2 <- function(df){
  quantile(df$NO2,na.rm = TRUE)
}
map_dbl(BW_list_tbl,mean_no2)
map_dbl(BW_list_tbl,median_no2)
map_df(BW_list_tbl,quntl_no2)
map(BW_list_tbl,~.$NO2%>% head)# Liste mit NO2 Werten
BW_list_tbl %>% map(sel_no2)
