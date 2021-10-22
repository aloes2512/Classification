library(tidyverse)
library(knitr)
url_Rdat<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path=url_Rdat)
load(file.path(url_Rdat,"BW_list_tbl.RData"))
summary(BW_list_tbl)
comp_detect <- function(df,cmp) {
  exst<- cmp %in% names(df)
  return(exst)
}
# function for all stations
plt_NO2_trnd <- function(df) {
  df<-df%>% na.omit()%>% as_tibble()
  ifelse (comp_detect(df,"NO2"),
  plt <-ggplot(df,aes(x= datetime,y=NO2))+
    geom_point(size = 0.001)+
    geom_smooth(method= "lm",col = "red")+
    ggtitle("NO2-immissions 20 years",
    subtitle = paste(first(df$name),first(df$station))),NA
    )
  ggsave(filename=paste0("NO2_trend_20y_",first(df$name),".png"),
         path = "figs/",
         plot = plt)
}
plt_NO2_trnd(BW_list_tbl$Nck)
BW_list_tbl%>% map(plt_NO2_trnd)
names(BW_list_tbl)
BW_mean_NO2<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"NO2"),
                                                  mean(x$NO2,na.rm=TRUE),NA)))
BW_median_NO2<- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"NO2"),
                                                  median(x$NO2,na.rm=TRUE),NA )))
BW_var_NO2 <- map_dbl(BW_list_tbl, function(x) (ifelse (comp_detect (x,"NO2"),
                                                  var(x$NO2,na.rm=TRUE),NA)))
BW_mean_WG <- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"WG"),
                                                  mean(x$WG,na.rm=TRUE),NA )))
BW_var_WG <-map_dbl(BW_list_tbl,function(x) (ifelse(comp_detect (x,"WG"),
                                                  var(x$WG,na.rm=TRUE),NA) ))


# summary of mean and median NO2, WG
BW_NO2_WG_summary<-BW_summary_statistic[-23,]%>% arrange(NO2_mean)%>% knitr::kable(digits=1)
# O3
BW_mean_O3<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"O3"),
                                                         mean(x$O3,na.rm=TRUE),NA)))
BW_median_O3<- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"O3"),
                                                         median(x$O3,na.rm=TRUE),NA )))
BW_var_O3 <- map_dbl(BW_list_tbl, function(x) (ifelse (comp_detect (x,"O3"),
                                                        var(x$O3,na.rm=TRUE),NA)))

BW_statistic <- tibble (Station =  names(BW_list_tbl),
                                NO2_mean=  BW_mean_NO2,
                                NO2_median=BW_median_NO2,
                                NO2_var =  BW_var_NO2,
                                WG_mean =  BW_mean_WG,
                                WG_var =   BW_var_WG,
                                O3_mean =  BW_mean_O3,
                                O3_median= BW_median_O3,
                                O3_var =   BW_var_O3)
save(BW_statistic, file = file.path(url_Rdat,"BW_statistic.RData"))
