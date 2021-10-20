library(tidyverse)
url_Rdat<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path=url_Rdat)
load(file.path(url_Rdat,"BW_list_tbl.RData"))
summary(BW_list_tbl)
BW_list_tbl %>% head(30)
"NO2" %in% (names(BW_list_tbl$Alb))
BW_list_tbl$Alb%>% summary()
comp_detect <- function(df,cmp) {
  exst<- cmp %in% names(df)
  return(exst)
}
comp_detect(BW_list_tbl$Alb,"NO2")
BW_list_tbl$Alb%>% na.omit()%>%
  ggplot(aes(x=datetime,y= NO2))+
  geom_point(size = 0.001)+
  geom_smooth(method = "lm",col= "red")+
  ggtitle("NO2-immissions 20 years",
  subtitle = "Schw. Alb 47650")
ggsave("NO2_trend_20y_47650.png",path = "figs/")
plt_NO2_trnd <- function(df) {
  df<-df%>% na.omit()%>% as_tibble()
  ifelse (comp_detect(df,"NO2"),
  plt <-ggplot(df,aes(x= datetime,y=NO2))+
    geom_point(size = 0.001)+
    geom_smooth(method= "lm",col = "red")+
    ggtitle("NO2-immissions 20 years",
    subtitle = paste(first(df$name),first(df$station))),NA
    )
  ggsave(paste0("NO2_trend_20y_",first(df$name),".png"),path = "figs/")

  return(plt)
}
map(BW_list_tbl,plt_NO2_trnd)
plt_NO2_trnd(BW_list_tbl$Alb)
plt_NO2_trnd(BW_list_tbl$Brn)
plt_NO2_trnd(BW_list_tbl$Egg)
plt_NO2_trnd(BW_list_tbl$Heid)
plt_NO2_trnd(BW_list_tbl$Stg_Nck)
plt_NO2_trnd(BW_list_tbl$Rt_leder)
plt_NO2_trnd(BW_list_tbl$Rt_pomol)
plt_NO2_trnd(BW_list_tbl$Stg_Klt)
plt_NO2_trnd(BW_list_tbl$Stg_Can)
plt_NO2_trnd(BW_list_tbl$Stg_Stadtg)
plt_NO2_trnd(BW_list_tbl$Stg_Schwz)
BW_mean_median_NO2<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"NO2"),
  mean(x$NO2,na.rm=TRUE)-
    median(x$NO2,na.rm=TRUE),NA))
)
BW_median_NO2<- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"WG"),
                                                         median(x$WG,na.rm=TRUE),NA )))
BW_var_NO2 <- map_dbl(BW_list_tbl, function(x) (ifelse (comp_detect (x,"NO2"),
  var(x$NO2,na.rm=TRUE),NA)))
BW_mean_WG <- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"WG"),
                                                       mean(x$WG,na.rm=TRUE),NA )))

BW_var_WG <-map_dbl(BW_list_tbl,function(x) { ifelse(comp_detect (x,"WG"),var(x$WG,na.rm=TRUE),NA) })

BW_summary_data <- tibble(NO2_mean=)





