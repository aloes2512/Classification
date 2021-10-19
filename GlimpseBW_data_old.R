library(tidyverse)
library(knitr)
url_Rdat<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path=url_Rdat)
load(file.path(url_Rdat,"BW_list_tbl.RData"))
summary(BW_list_tbl)
BW_list_tbl %>% head(30)
"NO2" %in% (names(BW_list_tbl$Alb))
head(BW_list_tbl$Alb,2)
BW_list_tbl$Alb%>% na.omit()%>%
  ggplot(aes(x = datetime))+
  geom_point(aes(y= NO2),size = 0.001)+
  geom_smooth(method = "lm",mapping =  aes(x=datetime,y= NO2),
              data=BW_list_tbl$Alb%>% na.omit(),col = "red")
comp_detect <- function(df,cmp) { 
  exst<- cmp %in% names(df)
  return(exst)
}
load(file.path(url_Rdat,"BW.RData"))
summary(BW.all_data)

BW_median_NO2<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"NO2"),
    median(x$NO2,na.rm=TRUE),NA))
)
<<<<<<< HEAD
BW_median_NO2<- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"WG"),
                                                         median(x$WG,na.rm=TRUE),NA )))
=======
BW_mean_NO2<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"NO2"),
                                                         mean(x$NO2,na.rm=TRUE),
                                                         NA))
)
>>>>>>> ea4cc1be20552c8e36ff851ad779fa8f965d5623
BW_var_NO2 <- map_dbl(BW_list_tbl, function(x) (ifelse (comp_detect (x,"NO2"),
  var(x$NO2,na.rm=TRUE),NA)))
BW_mean_WG <- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"WG"),
                                                       mean(x$WG,na.rm=TRUE),NA )))
BW_var_WG <-map_dbl(BW_list_tbl,function(x) { ifelse(comp_detect (x,"WG"),var(x$WG,na.rm=TRUE),NA) })

<<<<<<< HEAD
BW_summary_data <- tibble(NO2_mean=)



=======
BW_keydata<- tibble(Stations=names(BW_list_tbl),
                    Mean_NO2=BW_mean_NO2,
                    Median_NO2= BW_median_NO2,
                    Var_NO2= BW_var_NO2)
kable(BW_keydata,digits=1)
saveRDS(BW_keydata,"rdat/BW_keydata.RData")
>>>>>>> ea4cc1be20552c8e36ff851ad779fa8f965d5623


