library(tidyverse)
library(readxl)
url_Rdat<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path=url_Rdat)
load(file.path(url_Rdat,"BW_list_tbl.RData"))
summary(BW_list_tbl)
BW_list_tbl %>% head(30)
"NO2" %in% (names(BW_list_tbl$Alb))
comp_detect <- function(df,cmp) {
  exst<- cmp %in% names(df)
  return(exst)
}
comp_detect(BW_list_tbl$Alb,"NO2")
BW_list_tbl$Alb%>% exists(names(.))
load(file.path(url_Rdat,"BW.RData"))
summary(BW.all_data)

BW_mean_median_NO2<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"NO2"),
  mean(x$NO2,na.rm=TRUE)-
    median(x$NO2,na.rm=TRUE),NA))
)
BW_var_NO2 <- map_dbl(BW_list_tbl, function(x) (ifelse (comp_detect (x,"NO2"),
  var(x$NO2,na.rm=TRUE),NA)))
BW_mean_WG <- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"WG"),
                                                       mean(x$WG,na.rm=TRUE),NA )))

BW_var_WG <-map_dbl(BW_list_tbl,function(x) { ifelse(comp_detect (x,"WG"),var(x$WG,na.rm=TRUE),NA) })







