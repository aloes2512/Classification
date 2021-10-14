library(tidyverse)
library(readxl)
url_Rdat<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path=url_Rdat)
load(file.path(url_Rdat,"ALB_tbl.RData"))
load(file.path(url_Rdat,"BW.RData"))
summary(ALB_tbl)
summary(BW.all_data)
BW.all_data$Alb%>%summary()
ALB_data<- BW.all_data$Alb
ALB_data$Alb.O3%>% head()
ALB_data%>% map(function(x){~.[[}

