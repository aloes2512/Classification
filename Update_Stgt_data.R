BW_list_tbl$Stg_Klt%>% filter(datetime >= ymd("2000-01-01"))%>% summary()

path<-"~/Documents/Luftqualitaet/Daten/BW"
Klt_NO2<- rd_xlsx_lubw(path,"Klt_55006_NO2_00_21.xlsx")%>% rename(NO2=Wert)
Klt_NO<- rd_xlsx_lubw(path,"Klt_55006_NO_00_21.xlsx")%>% rename(NO=Wert)
Klt_O3<- rd_xlsx_lubw(path,"Klt_55006_NO_00_21.xlsx")%>% rename(O3=Wert)
Klt_CO<- rd_xlsx_lubw(path,"Klt_55006_CO_00_21.xlsx")%>% rename(CO=Wert)
Klt_data <- left_join(Klt_NO2,Klt_NO)%>%
  left_join(Klt_O3)%>% left_join(Klt_CO)%>% 
  mutate(name= "Stgt_Klett"%>% as_factor())%>% 
  dplyr::select(station,name,datetime,NO2,O3,NO,CO)
summary(Klt_data)
BW_list_tbl$Stg_Klt <-Klt_data
save (BW_list_tbl, file =file.path(url_Rdat,"BW_list_tbl.RData"))
Hpt_NO2<- rd_xlsx_lubw(path,"Hpt_9999136_NO2_15_21.xlsx")%>% rename(NO2=Wert)
Hpt_NO<- rd_xlsx_lubw(path,"Hpt_9999136_NO_15_21.xlsx")%>% rename(NO=Wert)
Hpt_data<- left_join(Hpt_NO2,Hpt_NO)%>% 
  mutate(name= "Hauptst_str"%>% as_factor())%>% 
  dplyr::select(station,name,datetime,NO2,NO)
BW_list_tbl$Stg_Hpt <-Hpt_data
# Hohenheimerstr
Hoh_NO2<- rd_xlsx_lubw(path,"Hoh_76362_NO2_03_21.xlsx")%>% rename(NO2=Wert)
Hoh_NO<- rd_xlsx_lubw(path,"Hoh_76362_NO_03_21.xlsx")%>% rename(NO=Wert)
Hoh_data<- left_join(Hpt_NO2,Hpt_NO)%>% 
  mutate(name= "Hohenh_str"%>% as_factor())%>% 
  dplyr::select(station,name,datetime,NO2,NO)
BW_list_tbl$Stg_Hoh <-Hoh_data
#======
save (BW_list_tbl, file =file.path(url_Rdat,"BW_list_tbl.RData"))
