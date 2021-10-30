BW_list_tbl$Stg_Nck%>% summary()
                            
 Stg_Nck<- BW_list_tbl$Stg_Nck
  
mod_prms<- Stg_Nck %>% lm(datetime~ NO2,data = .)
str(mod_prms)
