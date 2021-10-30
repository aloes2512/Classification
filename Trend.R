library(tidyverse)
library(tidymodels)
load("~/documents/Luftqualitaet/daten/BW_Rdat/BW_list_tbl.Rdata")
summary(BW_list_tbl)
list_names <-names(BW_list_tbl)
BW_list_tbl%>% str()

# lineares Regressions- Model
stat.model2 <- function (df) {
  df <- df%>%dplyr::select(datetime,NO2)
  tidy(lm(NO2 ~ datetime, data = df))
}
map(BW_list_tbl,stat.model2)
BW_list_tbl
# Input of raw data
Station_models<-tibble(name= list_names,
                       data =BW_list_tbl)

Station_models<-Station_models%>% mutate(model =map(data,stat.model))
# add  tidy data
Station_models<-Station_models%>% mutate(tidy_modl= map(model,tidy))
Station_models$tidy_modl$Alb

# Examples of results
Station_models$model$Brn
summary(Station_models$model)
summary(Station_models$model$Odw)
Station_models$model$Alb$coefficients
summary(Station_models$model$Stg_Nck$residuals)
Station_models$model$Stg_Nck$coefficients
# sichern der Modelparameter aus linearer Regression
save(Station_models, file = "~/documents/Luftqualitaet/daten/BW_Rdat/Station_models.RData")
# zwei Beispiel Auswertungen
Station_models$data[[1]]%>% na.omit()%>%ggplot( aes (x = datetime, y = NO2)) +
  geom_point(aes (x= datetime, y = NO2),size = 0.01,  alpha = 0.1 ) +
  geom_smooth(aes (x= datetime, y = NO2),col = "red")+
  ggtitle(paste("NO2  (12 Jahres - Trend) ",Station_models[1]$name[1]%>% as.character()),
          subtitle = "Trend (gleitend): rote Linie") +
  labs( x = "", y = "NO2 ")
ggplot(Station_models$data[[1]]%>% na.omit, aes (x = datetime, y = NO2)) +
  geom_point(aes (x= datetime, y = NO2), size = 0.01, alpha = 0.1 ) +
  geom_abline(slope = Station_models$model$Alb$coefficients[2],
              intercept = Station_models$model$Alb$coefficients[1],col = "red")+
  ggtitle( paste("NO2 (12 Jahres - Trend)",Station_models[1]$name[1]%>% as.character()),
           subtitle = "Trend(Regression): rote Linie") +
  labs( x = "", y = "NO2 ")
# select coefficients
coeff <- vector("list",length = length(list_names))
names(coeff)<- list_names
for (nm in list_names) coeff[[nm]]<- Station_models$model[[nm]]$coefficients

