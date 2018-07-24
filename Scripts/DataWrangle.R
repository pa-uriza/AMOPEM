library(tidyverse)
library(lubridate)
library(forecast)

# Se cargan los precios con valores imputados para cuando vale 0 (para que el logaritmo esté definido)
Prices <- read_csv("Data/Processed/Precio_Bolsa_Nacional_($kwh)_1995-2017_Imp.csv")
# FEchas de festivos
Festivos <- read_csv("Data/Processed/Festivos_1995-2018.csv",locale = locale(encoding = stringi::stri_enc_get()))
# IPC historico
IPC <- read_csv("Data/Raw/IPC.csv") %>% transmute(IPC,y=year(Fecha)-1994,Mes = month(Fecha,label=T,abbr = F))
# Definición periodo referencia IPC
ref <- IPC$IPC[IPC$y==1 & IPC$Mes=="julio"]
# Creación dataset modelos
Data <- Prices %>% mutate(Mes = month(DTTM,label=T,abbr = F),FDSF = (wday(DTTM)%in% c(1,7))|(date(DTTM) %in% Festivos$Fecha),
                          y = year(DTTM)-1994, d = wday(DTTM,label=T,abbr = F), Hora = hour(DTTM)) %>% left_join(IPC) %>% 
  mutate(Price = msts(Price, seasonal.periods = c(24,8784)), PriceC = msts(Price*IPC/ref, seasonal.periods = c(24,8784)),
         lPrice = msts(log(Price), seasonal.periods = c(24,8784)), lPriceC = msts(log(PriceC), seasonal.periods = c(24,8784)))
         
         
         