library(tidyverse)
library(readxl)
library(lubridate)
library(glue)

# Precios
anios<-1995:2017
Pr<-tibble()
for(y in anios){
  P <- read_excel(glue("Data/Raw/Precio_Bolsa_Nacional_($kwh)_{y}.xlsx"),sheet = 1,skip = 2,col_names = TRUE)
  
  Pr <- Pr %>% bind_rows(P)
}
Pr<-Pr[-26]

Pr <- Pr %>% gather(Hora,Precio,-Fecha) %>% transmute(DTTM = Fecha + hours(Hora),Price = Precio) %>% arrange(DTTM)

write_csv(Pr,glue("Data/Processed/Precio_Bolsa_Nacional_($kwh)_{anios[1]}-{anios[length(anios)]}.csv"))

# Imputacion precios en cero

Pr2 <- Pr %>% mutate(Price = if_else(Price == 0,NA_real_,Price)) %>% fill(Price,.direction = "up")

write_csv(Pr2,glue("Data/Processed/Precio_Bolsa_Nacional_($kwh)_{anios[1]}-{anios[length(anios)]}_Imp.csv"))

# Fechas festivos
Festivos <- read_csv("Data/Raw/Festivos_1995-2018.csv",locale = locale(encoding = stringi::stri_enc_get())) %>%
  gather(-Asignacion,-`Fecha base`, -Fiesta,key = "Año",value = "Fecha")

write_csv(Festivos,glue("Data/Processed/Festivos_1995-2018.csv"))
