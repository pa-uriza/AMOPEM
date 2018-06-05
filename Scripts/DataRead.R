library(tidyverse)
library(readxl)
library(lubridate)
library(glue)

anios<-1995:2017
Pr<-tibble()
for(y in anios){
  P <- read_excel(glue("Data/Raw/Precio_Bolsa_Nacional_($kwh)_{y}.xlsx"),sheet = 1,skip = 2,col_names = TRUE)
  
  Pr <- Pr %>% bind_rows(P)
}
Pr<-Pr[-26]

Pr <- Pr %>% gather(Hora,Precio,-Fecha) %>% transmute(DTTM = Fecha + hours(Hora),Precio) %>% arrange(DTTM)

write.csv(Pr,glue("Data/Processed/Precio_Bolsa_Nacional_($kwh)_{anios[1]}-{anios[length(anios)]}.csv"))
