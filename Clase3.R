
library(tidyverse)

Contagiados <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv")


Contagiados_Long <- Contagiados %>%  
  pivot_longer(cols = starts_with("2020"), names_to = "Fecha", values_to = "Infectados") %>% 
  mutate(Fecha = lubridate::ymd(Fecha), prevalencia = 100000*(Infectados/Poblacion)) %>% #lubdidate :: pasa la fecha desde class character a class Date y creamos la columna preevalencia
  dplyr::filter(Fecha == max(Fecha), !is.na(`Codigo comuna`))


hist(Contagiados_Long$prevalencia)

data("fish_encounters")
#114x3

unique(fish_encounters$fish) %>%  length() #19
unique(fish_encounters$station) %>%  length() #11

Para_captura <- fish_encounters %>% pivot_wider(names_from = "station", # me deja los peces como nombre de filas y las station como columnas asi se ve que pes fue capturado en que station
            values_from = "seen",
            values_fill = 0   #para remplazar los NA con un valor en particular.
)
















