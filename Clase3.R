
library(tidyverse)

## PIVOT transformando tablas ----

#cargamos los datos
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


data("warpbreaks")
colnames(warpbreaks)

Lana <- warpbreaks %>%  pivot_wider(names_from = "tension", 
                                    values_from = "breaks",
                                    values_fn = mean) %>% 
  summarise_if(is.numeric, round) #eso redondea sin decimales si se quiere un numero determinado de decimales ~round(.x,2) para dos decimales

#### JOIN uniendo bases de datos-----

#cargar datos

Episodes <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/The_Office_Episodes_per_Character.csv")
words <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/The_office_Words.csv")
stop_words <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/stop_words.csv")

# full_join() juunta dos bases de datos que tenga una columna en comun queaddno una nueva base del tammaño de la más grande.

Episodes_words <- full_join( Episodes, words) #Joining, by = "speaker" la consola avisa por cual columna se reaizó el join.

Episodes_top_10 <- Episodes %>%
  slice_max(order_by = n_episodes, n= 10) %>%
  left_join(words)

# inner: only rows with matching keys in both x and y
# left: all rows in x, adding matching columns from 
# right: all rows in y, adding matching columns from x


left <- left_join(Episodes_top_10, words)

Top_Words <- left %>% 
  anti_join(stop_words) %>%  #elimina de la base de datos left todas las filas que tienen el mismo dato de la columna words en stop_words
  group_by(word) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  slice_max(order_by = n, n = 20)



























