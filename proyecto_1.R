library(tidyverse)
library(lubridate)
datos<-read_csv("202004.csv")
datos<-drop_na(datos)
#puese ser por barras o puede ser por pie chart 
datos %>% 
  ggplot(mapping =aes(x=member_casual))+geom_bar()
summary(datos)
"entonces descubrimos que hay fechas que el punto de llegada es antes que el punto de partida
tambien hay filas en blanco son 99 valores nan los voy a mandar al carajo"
"entonces una de dos podemos trabajar con los datos que solo estan bien o de alguna manera unir
estos datos"
datos_raros<-datos %>% filter(ended_at<started_at)
df<-datos %>% filter(started_at<ended_at)
#Ahora agregamos una columna con los tiempos de cada reccorrido
df<-df %>% mutate(tiempos=seconds_to_period(ended_at-started_at))

df$tiempos

#como primer acercamiento para irnos a extremos vemos si hay usuarios que duran más de un día con el servicio
# Y si los hay ggggg
ver<-df %>% filter(tiempos>days(1))
minutes(df$tiempos)
as.numeric(df$tiempos)
summary(df)
"lo primero que haremos será el numero de días de todo el dataframe"
df %>%
  group_by(t=as.numeric(tiempos)/86400) %>% 
  summarise(
    n=n()
    
    ) %>% 
  ggplot(aes(x=t,y=n))+
  geom_point()

#Separamos por miembros y por casuales 
members<-df %>%  filter(member_casual=="member")
casuals<-df %>%  filter(member_casual=="casual")


members %>%
  group_by(t=as.numeric(tiempos)/86400) %>% 
  summarise(
    n=n()
  ) %>% 
  ggplot(aes(x=t,y=n))+
  geom_point()

casuals %>%
  group_by(t=as.numeric(tiempos)/86400) %>% 
  summarise(
    n=n()
  ) %>% 
  ggplot(aes(x=t,y=n))+
  geom_point()

"notamos cosas interesantes, la primera es que en los casuales son más las personas que se tardan más de 10 días en
llegar a sus destinos. "
1*TRUE
(sum(1*(as.numeric(casuals$tiempos)/86400<1))*100)/count(casuals)
(sum(1*(as.numeric(members$tiempos)/86400<1))*100)/count(members)

"vamos a contar la proporcion de tiempo en días de miembros y casuales, el promedio de tiempo o la media de tiempo "
#si pudiera cambial la escala temporal eso es transformalo ahoras y conviertelos a numeros 
#veremos el numero totals de miembros y de casuales 
count(members)*100/count(members)
count(casuals)

#Creamos un nueva columna en la cual tomamos la diferencia en minutos del tiempo de uso.
members<-members %>% 
  mutate(dife=abs(minute(ended_at)-minute(started_at)))

casuals<-casuals %>% 
  mutate(dife=abs(minute(ended_at)-minute(started_at)))

members2<-members %>% 
  mutate(dife=minute(ended_at-started_at))

casuals2<-casuals %>% 
  mutate(dife=seconds_to_period(ended_at-started_at))
seconds_to_period(casuals2$dife)
"son más los miembros que los casuales en el este mes "

#primero veremos si algo radica en que los casuales o no casuales si el punto de salida es el mismo que el de entrada

# En numeros son casi iguales pero en proporción tenemos que son 20% los casuales y 10% los miembros que usan distancias cortas 

# ahora trateremos con las fechas y horas 
# crearemos dos columnas en las cuales tomamos el tiempo entre puntos 


members %>% 
  mutate(minutes=dife) %>% 
  group_by(minutes) %>% 
  summarise(
    n=n()
  ) %>% 
  ggplot(mapping = aes(minutes,n))+
  geom_line()

casuals %>% 
  mutate(minutes=dife) %>% 
  group_by(minutes) %>% 
  summarise(
    n=n()
  ) %>% 
  ggplot(mapping = aes(minutes,n))+
  geom_line()

