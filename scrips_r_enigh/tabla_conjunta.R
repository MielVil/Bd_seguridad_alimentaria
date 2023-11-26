install.packages('tidyverse')

library(tidyverse)
library(dplyr)
library(ggplot2)

setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

list_df = list(ent_2016_1,ent_2018_1,ent_2020_1,ent_2022_1)
df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombr en comun
df2

anios<- c("2016","2018","2020","2022")
ents<- as.character(1:32)
num_iteraciones <- 32
suma_seg_al_m<- df2
tabla_datos_entidad<- as.data.frame(suma_seg_al_m)
# Crear un bucle for para iterar sobre las bases de datos
i<-1
for (i in 1:num_iteraciones) {
  # Crear los nombres de las variables dinámicamente
  enti_2016 <- get(paste0("tabla_2016_", i))[1, ]
  enti_2018 <- get(paste0("tabla_2018_", i))[1, ]
  enti_2020 <- get(paste0("tabla_2020_", i))[1, ]
  enti_2022 <- get(paste0("tabla_2022_", i))[1, ]
  tabla_2016_1
  
  list_df = list(enti_2016,enti_2018,enti_2020,enti_2022)
  df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombre en comun
  df2
  tabla_datos_entidad <- rbind(tabla_datos_entidad,df2)
#}. ### la funcion rbind sirve para juntar data.frames por filas
}
df <- tabla_datos_entidad[-1, ] #para quitar la fila que esta de más
df <- cbind(Entidad = ents, df) # agrega la columna de entidades
### pegar tablas x entidad, pero con anios
###guardar la base en excel
library(openxlsx)
write.xlsx(df,"tabla_entidades_prop.xlsx")







