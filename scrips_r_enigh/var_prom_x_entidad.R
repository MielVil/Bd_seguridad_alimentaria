library(dplyr)
library(survey)
library(srvyr)

setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

## simplificando el codigo haciendo uso de un for y leer varia bases de datos de misma extension

years<- c(2016,2018,2020,2022)

data_list <- list()

# Cargar los archivos CSV en un bucle
for (year in years) {
  file_name <- paste0("suma_enigh_", year, ".csv") # realiza una concatenacion para buscar el nombre del archivo
  data <- read.csv(file_name)
  data_list[[as.character(year)]] <- data
} # se creo una lista capaz de almacenar data frames en cada posicion

# Ahora, los data frames están almacenados en data_list
# Puedes acceder a ellos usando data_list$'2016', data_list$'2018', etc.

## crear una nueva variable que guarde unicamente el identificador de la entidas, el cual esta concatenado en la variable folioviv
data_list$'2016'$ent <- as.numeric(substr(as.character(data_list$'2016'$folioviv), start = 1,stop = nchar(as.character(data_list$'2016'$folioviv))-8))
data_list$'2018'$ent <- as.numeric(substr(as.character(data_list$'2018'$folioviv), start = 1,stop = nchar(as.character(data_list$'2018'$folioviv))-8))
data_list$'2020'$ent <- as.numeric(substr(as.character(data_list$'2020'$folioviv), start = 1,stop = nchar(as.character(data_list$'2020'$folioviv))-8))
data_list$'2022'$ent <- as.numeric(substr(as.character(data_list$'2022'$folioviv), start = 1,stop = nchar(as.character(data_list$'2022'$folioviv))-8))

length(table(data_list$'2020'$ent))
#comprobando la longitud de los datos y los valores que puede tomar la variable ent = entidad
# en México hay 32 entidades federativas, por lo cual los valores van del 1-32

## 2020 por entidad
entidades <- unique(data_list$'2022'$ent) # crea el vector del codigo de las entidades
#resultados_20 <- list() # es un vector vacio, el cual es una mausque herramienta que nos ayudara mas tarde
# almacenara los resultados de cada uno de los analisis x entidad

bd_list<- list()

## se van a crear varias listas, cada lista contrendra 32 bases de datos, donde cada una representara uno de los estados
years<- c(2016,2018,2020,2022)
for(year in years) {
  bd_list[[paste0("bd_", year)]] <- list()
  
  for (i in entidades) {
    entidad_i <- data_list[[as.character(year)]] %>%
      as_tibble() %>%
      filter(ent == i)
    
    bd_list[[paste0("bd_", year)]][[as.character(i)]] <- entidad_i
    
  } }

datab<-bd_list$'bd_2016'$'2' # Trae la base de datos del anio 2016 entidad 1
# a continuacion se busca realizar tablas que almacenen la informacion de proporciones e IC (Intervalos de Confianza)
setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

for(year in years) {
  bd_list[[paste0("bd_", year)]] <- list()
  
  for (i in entidades) {
    entidad_i <- data_list[[as.character(year)]] %>%
      as_tibble() %>%
      filter(ent == i)
    
    bd_list[[paste0("bd_", year)]][[as.character(i)]] <- entidad_i
  }
}

library(openxlsx)
library(writexl)
library(readxl)

# Define years and waves
years <- c("2016", "2018", "2020","2022")
waves <- as.character(1:32)

for (year in years) {
  for (wave in waves) {
    bd_list_c <- bd_list[[paste0("bd_", year)]][[wave]]
    
    # Assuming 'upm', 'est_dis', and 'factor' are defined somewhere
    bd_list_survey <- as_survey(bd_list_c, ids = upm, strata = est_dis, nest = TRUE, weights = factor)
    
    cat("Processing", year, "-", wave, "\n")
    
    survey_count(bd_list_survey, num_refri)
    table(bd_list_c$num_refri)
    
    tabla2 <- bd_list_survey |>
      group_by(suma_seg_al_m) |>
      summarise(prop = survey_mean(vartype = 'ci')) |>
      round(3)
    
    assign(paste0("tabla_", year, "_", wave), tabla2, envir = .GlobalEnv)
    
    
  }
}
### ser calculo ´por entidad las proporciones correspondientes
### al numero de refrigerados por hogar
### al tener bases de datos individuales, se busca juntarlas por anio

###juntar las bases de datos creadas

## a continuacion el codigo guarda los dataframe en una sola hoja de calculo
install.packages('tidyverse')

library(tidyverse)
library(dplyr)
library(ggplot2)



setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

list_df = list(enti_2016,enti_2018,enti_2020,enti_2022)
df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombr en comun
df2<-tabla_2016_1

anios<- c("2016","2018","2020","2022")
ents<- as.character(1:32)
num_iteraciones <- 32
suma_seg_al_m<- df2
tabla_datos_entidad<- as.data.frame(suma_seg_al_m)
tabla_datos_entidad <- tabla_datos_entidad[1,]
# Crear un bucle for para iterar sobre las bases de datos

### correrprimeras linas hasta definir el df2 y posteriormente correr las variables
### que definen "tabla:datos_entidad"
i<-2
for (i in 1:num_iteraciones) {
  # Crear los nombres de las variables dinámicamente
  enti_2016 <- get(paste0("tabla_2016_", i))[1, ]
  enti_2018 <- get(paste0("tabla_2018_", i))[1, ]
  enti_2020 <- get(paste0("tabla_2020_", i))[1, ]
  enti_2022 <- get(paste0("tabla_2022_", i))[1, ]
  #tabla_2016_1
  
  list_df = list(enti_2016,enti_2018,enti_2020,enti_2022)
  df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombre en comun
  df2
  tabla_datos_entidad <- rbind(tabla_datos_entidad,df2)
  #}. ### la funcion rbind sirve para juntar data.frames por filas
}

df <- tabla_datos_entidad[-1, ] #para quitar la fila que esta de mas
df <- cbind(Entidad = ents, df) # agrega la columna de entidades
### pegar tablas x entidad, pero con anios
###guardar la base en excel
library(openxlsx)
write.xlsx(df,"entidades_valores_prom_refri.xlsx") ## estan agrupados por año


#####AGUA#####X
##proporcion, por entidad y anio de agua 

for (year in years) {
  for (wave in waves) {
    bd_list_c <- bd_list[[paste0("bd_", year)]][[wave]]
    
    # Assuming 'upm', 'est_dis', and 'factor' are defined somewhere
    bd_list_survey <- as_survey(bd_list_c, ids = upm, strata = est_dis, nest = TRUE, weights = factor)
    
    cat("Processing", year, "-", wave, "\n")
    
    survey_count(bd_list_survey, num_refri)
    table(bd_list_c$disp_agua)
    
    tabla2 <- bd_list_survey |>
      group_by(suma_seg_al_m) |>
      summarise(prop = survey_mean(vartype = 'ci')) |>
      round(3)
    
    assign(paste0("tabla_", year, "_", wave), tabla2, envir = .GlobalEnv)
    
    
  }
}


list_df = list(enti_2016,enti_2018,enti_2020,enti_2022)
df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombr en comun
df2<-tabla_2016_1

anios<- c("2016","2018","2020","2022")
ents<- as.character(1:32)
num_iteraciones <- 32
suma_seg_al_m<- df2
tabla_datos_entidad<- as.data.frame(suma_seg_al_m)
tabla_datos_entidad <- tabla_datos_entidad[1,]
# Crear un bucle for para iterar sobre las bases de datos

### correrprimeras linas hasta definir el df2 y posteriormente correr las variables
### que definen "tabla:datos_entidad"
i<-2
for (i in 1:num_iteraciones) {
  # Crear los nombres de las variables dinámicamente
  enti_2016 <- get(paste0("tabla_2016_", i))[1, ]
  enti_2018 <- get(paste0("tabla_2018_", i))[1, ]
  enti_2020 <- get(paste0("tabla_2020_", i))[1, ]
  enti_2022 <- get(paste0("tabla_2022_", i))[1, ]
  #tabla_2016_1
  
  list_df = list(enti_2016,enti_2018,enti_2020,enti_2022)
  df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombre en comun
  df2
  tabla_datos_entidad <- rbind(tabla_datos_entidad,df2)
  #}. ### la funcion rbind sirve para juntar data.frames por filas
}

df <- tabla_datos_entidad[-1, ] #para quitar la fila que esta de mas
df <- cbind(Entidad = ents, df) # agrega la columna de entidades
### pegar tablas x entidad, pero con anios
###guardar la base en excel
library(openxlsx)
write.xlsx(df,"entidades_prom_agua.xlsx") ## estan agrupados por año


####HABITANTES X HOGAR####


##proporcion, por entidad y anio de agua 


for (year in years) {
  for (wave in waves) {
    bd_list_c <- bd_list[[paste0("bd_", year)]][[wave]]
    
    # Assuming 'upm', 'est_dis', and 'factor' are defined somewhere
    bd_list_survey <- as_survey(bd_list_c, ids = upm, strata = est_dis, nest = TRUE, weights = factor)
    
    cat("Processing", year, "-", wave, "\n")
    
    survey_count(bd_list_survey, num_refri)
    table(bd_list_c$tot_integ)
    
    tabla2 <- bd_list_survey |>
      group_by(suma_seg_al_m) |>
      summarise(prop = survey_mean(vartype = 'ci')) |>
      round(3)
    
    assign(paste0("tabla_", year, "_", wave), tabla2, envir = .GlobalEnv)
    
    
  }
}


list_df = list(enti_2016,enti_2018,enti_2020,enti_2022)
df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombr en comun
df2<-tabla_2016_1

anios<- c("2016","2018","2020","2022")
ents<- as.character(1:32)
num_iteraciones <- 32
suma_seg_al_m<- df2
tabla_datos_entidad<- as.data.frame(suma_seg_al_m)
tabla_datos_entidad <- tabla_datos_entidad[1,]
# Crear un bucle for para iterar sobre las bases de datos

### correrprimeras linas hasta definir el df2 y posteriormente correr las variables
### que definen "tabla:datos_entidad"
i<-2
for (i in 1:num_iteraciones) {
  # Crear los nombres de las variables dinámicamente
  enti_2016 <- get(paste0("tabla_2016_", i))[1, ]
  enti_2018 <- get(paste0("tabla_2018_", i))[1, ]
  enti_2020 <- get(paste0("tabla_2020_", i))[1, ]
  enti_2022 <- get(paste0("tabla_2022_", i))[1, ]
  #tabla_2016_1
  
  list_df = list(enti_2016,enti_2018,enti_2020,enti_2022)
  df2 <- list_df %>% reduce(inner_join, by='suma_seg_al_m') ## Se une mediante las filas en nombre en comun
  df2
  tabla_datos_entidad <- rbind(tabla_datos_entidad,df2)
  #}. ### la funcion rbind sirve para juntar data.frames por filas
}

df <- tabla_datos_entidad[-1, ] #para quitar la fila que esta de mas
df <- cbind(Entidad = ents, df) # agrega la columna de entidades
### pegar tablas x entidad, pero con anios
###guardar la base en excel
library(openxlsx)
write.xlsx(df,"entidades_prom_hab.xlsx") ## estan agrupados por año




