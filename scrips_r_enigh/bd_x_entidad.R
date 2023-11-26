library(dplyr)
library(survey)
library(srvyr)

#ELCSA POR ENTIDAD
#### para generar lo mismo para cada variable, es necesario separ foloviv 
## de tal manera que se crea una nueva variable de nombre ent: entidad

## PARA 2020 

setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

bd22 <- read.csv('suma_enigh_2022.csv')
bd20 <- read.csv('suma_enigh_2020.csv')
bd18 <- read.csv('suma_enigh_2018.csv')
bd16 <- read.csv('suma_enigh_2016.csv')

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


#La longitud de folioviv va de 9 a 10 caracteres, del primer al segundo digito representa la entidad
## las entidades de la 1 a la 9, en su variable folioviv estara estrucutrada de 9 digitos
#mientras que el resto de entidades contiene 10 digitos en folioviv
# para separar la entidad de la variable folioviv se indica que tome los primeros digitos a excepción de los ultimos 8
## es decir que puede tomar el primer digito en caso de que la entidad este representada por un solo digito
# o tomar dos digitos.
## toma el complemento de las posiciones, pues los ultimos 8 digitos (independientmente de si la variable es de extension 9 o 10) no representan a la entidad
## le decimos a R que tome todos los digitos empezando en 1 y terminando en el 8vo de derecha a izquierda


#data_list_2<-list()
#for (year in years) {
 # file_name <- paste0("data_list$'", year, "'$ent")
  #data2 <- as.numeric(substr(as.character(paste0("data_list$'",year,"'$folioviv")), start = 1,stop = nchar(as.character(paste0("data_list$'",year,"'$folioviv")))-8))
  #data_list_2[[as.character(year)]] <- file_name
  
  #}

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

bd_list$'bd_2016'$'1' # Trae la base de datos del anio 2016 entidad 1
# a continuacion se busca realizar tablas que almacenen la informacion de proporciones e IC (Intervalos de Confianza)
setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

for(year in years) {
  #bd_list$'bd_2016'$'1' 
  bd_list[[paste0("bd_", year)]] <- list()
  
  for (i in entidades) {
    entidad_i <- data_list[[as.character(year)]] %>%
      as_tibble() %>%
      filter(ent == i)
    
    bd_list[[paste0("bd_", year)]][[as.character(i)]] <- entidad_i
  }
}

#bd_2022 <- read.csv('suma_enigh_2022.csv')

bd_list_16_1_c <- bd_list$'bd_2016'$'1' |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_list_16_1_c |> survey_count(acc_alim2)
table(bd_list$'bd_2016'$'1'$acc_alim1)
bd_list$'bd_2016'$'1'$acc_alim1
tabla_16_1 <- bd_list_16_1_c |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla_16_1

bd_list_16_2_c <- bd_list$'bd_2016'$'2' |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_list_16_2_c |> survey_count(acc_alim2)
table(bd_list$'bd_2016'$'2'$acc_alim1)
bd_list$'bd_2016'$'2'$acc_alim1
tabla_16_2 <- bd_list_16_2_c |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla_16_2

bd_list_16_3_c <- bd_list$'bd_2016'$'3' |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_list_16_3_c |> survey_count(acc_alim2)
table(bd_list$'bd_2016'$'3'$acc_alim1)
bd_list$'bd_2016'$'3'$acc_alim1
tabla_16_3 <- bd_list_16_3_c |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla_16_3

bd_list_18_1_c <- bd_list$'bd_2018'$'1' |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_list_18_1_c |> survey_count(acc_alim2)
table(bd_list$'bd_2018'$'1'$acc_alim1)
bd_list$'bd_2018'$'1'$acc_alim1
tabla_18_1 <- bd_list_18_1_c |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla_18_1

bd_list_18_2_c <- bd_list$'bd_2018'$'2' |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_list_18_2_c |> survey_count(acc_alim2)
table(bd_list$'bd_2018'$'2'$acc_alim1)
bd_list$'bd_2018'$'2'$acc_alim1
tabla_18_2 <- bd_list_18_2_c |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla_18_2

bd_list_20_3_c <- bd_list$'bd_2020'$'3' |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_list_20_3_c |> survey_count(acc_alim2)
table(bd_list$'bd_2020'$'3'$acc_alim1)
bd_list$'bd_2020'$'3'$acc_alim1
tabla_20_3 <- bd_list_20_3_c |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla_20_3


wb <- createWorkbook() #crea un libro

#crea las hojas con nombre
addWorksheet(wb, "1")
addWorksheet(wb, "2")
addWorksheet(wb, "3")

#guarda los dataframe correspondientes
writeData(wb ,sheet = "1", x = tabla_16_1 )
writeData(wb ,sheet = "2", x = tabla_16_2)
writeData(wb ,sheet = "3", x = tabla_16_3)

#gurda los libros
saveWorkbook(wb,file="r_16.xlsx")

wb <- createWorkbook() #crea un libro

#crea las hojas con nombre
addWorksheet(wb, "1")
addWorksheet(wb, "2")
addWorksheet(wb, "3")

#guarda los dataframe correspondientes
writeData(wb ,sheet = "1", x = tabla_18_1 )
writeData(wb ,sheet = "2", x = tabla_18_2)
writeData(wb ,sheet = "3", x = tabla_18_3)

#gurda los libros
saveWorkbook(wb,file="r_18.xlsx")

wb <- createWorkbook() #crea un libro

#crea las hojas con nombre
addWorksheet(wb, "1")
addWorksheet(wb, "2")
addWorksheet(wb, "3")

#guarda los dataframe correspondientes
writeData(wb ,sheet = "1", x = tabla_20_1 )
writeData(wb ,sheet = "2", x = tabla_20_2)
writeData(wb ,sheet = "3", x = tabla_20_3)

#gurda los libros
saveWorkbook(wb,file="r_20.xlsx")


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
        
        survey_count(bd_list_survey, acc_alim2)
        table(bd_list_c$acc_alim1)
        
        tabla2 <- bd_list_survey |>
          group_by(suma_seg_al_m) |>
          summarise(prop = survey_mean(vartype = 'ci')) |>
          round(3)
        
        assign(paste0("tabla_", year, "_", wave), tabla2, envir = .GlobalEnv)
        
       
      }
    }
    

  




