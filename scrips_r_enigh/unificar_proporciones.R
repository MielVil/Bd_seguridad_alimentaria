### El proposito de este codigo es unir las columnas de datos de 
## seg_alimentaria, prop_refri, prop_agua,prop_hab_x_hog
## para poder compararlas por entridad y anio

library(dplyr)
#cargando directorio de donde se sustraeran los datos
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

#Cargando los datos que contienen las proporciones por entidad
refri<- read.xlsx("entidades_valores_prom_refri.xlsx")
hab<- read.xlsx("entidades_prom_hab.xlsx")
agua<- read.xlsx("entidades_prom_agua.xlsx")
seg_alim <- read.xlsx("entidades_prom_agua.xlsx")


### A continuacion extraemos
###proporciones del 2016
seg_alim_2016 <- seg_alim$prop.x
refri_2016 <- refri$pro_refri_2016
agua_2016 <- agua$prop.x
hab_2016 <- hab$prop.x

tabla_prom_2016<- data.frame(seg_alim_2016,refri_2016,agua_2016,hab_2016)

## proporciones del 2018

seg_alim_2018 <- seg_alim$prop.y
refri_2018 <- refri$pro_refri_2018
agua_2018 <- agua$prop.y
hab_2018 <- hab$prop.y

tabla_prom_2018<- data.frame(seg_alim_2018,refri_2018,agua_2018,hab_2018)

## proporciones del 2020

seg_alim_2020 <- seg_alim$prop.x.x
refri_2020 <- refri$pro_refri_2020
agua_2020 <- agua$prop.x.x
hab_2020 <- hab$prop.x.x

tabla_prom_2020<- data.frame(seg_alim_2020,refri_2020,agua_2020,hab_2020)

## proporciones del 2022

seg_alim_2022 <- seg_alim$prop.y.y
refri_2022 <- refri$pro_refri_2022
agua_2022 <- agua$prop.y.y
hab_2022 <- hab$prop.y.y

tabla_prom_2022<- data.frame(seg_alim_2022,refri_2022,agua_2022,hab_2022)

entidades <- c(1:32)

tabla_props <- data.frame(entidades,tabla_prom_2016,tabla_prom_2018,tabla_prom_2020,tabla_prom_2022)

library(openxlsx)

write.xlsx(tabla_props,"prop_totales.xlsx")
