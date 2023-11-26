library(tidyverse)
library(dplyr)
library(openxlsx)


setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb


valores <- read.xlsx('valores1.xlsx')
refri <-"refri"
refri <- read.xlsx('valores1.xlsx', sheet = refri)
agua <- read.xlsx('valores1.xlsx', sheet = "agua")
hab_x_hog <- read.xlsx('valores1.xlsx', sheet = "hab_x_hog")


##Procedemos a eliminar varias filas de hb_x_hog
##para simplemente quedarnos con los promedios

fila_elim<- c(2,3,4,6,7,8,10,11,12,14,15,16)

med_hab_x_hog <- hab_x_hog[-fila_elim,]

anios<-c("SA-2016","SA-2018","SA-2020","SA-2022")

##seleccionar columnas especificas de seguridad alimentaria
SA <- prop %>% select(seg_alim)

#seleccinar columnas especificas del data frame de agua
agua_1 <- agua %>% select(prop_agua_x_1)

#seleccinar columnas especificas del data frame de refri
refri_1 <- refri %>% select(prop_refri_x_1)

#seleccinar columnas especificas del data frame de hab_x_hog

h_x_h_1 <- med_hab_x_hog %>% select(hab)

SA_anio <- cbind(anios,SA,agua_1,refri_1,h_x_h_1)

write.xlsx(SA_anio,"sa_agua_refri_hxh.xlsx")

### A continuación se procede a realizar una sola fila que contenga todos los datos anteriores

r1 <- SA_anio[1,]
r2 <- SA_anio[2,]
r3 <- SA_anio[3,]
r4 <- SA_anio[4,]

SA_renglon <- cbind(r1,r2,r3,r4)

write.xlsx(SA_renglon,"variables_reglon.xlsx")


