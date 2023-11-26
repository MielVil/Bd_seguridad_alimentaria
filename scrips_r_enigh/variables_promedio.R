### VARIABLES DE PROMEDIO

##HABITANTES DEL HOGAR ==  tot_integ
## TENENCIA DE AGUA POTABLE == disp_agua num (1-7 )
## TENENCIA DE REFRIGERADOR == num_refri # refris en el hogar
install.packages("modeest") #se uasra para calcular la moda, pues esta no esta integrada a la paqueteria basica
install.packages("srvyr")
install.packages("survey")
install.packages("openxlsx")
install.packages("writexl")
install.packages("readxl")

library(openxlsx)
library(writexl)
library(readxl)
library(modeest)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr) #Libreria para trabajar con encuestas


setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

#CARGANDO las bases de datos

bd_2016 <- read.csv('suma_enigh_2016.csv')
bd_2018 <- read.csv('suma_enigh_2018.csv')
bd_2020 <- read.csv('suma_enigh_2020.csv')
bd_2022 <- read.csv('suma_enigh_2022.csv')

## A continuacion se busca crear una funcion MTC que calcule tres medidas de tendencia central
## Media, Mediana y Moda

####FILTADO Y CAMBIO DE VARIABLES DISP_AGUA
### Hay 7 posibles respuestas al tipo de agua que se consume, que se casificaran en 2 principales categoria
### x = 1 & x= 0 donde para x=1 se toma  disp_agua = 1 
###para x = 0 se toma el rango de 2 a 7 para disp_agua

### x = 1 = disp_agua = 1 

bd_2016$disp_agua[bd_2016$disp_agua %in% c(2,3,4,5,6,7)]<-0
bd_2018$disp_agua[bd_2018$disp_agua %in% c(2,3,4,5,6,7)]<-0
bd_2020$disp_agua[bd_2020$disp_agua %in% c(2,3,4,5,6,7)]<-0
bd_2022$disp_agua[bd_2022$disp_agua %in% c(2,3,4,5,6,7)]<-0

table(bd_2016$disp_agua)  # para 2016;  N° 0 = 20,651  N° 1 = 49,660

#se modifica la variables pues no nos interesa saber cuantos refris tienene, si no la tenencia de ellos
# X = 1, tiene tefri, x=0 no tiene
bd_2016$num_refri[bd_2016$num_refri %in% c(2,3,4,5,7,8,14,15)]<-1
bd_2018$num_refri[bd_2018$num_refri %in% c(2,3,4,5,6,7,9,10,15,20,30)]<-1
bd_2020$num_refri[bd_2020$num_refri %in% c(2,3,4,5,6,7,8,10,14,15,20)]<-1
bd_2022$num_refri[bd_2022$num_refri %in% c(2,3,4,5,7,9)]<-1

table(bd_2016$num_refri) ## N°0 = 10021   N°1 = 60290


##checar a detalle
## para agua potable

bc <- bd_2016 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_agua16 <- bc |> survey_count(disp_agua)
table(bd_2016$disp_agua)
num_agua16$n
tabla1<-bc|>group_by(disp_agua) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_agua_16 <- tabla1$prop #guarda la proporcion en vector
prop_agua_16_low <- tabla1$prop_low
prop_agua_16_upp <- tabla1$prop_upp

bc <- bd_2018 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_agua18 <- bc |> survey_count(disp_agua)
table(bd_2018$disp_agua)
num_agua18$n
tabla1<-bc|>group_by(disp_agua) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_agua_18 <- tabla1$prop #guarda la proporcion en vector
prop_agua_18_low <- tabla1$prop_low
prop_agua_18_upp <- tabla1$prop_upp

bc <- bd_2020 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_agua20 <- bc |> survey_count(disp_agua)
table(bd_2020$disp_agua)
num_agua20$n
tabla1<-bc|>group_by(disp_agua) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_agua_20 <- tabla1$prop #guarda la proporcion en vector
prop_agua_20_low <- tabla1$prop_low
prop_agua_20_upp <- tabla1$prop_upp

bc <- bd_2022 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_agua22 <- bc |> survey_count(disp_agua)
table(bd_2022$disp_agua)
num_agua22$n
tabla1<-bc|>group_by(disp_agua) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_agua_22 <- tabla1$prop #guarda la proporcion en vector
prop_agua_22_low <- tabla1$prop_low
prop_agua_22_upp <- tabla1$prop_upp

anio_agua <- c(2016,2018,2020,2022)



prop_agua_x_0<-c(prop_agua_16[1],prop_agua_18[1],prop_agua_20[1],prop_agua_22[1])
prop_agua_x_1<-c(prop_agua_16[2],prop_agua_18[2],prop_agua_20[2],prop_agua_22[2])

low_agua_x_0 <- c(prop_agua_16_low[1],prop_agua_18_low[1],prop_agua_20_low[1],prop_agua_22_low[1])
low_agua_x_1 <- c(prop_agua_16_low[2],prop_agua_18_low[2],prop_agua_20_low[2],prop_agua_22_low[2])

upp_agua_x_0 <- c(prop_agua_16_upp[1],prop_agua_18_upp[1],prop_agua_20_upp[1],prop_agua_22_upp[1])
upp_agua_x_1 <- c(prop_agua_16_upp[2],prop_agua_18_upp[2],prop_agua_20_upp[2],prop_agua_22_upp[2])

agua_x_0 <- c(num_agua16$n[1],num_agua18$n[1],num_agua20$n[1],num_agua22$n[1])
agua_x_1 <- c(num_agua16$n[2],num_agua18$n[2],num_agua20$n[2],num_agua22$n[2])

agua <- data.frame(anio_agua,agua_x_0,prop_agua_x_0,low_agua_x_0,upp_agua_x_0,agua_x_1,prop_agua_x_1,low_agua_x_0,upp_agua_x_0)

write.xlsx(agua,file = "valores_promedio.xlsx", sheetName = "agua") #crea un libro y una hoja con nombre agua de formato xlsx


### TENENCIA DE REFRIGERADOR
bc <- bd_2016 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_refri16 <- bc |> survey_count(num_refri)
table(bd_2016$num_refri)
num_refri16$n
tabla1<-bc|>group_by(num_refri) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_refri_16 <- tabla1$prop #guarda la proporcion en vector
prop_refri_16_low <- tabla1$prop_low
prop_refri_16_upp <- tabla1$prop_upp


bc <- bd_2018 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_refri18 <- bc |> survey_count(num_refri)
table(bd_2018$num_refri)
num_refri18$n
tabla1<-bc|>group_by(num_refri) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_refri_18 <- tabla1$prop #guarda la proporcion en vector
prop_refri_18_low <- tabla1$prop_low
prop_refri_18_upp <- tabla1$prop_upp

bc <- bd_2020 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_refri20 <- bc |> survey_count(num_refri)
table(bd_2020$num_refri)
num_refri20$n
tabla1<-bc|>group_by(num_refri) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_refri_20 <- tabla1$prop #guarda la proporcion en vector
prop_refri_20_low <- tabla1$prop_low
prop_refri_20_upp <- tabla1$prop_upp


bc <- bd_2022 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
num_refri22 <- bc |> survey_count(num_refri)
table(bd_2022$num_refri)
num_refri22$n
tabla1<-bc|>group_by(num_refri) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1
prop_refri_22 <- tabla1$prop #guarda la proporcion en vector
prop_refri_22_low <- tabla1$prop_low
prop_refri_22_upp <- tabla1$prop_upp

anio_refri <- c(2016,2018,2020,2022)



prop_refri_x_0<-c(prop_refri_16[1],prop_refri_18[1],prop_refri_20[1],prop_refri_22[1])
prop_refri_x_1<-c(prop_refri_16[2],prop_refri_18[2],prop_refri_20[2],prop_refri_22[2])

refri_x_0 <- c(num_refri16$n[1],num_refri18$n[1],num_refri20$n[1],num_refri22$n[1])
refri_x_1 <- c(num_refri16$n[2],num_refri18$n[2],num_refri20$n[2],num_refri22$n[2])

low_refri_x_0 <- c(prop_refri_16_low[1],prop_refri_18_low[1],prop_refri_20_low[1],prop_refri_22_low[1])
low_refri_x_1 <- c(prop_refri_16_low[2],prop_refri_18_low[2],prop_refri_20_low[2],prop_refri_22_low[2])

upp_refri_x_0 <- c(prop_refri_16_upp[1],prop_refri_18_upp[1],prop_refri_20_upp[1],prop_refri_22_upp[1])
upp_refri_x_1 <- c(prop_refri_16_upp[2],prop_refri_18_upp[2],prop_refri_20_upp[2],prop_refri_22_upp[2])


refri <- data.frame(anio_refri,refri_x_0,prop_refri_x_0,low_refri_x_0,upp_refri_x_0,refri_x_1,prop_refri_x_1,low_refri_x_1,upp_refri_x_1)

write.xlsx(refri,file = "valores_promedio.xlsx", sheetName = "refri", append = TRUE) #crea un libro y una hoja con nombre agua de formato xlsx

libro <- readxl::read_excel("valores_promedio.xlsx")
#funciona
wb <- createWorkbook() #crea un libro

#crea las hojas con nombre
addWorksheet(wb, "refri")
addWorksheet(wb, "agua")

#guarda los dataframe correspondientes
writeData(wb ,sheet = "refri", x = refri)
writeData(wb ,sheet = "agua", x = agua)

#gurda los libros
saveWorkbook(wb,file="valores.xlsx")



MTC <- function(variable) {
  promedio <- mean(variable)
  mediana <- median(variable)
  moda <- mfv1(variable)
  desv <- sd(variable)
  
  resultados_mtc <-c (promedio,mediana,moda,desv)
  return(resultados_mtc)
  
}


##varibles del 2016

hab_x_hog_2016 <- MTC(bd_2016$tot_integ)

##varibles del 2018

hab_x_hog_2018 <- MTC(bd_2018$tot_integ)

#sum(is.na(bd_2020$num_refri)) #cuenta el numero de na's
#table(bd_2018$num_refri) # devuelve el numero de elementos acorde a cada valor de la variable

##varibles del 2020

hab_x_hog_2020 <- MTC(bd_2020$tot_integ)

##varibles del 2022

hab_x_hog_2022 <- MTC(bd_2022$tot_integ)


###CREANDO EL DATAFRAME 
anio <- c(2016,2016,2016,2016,2018,2018,2018,2018,2020,2020,2020,2020,2022,2022,2022,2022)
mtc_h <- c("media","mediana","moda","desv","media","mediana","moda","desv","media","mediana","moda","desv","media","mediana","moda","desv")
hab <- c(hab_x_hog_2016,hab_x_hog_2018,hab_x_hog_2020,hab_x_hog_2022)

tabla_fin <- data.frame(anio,mtc_h,hab)

wb <- createWorkbook() #crea un libro

#crea las hojas con nombre
addWorksheet(wb, "refri")
addWorksheet(wb, "agua")
addWorksheet(wb, "hab_x_hog")

#guarda los dataframe correspondientes
writeData(wb ,sheet = "refri", x = refri)
writeData(wb ,sheet = "agua", x = agua)
writeData(wb ,sheet = "hab_x_hog", x = tabla_fin)

#gurda los libros
saveWorkbook(wb,file="valores1.xlsx")


