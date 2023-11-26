install.packages("survey")
install.packages("srvyr")
install.packages("ggplot2")

library(ggpubr)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr) #Libreria para trabajar con encuestas

#direccion
setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

#CARGANDO LAS BASES DE DATOS
con_hog <- read.csv('concentradohogar_2022.csv')
hogares <-read.csv('hogares_2022.csv')
viviendas <- read.csv('viviendas_2022.csv')


b3<-merge(viviendas,hogares,all=T) #realiza pegado de bases
b4 <- merge(b3,con_hog, all=T) #pega la base anterior con concentrado hogares

table(b4$acc_alim16)
## las variables de acceso a la alimentacion son acc_alim2

table(b4$acc_alim2)
#Nota: para identificar a los menores no existe una pregunta en especifica
#simplemente se deja NA, por lo cual debe de exisistir el mismo numero de NA
#de la pregunta 10 a la 16 

num_NA_all <- sum(is.na(b4$acc_alim16)) #nota Usar ColSums para una matriz y sum para un vector
print(num_NA_all)
## HAY 60855 NA, QUE REPRESENTA EL NUMERO DE ADULTOS EN CADA VARIABLE 

# Crear un vector con los nombres de las columnas
columnas <- c("acc_alim2", "acc_alim4","acc_alim5","acc_alim6","acc_alim7","acc_alim8","acc_alim11","acc_alim12","acc_alim13","acc_alim14","acc_alim15","acc_alim16" )

# Iterar sobre cada columna y realizar la operación
for (col in columnas) {
  b4[[col]][b4[[col]] == 2] <- 0
}

#table(b4$acc_alim16)
#table(b4$menores)

#a<-table(bd_menores_enigh$menores)
#b<-table(bd_adultos_enigh$menores)
#a+b 71587 =/ 89006
### base con menores
length(b4$acc_alim10)#89006
bd_menores_enigh <- b4 %>%
  as_tibble() %>%
  filter(!is.na(acc_alim10)) #60855

length(bd_menores_enigh$menores)

### base sin menores 
bd_adultos_enigh <- b4 %>%
  as_tibble() %>%
  filter(is.na(acc_alim10))
length(bd_adultos_enigh$menores) #60855

### reasignando valores al dataframe de menores
#column_number <- which(names(bd_menores_enigh) == "Sum_m") #para encontrar el numero de coluna en el dataframe
#columnas <- c("acc_alim2", "acc_alim4","acc_alim5","acc_alim6","acc_alim7","acc_alim8","acc_alim11","acc_alim12","acc_alim13","acc_alim14","acc_alim15","acc_alim16" )

ps_e<-bd_menores_enigh[,c(71,73,74,75,76,77,80,81,82,83,84,85)] #preguntas de la 1 a la 16

Sum_m <- as.data.frame(rowSums(ps_e, na.rm = TRUE))
Sum_m <- rename(Sum_m, "suma_seg_al_m" = "rowSums(ps_e, na.rm = TRUE)")

table(Sum_m$suma_seg_al_m) ######### checar valores

Sum_m$suma_seg_al_m[Sum_m$suma_seg_al_m %in% c(1,2,3)]<-2;
Sum_m$suma_seg_al_m[Sum_m$suma_seg_al_m %in% c(4,5,6,7)]<-3;
Sum_m$suma_seg_al_m[Sum_m$suma_seg_al_m %in% c(8,9,10,11,12)]<-4;
Sum_m$suma_seg_al_m[Sum_m$suma_seg_al_m == 0]<-1


bd_menores_enigh <-bd_menores_enigh %>% 
  mutate(Sum_m)


table(bd_menores_enigh$suma_seg_al_m) #CONFIRMAR 1,2,3,4
### SUMA PARA SEGURIDAD ALIMENTARIA HOGARES SIN MENORES
ad <- bd_adultos_enigh[,c(71,73,74,75,76,77)] #Preguntas de la 2 a la 8, para hogares sin menores

Sum_NSA_a <- as.data.frame(rowSums(ad,na.rm=TRUE))
Sum_NSA_a <- rename (Sum_NSA_a, "Suma_seg_al_a"= "rowSums(ad, na.rm = TRUE)")

table(Sum_NSA_a$Suma_seg_al_a)#########


#str(Sum_NSA_a)
#any(Sum_NSA_a$Suma_seg_al_a==0)
Sum_NSA_a$Suma_seg_al_a[Sum_NSA_a$Suma_seg_al_a %in% c(1,2)]<-2; 
Sum_NSA_a$Suma_seg_al_a[Sum_NSA_a$Suma_seg_al_a %in% c(3,4)]<-3;
Sum_NSA_a$Suma_seg_al_a[Sum_NSA_a$Suma_seg_al_a %in% c(5,6)]<-4;
Sum_NSA_a$Suma_seg_al_a[ Sum_NSA_a$Suma_seg_al_a == 0] <- 1

bd_adultos_enigh <-bd_adultos_enigh %>% 
  mutate(Sum_NSA_a)
table(Sum_NSA_a$Suma_seg_al_a)

### Pegar las bases a la original 
bd_adultos_enigh <- rename (bd_adultos_enigh, "suma_seg_al_m"= "Suma_seg_al_a")

#bd2 <- merge(bd1,bd_menores, all = TRUE)
#bd3 <- merge(bd1,bd_adultos, all = TRUE)
#b4 <- merge(bd2,bd3, all = T)

bd5 <- inner_join(b4,bd_menores_enigh) #solo aparece los que coinciden
bd6_2022 <- merge(bd5,bd_adultos_enigh, all = T) #junta las bases de datos

length(bd6_2022$suma_seg_al_m) # 89006
#son 89006
table(bd6_2022$suma_seg_al_m) #1: 53712  2:17663  3:10042 4:7589

write.csv(bd6_2022,"suma_enigh_2022.csv",row.names = FALSE)
table(bd6_2022$suma_seg_al_m)
getwd()


b6c <- bd6_2022 |> as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
b6c |> survey_count(acc_alim1)
table(bd6_2022$acc_alim1)

tabla1<-b6c|>group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci')) |>round(3);tabla1


###expandir población para analisis de ponderadores 
setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

bd_2022 <- read.csv('suma_enigh_2022.csv')
bd_c_2022 <- bd_2022 |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_c_2022 |> survey_count(acc_alim2)
table(bd_2022$acc_alim1)
bd_2022$acc_alim1
tabla2 <- bd_c_2022 |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla2


#suma_seg_al_m  prop prop_low prop_upp
#<dbl> <dbl>    <dbl>    <dbl>
 # 1             1 0.62     0.614    0.627
#2             2 0.193    0.188    0.197
#3             3 0.108    0.104    0.111
#4             4 0.079    0.076    0.083

#acc_alim1        n    n_se
#<int>    <dbl>   <dbl>
 # 1         1 13927552 119678.
#2         2 19047109 122560.



####PARA GRAFICAR EN BARRAR CON INTERVALOS DE CONFIANZA

setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb


resultados <- suppressWarnings(read.csv('resultados.csv'))
x<- resultados$fecha

####ORIGINAL

plot_comparar<-ggplot(resultados, aes(x=fecha,y=seg_alim)) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = lower_seg, ymax=upper_seg)) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) de seguridad alimentaria", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = c(2016,2018,2020,2022))+
  scale_y_continuous(breaks = c(62,62.9,60.4,68.8))

###CON GGARRENGE


plot_16 <- ggplot(resultados, aes(x=fecha[1],y=seg_alim[1])) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = lower_seg[1], ymax=upper_seg[1])) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) de seguridad alimentaria", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = 2016) +
  scale_y_continuous(breaks = c(61.4,62,62.7))

plot_18 <- ggplot(resultados, aes(x=fecha[2],y=seg_alim[2])) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = lower_seg[2], ymax=upper_seg[2])) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) de seguridad alimentaria", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = 2018) +
  scale_y_continuous(breaks = c(62,62.9,63.5))

plot_20 <-ggplot(resultados, aes(x=fecha[3],y=seg_alim[3])) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = lower_seg[3], ymax=upper_seg[3])) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) de seguridad alimentaria", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = 2020) +
  scale_y_continuous(breaks = c(60.4,59.9,61))

plot_22 <-ggplot(resultados, aes(x=fecha[4],y=seg_alim[4])) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = lower_seg[4], ymax=upper_seg[4])) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) de seguridad alimentaria", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = 2022) +
  scale_y_continuous(breaks = c(68.8,68.2,69.3))

ggarrange(plot_16, plot_18, plot_20, plot_22, nrow=2, ncol=2)



######graficos de bajo, mediano, alto grado de inseguridad alimentaria + seguridad alimentaria

  ### Primero se calcularan las partes proporcionales correspondientes a cada nivel, es decir los porcentajes por cada sector
setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb

bd_2020 <- read.csv('suma_enigh_2016.csv')
bd_c_2020 <- bd_2020 |>  as_survey(ids=upm, strata=est_dis,nest=T,weights=factor)
bd_c_2020 |> survey_count(acc_alim2)
table(bd_2020$acc_alim1)
bd_2020$acc_alim1
tabla2_2016 <- bd_c_2020 |> group_by(suma_seg_al_m) |>summarise(prop=survey_mean(vartype = 'ci'))|> round(3);tabla2


inseg_alt <- c(tabla2_2016$prop[4],tabla2_2018$prop[4],tabla2_2020$prop[4],tabla2_2022$prop[4])
inseg_alt_low <- c(tabla2_2016$prop_low[4],tabla2_2018$prop_low[4],tabla2_2020$prop_low[4],tabla2_2022$prop_low[4])
inseg_alt_upp <- c(tabla2_2016$prop_upp[4],tabla2_2018$prop_upp[4],tabla2_2020$prop_upp[4],tabla2_2022$prop_upp[4])
anio <- c(2016,2018,2020,2022)
#clasif <- c("anio","pro_ins_baja","low","upp")

tabla_insg_baja <- data.frame(anio,inseg_baja,inseg_baja_low,inseg_baja_upp)
tabla_insg_med <- data.frame(anio,inseg_med,inseg_med_low,inseg_med_upp)
tabla_insg_alt <- data.frame(anio,inseg_alt,inseg_alt_low,inseg_alt_upp)
library(openxlsx)
library(writexl)
library(readxl)

wb <- createWorkbook() #crea un libro

#crea las hojas con nombre
addWorksheet(wb, "baja")
addWorksheet(wb, "med")
addWorksheet(wb, "alt")




#guarda los dataframe correspondientes
writeData(wb ,sheet = "baja", x = tabla_insg_baja)
writeData(wb ,sheet = "med", x = tabla_insg_med)
writeData(wb ,sheet = "alt", x = tabla_insg_alt)

#gurda los libros
saveWorkbook(wb,file="clasif_niveles_inseguridad.xlsx")



## Graficando por cada nivel de inseguridad y seguridad alimentaria

setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb


resultados <- suppressWarnings(read.xlsx('clasif_niveles_inseguridad.xlsx'))
x<- resultados$anio

####ORIGINAL

plot_inseg_baja<-ggplot(resultados, aes(x=anio,y=inseg_baja)) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = inseg_baja_low, ymax=inseg_baja_upp)) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) inseguridad alimentaria baja", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = c(2016,2018,2020,2022))+
  scale_y_continuous(breaks = c(0.193,0.18,0.203,0.162))

plot_inseg_med<-ggplot(resultados, aes(x=anio,y=inseg_med)) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = inseg_med_low, ymax=inseg_med_upp)) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) inseguridad alimentaria media", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = c(2016,2018,2020,2022))+
  scale_y_continuous(breaks = c(0.108,0.11,0.112,0.088))

plot_inseg_alt<-ggplot(resultados, aes(x=anio,y=inseg_alt)) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = inseg_alt_low, ymax=inseg_alt_upp)) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) inseguridad alimentaria alta", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = c(2016,2018,2020,2022))+
  scale_y_continuous(breaks = c(0.079,0.081,0.081,0.062))

resultados_seg <- suppressWarnings(read.csv('resultados.csv'))
fecha<- resultados_seg$fecha
seg_alim <- resultados_seg$seg_alim
lower_seg <- resultados_seg$lower_seg
upper_seg <- resultados_seg$upper_seg

####ORIGINAL

plot_seg<-ggplot(resultados, aes(x=fecha,y=seg_alim)) + # contruye el plano, nombra los ejes acorde al nombre de la variable del dataframe
  geom_point() + #se grafican los puntos (x,y) que corresponda acorde a las variables anteriormente proporcionadas
  geom_errorbar(aes(ymin = lower_seg, ymax=upper_seg)) + #crea una barra de error y se introducen los intervalos
  labs(x="Año", y="(%) de seguridad alimentaria", caption = "FUENTE: Elaboración propia con datos del INEGI")+
  scale_x_continuous(breaks = c(2016,2018,2020,2022))+
  scale_y_continuous(breaks = c(62,62.9,60.4,68.8))

ggarrange(plot_inseg_baja, plot_inseg_med, plot_inseg_alt,plot_seg, nrow=2, ncol=2)


