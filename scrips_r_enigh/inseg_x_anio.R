library(tidyverse)
library(dplyr)

setwd('D:/UAEMitas/enigh/BD_enigh') #windows al usar usb
setwd('/Volumes/USB MARIA/UAEMitas/enigh/BD_enigh')#mac al usar usb


base_c<-read.xlsx('tabla_entidades_prop2.xlsx')

##DADO QUE EL EXCEL NO CUENTA CON LA ESTRUCTURA CONVENCIONAL, PROCEDEMOS A IDENTIFICAR 
### LOS NOMBRES DE LAS COLUMNAS DEL DATAFRAME
column_names <- colnames(base_c)
print(column_names)

anio_2016 <- base_c %>% select(X1,"SA-2016",X3,X4)
anio_2018 <- base_c %>% select(X1,"SA-2018",X6,X7)
anio_2020 <- base_c %>% select(X1,"SA-2020",X9,X10)
anio_2022 <- base_c %>% select(X1,"SA-2022",X12,X13)


wb <- createWorkbook() #crea un libro

#crea las hojas con nombre
addWorksheet(wb, "BD_2016")
addWorksheet(wb, "BD_2018")
addWorksheet(wb, "BD_2020")
addWorksheet(wb, "BD_2022")

#guarda los dataframe correspondientes
writeData(wb ,sheet = "BD_2016", x = anio_2016)
writeData(wb ,sheet = "BD_2018", x = anio_2018)
writeData(wb ,sheet = "BD_2020", x = anio_2020)
writeData(wb ,sheet = "BD_2022", x = anio_2022)

#gurda los libros
saveWorkbook(wb,file="BASES_ANIOS_ENTIDAD.xlsx")



