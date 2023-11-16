#TP FINAL 
rm(list=ls()) #limpio el environment
#setwd("/home/clinux01/Escritorio/Cande Labo Jueves/TP_FINAL/")
#setwd("/Users/cande/Desktop/Labo/TP_FINAL/")
setwd("/home/clinux01/Escritorio/Cande Labo Jueves/TP_FINAL/")
ruta <- "/home/clinux01/Escritorio/Cande Labo Jueves/TP_FINAL/"
#PRONOSTICO DE TEMPERATURA
#Salidas del modelo operativo WRF del SMN

#-------------------------------------------------------------------------------
#a
#Generar una funci?n de descarga de los archivos, para una dada fecha de inicializaci?n 
#de los pron?sticos, y los plazos en los cuales es v?lido dicho pron?stico (es 
#decir el pron?stico a 12, 24, 36 hs )

print(paste("Hola, soy Cande je. Porfi al ingresar la hora y el plazo de pronostico ingresarlo entre comillas"))

#Descarga archivos NCDF del pronostico del tiempo
#Argumentos
 #. Fecha: Fecha ingresada como escalar DDMMYYYY
 #. Hora: Hora del pronostico. Puede ser tanto 00, 06, 12 o 18
 #. Pronostico: El plazo de validez del pronostico

descarga <- function (fecha,hora,pronostico){
  #Agregar algo de un if si la fecha es menor a 10 agregarle un cero si no no abre
  fecha <-as.character(fecha)
  pronostico <- as.character(pronostico)
  anio <- as.numeric(substr(fecha,5,8))
  mes <- as.numeric(substr(fecha,3,4))
  dia <- as.numeric(substr(fecha,1,2)) #Ver con lubridate y sumarle a?os y demas
  
  url <- paste0("https://smn-ar-wrf.s3.amazonaws.com/DATA/WRF/DET/",anio,"/",mes,"/",dia,"/",hora,"/WRFDETAR_01H_",anio,mes,dia,"_",hora,"_0",pronostico,".nc")
  nombre <- paste0(fecha,"_",hora,"_",pronostico,".nc")
  directorio <- paste0(ruta,nombre)
  
  download.file(url,directorio)
}

#fijate con lubridate
descarga <- function (fecha,pronostico){
  fecha_inicial <- ymd_hms(fecha) 
  anio <- year(fecha_inicial)
  mes <- month(fecha_inicial)
  dia <- day(fecha_inicial)
  hora <- hour(fecha_inicial)
  url <- paste0("https://smn-ar-wrf.s3.amazonaws.com/DATA/WRF/DET/",anio,"/",mes,"/",dia,"/",hora,"/WRFDETAR_01H_",anio,mes,dia,"_",hora,"_0",pronostico,".nc")
  nombre <- paste0(fecha,"_",hora,"_",pronostico,".nc")
  directorio <- paste0(ruta,nombre)
  
  download.file(url,directorio)
}

#descarga(20102023,"06","02")

#Para el d?a 20/10/2023:
#-------------------------------------------------------------------------------
#b 
#Graficar la temperatura y viento en superficie en Argentina(75 O-55 O,50 S-20 S),
#para los tiempos de pron?sticos de 0hs, 12, 24, 36 y 48 hs

#Quiero el pronostico desde las 00:00:00 del 20 del 10 del 23. Inicializa
#Quiero el pronostico desde las 12:00:00 del 20 del 10 del 23. A 12 hrs
#Quiero el pronostico desde las 00:00:00 del 21 del 10 del 23. A 24 hrs
#Quiero el pronostico desde las 12:00:00 del 21 del 10 del 23. A 36 hrs
#Quiero el pronostico desde las 00:00:00 del 22 del 10 del 23. A 48 hrs
install.packages("ncdf4") 
require(ncdf4)
library(ncdf4)

#--------------------------------- 20/10/23 ------------------------------------
descarga(20102023,"00","00") #Descargo el de las 00 del 20/10/23

inicio<-nc_open("20102023_00_00.nc") #abro el archivo NCDF
class(inicio) #ncdf
temp_20_00 <- ncvar_get(inicio,"T2") #Extraigo la temperatura a 2m (?C). CHEQUEAR
direc_viento_20_00 <- ncvar_get(inicio,"dirViento10") #Extraigo la direccion del viento a 10 m (?). CHEQUEAR
veloc_viento_20_00 <- ncvar_get(inicio,"magViento10") #Extraigo la intensidad del viento a 10 m (m/s). CHEQUEAR
long <- ncvar_get(inicio,"lon")
lat <- ncvar_get(inicio,"lat")
time_20_00 <- ncvar_get(inicio,"time") #es cero porque es un tiempo solo, el tiempo cero
proy_x <- ncvar_get(inicio,"x")
proy_y <- ncvar_get(inicio,"y")
dim(temp_20_00)

library(ggplot2)
library(metR)
GlanceNetCDF("20102023_00_00.nc")

temp_20_00 <- ReadNetCDF("20102023_00_00.nc", vars = c("T2","lon","lat"))
head(temp_20_00)
mapa <- map_data("world")
head(mapa)
mi_mapa <- geom_path(data = mapa, aes(long, lat, group = group), size = 0.1)
ggplot(temp_20_00, aes(x, y)) + geom_raster(aes(fill = "deepskyblue")) + mi_mapa

#--------------------------------- 20/10/23 ------------------------------------
descarga(20102023,"00",12) #Descargo el de las 12 del 20/10/23
pronostico_doce<-nc_open("20102023_00_12.nc") 
temp_20_12 <- ncvar_get(pronostico_doce,"T2") 
direc_viento_20_12 <- ncvar_get(pronostico_doce,"dirViento10") 
veloc_viento_20_12 <- ncvar_get(pronostico_doce,"magViento10")
time_20_12 <- ncvar_get(pronostico_doce,"time")
