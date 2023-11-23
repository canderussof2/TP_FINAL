#TP FINAL 
rm(list=ls()) #limpio el environment
setwd("/Users/cande/Desktop/Labo/TP_FINAL/")
ruta <- "/Users/cande/Desktop/Labo/TP_FINAL/"

#PRONOSTICO DE TEMPERATURA
#Salidas del modelo operativo WRF del SMN

#-------------------------------------------------------------------------------
#a
#Generar una funcion de descarga de los archivos, para una dada fecha de inicializacion 
#de los pronosticos, y los plazos en los cuales es valido dicho pron?stico (es 
#decir el pronostico a 12, 24, 36 hs )

print(paste("Hola, soy Cande je. Porfi al ingresar la fecha ingresarlo en formato YYYY-MM-DD HH:MM:SS y entre comillas"))

#Descarga archivos NCDF del pronostico del tiempo
#Argumentos
#. Fecha: Fecha ingresada como escalar DDMMYYYY
#. Hora: Hora del pronostico. Puede ser tanto 00, 06, 12 o 18
#. Pronostico: El plazo de validez del pronostico

require(lubridate)
library(lubridate)

descarga <- function (fecha,pronostico){
  fecha_inicial <- ymd_hms(fecha) 
  anio <- year(fecha_inicial)
  mes <- month(fecha_inicial)
  if(mes<10){
    mes <- paste0(0,mes)
  }
  dia <- day(fecha_inicial)
  if(dia<10){
    dia <- paste0(0,dia)
  }
  hora <- hour(fecha_inicial)
  if(hora<10){
    hora <- paste0(0,hora)
  }
  if(pronostico==00){
    pronostico <- paste0(0,pronostico)
  }
  fecha_nombre <- as.numeric(paste0(anio,mes,dia))
  url <- paste0("https://smn-ar-wrf.s3.amazonaws.com/DATA/WRF/DET/",anio,"/",mes,"/",dia,"/",hora,"/WRFDETAR_01H_",anio,mes,dia,"_",hora,"_0",pronostico,".nc")
  nombre <- paste0(fecha_nombre,"_",hora,"_",pronostico,".nc")
  directorio <- paste0(ruta,nombre)
  
  download.file(url,directorio)
}
#descarga("2023-06-13 00:00:00",00)

################################################################################
descarga <- function (fecha,pronostico){
  fecha_inicial <- ymd_hms(fecha) 
  secuencia <- seq (0,12, by = 12)
  for(i in 0:2){
    fecha_inicial <- fecha_inicial + days (i)
    dia <- day(fecha_inicial)
    anio <- year(fecha_inicial)
    mes <- month(fecha_inicial)
    hora <- hour(fecha_inicial)
    if(mes<10){
      mes <- paste0(0,mes)
    }
    if(dia<10){
      dia <- paste0(0,dia)
    }
    if(hora<10){
      hora <- paste0(0,hora)
    }
    if(pronostico==00){
      pronostico <- paste0(0,pronostico)
    }
    fecha_nombre <- as.numeric(paste0(anio,mes,dia))
    url <- paste0("https://smn-ar-wrf.s3.amazonaws.com/DATA/WRF/DET/",anio,"/",mes,"/",dia,"/",hora,"/WRFDETAR_01H_",anio,mes,dia,"_",hora,"_0",pronostico,".nc")
    nombre <- paste0(fecha_nombre,"_",hora,"_",pronostico,".nc")
    directorio <- paste0(ruta,nombre)
    download.file(url,directorio)
    pronostico <- pronostico + 12
  }
}

descarga <- function (fecha){
  fecha_inicial <- ymd_hms(fecha) 
  secuencia <- seq(0,48,by=12)
  for(i in secuencia){
    dia <- day(fecha_inicial)
    anio <- year(fecha_inicial)
    mes <- month(fecha_inicial)
    hora <- hour(fecha_inicial)
    pronostico <- i
    if(mes<10){
      mes <- paste0(0,mes)
    }
    if(dia<10){
      dia <- paste0(0,dia)
    }
    if(hora<10){
      hora <- paste0(0,hora)
    }
    if(i==0){
      pronostico <- paste0(0,pronostico)
    } else {
      pronostico <- as.numeric(i)
    }
    fecha_nombre <- as.numeric(paste0(anio,mes,dia))
    url <- paste0("https://smn-ar-wrf.s3.amazonaws.com/DATA/WRF/DET/",anio,"/",mes,"/",dia,"/",hora,"/WRFDETAR_01H_",anio,mes,dia,"_",hora,"_0",pronostico,".nc")
    nombre <- paste0(fecha_nombre,"_",hora,"_",pronostico,".nc")
    directorio <- paste0(ruta,nombre)
    download.file(url,directorio)
  }
}
descarga("2023-06-13 00:00:00")

################################################################################

#Para el dia 20/10/2023:
#-------------------------------------------------------------------------------
#b 
#Graficar la temperatura y viento en superficie en Argentina(75?O-55?O,50|2?S-20?S),
#para los tiempos de pron?sticos de 0hs, 12, 24, 36 y 48 hs

#Quiero el pronostico desde las 00:00:00 del 20 del 10 del 23. Inicializa
#Quiero el pronostico desde las 12:00:00 del 20 del 10 del 23. A 12 hrs
#Quiero el pronostico desde las 00:00:00 del 21 del 10 del 23. A 24 hrs
#Quiero el pronostico desde las 12:00:00 del 21 del 10 del 23. A 36 hrs
#Quiero el pronostico desde las 00:00:00 del 22 del 10 del 23. A 48 hrs

require(ncdf4)
library(ncdf4)

#--------------------------------- 20/10/23 ------------------------------------
descarga("2023-10-20 00:00:00",00) #Descargo el de las 00 del 20/10/23

################################################################################
secuencia <- seq(0,48,by=12)
for (i in secuencia ){ 
  if (i == 0) {
    pronostico <- paste0("0",i)
  } else {
    pronostico <- i
  }
  nombre <- paste0("20231020_00_",pronostico)
  archivo <- nc_open(nombre) #iria con comillas?
}

################################################################################
inicio <- nc_open("20231020_00_00.nc") #abro el archivo NCF
class(inicio) #ncdf
temp_20_00 <- ncvar_get(inicio,"T2") #Extraigo la temperatura a 2m (?C). CHEQUEAR
direc_viento_20_00 <- ncvar_get(inicio,"dirViento10") #Extraigo la direccion del viento a 10 m (?). CHEQUEAR
veloc_viento_20_00 <- ncvar_get(inicio,"magViento10") #Extraigo la intensidad del viento a 10 m (m/s). CHEQUEAR
long <- ncvar_get(inicio,"lon")
lat <- ncvar_get(inicio,"lat")
time_20_00 <- ncvar_get(inicio,"time") #es cero porque es un tiempo solo, el tiempo cero
index_quiero<- which( (lat>-50) & (lat< -20) & (long>-75) & (long< -55))
latitudes <- lat[index_quiero]
longitudes <- long[index_quiero]
temp_20_00_arg <- temp_20_00[index_quiero]
direc_viento_20_00_arg <- direc_viento_20_00[index_quiero]
veloc_viento_20_00_arg <- veloc_viento_20_00[index_quiero] 
df_arg_20_00_00 <- data.frame("Temperatura"=temp_20_00_arg,"Direccion"=direc_viento_20_00_arg,"Velocidad"=veloc_viento_20_00_arg,"Latitudes"=latitudes,"Longitudes"=longitudes)
df_viento_arg_20_00 <-  data.frame("Velocidad"=veloc_viento_20_00_arg,"Direccion"=direc_viento_20_00_arg)
#df_arg_20_00_00<- data.frame("Temperatura"=as.vector(temp_20_00),"Latitudes"=as.vector(lati),"Longitudes"=as.vector(longi))
direcc <- df_viento_arg_20_00$Direccion 
radianes <- c()
for (i in 1:nrow(df_viento_arg_20_00)){
  radian <- direcc[i]*pi/180
  radianes <- c(radianes, radian)
}
df_viento_arg_20_00 <-  data.frame("Velocidad"=veloc_viento_20_00_arg,"Direccion"=radianes)

library(ggplot2)
library(maps)
library(mapproj)
library(sf)
# Get the map outlines
outlines <- as.data.frame(map("world", plot = FALSE, 
                              xlim = c(-94.33081,  -35.66919), 
                              ylim = c( -56.85317,-11.64596))[c("x","y")])
worldmap <-geom_path(aes(x, y), inherit.aes = FALSE, 
                     data = outlines, alpha = 0.8, show_guide = FALSE)

#Temperatura
# The layer for the observed variable
Tempmap_20_00 <- geom_point(aes(x=Longitudes, y=Latitudes, colour=Temperatura), data=df_arg_20_00_00) 

# Plot the first map
graf_temp_20_00 <- ggplot() + Tempmap_20_00 + worldmap +ggtitle("Pronostico incial de Temperatura 20/10 ") 

#Veloc Viento
speed_map_20_00 <- geom_point(aes(x=Longitudes, y=Latitudes, colour=Velocidad), data=df_arg_20_00_00) 
graf_veloc_20_00 <- ggplot() + speed_map_20_00 + worldmap
speed_map_20_00<- geom_arrow(aes(dx = u, dy = v))



#Direccion
library(circular)
windrose(df_viento_arg_20_00,ws = "Velocidad", wd = "Direccion",breaks = c(1,2,3,4,5,6,7,8,9,10,11),cols = c("yellow","green","blue","pink"))
colors()
windrose(df_viento_arg_20_00,ws = "Velocidad", wd = "Direccion", breaks = c(1,3,5,7,9,11), angle = 10, cols = c("yellow","green","blue","pink"), paddle = T,offset =
           5, key.header = "Velocidad de viento",key.footer = "metros por segundo", key.position =
           "left",main= "Rosa de vientos", width = 1)

windrose(df_viento_arg_20_00, ws = "Velocidad", wd = "Direccion",
         ws.int = 0.5, angle = 10, type = "default", bias.corr = TRUE, cols
         = c("yellow","green","blue","pink"), grid.line = NULL, width = 1, seg = NULL, auto.text 
         = TRUE, breaks = 4, offset = 10, normalise = FALSE, max.freq = 
           NULL, paddle = TRUE, key.header = NULL, key.footer = "(m/s)", 
         key.position = "bottom", key = TRUE, dig.lab = 5, statistic = 
           "prop.count", pollutant = NULL, annotate = TRUE, angle.scale = 
           315, border = NA)


############################################################
require(ggplot2)
library(ggplot2)

mapa <- map_data("world")
head(mapa)
argentina_mapa <- geom_path(data=mapa,aes(x=long,y=lat,group=group),size=0.1) #gripo 1 es mapa mundial cheqeuar
g <- ggplot(df_arg_20_00_00, aes(x=Longitudes,y=Latitudes)) + geom_tile(aes(fill =Temperatura)) + argentina_mapa
  
  geom_contour(aes(z=Temperatura))# + argentina_mapa
g<-g+argentina_mapa
g
projection <- coord_map(projection="lambert", lat0=30, lat1=60, 
                        orientation=c(87.5,0,255))


#--------------------------------- 20/10/23 ------------------------------------
descarga("2023-10-20 00:00:00",12) #Descargo el de las 12 del 20/10/23
pronostico_doce<-nc_open("20231020_00_12.nc") 
temp_20_12 <- ncvar_get(pronostico_doce,"T2") 
direc_viento_20_12 <- ncvar_get(pronostico_doce,"dirViento10") 
veloc_viento_20_12 <- ncvar_get(pronostico_doce,"magViento10")
time_20_12 <- ncvar_get(pronostico_doce,"time")

nc_open()

