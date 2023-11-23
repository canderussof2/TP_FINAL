rm(list=ls()) #limpio el environment
setwd("/Users/cande/Desktop/Labo/TP_FINAL/")
require(ncdf4)
library(ncdf4)

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
speed_map_20_00 <- geom_arrow(aes(dx = u, dy = v))

speed <- ggplot(df_arg_20_00_00,aes(x=Longitudes,y=Latitudes,fill=Velocidad)) + 
        geom_bar(stat = "identity", color = "white",
           lwd = 1, show.legend = FALSE)

speed + coord_polar()
# Datos
set.seed(4)
df <- data.frame(x = 1:10,
                 y = sample(1:10))

p <- ggplot(df, aes(x = x, y = y, fill = y)) +
  geom_bar(stat = "identity", color = "white",
           lwd = 1, show.legend = FALSE)

p + coord_polar() 

