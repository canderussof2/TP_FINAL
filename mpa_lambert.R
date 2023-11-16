library(ggplot2)
#library(ggmap)
library(maps)
#library(mapdata)
#library(maptools)
#gpclibPermit()
library(mapproj)


# Get the map outlines
outlines <- as.data.frame(map("world", plot = FALSE, 
                              xlim = c(-94.33081,  -35.66919), 
                              ylim = c( -56.85317,-11.64596))[c("x","y")])
worldmap <-geom_path(aes(x, y), inherit.aes = FALSE, 
                     data = outlines, alpha = 0.8, show_guide = FALSE)


# The layer for the observed variable
Tempmap <- geom_point(aes(x=Longitudes, y=Latitudes, colour=Temperatura), data=df_arg_20_00_00) 

# Plot the first map
ggplot() + Tempmap + worldmap
###############################################################################
# Fix the wrapping issue
dat2 <- df_arg_20_00_00
dat2$lon <- ifelse(dat2$Longitudes>0, dat2$Longitudes-max(dat2$Longitudes)+min(dat2$Longitudes), dat2$Longitudes)

# Remake the outlines
outlines2 <- as.data.frame(map("world", plot = FALSE, 
                               xlim = c(max(min(dat2$Longitudes)), max(dat2$Longitudes)), 
                               ylim = c(min(dat2$Latitudes), max(dat2$Latitudes)))[c("x","y")])
worldmap2 <- geom_path(aes(x, y), inherit.aes = FALSE, 
                       data = outlines2, alpha = 0.8, show_guide = FALSE)

# Remake the variable layer
ggp <- ggplot(aes(x=Longitudes, y=Latitudes), data=dat2)
Tempmap2 <- geom_point(aes(colour=Temperatura), shape=15)

lat0<-6370*cos(20*pi/180)/(2*pi)
lat1<-6370*cos(60*pi/180)/(2*pi)
# Try a projection
projection <- coord_map(projection="lambert", lat0=-35, lat1=-35, 
                        orientation=c(90,0,90))

# Plot
# Without projection
ggp + Tempmap2 + worldmap2
# With projection
ggp + Tempmap + worldmap + projection
