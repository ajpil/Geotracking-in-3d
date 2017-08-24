#############################################################################################
library(maptools)
library(raster)
alt <- getData('alt', country='PHL')
adm <- getData('GADM', country='PHL', leve=1)
mar<-(adm[adm$NAME_1=='Marinduque',])
maralt<-crop(alt,mar)
persp(maralt, exp=0.2,phi=35, xlab='Longitude', ylab='Latitude', zlab='Elevation')

#############################################################################################
persp(maralt, exp=0.1,phi=35, xlab='Longitude', ylab='Latitude', zlab='Elevation', shade=0.45, col='green4')
#############################################################################################




library(plotly)



# Generate data
library(reshape2) # for melt
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")

# Basic plot
v <- ggplot(volcano3d, aes(x, y, z = z))
v <- v + stat_contour()

ggplotly(v)

qplot(x, y, data=pp(100), geom="tile", fill=z)

