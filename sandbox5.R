# Load terrain data
library('raster')
dem_tile <- readRDS("dem_tile_R_66_20.Rdata")
names(dem_tile) <- c("x", "y", "z")

# restrict to small sample
test_dim <- 20
dem_tile_bounded <- dem_tile[dem_tile$x %in% head(unique(dem_tile$x),test_dim)
                             & dem_tile$y %in% head(unique(dem_tile$y),test_dim), ]


# cast to matrix format
library(reshape2)
dem_tile_bounded_cast <- acast(dem_tile_bounded, x ~ y, value.var = "z")


# Slice into desired intervals
rr <- raster(t(dem_tile_bounded_cast))
rc <- cut(rr, breaks= 5)
pols <- rasterToPolygons(rc, dissolve=T)

# Remove polygons less than area threshold
polygon_areas <- lapply(pols@polygons, function(x) sapply(x@Polygons, function(y) y@area))

      sizeth <- 0.01 #size threshold of polygons to be deleted
      mainPolys <- lapply(polygon_areas, function(x) which(x > sizeth))
      
      
      for(i in 1:length(mainPolys)){   if(length(mainPolys[[i]]) >= 1 &&
                                          mainPolys[[i]][1] >= 1){
        pols@polygons[[i]]@Polygons <- pols@polygons[[i]]@Polygons[mainPolys[[i]]]
        pols@polygons[[i]]@plotOrder <- 1:length(pols@polygons[[i]]@Polygons)   } }

      
      slot(pols, "polygons") <- lapply(slot(pols, "polygons"),
                                         "comment<-", NULL)

#simplify polygons
library(rgeos)
pols_simplified <- gSimplify(pols, tol=1 , topologyPreserve=TRUE)
class(pols_simplified)

# Convert from spatial to dataframe
obj <- fortify(pols)

# split into individual lines for plotting
out <- split( obj , f = obj$group )
  
  #list of line 'names'
  section_ids <- unique(obj$group)

  
  
# initiallise plot p
p  <- plot_ly( x = NULL, y =  NULL, z =  NULL , type = 'scatter3d', mode = 'lines') 


# Add trace (line) per section to this graphic with a loop
for(i in section_ids){ 
  p <- p %>%   add_trace(name =i, x = out[[i]]$long, y =  out[[i]]$lat, z =  out[[i]]$id
                         , type = 'scatter3d', mode = 'lines') 
}

p

plot_ly(rr)

persp(rr, exp=0.1,phi=35, xlab='Longitude', ylab='Latitude', zlab='Elevation', shade=0.45, col='green4')
lines()
