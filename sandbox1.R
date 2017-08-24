#### load package rgl ####
library(rgl)

#### construct test list with coordinate matrices ####
a <- list()
b1 <- matrix(rnorm(6), ncol = 2)
b2 <- matrix(rnorm(6), ncol = 2)
b3 <- matrix(rnorm(6), ncol = 2)

a[[1]] <- b1
a[[2]] <- b2
a[[3]] <- b3

#### define test height vector ####
h <- c(.3, .5, .1)

#### simple plot prism function ####
# a: list with coordinate matrices
# h: height vector
plotprism <- function(a, h){
  # general loop to plot every prism
  for(i in 1:length(h)){
    # transform matrizes to data.frames and add height column 
    # -> separation of top and bottom triangle
    top <- data.frame(a[[i]], h[i]) 
    bottom <- data.frame(a[[i]], 0) 
    # adjust colnames to axis names
    colnames(top) <- c("x", "y", "z") 
    colnames(bottom) <- c("x", "y", "z") 
    # plot triangles (as wireframes)
    triangles3d(bottom, front = "line", back = "line")
    triangles3d(top, front = "line", back = "line")
    # plot vertical lines to connect the triangles
    for(i in 0:2){
      segments3d(
        x = c(bottom$x[1+i], top$x[1+i]),
        y = c(bottom$y[1+i], top$y[1+i]),
        z = c(bottom$z[1+i], top$z[1+i])
      )
    }
  }
  #### add coordinate system ####
  axes3d()
}

#### call plot function for test data ####
plotprism(a, h)





tz <- sf::st_read('WRLDTZA')
rgl.Map(tz)
rgl.spheres(0,0,0,.999, col='darkblue')




library(rgl)

drw_dem <- read.csv("~/Documents/FlightPathShapeCollision/DEM/srtm_63_15.xyz")
dem_tile <- read.table(file = "~/Documents/FlightPathShapeCollision/DEM/srtm_63_15.xyz", sep = '\t', header = FALSE)

dem_tile <- drw_dem

north_lim <- -12
south_lim <- -13
west_lim <- 130
east_lim <- 131


dem_tile <- dem_tile[dem_tile$V1 >= west_lim
        & dem_tile$V1 <= east_lim
        & dem_tile$V2 >= south_lim
        & dem_tile$V2 <= north_lim,] 


#x <- unique(dem_tile$V1)
#y <- unique(dem_tile$V2)



dem_tile <- acast(dem_tile, V1 ~ V2, value.var = "V3")
#rownames(dem_tile) <- dem_tile$V1
#dem_tile$V1 <- NULL

z<-dem_tile
x <- 90 * (1:nrow(z)) # 90 meter spacing (S to N)
y <- 90 * (1:ncol(z)) # 90 meter spacing (E to W)

zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen,alpha=0) # height color lookup table
col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
open3d()
rgl.surface(x, y, z, color=col, alpha=0.75, back="lines")







z <- 2 * volcano # Exaggerate the relief
x <- 10 * (1:nrow(z)) # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z)) # 10 meter spacing (E to W)
zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen,alpha=0) # height color lookup table
col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
open3d()
rgl.surface(x, y, z, color=col, alpha=0.75, back="lines")

length(y)

temp <- seq(-pi, 0, length = 50)
x <- c(rep(1, 50) %*% t(cos(temp)))
y <- c(cos(temp) %*% t(sin(temp)))
z <- c(sin(temp) %*% t(sin(temp)))
scatterplot3d(x, y, z, highlight.3d=TRUE,
              col.axis="blue", col.grid="lightblue",
              main="scatterplot3d - 2", pch=20)
