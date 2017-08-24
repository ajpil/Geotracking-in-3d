

####
dem_tile <- read.table(file = "~/Documents/FlightPathShapeCollision/DEM/srtm_66_20.xyz", sep = '\t', header = FALSE)
saveRDS(dem_tile, "dem_tile_R_66_20.Rdata")

dem_tile <- readRDS("dem_tile_R_66_20.Rdata")
names(dem_tile) <- c("x", "y", "z")

test_dim <- 200
#elevation_contour_interval <- 1

dem_tile_bounded <- dem_tile[dem_tile$x %in% head(unique(dem_tile$x),test_dim)
                             & dem_tile$y %in% head(unique(dem_tile$y),test_dim), ]
                            # & dem_tile$z %% elevation_contour_interval == 0,] 
# 
# north_lim <- -36
# south_lim <- -37
# west_lim <- 148
# east_lim <- 149
# 
# dem_tile_bounded <- dem_tile[dem_tile$x >= west_lim
#                      & dem_tile$x <= east_lim
#                      & dem_tile$y >= south_lim
#                      & dem_tile$y <= north_lim,] 


library(reshape2)
dem_tile_bounded_cast <- acast(dem_tile_bounded, x ~ y, value.var = "z")
saveRDS(dem_tile_bounded_cast, paste0("dem_tile_bounded_cast_S",-north_lim,"_E",west_lim,".Rdata"))


x <- 90 *1:nrow(dem_tile_bounded_cast)
y <- 90 *1:ncol(dem_tile_bounded_cast)
contourLines(x, y, dem_tile_bounded_cast, nlevels = 10)


########

# Generate data
library(reshape2) # for melt
volcano3d <- melt(volcano)
names(dem_tile_bounded) <- c("x", "y", "z")


dem_tile_bounded_df <- as.matrix(dem_tile_bounded)
names(dem_tile_bounded_df) <- c("x", "y", "z")


# Basic plot
v <- ggplot(dem_tile_bounded, aes(x, y, z = z))
v <- v + stat_contour()

ggplotly(v)


####
library(lattice)
wireframe(z~x*y,dem_tile_bounded,colorkey=TRUE,drape=FALSE)




###
dem_tile <- readRDS("dem_tile_R_66_20.Rdata")
names(dem_tile) <- c("x", "y", "z")

# Chose dimension of test plot
test_dim <- 20
elevation_contour_interval <- 1

dem_tile_bounded <- dem_tile[dem_tile$x %in% head(unique(dem_tile$x),test_dim)
                             & dem_tile$y %in% head(unique(dem_tile$y),test_dim)
                             & dem_tile$z %% elevation_contour_interval == 0,] 

dem_tile_bounded <- dem_tile_bounded[sort(dem_tile_bounded$x),]
dem_tile_bounded <- dem_tile_bounded[sort(dem_tile_bounded$y),]

contour(dem_tile_bounded)

data <- dem_tile_bounded
library(plotly)
q <- plot_ly(data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers',
             marker = list(size = 1, color = 'red'))
q
