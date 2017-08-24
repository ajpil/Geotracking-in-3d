# devtools::install_github("ropensci/plotly")
# library(dplyr)

###LOAD TERRAIN
library(plotly)

#dem_tile <- readRDS("dem_tile_R_66_20.Rdata")
dem_tile <- readRDS("dem_tile_R_63_15.Rdata")

names(dem_tile) <- c("x", "y", "z")

# Re-assign values <0 to 0
dem_tile$z <- pmax(dem_tile$z,0)


# cast to matrix format
library(reshape2)
dem_tile_cast <- acast(dem_tile, x ~ y, value.var = "z")
rm(dem_tile)

# cast to raster
library(raster)
dem_tile_raster <- raster(t(dem_tile_cast))
#rm(dem_tile_cast)
plot(dem_tile_raster)

# # restrict to small sample
# test_dim <- 500
# dem_tile_cast_bounded <- dem_tile_cast[1:test_dim,1:test_dim]


# reduce resolution to managable number of points
  # sample_interval of 10 means new grid takes every 10th row and 10th column
  # reducing number of points to 1/100th (ie 10^2 of original)
sample_interval <- 50
sample_interval_vector <- c(TRUE, rep(FALSE,sample_interval-1))
dem_tile_resampled <- dem_tile_cast[sample_interval_vector,sample_interval_vector]
dem_tile_raster_resampled <- raster(t(dem_tile_resampled))
plot(dem_tile_raster)



x <- rownames(dem_tile_resampled)
y <- colnames(dem_tile_resampled)
z <- dem_tile_resampled

# For setting plot bounds
max_long <- max(x)
max_lat <- max(y)
max_elev <- max(z)





### ADD ANIMATED POINTS
# Key frame animation does not work in 3d plots in plotly yet (only 2d).
# Therefore, each frame is animated individualy

# Z axis range cannot be fixed in plotly yet, so a dummy 
# point is inserted at the maximum elevation found in the data 

library(tibble)
markers_attributes <- frame_data(
    ~id, ~r, ~col, ~freq
       ,1     ,20 ,"purple"    ,NA
       ,2    ,25   ,"blue"    ,12
       ,3    ,40   ,"blue"    ,25
       ,4    ,40   ,"blue"    ,25
       ,5    ,15   ,"blue"    ,21
       ,6    ,38    ,"red"    ,31
       ,7    ,45    ,"red"    ,25
       ,8    ,20    ,"red"    ,25
       ,9    ,48    ,"red"    ,25
      ,10    ,20    ,"red"    ,25
      ,11    , 5    ,"red"    ,25
      ,12    ,19    ,"red"    ,25
      ,13    ,59    ,"red"    ,25
      ,14     ,9    ,"red"    ,32
      ,15    ,27    ,"red"    ,25
      ,16    ,17    ,"red"    ,25
      ,17    ,38 ,"orange"    ,25
      ,18    ,28 ,"orange"    ,14
      ,19    ,23 ,"orange"    ,17
      ,20    ,48 ,"orange"    ,25
  
)


markers_key_frames <- frame_data(
  ~id, ~frame , ~x, ~y, ~z
  ,1, 0, 130.5, -12.5, 200
  ,1, 5, 131, -13.5, 1800
  ,1, 25, 134.5, -12.75, 1800
  ,1, 30, 134.5, -10.5, 1200
  ,1, 45, 131, -10.5, 1200
  ,1, 50, 130.5, -12.5, 200
  ,2, 0, 134.2, -13, 1100
  ,2, 25, 132.1, -13, 200
  ,2, 35, 132.7, -12, 500
  ,3, 0, 130.1, -13, 800
  ,4, 0, 131, -13, 300
  ,5, 0, 131, -12, 600
  ,6, 0, 132.7, -13, 280
  ,6, 31, 134.8, -13, 2500
  ,6, 50, 135, -10, 700
  ,7, 0, 131, -11, 800
  ,7, 31, 134, -15, 800
  ,7, 50, 133, -12, 800
  ,8, 0, 132.4, -13, 200
  ,8, 16, 132.7, -13, 200
  ,8, 44, 132, -12, 200
  ,8, 50, 134, -13, 200
  ,9, 0, 132, -13, 500
  ,9, 50, 132, -12, 500
  ,10, 0, 134.5, -12.3, 200
  ,11, 0, 132, -10.5, 200
  ,12, 0, 132, -11, 200
  ,13, 0, 133.8, -11.6, 200
  ,14, 0, 133.1, -13, 200
  ,15, 0, 130.7, -11.1, 200
  ,16, 0, 134.9, -11.6, 200
  ,17, 0, 132.45, -13.9, 200
  ,18, 0, 131.2, -14.5, 200
  ,18, 25, 131.3, -14.3, 200
  ,19, 0, 131.2, -14.9, 200
  ,20, 0, 131.0, -14.8, 200
 )

# Combine static and temporal marker key frames
markers_key_frames <- merge(x = markers_key_frames, y = markers_attributes, by = "id")


# Z axis range cannot be fixed in plotly yet, so a dummy 
# point is inserted at the maximum elevation found in the data 
markers_key_frames <- rbind(markers_key_frames, c(
                              min(markers_key_frames$id) -1
                            , min(markers_key_frames$frame)
                            , max_long
                            , max_lat
                            , max(markers_key_frames$z, max_elev)
                            , 0
                            , "white")
                            )
markers_key_frames <- as.data.frame(markers_key_frames)

markers_key_frames$index <- as.integer(row.names(markers_key_frames))
markers_key_frames$index_with_offset <- markers_key_frames$index + 1

# Create dataframe for interpolating keyframe data: markers_kfi
markers_kfi <- merge(x = markers_key_frames
                            , y = markers_key_frames[c("index","id","frame", "x", "y", "z", "r", "col")]
                            , by.x = "index_with_offset", by.y = "index", suffixes = c("","_next")
                            , all.x = TRUE)

#markers_kfi -> markers_kfi
markers_kfi$frame_interval <- as.numeric(markers_kfi$frame_next) - as.numeric(markers_kfi$frame)

markers_kfi[na.omit(markers_kfi$id != markers_kfi$id_next),c( "x_next", "y_next", "z_next", "r_next", "col_next", "frame_interval")] <- NA
markers_kfi$index_with_offset <- NULL
markers_kfi$id_next <- NULL

sapply( markers_kfi, class)


numeric_cols = c("id", "frame"
                  , "x", "y", "z"
                  , "r"
                  , "index", "frame_next"
                  , "x_next", "y_next", "z_next"
                  , "r_next", "frame_interval")
char_cols = c("col", "col_next")

markers_kfi[,numeric_cols]  <- sapply( markers_kfi[,numeric_cols], as.numeric )




markers_kfi <- within(markers_kfi, x_interval <- (x_next - x)/frame_interval )
markers_kfi <- within(markers_kfi, y_interval <- (y_next - y)/frame_interval )
markers_kfi <- within(markers_kfi, z_interval <- (z_next - z)/frame_interval )
markers_kfi <- within(markers_kfi, r_interval <- (r_next - r)/frame_interval )
#markers_kfi <- within(markers_kfi, col_interval <- (col_next - col)/frame_interval )

markers_kfi[,c("x_interval", "y_interval", "z_interval", "r_interval")][is.na(markers_kfi[,c("x_interval", "y_interval", "z_interval", "r_interval")])] <- 0
# markers_kfi[,c("x_interval", "y_interval", "z_interval", "r_interval", "col_interval")][is.na(markers_kfi[,c("x_interval", "y_interval", "z_interval", "r_interval")])] <- 0


markers_kfi <- as.data.frame(markers_kfi[order(markers_kfi$id, markers_kfi$frame),])
# Define frame_max (i.e. frames)
frame_min <- 0
frame_max <- 50



empty_frames <- data.frame(frame = frame_min:frame_max
                           # , x = NA
                           # , y = NA
                           # , z = NA
                           # , r = NA
                           # , col = NA
)


## Interpolate key frames over all frames
# Populate data with one 'skeleton' per object per frame
markers <- merge(x = unique(markers_kfi[,"id"]), y = empty_frames)
names(markers)[names(markers) == 'x'] <- 'id'
markers <- markers[order(markers$frame, markers$id),]
#markers <- markers[order(markers$id, markers$frame),]


# Merge with keyframe data
markers <- merge(x = markers, y = markers_kfi, by=c("id","frame"), all.x=TRUE)

markers_bckp <- markers

markers <- markers_bckp

for (i in 1:nrow(markers)) {
  if(is.na(markers$x[i])) {
    markers$x_interval[i] <- markers$x_interval[i-1]
    markers$y_interval[i] <- markers$y_interval[i-1]
    markers$z_interval[i] <- markers$z_interval[i-1]
    markers$r_interval[i] <- markers$r_interval[i-1]
    markers$col_interval[i] <- markers$col_interval[i-1]
    
    markers$x[i] <- markers$x[i-1] + markers$x_interval[i]
    markers$y[i] <- markers$y[i-1] + markers$y_interval[i]
    markers$z[i] <- markers$z[i-1] + markers$z_interval[i]
    markers$r[i] <- markers$r[i-1] + markers$r_interval[i]
    markers$col[i] <- markers$col[i-1]
    #    markers$col[i] <- markers$col[i-1] + markers$col_interval[i]
  }
  next
}

markers <- markers[,c("id", "frame","x", "y", "z", "r", "col")]
markers <- markers[order(markers$frame, markers$id),]


p <- plot_ly(x = x, y = y, z = z, type = "surface"
             , showscale = FALSE
             , showlegend = FALSE
             
             , colors = c("#0080FF"
                           , "#66B2FF"
                           , "#FFFF00"
                           , "#66CC00"
                           , "#66CC00"
                           , "#66CC00"
                           , "#66CC00"
                           , "#66CC00"
                           , "#66CC00"
                           , "#336600"
                           , "#336600"
                           , "#336600"
                           , "#336600"
                           , "#336600"
                           , "#336600"
                           , "#336600"
                           , "#336600"
                           , "#999900"
                           , "#FFFFFF"
                           , "#FFFFFF"
             ))
             
p <- p  %>%  layout(hovermode = FALSE
               , scene = list(aspectmode = 'manual'
                              , camera = list(eye = list(x = 0, y = -3, z =1))
                              # , domain = list(x = list(0,0.5), y = list(0,.5))
                              , aspectratio = list(x=1, y=1, z=0.05)) )  # "squishes" z axis 

q <- p %>%   add_trace(data = markers, x = ~x, y =  ~y, z = ~z
                  , type = 'scatter3d', mode = 'markers', frame = ~frame
                  , marker = list(color = ~col
                                  , size = ~r*3
                                  ))

# Add mission path
mission_id <- 1 # define marker object to be shown as paths
q <- q %>%   add_trace( name = 'mission'
                        , data = markers_key_frames[markers_key_frames$id == mission_id, ]
                       , x = ~x, y =~y, z = ~z
                       , type = 'scatter3d', mode = 'line'
                       , line = list(color = ~col
                                     , width = 5
                                       , dash = 'dot'
                       ))

# Render plot
q


