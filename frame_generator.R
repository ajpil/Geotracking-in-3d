# set number of frames
frames <- 100

# define keyframes for each broadcast location 
# assumes non-directional broadcast
library(tibble)
b1_kf <- frame_data(~t, ~lat, ~lon, ~freq, ~dB,
                    0, )



b2_kf <- frame_data(~t, ~lat, ~lon, ~freq, ~dB)

  

flight_path_kf <- frame_data(~t, ~lat, ~lon, -alt)


# display known broadcast frequencies.
# User sets threshold per frequency
default_threshold <- 20


# calculate broadcast radius per broadcast location


# interpolate over frames 


# animate


