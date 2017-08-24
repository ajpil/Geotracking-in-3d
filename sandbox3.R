
library(scales)

# Bounding box
minlat=40
maxlat=90
minlong=-20
maxlong=20

# Range of bounding box
longrange=maxlong-minlong
latrange=maxlat-minlat

# Number of columns and rows of data in the bathymetry file
ncols=60 * 2 * longrange
nrows=60 * 2 * latrange

ntot=ncols*nrows # Total number of points -- probably too many to plot ...
steps=10         # ... so we use only every 'steps' point in each direction 

ux=seq(1,ncols,by=steps) # X coordinates (longitude)
uy=seq(1,nrows,by=steps)  # Y coordinates (latitude)

dat=expand.grid(x=ux,y=uy)             # Create grid
dat=transform(dat,index=(y-1)*ncols+x) # Calculate byte index in file

# Transform indices to real coordinates
dat=transform(dat, long=rescale(x, from=c(1,ncols),
                                         to=c(minlong+1/60/4, maxlong-1/60/4)),
              lat=rescale(y, from=c(1,nrows),
                                   to=c(maxlat-1/60/4, minlat+1/60/4)))

# Name of file containing the bathymetry data
filename="w020n90.Bathymetry.srtm"

# Read bathymetry data from the file
library(R.utils)
dat$z=readBinFragments(filename, what="integer", size=2, signed=TRUE,
                       idxs=dat$index,endian="big")

# Create matrix containing the data
dat.mat=matrix(dat$z,ncol=length(ux),byrow=TRUE)
dat.mat=t(dat.mat)[,nrow(dat.mat):1]

# Calculate topographic colours for elevation values
map.colours=function(z, water=c(-5000,0), land=c(0,1500), mountain,...)
{
  z.min=min(z)
  z.max=max(z)
  
  # Divide the land gradient into two parts
  land=c(land[1],mean(land),land[2]) 
  
  # Number of unique water and land values
  nw=length(unique(z[z<0]))
  nl=length(unique(z[z>=0]))
  
  # Create the output colour component vectors
  h <- s <- v <- rep(1, length(z))
  
  # h-values for water
  hvals=c(43/60,31/60)
  
  # Create water colours
  if(nw==1) h[z<0]=hvals[2] else if(nw>1)
    h[z<0]=approx(water,hvals,xout=z[z<0],rule=2)$y
  
  # h, s and v values for land
  hvals=c(4/12,2/12,0/12)
  svals=c(1,1,0)
  vvals=c(.65,.9,.95)
  
  # Create land colours
  if(nl==1)
  { h[z>=0] = hvals[1]; s[z>=0] = svals[1]; v[z>=0] = vvals[1]} else
    if(nl>1)
    {
      h[z>=0]=approx(land,hvals,xout=z[z>=0],rule=2)$y
      s[z>=0]=approx(land,svals,xout=z[z>=0],rule=2)$y
      v[z>=0]=approx(land,vvals,xout=z[z>=0],rule=2)$y
    }
  
  hsv(h,s,v)
}

# Basic contour plot -- not very pretty ...
with(dat, contour(unique(long),rev(unique(lat)),dat.mat))

# Colours make the whole thing look quite pretty ... 
ncolours=100
image(unique(dat$long),rev(unique(dat$lat)),
      matrix(1:length(dat.mat),nrow=nrow(dat.mat)),
      col=map.colours(dat.mat),xlab="Longitude",ylab="Latitude")

# And it looks even prettier in 3D ... 
zfacet <- dat.mat[-nrow(dat.mat), -ncol(dat.mat)]
with(dat, persp(unique(long), rev(unique(lat)), dat.mat, 
                expand=.2, col=map.colours(zfacet), border=NA,
                shade=.3, phi=60, theta=0, axes=FALSE))

# How about an *interactive* 3D map ...
# Try holding the left mouse button and dragging
# Now try the mouse wheel (or right button and dragging)
# Finally, try clicking the middle button (or wheel) and dragging
library(rgl)
with(dat, surface3d(unique(long), rev(unique(lat)), dat.mat/1000,
                    col=map.colours(dat.mat)))

# Note that the elevation values are of course very much exaggerated
# In 'real life', it would look something like this (i.e., completely flat)
with(dat, surface3d(unique(long), rev(unique(lat)), dat.mat/1000000,
                    col=map.colours(dat.mat)))
