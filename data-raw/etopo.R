#
# Read ETOPO1 colors and bathy example
#

## Colors ----

# Palette from http://soliton.vm.bytemark.co.uk/pub/cpt-city/ngdc/tn/ETOPO1.png.index.html

# read postgis palette
etopo_pg <- read.table("data-raw/ETOPO1.pg")

# reverse the order, which is more logical, and parse colors
etopo <- data.frame(
  altitude=rev(etopo_pg[,1]),
  color=rev(rgb(etopo_pg[,2:4], maxColorValue=255))
)
# set 0 to green
etopo$altitude[24] <- 0

# save the colors
use_data(etopo, internal=FALSE, overwrite=TRUE)

## Bathy ----

library("ncdf4")
nc <- nc_open("data-raw/etopo1.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
alt <- ncvar_get(nc, "Band1")

thai <- data.frame(lon=rep(lon, times=length(lat)), lat=rep(lat, each=length(lon)), alt=as.numeric(alt))

library("dplyr")
thai$lon <- plyr::round_any(thai$lon, 0.2)
thai$lat <- plyr::round_any(thai$lat, 0.2)
# thai$lon <- round(thai$lon, digits=1)
# thai$lat <- round(thai$lat, digits=1)
thai <- thai %>% group_by(lon, lat) %>% summarise(alt=mean(alt))

# library("ggplot2")
# ggplot(thai) + geom_raster(aes(lon, lat, fill=alt)) + coord_quickmap()

thai <- list(
  x=unique(thai$lon),
  y=unique(thai$lat),
  z=matrix(thai$alt, nrow=length(unique(thai$lon)), byrow=TRUE)
)
image(thai)

library("broom")
thaixyz <- tidy(thai)

use_data(thai, thaixyz, internal=FALSE, overwrite=TRUE)
