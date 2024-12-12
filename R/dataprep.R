#lecture helpers: graphics etc

library(lidR)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(sf)




las <- readLAS("data/bc_092g024_4_2_2_xyes_8_utm10_20170714.laz")#, filter = "-inside 481280 3812940 481330 3812990")

#crop 
xleft = 483625
ybottom = 5455273
xright = xleft + 800
ytop = ybottom + 450

las <- lidR::clip_rectangle(las, xleft, ybottom , xright, ytop )

#normalize
las_norm <- lidR::normalize_height(las, algorithm = tin())


#dtm
dtm <- lidR::grid_terrain(las, res = 1, algorithm = tin())
terra::writeRaster(dtm, "data/dtm.tif", overwrite=T)

#chm
chm <- rasterize_canopy(las_norm, res = 0.5, pitfree(c(0,2,5,10,15), c(0, 1.5)) )
terra::writeRaster(chm, "data/chm.tif", overwrite=T)

# plot(chm)
# 
# 
# ggplot()+
#   geom_spatraster(data=chm)+
#   theme_minimal()


library(tmap)

m <- tm_shape(chm)+
  tm_raster(style = "cont", palette = rev(RColorBrewer::brewer.pal(name="Spectral",n=11)), title = "")+
  tm_layout(frame = FALSE)
m
tmap_save(m, "images/chm.png", width = 16, height = 9)


#overlay a grid
grid20 <- st_make_grid(st_bbox(chm), cellsize = 20)


m1 <- m +
  tm_shape(grid20)+
  tm_borders()
tmap_save(m1, "images/chm_wGrid.png", width = 16, height = 9)


#run itd
ttops <- locate_trees(chm, lmf(4, 2))
las_norm   <- segment_trees(las_norm, dalponte2016(chm, ttops))
crowns <- crown_metrics(las_norm, .stdtreemetrics, geom = "concave")

m2 <- m +
  tm_shape(crowns)+
  tm_borders()
tmap_save(m2, "images/chm_wCrowns.png", width = 16, height = 9)

