#lecture helpers: graphics etc

library(lidR)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(sf)
library(tmap)
# library(ggforce)

#preprocess ####

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

chm <- terra::focal(chm, 3, "median")

# terra::writeRaster(chm, "data/chm.tif", overwrite=T)

sl <- terrain(chm, "slope", unit = "radians")
asp <- terrain(chm, "aspect", unit = "radians")
chm_hillshade <- shade(sl, asp, 
                       angle = 45, 
                       direction = 300,
                       normalize= TRUE)


# CHM, ABA, ITD ####

## CHM as background ####
m <- 
  tm_shape(chm_hillshade) +
  tm_raster(style="cont", palette = grey(1:100/100), legend.show = F)+
  tm_shape(chm)+
  tm_raster(style = "cont", palette = rev(RColorBrewer::brewer.pal(name="Spectral",n=11)), title = "", alpha = 0.8)+
  tm_layout(frame = FALSE, outer.margins=0, inner.margins = 0)
m
tmap_save(m, "images/chm.png", width = 16, height = 9)

## ABA ####

### overlay a grid ####
grid20 <- st_make_grid(st_bbox(chm), cellsize = 20)

m1 <- m +
  tm_shape(grid20)+
  tm_borders(col="black")
tmap_save(m1, "images/chm_wGrid.png", width = 16, height = 9)

### fake ABA ####
grid20 <- grid20 %>% st_as_sf()
aba <- terra::extract(chm, grid20, mean, bind=T, na.rm=T) %>% st_as_sf()#get average CHM per gridcell
aba <- aba %>% mutate(V = focal_median ^1.65)

m1_aba <- tm_shape(aba) +
  tm_polygons(col="V", palette = rev(RColorBrewer::brewer.pal(name="Spectral",n=11)))+
  tm_layout(frame = FALSE, outer.margins=0, inner.margins = 0, legend.bg.color = "white")
tmap_save(m1_aba, "images/m1_aba.png", width = 16, height = 9)

# ### ABA with annotations ####
# m1_aba + tm_text()


## ITD ####

ttops <- locate_trees(chm, lmf(4, 2))
las_norm   <- segment_trees(las_norm, dalponte2016(chm, ttops))
crowns <- crown_metrics(las_norm, .stdtreemetrics, geom = "concave")

### crowns on top of CHM ####
m2 <- m +
  tm_shape(crowns)+
  tm_borders(col = "black")
tmap_save(m2, "images/chm_wCrowns.png", width = 16, height = 9)


### crowns with BA ####

#very fake BA values
crowns <- crowns %>% mutate(BA = (pi * (Z/2)^2)/1000)

m3 <- tm_shape(crowns)+
  tm_polygons(col = "BA", palette = rev(RColorBrewer::brewer.pal(name="Spectral",n=11)))+
  tm_layout(frame = FALSE, outer.margins=0, inner.margins = 0, legend.bg.color = "white")
tmap_save(m3, "images/CrownsBA.png", width = 16, height = 9)



# focus area showing treetops and crowns ####

#crop 
xleft = 484000
ybottom = 5455300 
xright = xleft + 100
ytop = ybottom + 100

d <- terra::crop(chm, ext(c(xleft, xright, ybottom, ytop)))
plot(d)

mm <- tm_shape(chm_hillshade, bbox = sf::st_bbox(ext(c(xleft, xright, ybottom, ytop)))) +
  tm_raster(style="cont", palette = grey(1:100/100), legend.show = F)+
  tm_shape(chm)+
  tm_raster(style = "cont", palette = rev(RColorBrewer::brewer.pal(name="Spectral",n=11)), title = "", alpha = 0.8)+
  tm_layout(frame = FALSE, outer.margins=0, inner.margins = 0)

mm1 <- mm + 
  tm_shape(ttops)+
  tm_symbols(shape = 16, col = "black")

mm2 <- mm +
  tm_shape(crowns)+
  tm_borders(col = "black")

tmap_save(
  tmap_arrange(mm1, mm2, ncol = 1),
  "images/itd-its.png",  
  width=5, height=10
)



# a crossection ####
devtools::source_gist("https://gist.github.com/ptompalski/1aea3e7764b73703a24437adc3c1ed0e") #gist containing the plot_crossection function
p <- plot_crossection(las_norm, p1 = c(483700, 5455300), p2 = c(483900, 5455300)) + theme_void()
ggsave(p, filename = "images/crossection.png", width=8, height=3)

# crossection with trees
p <- plot_crossection(las_norm, p1 = c(483700, 5455300), p2 = c(483900, 5455300), colour_by = factor(treeID)) + theme_void()+
  theme(legend.position = "none")
ggsave(p, filename = "images/crossection_colored.png", width=8, height=3)

# crossection with trees and annotation
p <- plot_crossection(las_norm, p1 = c(483700, 5455300), p2 = c(483900, 5455300), colour_by = factor(treeID)) +
  theme_void()+theme(legend.position = "none")+
  annotate("text", x=c(483730,483800,483853,483882), y=c(20, 15, 13, 13), label="?", size=11)
ggsave(p, filename = "images/crossection_colored2.png", width=8, height=3)
  
#   ggforce::geom_mark_ellipse(data=data.frame(X=c(483720, 483750), Z=c(0, 20)), aes(X,Z),inherit.aes = F)
# 
# p + ggforce::geom_mark_ellipse(data=data.frame(X=c(483700, 483750), Z=c(0, 20)), aes(X,Z),inherit.aes = F)
#   




# lmf comparison graphic ####
ttops_1 <- locate_trees(d, lmf(ws = 5)) 

f <- function(x) x * 0.1 + 3
ttops_2 <- locate_trees(d, lmf(f)) 

m_ttops <- tm_shape(d)+
  tm_raster(style = "cont", palette = rev(RColorBrewer::brewer.pal(name="Spectral",n=11)), title = "", legend.show = F)+
  tm_layout(frame = FALSE, outer.margins=0, inner.margins = 0)

tmap_save(
  tmap_arrange(
    m_ttops + tm_shape(ttops_1)+  tm_dots(size = 0.5),
    m_ttops + tm_shape(ttops_2)+  tm_dots(size = 0.5),
    
    ncol = 1
    ),
  "images/ttops_comparison.png",  
  width=5, height=10
)  


# different CHM vs lmf graphic ####

#code from https://r-lidar.github.io/lidRbook/itd.html#sec-lmfchm
las_norm2 <- lidR::clip_rectangle(las_norm, xleft, ybottom , xright, ytop )


# Point-to-raster 2 resolutions
chm_p2r_02 <- rasterize_canopy(las_norm2, 0.2, p2r(subcircle = 0.2), pkg = "terra")
# chm_p2r_05 <- rasterize_canopy(las_norm2, 0.5, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1 <- rasterize_canopy(las_norm2, 1, p2r(subcircle = 0.2), pkg = "terra")

# Pitfree with and without subcircle tweak
# chm_pitfree_05_1 <- rasterize_canopy(las_norm2, 0.5, pitfree(), pkg = "terra")
# chm_pitfree_05_2 <- rasterize_canopy(las_norm2, 0.5, pitfree(subcircle = 0.2), pkg = "terra")

# Post-processing median filter
kernel <- matrix(1,3,3)
# chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)


ttops_chm_p2r_02 <- locate_trees(chm_p2r_02, lmf(5))
# ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_1 <- locate_trees(chm_p2r_1, lmf(5))
# ttops_chm_pitfree_05_1 <- locate_trees(chm_pitfree_05_1, lmf(5))
# ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(5))
# ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))
ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5))


plotCHMwithTreetops <- function(chm, tt) {
  tm_shape(chm)+
    tm_raster(style = "cont", palette = rev(RColorBrewer::brewer.pal(name="Spectral",n=11)), title = "", legend.show = F)+
    tm_layout(frame = FALSE, outer.margins=0, inner.margins = 0)+
    tm_shape(tt)+  tm_dots(size = 0.25)
}
tmap_save(
tmap_arrange(
plotCHMwithTreetops(chm_p2r_02, ttops_chm_p2r_02),
# plotCHMwithTreetops(chm_p2r_05, ttops_chm_p2r_05),
plotCHMwithTreetops(chm_p2r_1, ttops_chm_p2r_1),
# plotCHMwithTreetops(chm_p2r_05_smoothed, ttops_chm_pitfree_05_1),
plotCHMwithTreetops(chm_p2r_1_smoothed, ttops_chm_pitfree_05_2)
# plotCHMwithTreetops(chm_pitfree_05_1, ttops_chm_p2r_05_smoothed),
# plotCHMwithTreetops(chm_pitfree_05_2, ttops_chm_p2r_1_smoothed)
),
"images/itd_chm_comparison.png",  
width=12, height=4
)  


# animating UAS point cloud ####

d <- readLAS(r"(C:\Users\ptompals\Downloads\FORinstance_dataset\NIBIO\plot_5_annotated.las)")

d <- lidR::filter_poi(d, treeID > 0)

plot(d,color="treeID", bg="white",pal = random.colors)

library(rgl)

view3d(theta = 0, phi = -90, fov = 60, zoom = 1)
# play3d(spin3d())
movie3d(spin3d(rpm = 6), fps=20, duration = 10, top=T, movie = "itd_spin", dir="C:\\TEMP\\temp_frames")

# Modify the GIF to loop indefinitely
library(magick)

# Path to the generated GIF
gif_path <- file.path("C:\\TEMP\\temp_frames", "itd_spin.gif")

# Read and modify the GIF
gif <- image_read(gif_path)               # Read the GIF
gif <- image_animate(gif, fps = 20, loop = 0)  # Set to loop indefinitely
image_write(gif, gif_path)                # Save the modified GIF
