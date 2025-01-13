library(ggplot2)
library(reshape2)
library(dplyr)
library(ggtext)
library(grid)
library(RStoolbox)
# library(khroma)
library(scico)
library(extrafont)
# extrafont::font_import()
extrafont::loadfonts(device="win")

# cols_gradient <- c(low = "#002070", mid = "#DD6677", high = "#F1F1A1")
# cols_gradient <- c(low = "#043206", mid = "#E1CA34", high = "#FBF3BA")
cols_gradient <- rev(RColorBrewer::brewer.pal(name="Spectral",n=11))
# cols_gradient <- viridis::mako(n = 10)
# cols_gradient <- scico::scico(10, palette = "navia")

theme_map = theme(
  panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  # panel.border = element_blank(),

  legend.position = "inside",
  legend.position.inside = c(.98, .02),
  legend.justification = c(1, 0),
  # panel.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = "white"),
  legend.title = element_text(hjust = 0.5),
  legend.direction = "horizontal",
  legend.key.width = unit(.47, "in"),
  legend.key.height = unit(.1, "in"),
  legend.background = element_rect(color = "black", fill = "white", linewidth = .5),
  legend.margin = ggplot2::margin(4, 10, 4, 10),
  
  plot.margin = unit(c(0, 0, 0, 0), "null"),
  panel.spacing = unit(c(0, 0, 0, 0), "null"),
  panel.margin = unit(c(0, 0, 0, 0), "null"),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank(),
  axis.ticks.length = unit(0, "null"),
  axis.ticks.margin = unit(0, "null")
  
)


chm <- terra::rast("data/chm.tif")

p1 <- ggR(chm, geom_raster = T) +
  scale_fill_gradientn(colors = cols_gradient,
                       na.value = "grey70",name = "Canopy height (m)")+
  guides(fill = guide_colourbar(title.position="top"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_bw(base_size = 18,base_family="Lato") +
  theme_map 
p1
ggsave(p1, filename = "images/p1.png", width = 16, height = 9)


grid20 <- st_make_grid(st_bbox(chm), cellsize = 20)
grid20 <- st_crop(grid20, st_bbox(chm))


p2 <- p1 + 
  geom_sf(data=grid20, fill=NA, color="black", linewidth=0.5)+
  guides(fill = guide_none())
ggsave(p2, filename = "images/p2.png", width = 16, height = 9)


#p2 annotated
textsize <- 8.5

p3 <- 
  ggR(chm, geom_raster = T, alpha = 0.5) +
  scale_fill_gradientn(colors = cols_gradient,
                       na.value = "grey70",name = "Canopy height (m)")+
  guides(fill = guide_colourbar(title.position="top"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_bw(base_size = 18,base_family="Lato") +
  theme_map+ 
  geom_sf(data=grid20, fill=NA, color="grey50", linewidth=0.5, alpha=0.5)+
  guides(fill = guide_none())+
  
  annotate("richtext", x = 484025-200, y= 5455583, label = "**Point cloud metrics**<br><span style = 'font-size:14pt'>e.g.:<i> zq95, zsd, pzabove2</i></span>", hjust=0.5,  size=textsize, family="Lato", label.padding = unit(c(1, 1, 1, 1), "lines"))+
  annotate("richtext", x = 484025 , y= 5455583, label = "**Plot-level attributes**<br><span style = 'font-size:14pt'>e.g. DBH, HL, V</span>", hjust=0.5,  size=textsize, family="Lato", label.padding = unit(c(1, 1, 1, 1), "lines"))+
  annotate("richtext", x = 484025-100, y= 5455583-120, label = "**Modeling**<br><span style = 'font-size:14pt'><i>V = 3.5 * zq95<sup>1.4</sup> * pzabove2<sup>1.54</sup></span> ", hjust=0.5,  size=textsize, family="Lato", label.padding = unit(c(1, 1, 1, 1), "lines"))+
  annotate("richtext", x = 484025+120, y= 5455583-120, label = "**Apply models<br>to entire area**", hjust=0.5,  size=textsize, family="Lato", label.padding = unit(c(1, 1, 1, 1), "lines"))+
  annotate("segment", x=484025-150, y=5455583-30, xend=484025-110, yend=5455583-90, arrow = arrow(angle = 15,type = "closed", length = unit(1, "cm")), alpha=0.85)+
  annotate("segment", x=484025-50, y=5455583-30, xend=484025-90, yend=5455583-90, arrow = arrow(angle = 15,type = "closed", length = unit(1, "cm")), alpha=0.85)+
  annotate("segment", x=484025-10, y=5455583-120, xend=484025+50, yend=5455583-120, arrow = arrow(angle = 15,type = "closed", length = unit(1, "cm")), alpha=0.85)
ggsave(p3, filename = "images/p3.png", width = 16, height = 9)


grid20 <- grid20 %>% st_as_sf()
aba <- terra::extract(chm, grid20, mean, bind=T, na.rm=T) %>% st_as_sf()#get average CHM per gridcell
aba <- aba %>% mutate(V = Z ^1.65)

p4 <- ggplot() +
  geom_sf(data=aba, aes(fill=V))+
  scale_fill_gradientn(colors = cols_gradient,
                       na.value = "grey70",name = "V (m³/ha)")+
  guides(fill = guide_colourbar(title.position="top"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_bw(base_size = 18,base_family="Lato") +
  theme_map
ggsave(p4, filename = "images/p4.png", width = 16, height = 9)



p5 <- p4 + 
  annotate("segment", xend = 484215   ,yend= 5455563, x = 484105, y = 5455573, arrow = arrow(angle = 15,type = "closed", length = unit(1, "cm")))+
  annotate("richtext", x = 484035 ,y= 5455583, label="E.g. <br>V = 508 m³/ha <br>HTOP = 36 m<br>QMD = 41 cm<br>...<br>...", size=7,  hjust = 0, family="Lato")
ggsave(p5, filename = "images/p5.png", width = 16, height = 9)



#ITD
ttops <- locate_trees(chm, lmf(4, 2))
las_norm   <- segment_trees(las_norm, dalponte2016(chm, ttops))
crowns <- crown_metrics(las_norm, .stdtreemetrics, geom = "concave")

p10 <- p1 + 
  geom_sf(data=crowns, fill=NA, color="black", linewidth=0.5)+
  guides(fill = guide_none())
ggsave(p10, filename = "images/p10.png", width = 16, height = 9)


#very fake BA values
crowns <- crowns %>% mutate(BA = (pi * (Z/2)^2)/1000)

p11 <- ggplot() +
  geom_sf(data=crowns, aes(fill=BA))+
  scale_fill_gradientn(colors = cols_gradient,
                       na.value = "grey70",name = "BA (m²/ha)")+
  guides(fill = guide_colourbar(title.position="top"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_bw(base_size = 18,base_family="Lato") +
  theme_map
ggsave(p11, filename = "images/p11.png", width = 16, height = 9)
