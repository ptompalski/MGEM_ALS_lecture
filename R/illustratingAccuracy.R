library(tidyverse)
library(sf)


# accuracy assessment - visualization of the differnt metrics

# # say we have a square are with 100 trees
# plot_size <- 30
# 
# thePlot <- st_polygon(list(cbind(c(0,plot_size,plot_size,0,0), c(0,0,plot_size,plot_size,0))))
# refTrees <- sf::st_sample(thePlot, 100)
# 
# crown_sizes <- sample(seq(0.5,2,0.1), 100, replace = T)
# refCrowns <- st_buffer(refTrees, dist = crown_sizes)
# 
# plot(thePlot)
# plot(refTrees, add=T)
# plot(refCrowns, add=T)


set.seed(123)
# Define a bounding box (e.g., 1000x1000 meter area)
bounding_box <- st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)))

generate_points <- function(num_points, min_distance, bounding_box) {
  # Create an empty sf object with a geometry column
  points <- st_sf(geometry = st_sfc()) 
  
  while (nrow(points) < num_points) {
    # Generate a new random point
    new_point <- st_sample(bounding_box, size = 1, type = "random", exact = TRUE) %>%
      st_sf(geometry = .) %>% 
      mutate(id = nrow(points) + 1)  # Assign an ID for tracking
    
    if (nrow(points) > 0) {
      # Check distances between the new point and existing points
      distances <- st_distance(new_point, points)
      if (all(distances >= min_distance)) {
        points <- bind_rows(points, new_point)
      }
    } else {
      # Add the first point unconditionally
      points <- bind_rows(points, new_point)
    }
  }
  
  return(points)
}
# Parameters
num_points <- 10
min_distance <- 3 # Minimum distance in meters

# reference trees
refTrees <- generate_points(num_points, min_distance, bounding_box)


# crown_sizes <- sample(seq(1,2,0.2), 10, replace = T)
refCrowns <- st_buffer(refTrees, dist = 1.5)

plot(st_geometry(refCrowns))
plot(st_geometry(refTrees), pch = 20, add=T)


# detected trees
num_detected <- 12
min_distance_detected <- 2
detectedTrees <- generate_points(num_detected, min_distance_detected, bounding_box)
plot(st_geometry(detectedTrees), pch = 2, col="red", add=T)


#true positives
# st_intersects(refCrowns, detectedTrees)
# st_intersects(detectedTrees, refCrowns)

int <- st_intersection(refCrowns, detectedTrees) %>% as_tibble() %>% select(-geometry)

d1 <- left_join(refTrees, int, by="id") %>% 
  mutate(type = case_when(
    !is.na(id.1) ~ "TP",
    is.na(id.1) ~ "FN"
      ))


d2 <- left_join(detectedTrees, int, by=join_by("id"=="id.1")) %>%
  mutate(type = case_when(
  !is.na(id.y) ~ "a1",
  is.na(id.y) ~ "FP"
))

d <- bind_rows(
select(d1, type),
select(d2, type) %>% filter(type=="FP")
)
d$type <- factor(d$type)
d$type <- fct_relevel(d$type, c("TP", "FN", "FP"))

#make a plot by incrementally adding layers
theTheme <-   theme_void()+
  theme(legend.position = "bottom", legend.title = element_blank())

p1 <- ggplot(data=NULL) +
  geom_sf(data=refCrowns, color="white",alpha=0)+
  #reference trees
  geom_sf(data=refTrees, pch=1, size=6) + 
  theTheme


p2 <- p1 + geom_sf(data=detectedTrees, pch=3, size=6)

p3<- ggplot(data=NULL) +
  geom_sf(data=refCrowns, linetype="dashed")+
  #reference trees
  geom_sf(data=refTrees, pch=1, size=6) + 
  geom_sf(data=detectedTrees, pch=3, size=6)+
  theTheme


p4 <- p3 +
  #error types
  geom_sf(data=d %>% filter(type=="TP"), aes(color=type), size=4) +
  scale_color_manual(values = c("#7fc97f","#beaed4", "#fdc086"))

p5 <- p3 +
  #error types
  geom_sf(data=d %>% filter(type!="FP"), aes(color=type), size=4) +
  scale_color_manual(values = c("#7fc97f","#beaed4", "#fdc086"))



p6 <- ggplot(data=NULL) +
  geom_sf(data=refCrowns, linetype="dashed")+
  #reference trees
  geom_sf(data=refTrees, pch=1, size=6)+
  
  #detected trees
  geom_sf(data=detectedTrees, pch=3, size=6)+
  
  #error types
  geom_sf(data=d, aes(color=type), size=4) +
  
  #settings
  scale_color_manual(values = c("#7fc97f","#beaed4", "#fdc086"))+
  theme_void()+
  theme(legend.position = "bottom", legend.title = element_blank())

p6  

dd <- d %>% as_tibble() %>% group_by(type) %>% count()%>% pivot_wider(names_from = type, values_from = n)


#recall
recall <- round(dd$TP / (dd$TP+dd$FN),2)

#precision
precision <- round(dd$TP / (dd$TP+dd$FP),2)

# Fscore 
Fscore <- round(2 * (recall*precision) / (recall+precision),2)


p7 <- p6 +
  labs(subtitle = glue("Precision={precision}; Recall={recall}; F-score={Fscore}"))



library(cowplot)
pp <- cowplot::align_plots(p1, p2, p3, p4, p5, p6, p7, align = "hv")

# ggdraw(pp[[7]])

for(i in 1:7) {
  ggsave(pp[[i]], filename=glue("images/AccAssessm_{i}.png"), width=4, height=4)
}


### example - high recall, low precision? ####
num_detected <- 30
min_distance_detected <- 1.5
detectedTrees2 <- generate_points(num_detected, min_distance_detected, bounding_box)


int <- st_intersection(refCrowns, detectedTrees2) %>% as_tibble() %>% select(-geometry)

#if duplicates, then pick random

int <- int %>% mutate(rowid = row_number())
int2 <- int %>% group_by(id) %>% slice_sample(n = 1)
int3 <- int %>% filter(!rowid %in% int2$rowid) %>% mutate(id = id*-1)

int <- bind_rows(int2,int3)


d1 <- left_join(refTrees, int, by="id") %>% 
  mutate(type = case_when(
    !is.na(id.1) ~ "TP",
    is.na(id.1) ~ "FN"
  ))


d2 <- left_join(detectedTrees2, int, by=join_by("id"=="id.1")) %>%
  mutate(type = case_when(
    !is.na(id.y) ~ "a1",
    is.na(id.y) ~ "FP"
  ))

d <- bind_rows(
  select(d1, type),
  select(d2, type) %>% filter(type=="FP")
)
d$type <- factor(d$type)
d$type <- fct_relevel(d$type, c("TP", "FN", "FP"))

dd <- d %>% as_tibble() %>% group_by(type) %>% count()%>% pivot_wider(names_from = type, values_from = n)


#recall
recall <- round(dd$TP / (dd$TP+dd$FN),2)

#precision
precision <- round(dd$TP / (dd$TP+dd$FP),2)

# Fscore 
Fscore <- round(2 * (recall*precision) / (recall+precision),2)


ggplot(data=NULL) +
  geom_sf(data=refCrowns, linetype="dashed")+
  #reference trees
  geom_sf(data=refTrees, pch=1, size=6)+
  
  #detected trees
  geom_sf(data=detectedTrees2, pch=3, size=6)+
  
  #error types
  geom_sf(data=d, aes(color=type), size=4) +
  
  #settings
  scale_color_manual(values = c("#7fc97f","#beaed4", "#fdc086"))+
  theme_void()+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(subtitle = glue("Precision={precision}; Recall={recall}; F-score={Fscore}"))
