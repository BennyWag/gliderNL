library(raster)
library(sf)
library(landscapemetrics)
library(landscapetools)
library(NLMR)
library(exactextractr)
library(scclust)
library(patchwork)
library(tidyverse)


# load prediction rasters from C3 -----------------------------------------

highlands<-raster('data/C3 rasters/T35P1.tif')
plot(highlands, col = c('#440154FF', '#29AF7FFF', '#FDE725FF'))

lowlands<-raster('data/C3 rasters/T15P5.tif')
plot(lowlands, col = c('#440154FF', '#29AF7FFF', '#FDE725FF'))


# get areas ---------------------------------------------------------------

raster_stats<-function(class_raster){
  
  rast_df<-class_raster%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)
  
  rast_df$ID<-1
  
  res<-res(class_raster)[1]
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  
  metrics<-calculate_lsm(class_raster, what = c("lsm_c_np", "lsm_c_area_mn", 'lsm_c_clumpy'),
                         directions = 8)
  
  metrics_wide<-pivot_wider(metrics, names_from = c('metric'), values_from = c('value'))%>%select(3, 5:7)
  
  
  rast_all<-inner_join(rast_area, metrics_wide, by = 'class')
  
  
  return(rast_all)
  
}

rasters<-list(highlands, lowlands)
names(rasters)<-c('highlands', 'lowlands')

rasterstats<-map(rasters, raster_stats)

rasterstats_bind<-bind_rows(rasterstats, .id = 'plot')


# create representative neutral landscapes --------------------------------

#check aggregation

highlands_agg<-calculate_lsm(highlands, what = c('lsm_l_ai', 'lsm_l_np'),
              directions = 8)

lowlands_agg<-calculate_lsm(lowlands, what = c('lsm_l_ai'),
                             directions = 8)

dreisatz<-function(new, total, respective){
  
  factor<-respective/total
  new*factor
  
}

#create random highlands - 200 x 200 cells at 1m2 per cell = 4ha

highland_random<-nlm_randomcluster(200,200, resolution = 1,
                                   p = dreisatz(88.9,100,40)/100, ai = c(0.65,0.27,0.09),
                                   neighbourhood = 8)

plot(highland_random, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

#check if aggregation matches

highland_random_class<-reclassify(highland_random, c(0,0.25,0,0.25,0.75,1,0.75,Inf,2))

highlands_random_agg<-calculate_lsm(highland_random_class, what = c('lsm_l_ai', 'lsm_l_np'),
                             directions = 8)

#create random lowlands

lowland_random<-nlm_randomcluster(200,200, resolution = 1,
                                   p = dreisatz(88.85,100,40)/100, ai = c(0.3,0.58,0.12),
                                   neighbourhood = 8)

plot(lowland_random, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

lowland_random_class<-reclassify(lowland_random, c(0,0.25,0,0.25,0.75,1,0.75,Inf,2))

# 0 = habitat, 1 = non-habitat, 2 = not feeding habitat

# check area and no. of pot. home ranges ----------------------------------

rasters_sim<-list(highland_random_class, lowland_random_class)
names(rasters_sim)<-c('highlands_sim', 'lowlands_sim')

rasterstats_sim<-map(rasters_sim, raster_stats)

rasterstats_sim_bind<-bind_rows(rasterstats_sim, .id = 'plot')

rasterstats_sim_bind_pres<-rasterstats_sim_bind%>%
  filter(class == 0)%>%
  mutate(HRU = areaha/2.6)


# larger area lower res ---------------------------------------------------

#each cell is assumed to be 10x10 m - creates a 100ha landscape

highland_random_large<-nlm_randomcluster(100,100, resolution = 1,
                                   p = (dreisatz(88.9,100,40)/100)/2, ai = c(0.65,0.27,0.09),
                                   neighbourhood = 8)

plot(highland_random, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(highland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

rast_df<-highland_random_large%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)

rast_df$ID<-1

res<-10

rast_area<-rast_df%>%group_by(layer)%>%
  summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
  mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)

rast_area_large_pres<-rast_area%>%
  filter(class == 0)%>%
  mutate(HRU = areaha/2.6)

#low

lowland_random_large<-nlm_randomcluster(100,100, resolution = 1,
                                  p = (dreisatz(88.85,100,40)/100)/2, ai = c(0.3,0.58,0.12),
                                  neighbourhood = 8)

plot(lowland_random, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(lowland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

rast_df_low<-lowland_random_large%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)

rast_df_low$ID<-1

rast_area_low<-rast_df_low%>%group_by(layer)%>%
  summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
  mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)

rast_area__low_large_pres<-rast_area_low%>%
  filter(class == 0)%>%
  mutate(HRU = areaha/2.6)


# make polygons -----------------------------------------------------------

#points approach

highland_sim_res<-highland_random_large==0
plot(highland_sim_res)

highland_sim_res_pts<-rasterToPoints(highland_sim_res, spatial = T)%>%
  st_as_sf()%>%
  filter(layer == 1)

plot(highland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(highland_sim_res_pts$geometry, add = T, col = 'red')

#cluster

highland_sim_res_df<-rasterToPoints(highland_sim_res, spatial = F)%>%
  as.data.frame()%>%
  filter(layer == 1)

cluster<-kmeans(highland_sim_res_df, centers = 24, nstart = 10)

highland_sim_res_df$clust<-cluster$cluster

ggplot(highland_sim_res_df, aes(x = x, y = y, color = as.factor(clust)))+
  geom_point()

#make sf

highland_sim_res_df_sf<-st_as_sf(highland_sim_res_df, coords = c('x', 'y'))%>%group_split(clust)

hull_creation<-function(points){
  
  hull<-st_convex_hull(st_union(points))%>%st_as_sf()
  
  return(hull)
}

hulls<-map(highland_sim_res_df_sf, hull_creation)

names(hulls)<-1:24

nrows<-length(hulls)

hull_all<-hulls%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:nrows)

plot(highland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(hull_all$x, add = T, col = rainbow(24))

#lowlands

lowlands_sim_res<-lowland_random_large==0
plot(lowlands_sim_res)

lowlands_sim_res_df<-rasterToPoints(lowlands_sim_res, spatial = F)%>%
  as.data.frame()%>%
  filter(layer == 1)

cluster_low<-kmeans(lowlands_sim_res_df, centers = 10, nstart = 10)

lowlands_sim_res_df$clust<-cluster_low$cluster

ggplot(lowlands_sim_res_df, aes(x = x, y = y, color = as.factor(clust)))+
  geom_point()

lowlands_sim_res_df_sf<-st_as_sf(lowlands_sim_res_df, coords = c('x', 'y'))%>%group_split(clust)

hulls_low<-map(lowlands_sim_res_df_sf, hull_creation)

names(hulls_low)<-1:10

nrows<-length(hulls_low)

hull_all_low<-hulls_low%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:nrows)

plot(lowland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(hull_all_low$x, add = T, col = rainbow(10))


# get hull areas ----------------------------------------------------------

hull_all$area<-st_area(hull_all)/100
hull_all$cent<-st_centroid(hull_all)
hull_all$coord_x<-st_coordinates(hull_all$cent)[,1]
hull_all$coord_y<-st_coordinates(hull_all$cent)[,2]

hull_all_low$area<-st_area(hull_all_low)/100
hull_all_low$cent<-st_centroid(hull_all_low)
hull_all_low$coord_x<-st_coordinates(hull_all_low$cent)[,1]
hull_all_low$coord_y<-st_coordinates(hull_all_low$cent)[,2]



#plot

highlands_sim_df<-highland_random_large%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

ggplot()+
  geom_raster(data = highlands_sim_df, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF'),
                    labels = c('>1% N/DM', 'non-habitat', '<1% N/DM'))+
  geom_sf(data=hull_all, size = 1.2, color = 'red', fill = NA)+
  geom_text(data = hull_all, aes(x = coord_x, y = coord_y, label = area), size = 5, col = 'blue')+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))

lowlands_sim_df<-lowland_random_large%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

ggplot()+
  geom_raster(data = lowlands_sim_df, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF'),
                    labels = c('>1% N/DM', 'non-habitat', '<1% N/DM'))+
  geom_sf(data=hull_all_low, size = 1.2, color = 'red', fill = NA)+
  geom_text(data = hull_all_low, aes(x = coord_x, y = coord_y, label = area), size = 5, col = 'blue')+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))



# function ----------------------------------------------------------------

optimal_hr<-function(raster, res, hr){

rast_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)

rast_df$ID<-1

res<-res

rast_area<-rast_df%>%group_by(layer)%>%
  summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
  mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)

rast_area_large_pres<-rast_area%>%
  filter(class == 0)%>%
  mutate(HRU = areaha/hr)

#cluster

sim_res<-raster==0

sim_res_df<-rasterToPoints(sim_res, spatial = F)%>%
  as.data.frame()%>%
  filter(layer == 1)

cluster<-kmeans(sim_res_df, centers = round(rast_area_large_pres$HRU), nstart = 10)

sim_res_df$clust<-cluster$cluster

sim_res_df_area<-sim_res_df%>%group_by(clust)%>%tally(layer)%>%mutate(feed_ha = n/res^2)

#make sf

sim_res_df_sf<-st_as_sf(sim_res_df, coords = c('x', 'y'))%>%group_split(clust)

hull_creation<-function(points){
  
  hull<-st_convex_hull(st_union(points))%>%st_as_sf()
  
  return(hull)
}

hulls<-map(sim_res_df_sf, hull_creation)

names(hulls)<-1:round(rast_area_large_pres$HRU)

nrows<-length(hulls)

hull_all<-hulls%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:nrows)

hull_all$area<-st_area(hull_all)/100
hull_all$cent<-st_centroid(hull_all)
hull_all$coord_x<-st_coordinates(hull_all$cent)[,1]
hull_all$coord_y<-st_coordinates(hull_all$cent)[,2]

hull_all$feed_ha<-sim_res_df_area$feed_ha

raster_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

plot<-ggplot()+
  geom_raster(data = raster_df, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                    labels = c('>1% N/DM', 'non-habitat', '<1% N/DM', 'climate'))+
  geom_sf(data=hull_all, size = 1.2, color = 'red', fill = NA)+
  geom_text(data = hull_all, aes(x = coord_x, y = coord_y, label = area), size = 5, col = 'blue')+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))

list<-list(rast_area_large_pres, hull_all, plot, sim_res_df_area)

names(list)<-c('data', 'hulls', 'plot', 'clust_area')

return(list)

plot

}

high<-optimal_hr(highland_random_large, 10, 2.6)
low<-optimal_hr(lowland_random_large, 10, 2.6)


# logging mask ------------------------------------------------------------

#aggregated retention

logging_agg<-nlm_randomcluster(100,100, resolution = 1,
                                         p = 0.58, ai = c(0.6,0.4),
                                         neighbourhood = 4)

plot(logging_agg)

logging_agg%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()


highlands_logged<-mask(highland_random_large, logging_agg, maskvalue=0, inverse = T)

highlands_logged[is.na(highlands_logged[])] <- 0.5 

plot(highlands_logged, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))


optimal_hr(highland_random_large, 10, 2.6)
optimal_hr(highlands_logged, 10, 2.6)

#check actual nitrogen area

high_agg<-optimal_hr(highlands_logged, 10, 2.6)

high_agg$plot

hull<-high_agg$hulls%>%filter(ID == '3')

plot(highlands_logged, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(hull$x, add = T)

highlands_log_crop<-crop(highlands_logged, hull)
highlands_log_crop<-mask(highlands_log_crop, hull)

rast_df<-highlands_log_crop%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)

rast_df$ID<-1

res<-10

rast_df%>%group_by(layer)%>%
  summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
  mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)


#dispersed retention

logging_disp<-nlm_randomcluster(100,100, resolution = 1,
                               p = 0.25, ai = c(0.6,0.4),
                               neighbourhood = 4)

plot(logging_disp)
logging_disp%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
logging_agg%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()

highlands_logged_disp<-mask(highland_random_large, logging_disp, maskvalue=0, inverse = T)

highlands_logged_disp[is.na(highlands_logged_disp[])] <- 0.5 

plot(highland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(highlands_logged_disp, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

optimal_hr(highlands_logged_disp, 10, 2.6)

#lowlands

#agg

lowlands_logged<-mask(lowland_random_large, logging_agg, maskvalue=0, inverse = T)

lowlands_logged[is.na(lowlands_logged[])] <- 0.5 

plot(lowlands_logged, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

optimal_hr(lowland_random_large, 10, 2.6)
optimal_hr(lowlands_logged, 10, 2.6)

#disp

lowlands_logged_disp<-mask(lowland_random_large, logging_disp, maskvalue=0, inverse = T)

lowlands_logged_disp[is.na(lowlands_logged_disp)] <- 0.5 

plot(lowlands_logged_disp, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

plot(lowlands_logged_disp, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

optimal_hr(lowlands_logged_disp, 10, 2.6)



# size constrained clustering ---------------------------------------------

rast_df_low<-lowlands_logged_disp%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)

rast_df_low$ID<-1

rast_area_low<-rast_df_low%>%group_by(layer)%>%
  summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
  mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)

lowlands_log_pres<-lowlands_logged_disp==0

lowlands_scclust<-rasterToPoints(lowlands_log_pres, spatial = F)%>%
  as.data.frame()%>%
  filter(layer == 1)%>%
  mutate(ID = row_number())

dist_log <- distances(lowlands_scclust,
                      id_variable = "ID",
                      dist_variables = c("x", "y"))

scclust_log <- sc_clustering(dist_log, 267)

scclust_log_df<-as.data.frame(scclust_log)

lowlands_scclust$cluster<-scclust_log_df$cluster_label

ggplot(lowlands_scclust, aes(x = x, y = y, color = as.factor(cluster)))+
  geom_point()

scc_test<-lowlands_scclust%>%group_by(cluster)%>%tally(layer)

#get hulls

lowlands_clust_sf<-st_as_sf(lowlands_scclust, coords = c('x', 'y'))%>%group_split(cluster)

hulls_scclust<-map(lowlands_clust_sf, hull_creation)

names(hulls_scclust)<-1:length(hulls_scclust)

hull_all_scclust<-hulls_scclust%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:length(hulls_scclust))

plot(lowlands_logged_disp, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(hull_all_scclust$x, add = T, col = rainbow(length(hulls_scclust)))

#plot

hull_all_scclust$area<-st_area(hull_all_scclust)/100
hull_all_scclust$cent<-st_centroid(hull_all_scclust)
hull_all_scclust$coord_x<-st_coordinates(hull_all_scclust$cent)[,1]
hull_all_scclust$coord_y<-st_coordinates(hull_all_scclust$cent)[,2]

#plot

lowlands_logged_disp_df<-lowlands_logged_disp%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

ggplot()+
  geom_raster(data = lowlands_logged_disp_df, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF'),
                    labels = c('>1% N/DM', 'non-habitat', '<1% N/DM'))+
  geom_sf(data=hull_all_scclust, size = 1.2, color = 'red', fill = NA)+
  geom_text(data = hull_all_scclust, aes(x = coord_x, y = coord_y, label = area), size = 5, col = 'blue')+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))


# function for scclust------------------------------------------

optimal_hr_sc<-function(raster, res, hr){
  
  rast_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)
  
  rast_df$ID<-1
  
  res<-res
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  rast_area_large_pres<-rast_area%>%
    filter(class == 0)%>%
    mutate(HRU = areaha/hr)
  
  sim_res<-raster==0
  
  sim_res_df<-rasterToPoints(sim_res, spatial = F)%>%
    as.data.frame()%>%
    filter(layer == 1)%>%
    mutate(ID = row_number())
  
  # size constraint cluster
  
  dist <- distances(sim_res_df,
                        id_variable = "ID",
                        dist_variables = c("x", "y"))
  
  scclust<- sc_clustering(dist, hr*res^2)
  
  scclust_df<-as.data.frame(scclust)
  
  sim_res_df$cluster_sc<-scclust_df$cluster_label
  
  sim_res_df_area_sc<-sim_res_df%>%group_by(cluster_sc)%>%tally(layer)%>%mutate(feed_ha = n/res^2)
  
  #make hulls scclust
  
  sim_res_df_sf_scclust<-st_as_sf(sim_res_df, coords = c('x', 'y'))%>%group_split(cluster_sc)
  
  hulls_scclust<-map(sim_res_df_sf_scclust, hull_creation)
  
  names(hulls_scclust)<-1:length(hulls_scclust)
  
  hull_all_scclust<-hulls_scclust%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:length(hulls_scclust))
  
  hull_all_scclust$area<-st_area(hull_all_scclust)/100
  hull_all_scclust$cent<-st_centroid(hull_all_scclust)
  hull_all_scclust$coord_x<-st_coordinates(hull_all_scclust$cent)[,1]
  hull_all_scclust$coord_y<-st_coordinates(hull_all_scclust$cent)[,2]
  
  hull_all_scclust$feed_ha_scclust<-sim_res_df_area_sc$feed_ha
  
  #plot
  
  raster_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%
    rename('value' = 3)

  plot_scclust<-ggplot()+
    geom_raster(data = raster_df, aes(x = x, y = y, fill = as.factor(value)))+
    scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                      labels = c('>1% N/DM', 'non-habitat', '<1% N/DM', 'climate'))+
    geom_sf(data=hull_all_scclust, size = 1.2, color = 'red', fill = NA)+
    geom_text(data = hull_all_scclust, aes(x = coord_x, y = coord_y, label = area), size = 5, col = 'blue')+
    labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw()+
    theme(axis.text.x = element_text(size = 12, face = 'bold'), 
          axis.text.y = element_text(size = 12, face = 'bold'),
          legend.title = element_text(size = 13, face = 'bold'),
          legend.text = element_text(size = 11, face = 'bold'))
  
  list<-list(rast_area_large_pres, 
             hull_all_scclust, 
             plot_scclust,
             sim_res_df_area_sc)
  
  names(list)<-c('data', 
                 'hulls_scclust', 
                 'plot_scclust',
                 'clust_area_scclust')
  
  return(list)
  
  plot_scclust
  
}

test_km<-optimal_hr(highlands_logged, 10, 2.6)
test_scc<-optimal_hr_sc(highlands_logged, 10, 2.6)

test_km$plot + test_scc$plot_scclust

# ggploting ---------------------------------------------------------------

#proj<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

#highlands_re<-projectRaster(highlands, crs = proj, method = 'ngb', progress = 'text')

highlands_df<-highlands%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)%>%mutate(value = as.integer(value))

ggplot()+
  geom_raster(data = highlands_df, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#440154FF', '#29AF7FFF', '#FDE725FF'),
                    labels = c('<1% N/DM', '>1% N/DM', 'non-habitat'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  coord_fixed(1)
  scale_x_continuous(expand = c(0, 0),                      
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), 
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_blank(),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))
