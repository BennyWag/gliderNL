
# 100 ha feeding ----------------------------------------------------------

library(raster)
library(sf)
library(landscapemetrics)
library(landscapetools)
library(NLMR)
library(exactextractr)
library(scclust)
library(patchwork)
library(effects)
library(spatialEco)
library(tidyverse)


# functions ---------------------------------------------------------------

simulate_landscape = function(p, ai, neigh = 8){
  
  pb$tick()$print()
  
  nlm_randomcluster(ncol = 100, nrow = 100, 
                    resolution = 1,
                    p = p, 
                    ai = ai,
                    neighbourhood = neigh)
  
}

optimal_hr<-function(raster, res = 10, hr){
  
  rast_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)
  
  rast_df$ID<-1
  
  res<-res
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  rast_area_large_pres<-rast_area%>%
    filter(class == 0)%>%
    mutate(HRU = areaha/hr,
           HR_size = hr)
  
  #cluster
  
  sim_res<-raster==0
  
  sim_res_df<-rasterToPoints(sim_res, spatial = F)%>%
    as.data.frame()%>%
    filter(layer == 1)
  
  cluster<-kmeans(sim_res_df, centers = if(rast_area_large_pres$HRU>=0.51){
    round(rast_area_large_pres$HRU)} else {1},
    nstart = 10)
  
  
  sim_res_df$clust<-cluster$cluster
  
  sim_res_df_area<-sim_res_df%>%group_by(clust)%>%tally(layer)%>%mutate(feed_ha = n/res^2)
  
  #make sf
  
  sim_res_df_sf<-st_as_sf(sim_res_df, coords = c('x', 'y'))%>%group_split(clust)
  
  hull_creation<-function(points){
    
    hull<-st_convex_hull(st_union(points))%>%st_as_sf()
    
    return(hull)
  }
  
  hulls<-map(sim_res_df_sf, hull_creation)
  
  names(hulls)<-length(hulls)
  
  nrows<-length(hulls)
  
  hull_all<-hulls%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:nrows)
  
  hull_all$area<-st_area(hull_all)/100
  hull_all$cent<-st_centroid(hull_all)
  hull_all$coord_x<-st_coordinates(hull_all$cent)[,1]
  hull_all$coord_y<-st_coordinates(hull_all$cent)[,2]
  
  hull_all$feed_ha<-sim_res_df_area$feed_ha
  hull_all$HR_size<-hr
  
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
  
  list<-list(rast_area_large_pres, hull_all, plot)
  
  names(list)<-c('data', 'hulls', 'plot')
  
  return(list)
  
  plot
  
}

get_listdata<-function(list, name, id = 'plot'){
  
  names<-names(list)
  flat<-lapply(list, `[`, c(name))%>%flatten()
  names(flat)<-names
  flat<-flat%>%bind_rows(.id = id)
  return(flat)
  
}

optimal_hr_figures<-function(raster, res = 10, hr){
  
  rast_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)
  
  rast_df$ID<-1
  
  res<-res
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  rast_area_large_pres<-rast_area%>%
    filter(class == 0)%>%
    mutate(HRU = areaha/hr,
           HR_size = hr)
  
  #cluster
  
  sim_res<-raster==0
  
  sim_res_df<-rasterToPoints(sim_res, spatial = F)%>%
    as.data.frame()%>%
    filter(layer == 1)
  
  cluster<-kmeans(sim_res_df, centers = if(rast_area_large_pres$HRU>=0.51){
    round(rast_area_large_pres$HRU)} else {1},
    nstart = 10)
  
  
  sim_res_df$clust<-cluster$cluster
  
  sim_res_df_area<-sim_res_df%>%group_by(clust)%>%tally(layer)%>%mutate(feed_ha = n/res^2)
  
  #make sf
  
  sim_res_df_sf<-st_as_sf(sim_res_df, coords = c('x', 'y'))%>%group_split(clust)
  
  hull_creation<-function(points){
    
    hull<-st_convex_hull(st_union(points))%>%st_as_sf()
    
    return(hull)
  }
  
  hulls<-map(sim_res_df_sf, hull_creation)
  
  names(hulls)<-length(hulls)
  
  nrows<-length(hulls)
  
  hull_all<-hulls%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:nrows)
  
  hull_all$area<-st_area(hull_all)/100
  hull_all$cent<-st_centroid(hull_all)
  hull_all$coord_x<-st_coordinates(hull_all$cent)[,1]
  hull_all$coord_y<-st_coordinates(hull_all$cent)[,2]
  
  hull_all$feed_ha<-sim_res_df_area$feed_ha
  hull_all$HR_size<-hr
  
  raster_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%
    rename('value' = 3)
  
  plot<-ggplot()+
    geom_raster(data = raster_df, aes(x = x, y = y, fill = as.factor(value)))+
    scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                      labels = c('Feeding \nhabitat', 'Not \nhabitat', 
                                 'Non-feeding \nhabitat', 'Climatically \nunsuitable'))+
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
  
  list<-list(rast_area_large_pres, hull_all, plot, sim_res_df)
  
  names(list)<-c('data', 'hulls', 'plot', 'cluster_data')
  
  return(list)
  
  plot
  
}

optimal_hr_functional<-function(raster, res = 10, hr){
  
  rast_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)
  
  rast_df$ID<-1
  
  res<-res
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  rast_area_large_pres<-rast_area%>%
    filter(class == 0)%>%
    mutate(HRU = areaha/hr,
           HR_size = hr)
  
  #cluster
  
  sim_res<-raster==0
  
  sim_res_df<-rasterToPoints(sim_res, spatial = F)%>%
    as.data.frame()%>%
    filter(layer == 1)
  
  cluster<-kmeans(sim_res_df, centers = if(rast_area_large_pres$HRU>=0.51){
    round(rast_area_large_pres$HRU)} else {1},
    nstart = 10)
  
  
  sim_res_df$clust<-cluster$cluster
  
  sim_res_df_area<-sim_res_df%>%group_by(clust)%>%tally(layer)%>%mutate(feed_ha = n/res^2)
  
  #make sf
  
  sim_res_df_sf<-st_as_sf(sim_res_df, coords = c('x', 'y'))%>%group_split(clust)
  
  hull_creation<-function(points){
    
    hull<-st_convex_hull(st_union(points))%>%st_as_sf()
    
    return(hull)
  }
  
  hulls<-map(sim_res_df_sf, hull_creation)
  
  names(hulls)<-length(hulls)
  
  nrows<-length(hulls)
  
  hull_all<-hulls%>%bind_rows()%>%bind_rows()%>%mutate(ID = 1:nrows)
  
  hull_all$area<-st_area(hull_all)/100
  hull_all$cent<-st_centroid(hull_all)
  hull_all$coord_x<-st_coordinates(hull_all$cent)[,1]
  hull_all$coord_y<-st_coordinates(hull_all$cent)[,2]
  
  hull_all$feed_ha<-sim_res_df_area$feed_ha
  hull_all$HR_size<-hr
  
  hull_all_filter<-hull_all%>%mutate(feed_area_ratio = feed_ha/area)%>%
    filter(area<=6 & feed_area_ratio>=0.25)
  
  raster_df<-raster%>%as.data.frame(xy = T, na.rm = T)%>%
    rename('value' = 3)
  
  plot<-ggplot()+
    geom_raster(data = raster_df, aes(x = x, y = y, fill = as.factor(value)))+
    scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                      labels = c('Feeding \nhabitat', 'Not \nhabitat', 
                                 'Non-feeding \nhabitat', 'Climatically \nunsuitable'))+
    geom_sf(data=hull_all_filter, size = 1.2, color = 'red', fill = NA)+
    geom_text(data = hull_all_filter, aes(x = coord_x, y = coord_y, label = area), size = 5, col = 'blue')+
    labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw()+
    theme(axis.text.x = element_text(size = 12, face = 'bold'), 
          axis.text.y = element_text(size = 12, face = 'bold'),
          legend.title = element_text(size = 13, face = 'bold'),
          legend.text = element_text(size = 11, face = 'bold'))
  
  list<-list(rast_area_large_pres, hull_all, hull_all_filter, plot, sim_res_df)
  
  names(list)<-c('data', 'hulls', 'hulls_functional', 'plot', 'cluster_data')
  
  return(list)
  
  plot
  
}

# create rasters of disp and agg feeding habitat --------------------------

#dispersed

C4_feeding_disp_param <- expand.grid(p = c(0.10),
                                  ai = list(c(0.1, 0.9), 
                                            c(0.2, 0.8),
                                            c(0.3, 0.7),
                                            c(0.4, 0.6),
                                            c(0.5, 0.5), 
                                            c(0.6, 0.4),
                                            c(0.7, 0.3),
                                            c(0.8, 0.2),
                                            c(0.9, 0.1)))%>%as_tibble()


pb <- progress_estimated(nrow(C4_feeding_disp_param))

C4_feeding_disp<-map2(C4_feeding_disp_param$p, C4_feeding_disp_param$ai, simulate_landscape, neigh = 4)

names(C4_feeding_disp)<-c('10%', '20%', '30%', '40%','50%', '60%', '70%', '80%', '90%')

#check

C4_feeding_disp[[1]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()

#aggregated

C4_feeding_agg<-readRDS('outputs/hr/logmasks_agg_all.rds')

C4_feeding_agg_inv<-map(C4_feeding_agg, raster.invert)

#check

C4_feeding_agg_inv[[1]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()

C4_feeding_agg_inv<-rev(C4_feeding_agg_inv)

#save

saveRDS(C4_feeding_disp, 'outputs/hr/100ha_feeding_disp.rds')
saveRDS(C4_feeding_agg_inv, 'outputs/hr/100ha_feeding_agg.rds')

# run optimal hr ----------------------------------------------------------

#test

optimal_hr(C4_feeding_disp$`20%`, hr = 2.6)
optimal_hr(C4_feeding_agg_inv$`20%`, hr = 2.6)


#all

optimal_disp<-map(C4_feeding_disp, optimal_hr, hr = 2.6)
optimal_agg<-map(C4_feeding_agg_inv, optimal_hr, hr = 2.6)


# compile data ------------------------------------------------------------

data_feeding_area<-bind_rows(get_listdata(optimal_disp, 'data', id = 'perc')%>%mutate(agg = 'disp'),
                        get_listdata(optimal_agg, 'data', id = 'perc')%>%mutate(agg = 'agg'))
                        

ggplot(data_feeding_area, 
       aes(x = perc, y = HRU, color = as.factor(agg)))+
  geom_point()+
  labs(color = 'Retention \nstrategy', x = '% feeding habitat', y = 'No. of home ranges')+
  scale_color_discrete(labels = c('Dispersed', "Aggregated")) +
  theme_bw()

data_feeding_hull<-bind_rows(get_listdata(optimal_disp, 'hulls', id = 'perc')%>%mutate(agg = 'disp'),
                             get_listdata(optimal_agg, 'hulls', id = 'perc')%>%mutate(agg = 'agg'))%>%
  st_drop_geometry()


data_feeding<-right_join(data_feeding_hull, 
                         data_feeding_area%>%
                                    group_by(perc)%>%summarise(feeding_total = max(areaha)),
                                  by = 'perc')%>%
  mutate(feed_area_ratio = (feed_ha/area)*100)

ggplot(data_feeding, aes(x = perc, y = area, color = as.factor(agg)))+
  geom_boxplot()

ggplot(data_feeding, aes(x = perc, y = feed_area_ratio, color = as.factor(agg)))+
  geom_boxplot()

#save

write.csv(data_feeding_area, 'outputs/hr/feeding_data_areas.csv', row.names = F)

data_feeding_save<-data_feeding%>%select(1:3, 7:11)

write.csv(data_feeding_save, 'outputs/hr/feeding_data_hulls.csv', row.names = F)

# figures -----------------------------------------------------------------

#clumped and dispersed feeding habitat

raster_df_feeding_agg1<-C4_feeding_agg_inv$`50%`%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

raster_df_feeding_disp<-C4_feeding_disp$`50%`%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)


a<-ggplot()+
  geom_raster(data = raster_df_feeding_agg1, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                    labels = c('Feeding \nhabitat', 'Not \nhabitat', 'Non-feeding \nhabitat', 'Climatically \nunsuitable'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))

b<-ggplot()+
  geom_raster(data = raster_df_feeding_disp, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                    labels = c('Feeding \nhabitat', 'Not \nhabitat', 'Non-feeding \nhabitat', 'Climatically \nunsuitable'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_blank(),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))

a + b + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('100_feeding_maps.svg', path = 'figures', device = 'svg',
       width = 35, height = 15, units = 'cm', dpi = 600)

# cluster and optimal HR plot

cluster_plotting<-optimal_hr_figures(C4_feeding_agg_inv$`50%`, hr = 2.6)

a<-ggplot(cluster_plotting$cluster_data, aes (x = x, y = y, color = as.factor(clust)))+
  geom_point()+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.position = 'none')

b<-cluster_plotting$plot+
  theme(axis.text.y = element_blank())+  
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75))


a + b + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('100_feeding_clustering.svg',path = 'figures', device = 'svg',
       width = 35, height = 15, units = 'cm', dpi = 600)

#30% clumped map, filtered for functional HRs

initial_hr<-optimal_hr_figures(C4_feeding_agg_inv$`30%`, hr = 2.6)

a<-initial_hr$plot+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75))


functioal_hr<-optimal_hr_functional(C4_feeding_agg_inv$`30%`, hr = 2.6)

b<-functioal_hr$plot+
  theme(axis.text.y = element_blank())+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75))

a + b + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('100_feeding_functional.svg',path = 'figures', device = 'svg',
       width = 35, height = 15, units = 'cm', dpi = 600)  

