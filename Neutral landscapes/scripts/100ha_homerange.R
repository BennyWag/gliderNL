
# 100 ha home range analysis ----------------------------------------------

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

loadrasterlist_select<-function(rasterdir, func,  bands, bandnames){
  
  z<-getwd()
  setwd(rasterdir)
  
  #
  print('listing files in raster folder') 
  temp<-list.files(pattern = '*.tif', full.names=FALSE)
  
  #
  print('compiling rater names')
  names_plots<-temp
  names_plots<-str_replace(names_plots,'.tif','')
  names_band<-bandnames
  
  #
  print('load all rasters into list')
  allrasters <- lapply(temp, func, bands = bands)
  
  #
  print('set stack names')
  allrasters<-lapply(allrasters, setNames, names_band)
  names(allrasters)<-names_plots
  
  setwd(z)
  return(allrasters)
  
}

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

raster_per<-function(class_raster){
  
  rast_df<-class_raster%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)
  
  rast_df$ID<-1
  
  res<-res(class_raster)[1]
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%
    rename(class = layer)%>%
    mutate(class_arr = case_when(class == 0 ~ 3,
                                 TRUE ~ class))%>%
    arrange(class_arr)
  
}

raster_agg<-function(class_raster){
  
  calculate_lsm(class_raster, what = c('lsm_l_ai'),
                     directions = 8)%>%select(agg = value)
  
}

dreisatz<-function(new, total, respective){
  
  factor<-respective/total
  new*factor
  
}

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

log_forest<-function(raster, mask_rast, maskval = 0, updateval = 0.5, inv = T){
  
  x<-raster::mask(raster, mask_rast, maskvalue=maskval, updatevalue = updateval, inverse = inv)
  
}

get_list_mutate<-function(list, id, ret_strat, perc_ret){
  
  get_listdata(list, id)%>%mutate(ret_strat = ret_strat, perc_ret = perc_ret)
  
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

# load all spatial predictions (C3) ---------------------------------------

C3_preds<-loadrasterlist_select('data/C3 rasters', func = raster::raster, bands = 1, bandnames = 'layer')

plot(C3_preds$T35P1, col = c('#440154FF', '#29AF7FFF', '#FDE725FF'))

plotnames<-names(C3_preds)

# get % area per class and agg ----------------------------------------------

C3_areas<-map(C3_preds, raster_per)

#C3_areas_bind<-bind_rows(C3_areas, .id = 'plot')

#create list of fraction vectors

C3_all_frac<-list()

for (i in seq_along(C3_areas)){
  
  a<-C3_areas[[i]][["per"]]
  
  C3_all_frac[[length(C3_all_frac)+1]] <- a
  
}

names(C3_all_frac)<-names(C3_preds)

# get aggregation list

C3_agg<-map(C3_preds, raster_agg)

C3_agg_bind<-bind_rows(C3_agg, .id = 'plot')%>%mutate(agg_nlm = (dreisatz(agg, 100, 40)/100)/2)

#save

saveRDS(C3_all_frac, 'outputs/hr/C3_fractions.rds')
write.csv(C3_agg_bind, 'outputs/hr/C3_aggregation.csv', row.names = F)

# simulate NLMs for each site ---------------------------------------------

#test

a<-simulate_landscape(p = C3_agg_bind$agg_nlm[1], ai = C3_all_frac$T15P1)

plot(a, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

#all

pb <- progress_estimated(length(C3_all_frac))

C3_NLM<-map2(C3_agg_bind$agg_nlm, C3_all_frac, simulate_landscape)

names(C3_NLM)<-plotnames

plot(C3_NLM$T1P5, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

saveRDS(C3_NLM, 'outputs/hr/100ha_NLMs.rds')

# get areas and HRUs ------------------------------------------------------

#HR sizes: 1, 2.6, 4, 7, 18(?)

C4_hr_1<-map(C3_NLM, optimal_hr, hr = 1)
C4_hr_26<-map(C3_NLM, optimal_hr, hr = 2.6)
C4_hr_4<-map(C3_NLM, optimal_hr, hr = 4)
C4_hr_7<-map(C3_NLM, optimal_hr, hr = 7)

C4_hr_control<-list(C4_hr_1, C4_hr_26, C4_hr_4, C4_hr_7)

saveRDS(C4_hr_control, 'outputs/hr/optimal_hr_control.rds')

#get total area of feeding habitat

C4_hr_area<-bind_rows(get_listdata(C4_hr_1, 'data'),
                     get_listdata(C4_hr_26, 'data'),
                     get_listdata(C4_hr_4, 'data'),
                     get_listdata(C4_hr_7, 'data'))

ggplot(C4_hr_area, aes(x = areaha, y = HRU, color = as.factor(HR_size)))+
  geom_point()+
  geom_line()

#get no of hulls, hull areas and fraction of feeding habitat

C4_hull_area<-bind_rows(get_listdata(C4_hr_1, 'hulls'),
                      get_listdata(C4_hr_26, 'hulls'),
                      get_listdata(C4_hr_4, 'hulls'),
                      get_listdata(C4_hr_7, 'hulls'))%>%
  st_drop_geometry()%>%
  select(plot, ID, area, feed_ha, HR_size)

C4_hull_area<-right_join(C4_hull_area, 
                         C4_hr_area%>%group_by(plot)%>%summarise(feeding_total = max(areaha)), 
                         by = 'plot')

C4_hull_area_stats<-C4_hull_area%>%
  group_by(plot, HR_size)%>%
  summarise(n_homeranges = max(ID),
            mean_HR_size = mean(area),
            mean_feed_size = mean(feed_ha),
            total_feed_area = max(feeding_total))

ggplot(C4_hull_area, aes(area, feed_ha, color = as.factor(HR_size)))+
  geom_point()

# create logging masks ----------------------------------------------------

#all

# C4_mask_param <- expand.grid(p = c(0.58, 0.25),
#                              ai = list(c(0.5, 0.5), 
#                                               c(0.6, 0.4),
#                                               c(0.7, 0.3),
#                                               c(0.8, 0.2),
#                                               c(0.9, 0.1)))%>%as_tibble()

#dispersed

C4_mask_param_disp <- expand.grid(p = c(0.10),
                             ai = list(c(0.5, 0.5), 
                                       c(0.6, 0.4),
                                       c(0.7, 0.3),
                                       c(0.8, 0.2),
                                       c(0.9, 0.1)))%>%as_tibble()

pb <- progress_estimated(nrow(C4_mask_param_disp))

C4_logmasks_disp<-map2(C4_mask_param_disp$p, C4_mask_param_disp$ai, simulate_landscape, neigh = 4)

#add additional

C4_mask_param_disp_more <- expand.grid(p = c(0.10),
                                      ai = list(c(0.1, 0.9), 
                                                c(0.2, 0.8),
                                                c(0.3, 0.7),
                                                c(0.4, 0.6)))%>%as_tibble()

pb <- progress_estimated(nrow(C4_mask_param_disp_more))

C4_logmasks_disp_more<-map2(C4_mask_param_disp_more$p, C4_mask_param_disp_more$ai, simulate_landscape, neigh = 4)

#check fractions

C4_logmasks_disp[[5]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()

C4_logmasks_disp_more[[4]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()


#aggregated

C4_mask_param_agg <- expand.grid(p = c(0.58),
                                  ai = list(c(0.5, 0.5), 
                                            c(0.6, 0.4),
                                            c(0.7, 0.3),
                                            c(0.8, 0.2),
                                            c(0.9, 0.1)))%>%as_tibble()

#add additional


pb <- progress_estimated(nrow(C4_mask_param_agg))

C4_logmasks_agg<-map2(C4_mask_param_agg$p, C4_mask_param_agg$ai, simulate_landscape, neigh = 4)

#check fractions

C4_logmasks_agg[[1]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()


### THIS THROWS RANDOM FRACTIONS :(


#rerun aggregated masks manually multiple times to get desired levels of ret. --------

#50:50

pb <- progress_estimated(10)
C4_agg50_list<-rerun(10, simulate_landscape(p = 0.58, ai = c(0.5,0.5), neigh = 4))

agg50_frac<-list()

for (i in seq_along(C4_agg50_list)){

  a<-C4_agg50_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg50_frac[[length(agg50_frac)+1]] <- a
  
}

plot(C4_agg50_list[[4]]) #50:50 plot
plot(C4_agg50_list[[5]]) #60:40 plot


#60:40

pb <- progress_estimated(10)
C4_agg40_list<-rerun(10, simulate_landscape(p = 0.58, ai = c(0.6,0.4), neigh = 4))

agg40_frac<-list()

for (i in seq_along(C4_agg40_list)){
  
  a<-C4_agg40_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg40_frac[[length(agg40_frac)+1]] <- a
  
}

plot(C4_agg40_list[[1]]) #70:30 plot


#70:30

pb <- progress_estimated(10)
C4_agg30_list<-rerun(10, simulate_landscape(p = 0.58, ai = c(0.7,0.3), neigh = 4))

agg30_frac<-list()

for (i in seq_along(C4_agg30_list)){
  
  a<-C4_agg30_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg30_frac[[length(agg30_frac)+1]] <- a
  
}

#80:20

pb <- progress_estimated(10)
C4_agg20_list<-rerun(10, simulate_landscape(p = 0.58, ai = c(0.8,0.2), neigh = 4))

agg20_frac<-list()

for (i in seq_along(C4_agg20_list)){
  
  a<-C4_agg20_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg20_frac[[length(agg20_frac)+1]] <- a
  
}

plot(C4_agg20_list[[4]]) #90:10 plot (but too scattered?)
plot(C4_agg20_list[[8]]) #81:19 plot closest


#90:10

pb <- progress_estimated(10)
C4_agg10_list<-rerun(10, simulate_landscape(p = 0.58, ai = c(0.9,0.10), neigh = 4))

agg10_frac<-list()

for (i in seq_along(C4_agg10_list)){
  
  a<-C4_agg10_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg10_frac[[length(agg10_frac)+1]] <- a
  
}

plot(C4_agg10_list[[3]]) #92:8 plot (closest)

#combine manually

C4_logmasks_agg_manual<-list(C4_agg50_list[[4]], 
                             C4_agg50_list[[5]],
                             C4_agg40_list[[1]],
                             C4_agg20_list[[8]],
                             C4_agg10_list[[3]])

#add more aggregated

#10:90

pb <- progress_estimated(5)
C4_agg90_list<-rerun(5, simulate_landscape(p = 0.58, ai = c(0.1,0.9), neigh = 4))

agg90_frac<-list()

for (i in seq_along(C4_agg90_list)){
  
  a<-C4_agg90_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg90_frac[[length(agg90_frac)+1]] <- a
  
}

plot(C4_agg90_list[[3]]) #20:80 plot

#20:80

pb <- progress_estimated(5)
C4_agg80_list<-rerun(5, simulate_landscape(p = 0.58, ai = c(0.2,0.8), neigh = 4))

agg80_frac<-list()

for (i in seq_along(C4_agg80_list)){
  
  a<-C4_agg80_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg80_frac[[length(agg80_frac)+1]] <- a
  
}

plot(C4_agg80_list[[1]]) #10:90 plot - this

plot(C4_agg80_list[[5]]) #20:80 plot - this


#30:70

pb <- progress_estimated(10)
C4_agg70_list<-rerun(10, simulate_landscape(p = 0.58, ai = c(0.3,0.7), neigh = 4))

agg70_frac<-list()

for (i in seq_along(C4_agg70_list)){
  
  a<-C4_agg70_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg70_frac[[length(agg70_frac)+1]] <- a
  
}

plot(C4_agg70_list[[5]]) #40:60 plot - this

plot(C4_logmasks_agg_manual$`30%`)

plot(raster.invert(C4_logmasks_agg_manual$`30%`))

#40:60

pb <- progress_estimated(5)
C4_agg60_list<-rerun(5, simulate_landscape(p = 0.58, ai = c(0.4,0.6), neigh = 4))

agg60_frac<-list()

for (i in seq_along(C4_agg60_list)){
  
  a<-C4_agg60_list[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  agg60_frac[[length(agg60_frac)+1]] <- a
  
}

plot(C4_agg60_list[[3]]) #40:60 plot


#combine new layers

#dispersed

C4_logmasks_disp_all<-list(C4_logmasks_disp_more, C4_logmasks_disp)%>%flatten()

#aggregated

#C4_logmasks_agg_manual<-readRDS('outputs/hr/logmasks_agg.rds')

C4_logmasks_agg_manual_more<-list(C4_agg80_list[[1]], 
                             C4_agg80_list[[5]],
                             raster.invert(C4_logmasks_agg_manual$`30%`),
                             C4_agg70_list[[5]])

C4_logmasks_agg_manual_all<-list(C4_logmasks_agg_manual_more, C4_logmasks_agg_manual)%>%flatten()


#set names

masknames<-c('50%', '40%', '30%', '20%', '10%')

names(C4_logmasks_agg_manual)<-masknames
names(C4_logmasks_disp)<-masknames

saveRDS(C4_logmasks_agg_manual, 'outputs/hr/logmasks_agg.rds')
saveRDS(C4_logmasks_disp, 'outputs/hr/logmasks_disp.rds')

#for all layers

masknames_more<-c('90%', '80%', '70%', '60%','50%', '40%', '30%', '20%', '10%')

names(C4_logmasks_disp_all)<-masknames_more
names(C4_logmasks_agg_manual_all)<-masknames_more


saveRDS(C4_logmasks_disp_all, 'outputs/hr/logmasks_disp_all.rds')
saveRDS(C4_logmasks_agg_manual_all, 'outputs/hr/logmasks_agg_all.rds')

# apply logging masks to NLMs -------------------------------------------------

#test for function

x<-mask(C3_NLM$T3P5, a, maskvalue=0, updatevalue = 0.5 , inverse = F)
y<-mask(C3_NLM$T3P5, C4_logmasks_disp$`40%`, maskvalue=0, updatevalue = 0.5 , inverse = F)


plot(C3_NLM$T3P5, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(x, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(y, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

optimal_hr(x, hr = 2.6)
optimal_hr(y, hr = 2.6)


#function test

b<-log_forest(C3_NLM$T3P5, C4_logmasks_agg$`50%`)

plot(a, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(b, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

#log NLMs

C4_NLMs_log_agg_90<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`90%`, inv = F)
C4_NLMs_log_agg_80<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`80%`, inv = F)
C4_NLMs_log_agg_70<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`70%`, inv = F)
C4_NLMs_log_agg_60<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`60%`, inv = F)
C4_NLMs_log_agg_50<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`50%`, inv = F)
C4_NLMs_log_agg_40<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`40%`, inv = F)
C4_NLMs_log_agg_30<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`30%`, inv = F)
C4_NLMs_log_agg_20<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`20%`, inv = F)
C4_NLMs_log_agg_10<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_agg_manual_all$`10%`, inv = F)

C4_NLMs_log_disp_90<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`90%`, inv = F)
C4_NLMs_log_disp_80<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`80%`, inv = F)
C4_NLMs_log_disp_70<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`70%`, inv = F)
C4_NLMs_log_disp_60<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`60%`, inv = F)
C4_NLMs_log_disp_50<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`50%`, inv = F)
C4_NLMs_log_disp_40<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`40%`, inv = F)
C4_NLMs_log_disp_30<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`30%`, inv = F)
C4_NLMs_log_disp_20<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`20%`, inv = F)
C4_NLMs_log_disp_10<-map(C3_NLM, log_forest, mask_rast = C4_logmasks_disp_all$`10%`, inv = F)

#test

# plot(C3_NLM$T35P1, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
# plot(C4_logmasks_agg$`50%`)
# plot(C4_NLMs_log_agg_50$T35P1, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))


optimal_hr(C3_NLM$T15P5, hr = 2.6)
optimal_hr(C4_NLMs_log_agg_50$T15P5, hr = 2.6)
optimal_hr(C4_NLMs_log_disp_50$T15P5, hr = 2.6)


# compile optimal hrs on logged rasters -----------------------------------

C4_hrs<-c(1, 2.6, 4, 7)

C4_hr_agg_90<-list()
C4_hr_agg_80<-list()
C4_hr_agg_70<-list()
C4_hr_agg_60<-list()
C4_hr_agg_50<-list()
C4_hr_agg_40<-list()
C4_hr_agg_30<-list()
C4_hr_agg_20<-list()
C4_hr_agg_10<-list()

C4_hr_disp_90<-list()
C4_hr_disp_80<-list()
C4_hr_disp_70<-list()
C4_hr_disp_60<-list()
C4_hr_disp_50<-list()
C4_hr_disp_40<-list()
C4_hr_disp_30<-list()
C4_hr_disp_20<-list()
C4_hr_disp_10<-list()


for (i in C4_hrs){
  
  #agg
  
  a<-map(C4_NLMs_log_agg_90, optimal_hr, hr = i)
  C4_hr_agg_90[[length(C4_hr_agg_90)+1]] <- a
  print('agg 90 check')
  
  a<-map(C4_NLMs_log_agg_80, optimal_hr, hr = i)
  C4_hr_agg_80[[length(C4_hr_agg_80)+1]] <- a
  print('agg 80 check')
  
  a<-map(C4_NLMs_log_agg_70, optimal_hr, hr = i)
  C4_hr_agg_70[[length(C4_hr_agg_70)+1]] <- a
  print('agg 70 check')
  
  a<-map(C4_NLMs_log_agg_60, optimal_hr, hr = i)
  C4_hr_agg_60[[length(C4_hr_agg_60)+1]] <- a
  print('agg 60 check')
  
  a<-map(C4_NLMs_log_agg_50, optimal_hr, hr = i)
  C4_hr_agg_50[[length(C4_hr_agg_50)+1]] <- a
  print('agg 50 check')
  
  a<-map(C4_NLMs_log_agg_40, optimal_hr, hr = i)
  C4_hr_agg_40[[length(C4_hr_agg_40)+1]] <- a
  print('agg 40 check')
  
  a<-map(C4_NLMs_log_agg_30, optimal_hr, hr = i)
  C4_hr_agg_30[[length(C4_hr_agg_30)+1]] <- a
  print('agg 30 check')
  
  a<-map(C4_NLMs_log_agg_20, optimal_hr, hr = i)
  C4_hr_agg_20[[length(C4_hr_agg_20)+1]] <- a
  print('agg 20 check')
  
  a<-map(C4_NLMs_log_agg_10, optimal_hr, hr = i)
  C4_hr_agg_10[[length(C4_hr_agg_10)+1]] <- a
  print('agg 10 check')
  
  # disp
  
  a<-map(C4_NLMs_log_disp_90, optimal_hr, hr = i)
  C4_hr_disp_90[[length(C4_hr_disp_90)+1]] <- a
  print('disp 90 check')
  
  a<-map(C4_NLMs_log_disp_80, optimal_hr, hr = i)
  C4_hr_disp_80[[length(C4_hr_disp_80)+1]] <- a
  print('disp 80 check')
  
  a<-map(C4_NLMs_log_disp_70, optimal_hr, hr = i)
  C4_hr_disp_70[[length(C4_hr_disp_70)+1]] <- a
  print('disp 70 check')
  
  a<-map(C4_NLMs_log_disp_60, optimal_hr, hr = i)
  C4_hr_disp_60[[length(C4_hr_disp_60)+1]] <- a
  print('disp 60 check')
  
  a<-map(C4_NLMs_log_disp_50, optimal_hr, hr = i)
  C4_hr_disp_50[[length(C4_hr_disp_50)+1]] <- a
  print('disp 50 check')
  
  a<-map(C4_NLMs_log_disp_40, optimal_hr, hr = i)
  C4_hr_disp_40[[length(C4_hr_disp_40)+1]] <- a
  print('disp 40 check')
  
  a<-map(C4_NLMs_log_disp_30, optimal_hr, hr = i)
  C4_hr_disp_30[[length(C4_hr_disp_30)+1]] <- a
  print('disp 30 check')
  
  a<-map(C4_NLMs_log_disp_20, optimal_hr, hr = i)
  C4_hr_disp_20[[length(C4_hr_disp_20)+1]] <- a
  print('disp 20 check')
  
  a<-map(C4_NLMs_log_disp_10, optimal_hr, hr = i)
  C4_hr_disp_10[[length(C4_hr_disp_10)+1]] <- a
  print('disp 10 check')
  
}

names(C4_hr_agg_90)<-C4_hrs
names(C4_hr_agg_80)<-C4_hrs
names(C4_hr_agg_70)<-C4_hrs
names(C4_hr_agg_60)<-C4_hrs
names(C4_hr_agg_50)<-C4_hrs
names(C4_hr_agg_40)<-C4_hrs
names(C4_hr_agg_30)<-C4_hrs
names(C4_hr_agg_20)<-C4_hrs
names(C4_hr_agg_10)<-C4_hrs

names(C4_hr_disp_90)<-C4_hrs
names(C4_hr_disp_80)<-C4_hrs
names(C4_hr_disp_70)<-C4_hrs
names(C4_hr_disp_60)<-C4_hrs
names(C4_hr_disp_50)<-C4_hrs
names(C4_hr_disp_40)<-C4_hrs
names(C4_hr_disp_30)<-C4_hrs
names(C4_hr_disp_20)<-C4_hrs
names(C4_hr_disp_10)<-C4_hrs


# compile dataset ---------------------------------------------------------

#add names for control/non-logged rasters

names(C4_hr_control)<-C4_hrs

# extract 'data' column

data_control<-map(C4_hr_control, get_list_mutate, id = 'data', ret_strat = 'control', perc_ret = '100%')%>%
  bind_rows()

data_disp_90<-map(C4_hr_disp_90, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '90%')%>%bind_rows()
data_disp_80<-map(C4_hr_disp_80, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '80%')%>%bind_rows()
data_disp_70<-map(C4_hr_disp_70, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '70%')%>%bind_rows()
data_disp_60<-map(C4_hr_disp_60, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '60%')%>%bind_rows()
data_disp_50<-map(C4_hr_disp_50, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '50%')%>%bind_rows()
data_disp_40<-map(C4_hr_disp_40, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '40%')%>%bind_rows()
data_disp_30<-map(C4_hr_disp_30, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '30%')%>%bind_rows()
data_disp_20<-map(C4_hr_disp_20, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '20%')%>%bind_rows()
data_disp_10<-map(C4_hr_disp_10, get_list_mutate, id = 'data', ret_strat = 'disp', perc_ret = '10%')%>%bind_rows()

data_agg_90<-map(C4_hr_agg_90, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '90%')%>%bind_rows()
data_agg_80<-map(C4_hr_agg_80, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '80%')%>%bind_rows()
data_agg_70<-map(C4_hr_agg_70, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '70%')%>%bind_rows()
data_agg_60<-map(C4_hr_agg_60, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '60%')%>%bind_rows()
data_agg_50<-map(C4_hr_agg_50, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '50%')%>%bind_rows()
data_agg_40<-map(C4_hr_agg_40, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '40%')%>%bind_rows()
data_agg_30<-map(C4_hr_agg_30, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '30%')%>%bind_rows()
data_agg_20<-map(C4_hr_agg_20, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '20%')%>%bind_rows()
data_agg_10<-map(C4_hr_agg_10, get_list_mutate, id = 'data', ret_strat = 'agg', perc_ret = '10%')%>%bind_rows()

C4_hr_areas<-bind_rows(data_control,
                       data_disp_90,
                       data_disp_80,
                       data_disp_70,
                       data_disp_60,
                       data_disp_50,
                       data_disp_40,
                       data_disp_30,
                       data_disp_20,
                       data_disp_10,
                       data_agg_90,
                       data_agg_80,
                       data_agg_70,
                       data_agg_60,
                       data_agg_50,
                       data_agg_40,
                       data_agg_30,
                       data_agg_20,
                       data_agg_10)%>%
  mutate(transect = case_when(grepl('T1', plot) ~ 'lowele',
                              grepl('T2', plot) ~ 'midele',
                              grepl('T3', plot) ~ 'highele'),
         transect = factor(transect, levels = c('lowele', 'midele', 'highele')),
         perc_ret = factor(perc_ret, levels = c('100%', '90%','80%','70%','60%',
                                                '50%', '40%', '30%', '20%', '10%')),
         ret_strat = factor(ret_strat, levels = c('control', 'agg', 'disp')))

#number of home-ranges

ggplot(C4_hr_areas%>%filter(HR_size == 2.6), aes(x = perc_ret, y = HRU, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(~transect)

ggplot(C4_hr_areas, 
       aes(x = perc_ret, y = HRU, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(transect~HR_size,
             labeller = labeller(transect = transect.labs,
                                 HR_size = HR.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'No. of home ranges')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()

#facet_grid(transect~HR_size)

#save

write.csv(C4_hr_areas, 'outputs/hr/hr_data_areas.csv', row.names = F)

# extract 'hulls' column

hulls_control<-map(C4_hr_control, get_list_mutate, id = 'hulls', ret_strat = 'control', perc_ret = '100%')%>%
  bind_rows()%>%st_drop_geometry()

hulls_disp_90<-map(C4_hr_disp_90, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '90%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_80<-map(C4_hr_disp_80, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '80%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_70<-map(C4_hr_disp_70, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '70%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_60<-map(C4_hr_disp_60, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '60%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_50<-map(C4_hr_disp_50, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '50%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_40<-map(C4_hr_disp_40, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '40%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_30<-map(C4_hr_disp_30, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '30%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_20<-map(C4_hr_disp_20, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '20%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_disp_10<-map(C4_hr_disp_10, get_list_mutate, id = 'hulls', ret_strat = 'disp', perc_ret = '10%')%>%
  bind_rows()%>%st_drop_geometry()

hulls_agg_90<-map(C4_hr_agg_90, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '90%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_80<-map(C4_hr_agg_80, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '80%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_70<-map(C4_hr_agg_70, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '70%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_60<-map(C4_hr_agg_60, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '60%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_50<-map(C4_hr_agg_50, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '50%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_40<-map(C4_hr_agg_40, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '40%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_30<-map(C4_hr_agg_30, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '30%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_20<-map(C4_hr_agg_20, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '20%')%>%
  bind_rows()%>%st_drop_geometry()
hulls_agg_10<-map(C4_hr_agg_10, get_list_mutate, id = 'hulls', ret_strat = 'agg', perc_ret = '10%')%>%
  bind_rows()%>%st_drop_geometry()

C4_hull_areas<-bind_rows(hulls_control,
                         hulls_disp_90,
                         hulls_disp_80,
                         hulls_disp_70,
                         hulls_disp_60,
                        hulls_disp_50,
                        hulls_disp_40,
                        hulls_disp_30,
                        hulls_disp_20,
                        hulls_disp_10,
                        hulls_agg_90,
                        hulls_agg_80,
                        hulls_agg_70,
                        hulls_agg_60,
                        hulls_agg_50,
                        hulls_agg_40,
                        hulls_agg_30,
                        hulls_agg_20,
                        hulls_agg_10)%>%
  mutate(transect = case_when(grepl('T1', plot) ~ 'lowele',
                              grepl('T2', plot) ~ 'midele',
                              grepl('T3', plot) ~ 'highele'),
         transect = factor(transect, levels = c('lowele', 'midele', 'highele')),
         perc_ret = factor(perc_ret, levels = c('100%', '90%','80%','70%','60%',
                                                '50%', '40%', '30%', '20%', '10%')),
         ret_strat = factor(ret_strat, levels = c('control', 'agg', 'disp')))

# combine

C4_hull_areas_combine<-right_join(C4_hull_areas, 
                                  C4_hr_areas%>%
                                    group_by(plot)%>%summarise(feeding_total = max(areaha)),
                                  by = 'plot')%>%
  mutate(feed_area_ratio = (feed_ha/area)*100)


ggplot(C4_hull_areas_combine%>%filter(HR_size == 2.6), 
       aes(x = perc_ret, y = area, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)

ggplot(C4_hull_areas_combine%>%filter(HR_size == 2.6), 
       aes(x = perc_ret, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)

#plot all

transect.labs <- c('Low-', 'Mid-', 'High-elevation')
names(transect.labs) <- c("lowele", "midele", "highele")

HR.labs <- c('Feeding area = 1 ha', '2.6 ha', '4 ha', '7 ha')
names(HR.labs) <- c('1', '2.6', '4', '7')

#feed-area ratio

ggplot(C4_hull_areas_combine, 
       aes(x = perc_ret, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(transect~HR_size,
             labeller = labeller(transect = transect.labs,
                                 HR_size = HR.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'Feed area ratio [%]')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()

#HR area

ggplot(C4_hull_areas_combine, 
       aes(x = perc_ret, y = area, color = as.factor(ret_strat)))+
  geom_hline(yintercept=20, color = 'blue', size = 1)+
  geom_boxplot()+
  facet_grid(transect~HR_size,
             labeller = labeller(transect = transect.labs,
                                 HR_size = HR.labs))+
  labs(color = 'Retention \nstrategy', x = '% retention', y = 'Home range area')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()
  

C4_hull_areas_combine_save<-C4_hull_areas_combine%>%select(1:3, 7:13)

write.csv(C4_hull_areas_combine_save, 'outputs/hr/hr_data_hulls.csv', row.names = F)

a<-C4_hull_areas_combine%>%
  select(plot, ID, area, feed_ha, HR_size, ret_strat, perc_ret, feeding_total, feed_area_ratio)


# stats -------------------------------------------------------------------

#test sign

summary(glm(feeding_total~as.integer(perc_ret), data = C4_hull_areas_combine))

summary(glm(feed_area_ratio~as.numeric(perc_ret)+as.numeric(ret_strat), data = C4_hull_areas_combine))

summary(glm(feed_area_ratio~ret_strat, data = C4_hull_areas_combine))

summary(glm(HRU~ret_strat, data = C4_hr_areas))
summary(glm(area~ret_strat, data = C4_hull_areas_combine))
summary(glm(feed_ha~ret_strat, data = C4_hull_areas_combine))

#seperate models

C4_glm_agg<-C4_hull_areas_combine%>%filter(ret_strat == 'agg')
C4_glm_disp<-C4_hull_areas_combine%>%filter(ret_strat == 'disp')

summary(glm(area~as.integer(perc_ret), data = C4_glm_agg))
summary(glm(area~as.integer(perc_ret), data = C4_glm_disp))

plot(allEffects(glm(area~perc_ret, data = C4_glm_agg)), ylim = c(0,45))
plot(allEffects(glm(area~perc_ret, data = C4_glm_disp)), ylim = c(0,45))

plot(allEffects(glm(feed_area_ratio~perc_ret, data = C4_glm_agg)), ylim = c(0, 55))
plot(allEffects(glm(feed_area_ratio~perc_ret, data = C4_glm_disp)), ylim = c(0, 55))

#ggplot viz

C4_glm_aggdisp<-C4_hull_areas_combine%>%filter(ret_strat %in% c('agg', 'disp'))
C4_glm_aggdisp_HRU<-C4_hr_areas%>%filter(ret_strat %in% c('agg', 'disp'))


ggplot(C4_glm_aggdisp, aes(perc_ret, feed_area_ratio, color = ret_strat))+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'Feed area ratio (%)')+
  theme_bw()

ggplot(C4_glm_aggdisp, aes(perc_ret, area, color = ret_strat))+
  geom_hline(yintercept=20, color = 'blue', size = 1)+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'Home range area (ha)')+
  theme_bw()

ggplot(C4_glm_aggdisp_HRU, aes(perc_ret, HRU, color = ret_strat))+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'No. of home ranges')+
  theme_bw()

#test cv

#make wide table

C4_ret_wide<-C4_hull_areas_combine%>%pivot_wider(names_from = ret_strat, values_from = c(feed_area_ratio, area))

cv(C4_ret_wide$area_control, na.rm = T)
cv(C4_ret_wide$area_disp, na.rm = T)
cv(C4_ret_wide$area_agg, na.rm = T)

t.test(C4_ret_wide$area_disp, C4_ret_wide$area_agg, na.rm = T)

# code from above but manual (for reference) ----------------------------------------------


C4_hr_areas_26<-bind_rows(get_listdata(C4_hr_26, 'data')%>%mutate(ret_strat = 'control', perc_ret = '100%'),
                      get_listdata(C4_hr_disp_50$`2.6`, 'data')%>%mutate(ret_strat = 'disp', perc_ret = '50%'),
                      get_listdata(C4_hr_disp_40$`2.6`, 'data')%>%mutate(ret_strat = 'disp', perc_ret = '40%'),
                      get_listdata(C4_hr_disp_30$`2.6`, 'data')%>%mutate(ret_strat = 'disp', perc_ret = '30%'),
                      get_listdata(C4_hr_disp_20$`2.6`, 'data')%>%mutate(ret_strat = 'disp', perc_ret = '20%'),
                      get_listdata(C4_hr_disp_10$`2.6`, 'data')%>%mutate(ret_strat = 'disp', perc_ret = '10%'),
                      get_listdata(C4_hr_agg_50$`2.6`, 'data')%>%mutate(ret_strat = 'agg', perc_ret = '50%'),
                      get_listdata(C4_hr_agg_40$`2.6`, 'data')%>%mutate(ret_strat = 'agg', perc_ret = '40%'),
                      get_listdata(C4_hr_agg_30$`2.6`, 'data')%>%mutate(ret_strat = 'agg', perc_ret = '30%'),
                      get_listdata(C4_hr_agg_20$`2.6`, 'data')%>%mutate(ret_strat = 'agg', perc_ret = '20%'),
                      get_listdata(C4_hr_agg_10$`2.6`, 'data')%>%mutate(ret_strat = 'agg', perc_ret = '10%'))%>%
  mutate(transect = case_when(grepl('T1', plot) ~ 'lowele',
                              grepl('T2', plot) ~ 'midele',
                              grepl('T3', plot) ~ 'highele'),
         transect = factor(transect, levels = c('lowele', 'midele', 'highele')),
         perc_ret = factor(perc_ret, levels = c('100%', '50%', '40%', '30%', '20%', '10%')),
         ret_strat = factor(ret_strat, levels = c('control', 'agg', 'disp')))

ggplot(C4_hr_areas_26, aes(x = perc_ret, y = HRU, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)

#get no of hulls, hull areas and fraction of feeding habitat

C4_hull_area_26<-bind_rows(get_listdata(C4_hr_26, 'hulls')%>%mutate(ret_strat = 'control', perc_ret = '100%'),
                           get_listdata(C4_hr_disp_50$`2.6`, 'hulls')%>%mutate(ret_strat = 'disp', perc_ret = '50%'),
                           get_listdata(C4_hr_disp_40$`2.6`, 'hulls')%>%mutate(ret_strat = 'disp', perc_ret = '40%'),
                           get_listdata(C4_hr_disp_30$`2.6`, 'hulls')%>%mutate(ret_strat = 'disp', perc_ret = '30%'),
                           get_listdata(C4_hr_disp_20$`2.6`, 'hulls')%>%mutate(ret_strat = 'disp', perc_ret = '20%'),
                           get_listdata(C4_hr_disp_10$`2.6`, 'hulls')%>%mutate(ret_strat = 'disp', perc_ret = '10%'),
                           get_listdata(C4_hr_agg_50$`2.6`, 'hulls')%>%mutate(ret_strat = 'agg', perc_ret = '50%'),
                           get_listdata(C4_hr_agg_40$`2.6`, 'hulls')%>%mutate(ret_strat = 'agg', perc_ret = '40%'),
                           get_listdata(C4_hr_agg_30$`2.6`, 'hulls')%>%mutate(ret_strat = 'agg', perc_ret = '30%'),
                           get_listdata(C4_hr_agg_20$`2.6`, 'hulls')%>%mutate(ret_strat = 'agg', perc_ret = '20%'),
                           get_listdata(C4_hr_agg_10$`2.6`, 'hulls')%>%mutate(ret_strat = 'agg', perc_ret = '10%'))%>%
  st_drop_geometry()%>%
  select(plot, ID, area, feed_ha, HR_size, ret_strat, perc_ret)%>%
  mutate(transect = case_when(grepl('T1', plot) ~ 'lowele',
                              grepl('T2', plot) ~ 'midele',
                              grepl('T3', plot) ~ 'highele'),
         transect = factor(transect, levels = c('lowele', 'midele', 'highele')),
         perc_ret = factor(perc_ret, levels = c('100%', '50%', '40%', '30%', '20%', '10%')),
         ret_strat = factor(ret_strat, levels = c('control', 'agg', 'disp')))


C4_hull_area_26<-right_join(C4_hull_area_26, 
                            C4_hr_areas_26%>%group_by(plot)%>%summarise(feeding_total = max(areaha)), 
                            by = 'plot')


C4_hull_area_stats_26<-C4_hull_area_26%>%
  group_by(plot, transect, ret_strat, perc_ret)%>%
  summarise(n_homeranges = max(ID),
            mean_HR_size = mean(area),
            mean_feed_size = mean(feed_ha),
            total_feed_area = max(feeding_total))


ggplot(C4_hull_area_stats_26, aes(x = perc_ret, y = mean_HR_size, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)

ggplot(C4_hull_area_26, aes(x = perc_ret, y = area, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)


ggplot(C4_hull_area_stats_26, aes(x = perc_ret, y = mean_feed_size, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)

#calculate ratio of HR size and feed size

C4_hull_area_26<-C4_hull_area_26%>%mutate(feed_area_ratio = (feed_ha/area)*100)

ggplot(C4_hull_area_26, aes(x = perc_ret, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)


# figures -----------------------------------------------------------------

#Actual plot

raster_df_100<-C3_NLM$T35P1%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

a<-ggplot()+
  geom_raster(data = raster_df_100, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                    labels = c('>1% N/DM', 'non-habitat', '<1% N/DM', 'climate'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75))+
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.position = "none")

#climate mask

raster_df_disturbance<-C4_logmasks_agg_manual_all$`40%`%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

b1<-ggplot()+
  geom_raster(data = raster_df_disturbance, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#FDE725FF', '#FFFFFF'),
                    labels = c('Disturbed', 'Undisturbed'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75))+
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.position = "none")

raster_df_disturbance2<-C4_logmasks_disp_all$`40%`%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

b2<-ggplot()+
  geom_raster(data = raster_df_disturbance2, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#FDE725FF', '#FFFFFF'),
                    labels = c('Disturbed', 'Undisturbed'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75))+
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.position = "none")

#masked map

c1_data<-optimal_hr_figures(C4_NLMs_log_agg_40$T35P1, hr = 2.6)

c1<-c1_data$plot+theme(axis.text.x = element_blank(),
                       axis.text.y = element_blank())

c2_data<-optimal_hr_figures(C4_NLMs_log_disp_40$T35P1, hr = 2.6)

c2<-c2_data$plot+theme(axis.text.y = element_blank())+
  scale_x_continuous(expand = c(0, 0), breaks = c(25,50,75))

a+(b1+c1)/(b2+c2) + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')


ggsave('100_ha_dist.svg',path = 'figures', device = 'svg',
       width = 35, height = 15, units = 'cm', dpi = 600)

