
# 1000 ha climate analysis ------------------------------------------------

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
library(gt)
library(tidyverse)

#funs

dreisatz<-function(new, total, respective){
  
  factor<-respective/total
  new*factor
  
}

simulate_landscape_1000 = function(p, ai, neigh = 8){
  
  pb$tick()$print()
  
  nlm_randomcluster(ncol = 32, nrow = 32, 
                    resolution = 100,
                    p = p, 
                    ai = ai,
                    neighbourhood = neigh)
  
}

raster_stats_climate<-function(class_raster, res = 100, hr = 2.6){
  
  a<-reclassify(class_raster, c(0,0.25,0,0.25,0.75,1,0.75,1,2,1,Inf,3))
  
  rast_df<-a%>%as.data.frame(xy = T, na.rm = T)%>%rename('layer' = 3)
  
  rast_df$ID<-1
  
  res<-res
  
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(area = sum(ID), areaha = (area*res^2)/10000)%>%
    mutate(sumA = sum(area), per = 100*area/sumA)%>%rename(class = layer)
  
  rast_area_large_pres<-rast_area%>%
    filter(class == 0)%>%
    mutate(HRU = areaha/hr,
           HR_size = hr)
  
  metrics<-calculate_lsm(a, what = c("lsm_c_np", "lsm_c_area_mn", 'lsm_c_clumpy', "lsm_c_ed", "lsm_c_ca"),
                         directions = 8)
  
  metrics_wide<-pivot_wider(metrics, names_from = c('metric'), values_from = c('value'))%>%select(3, 5:9)
  
  
  rast_all<-inner_join(rast_area, metrics_wide, by = 'class')%>%mutate(e_a_ratio = ed/ca)
  
  rast_area_large_pres<-rast_all%>%
    filter(class == 0)%>%
    mutate(HRU = areaha/2.6,
           HR_size = 2.6)
  
  list<-list(rast_all, rast_area_large_pres, a)
  names(list)<-c('stas_all', 'stats_feeding', 'reclass_rast')
  
  return(list)
  
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

optimal_hr_1000<-function(raster, res = 10, hr){
  
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
  
  hull_all$area<-st_area(hull_all)/10000
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

# generate 1000 ha areas for highlands ------------------------------------

#get data

climate_fracs<-readRDS('outputs/hr/C3_fractions.rds')

climate_fracs<-climate_fracs[21:30]

climate_agg<-read.csv('outputs/hr/C3_aggregation.csv')%>%
  filter(str_detect(plot, 'T3'))%>%
  mutate(agg_1000 = (dreisatz(agg,100,40)/100)/3)

plotnames_clim<-names(climate_fracs)


#make a table for paper

climate_fracs_df<-climate_fracs%>%bind_rows()%>%
  rownames_to_column()%>%
  pivot_longer(-rowname)%>%
  pivot_wider(names_from=rowname, values_from=value)%>%
  rename('plot' = 1, 
         '% Feeding habitat' = 2,
         '% Not habitat' = 3,
         '% Non-feeding habitat' = 4)%>%
  mutate(transect = case_when(grepl('T1', plot) ~ 'Low elevation',
                              grepl('T2', plot) ~ 'Mid-elevation',
                              grepl('T3', plot) ~ 'High elevation'))


climate_agg_table<-read.csv('outputs/hr/C3_aggregation.csv')

agg_table<-cbind(climate_fracs_df, climate_agg_table)%>%select(-6)%>%select(Elevation = transect,
                                                                            Site = plot,
                                                                            2:4,
                                                                            'Aggregation (%)' = agg)

agg_gt<-agg_table%>%gt()%>%
  fmt_number(columns = 3:6, decimals = 2)

#dir.create('tables')

gtsave(agg_gt, 'tables/frac_agg.rtf')

#generate landscapes

pb <- progress_estimated(length(climate_fracs))
climate_NLM<-map2(climate_agg$agg_1000, climate_fracs, simulate_landscape_1000)

names(climate_NLM)<-plotnames_clim

plot(climate_NLM$T3P4, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

saveRDS(climate_NLM, 'outputs/hr/1000ha_NLMs.rds')

climate_NLM<-readRDS('outputs/hr/1000ha_NLMs.rds')

# generate climate masks --------------------------------------------------

climate_mask_param <- expand.grid(p = c(0.58),
                                     ai = list(c(0.1, 0.9), 
                                               c(0.2, 0.8),
                                               c(0.3, 0.7),
                                               c(0.4, 0.6),
                                               c(0.5, 0.5), 
                                               c(0.6, 0.4),
                                               c(0.7, 0.3),
                                               c(0.8, 0.2),
                                               c(0.9, 0.1)))%>%as_tibble()

climate_mask_param2<-rbind(climate_mask_param,climate_mask_param)

pb <- progress_estimated(nrow(climate_mask_param2))
climate_masks<-map2(climate_mask_param2$p, climate_mask_param2$ai, simulate_landscape_1000, neigh = 4)

pb <- progress_estimated(nrow(climate_mask_param2))
climate_masks2<-map2(climate_mask_param2$p, climate_mask_param2$ai, simulate_landscape_1000, neigh = 4)


#check

climate_mask_check<-list()

for (i in seq_along(climate_masks)){
  
  a<-climate_masks[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  climate_mask_check[[length(climate_mask_check)+1]] <- a
  
}

climate_mask_check2<-list()

for (i in seq_along(climate_masks2)){
  
  a<-climate_masks2[[i]]%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
  
  climate_mask_check2[[length(climate_mask_check2)+1]] <- a
  
}

#combine

climate_masks_fix<-list(climate_masks[[17]],#10:90
                        raster.invert(climate_masks[[3]]),#20:80
                        raster.invert(climate_masks2[[5]]), #30:70
                        climate_masks[[6]], #40:60
                        climate_masks[[9]], #50:50
                        raster.invert(climate_masks2[[14]]),#60:40
                        climate_masks2[[5]], #70:30
                        climate_masks[[3]],#80:20
                        climate_masks2[[13]]) #90:10


names(climate_masks_fix)<-c('10%', '20%', '30%', '40%','50%', '60%', '70%', '80%', '90%')

saveRDS(climate_masks_fix, 'outputs/hr/climmasks_1000ha.rds')

# calculate initial stats -------------------------------------------------

climate_stats_initial<-map(climate_NLM, raster_stats_climate)

#bind

climate_feeding_control<-get_listdata(climate_stats_initial, 'stats_feeding')

# apply disturbance masks -------------------------------------------------

#test

plot(climate_NLM$T35P1, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(climate_masks_fix$`10%`)

a<-log_forest(climate_NLM$T35P1, climate_masks_fix$`10%`, updateval = 1.5, inv = F)

plot(a, col = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'))

raster_stats_climate(climate_NLM$T35P1)
raster_stats_climate(a)

#all

climate_NLM_90<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`90%`, inv = F, updateval = 1.5)
climate_NLM_80<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`80%`, inv = F, updateval = 1.5)
climate_NLM_70<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`70%`, inv = F, updateval = 1.5)
climate_NLM_60<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`60%`, inv = F, updateval = 1.5)
climate_NLM_50<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`50%`, inv = F, updateval = 1.5)
climate_NLM_40<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`40%`, inv = F, updateval = 1.5)
climate_NLM_30<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`30%`, inv = F, updateval = 1.5)
climate_NLM_20<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`20%`, inv = F, updateval = 1.5)
climate_NLM_10<-map(climate_NLM, log_forest, mask_rast = climate_masks_fix$`10%`, inv = F, updateval = 1.5)

#summarise

climate_NLM_90_data<-map(climate_NLM_90, raster_stats_climate)
climate_NLM_80_data<-map(climate_NLM_80, raster_stats_climate)
climate_NLM_70_data<-map(climate_NLM_70, raster_stats_climate)
climate_NLM_60_data<-map(climate_NLM_60, raster_stats_climate)
climate_NLM_50_data<-map(climate_NLM_50, raster_stats_climate)
climate_NLM_40_data<-map(climate_NLM_40, raster_stats_climate)
climate_NLM_30_data<-map(climate_NLM_30, raster_stats_climate)
climate_NLM_20_data<-map(climate_NLM_20, raster_stats_climate)
climate_NLM_10_data<-map(climate_NLM_10, raster_stats_climate)

# get data ----------------------------------------------------------------

climate_area<-bind_rows(get_listdata(climate_stats_initial, 'stats_feeding', id = 'plot')%>%mutate(perc = '100%'),
                        get_listdata(climate_NLM_10_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '90%'),
                        get_listdata(climate_NLM_20_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '80%'),
                        get_listdata(climate_NLM_30_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '70%'),
                        get_listdata(climate_NLM_40_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '60%'),
                        get_listdata(climate_NLM_50_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '50%'),
                        get_listdata(climate_NLM_60_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '40%'),
                        get_listdata(climate_NLM_70_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '30%'),
                        get_listdata(climate_NLM_80_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '20%'),
                        get_listdata(climate_NLM_90_data, 'stats_feeding', id = 'plot')%>%mutate(perc = '10%'))%>%
  mutate(perc = factor(perc, levels = c('100%', '90%','80%','70%','60%',
                                        '50%', '40%', '30%', '20%', '10%')),
         density = HRU/sumA)

write.csv(climate_area, 'outputs/hr/climate_data_areas.csv', row.names = F)

ggplot(climate_area, 
       aes(x = perc, y = density))+
  geom_boxplot()+
  labs(x = '% area climatically suitable', y = 'Animals/ha')+
  theme_bw()

ggplot(climate_area, 
       aes(x = perc, y = areaha))+
  geom_boxplot()+
  labs(x = '% area', y = 'Accessible feeding area')+
  theme_bw()

ggplot(climate_area, 
       aes(x = perc, y = density))+
  stat_summary(geom = 'line', lwd = 1, aes(group = 1))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  labs(x = '% area', y = 'Animals/ha')+
  theme_bw()

ggplot(climate_area, 
       aes(x = perc, y = density, color = plot))+
  geom_point()+
  geom_smooth(method = 'lm', aes(x = as.integer(perc), y = density))+
  labs(x = '% area', y = 'Animals/ha')+
  theme_bw()


ggplot(climate_area, 
       aes(x = perc, y = ed))+
  geom_boxplot()+
  labs(x = '% area', y = 'Edge density')+
  theme_bw()

ggplot(climate_area, 
       aes(x = perc, y = area_mn))+
  geom_boxplot()+
  labs(x = '% area', y = 'Mean patch area')+
  ylim(0,400)+
  theme_bw()

ggplot(climate_area, 
       aes(x = perc, y = clumpy))+
  geom_boxplot()+
  labs(x = '% area', y = 'Patch aggregation')+
  theme_bw()


# lowland and mid hills climate area ------------------------------------------

#lowlands

EG_habitat_low%>%as.data.frame(xy = T, na.rm = T)%>%group_by(EG_pres_abs )%>%tally()%>%
  mutate(sum = sum(n), perc = n/sum*100)

EG_habitat_low_EG%>%as.data.frame(xy = T, na.rm = T)%>%group_by(EG_pres_abs )%>%tally()%>%
  mutate(sum = sum(n), perc = n/sum*100)

#midhills

EG_habitat_mid%>%as.data.frame(xy = T, na.rm = T)%>%group_by(EG_pres_abs )%>%tally()%>%
  mutate(sum = sum(n), perc = n/sum*100)

EG_habitat_mid_EG%>%as.data.frame(xy = T, na.rm = T)%>%group_by(EG_pres_abs )%>%tally()%>%
  mutate(sum = sum(n), perc = n/sum*100)

#high country

EG_habitat_high%>%as.data.frame(xy = T, na.rm = T)%>%group_by(EG_pres_abs )%>%tally()%>%
  mutate(sum = sum(n), perc = n/sum*100)

EG_habitat_high_EG%>%as.data.frame(xy = T, na.rm = T)%>%group_by(EG_pres_abs )%>%tally()%>%
  mutate(sum = sum(n), perc = n/sum*100)



# disaggregate test -------------------------------------------------------


climate_NLM$T35P1

disaggregate(climate_NLM$T35P1, fact = 10)

optimal_hr_1000(disaggregate(climate_NLM$T35P1, fact = 10), hr = 2.6)

optimal_hr_1000(disaggregate(climate_NLM_90$T35P1, fact = 10), hr = 2.6)

optimal_hr_1000(disaggregate(climate_NLM_50$T35P1, fact = 10), hr = 2.6)

optimal_hr_1000(disaggregate(climate_NLM_20$T35P1, fact = 10), hr = 2.6)


# figures -----------------------------------------------------------------

#Actual plot

raster_df_plot_sample<-sampleRegular(C3_preds$T35P1, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)%>%mutate(value = as.integer(value))

# raster_df_plot<-C3_preds$T35P1%>%as.data.frame(xy = T, na.rm = T)%>%
#   rename('value' = 3)%>%mutate(value = as.integer(value))

a<-ggplot()+
  geom_raster(data = raster_df_plot_sample, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#440154FF', '#29AF7FFF', '#FDE725FF'),
                    labels = c('<1% N/DM', '>1% N/DM', 'non-habitat'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.position = "none")

#1000 ha neutral landscape

raster_df_1000<-climate_NLM$T35P1%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

b<-ggplot()+
  geom_raster(data = raster_df_1000, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                    labels = c('>1% N/DM', 'non-habitat', '<1% N/DM', 'climate'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.position = "none")

#climate mask

raster_df_climate<-climate_masks_fix$`50%`%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

c<-ggplot()+
  geom_raster(data = raster_df_climate, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#E66101', '#FFFFFF'),
                    labels = c('Unsuitable', 'Suitable'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.position = "none")

#masked map

climate_NLM_50$T35P1

raster_df_climate_masked<-climate_NLM_50$T35P1%>%as.data.frame(xy = T, na.rm = T)%>%
  rename('value' = 3)

d<-ggplot()+
  geom_raster(data = raster_df_climate_masked, aes(x = x, y = y, fill = as.factor(value)))+
  scale_fill_manual(values = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'),
                    labels = c('Feeding \nhabitat', 'Not \nhabitat', 'Non-feeding \nhabitat', 'Climatically \nunsuitable'))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_blank(),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 11, face = 'bold'))

a + b + c + d + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

#dir.create('figures')

ggsave('1000_ha_maps.svg',path = 'figures', width = 30, height = 25, units = 'cm', dpi = 600)
