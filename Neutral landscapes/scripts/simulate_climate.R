# get spatial aggregation of climate per elev. gradient -------------------

library(raster)
library(sf)
library(landscapemetrics)
library(landscapetools)
library(NLMR)
library(tidyverse)

proj_utm<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'


# crop prediction raster based on transect elev. --------------------------

EG_habitat<-raster('data/C1 data/EG_pres_abs.tif')
EG_habitat_pred<-raster('data/C1 data/EG_full.tif')


DEM<-raster('data/C1 data/DEM_VIC.tif')%>%
  #projectRaster(crs = crs(EG_habitat), method = 'ngb', progress = 'text')

EG_mask<-st_read('data/shapes/East Gippsland.shp')%>%st_transform(crs = crs(EG_habitat))

DEM_eg<-crop(DEM, EG_mask)
DEM_eg<-mask(DEM_eg, EG_mask)

#crop by plot network extent

plots<-st_read('data/shapes/plots.shp')%>%st_transform(crs = crs(EG_habitat))

EG_habitat_plots<-crop(EG_habitat, plots)
EG_dem_plots<-crop(DEM_eg, plots)
EG_dem_plots_re<-resample(EG_dem_plots, EG_habitat_plots, progress = 'text')

plots$ele<-raster::extract(DEM_eg, plots)
plots<-plots%>%mutate(ele = raster::extract(DEM_eg, plots), transect = strtrim(waypoint, 2),)

plots%>%st_drop_geometry()%>%group_by(transect)%>%summarise(min = min(ele),
                                                                     max = max(ele))

#reclassify by transect

DEM_class<-reclassify(EG_dem_plots_re, c(0, 400, 1,
                                         400, 900, 2,
                                         900, Inf, 3))

DEM_class_all<-reclassify(DEM_eg, c(0, 400, 1,
                                         400, 900, 2,
                                         900, Inf, 3))

#separate prediction into three layers

EG_habitat_low<-mask(EG_habitat_plots, DEM_class, maskvalue=1, inverse = T)
EG_habitat_mid<-mask(EG_habitat_plots, DEM_class, maskvalue=2, inverse = T)
EG_habitat_high<-mask(EG_habitat_plots, DEM_class, maskvalue=3, inverse = T)

#for all

EG_dem_re<-resample(DEM_class_all, EG_habitat, progress = 'text')

EG_habitat_low_EG<-mask(EG_habitat, EG_dem_re, maskvalue=1, inverse = T)
EG_habitat_mid_EG<-mask(EG_habitat, EG_dem_re, maskvalue=2, inverse = T)
EG_habitat_high_EG<-mask(EG_habitat, EG_dem_re, maskvalue=3, inverse = T)


# get spatial aggregation of clim. suitable habitat per transect ----------

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

rasters<-list(EG_habitat_low, EG_habitat_mid, EG_habitat_high)

names(rasters)<-c('low', 'mid', 'high')

aggreg_all<-map(rasters, raster_stats)

aggreg_all_df<-bind_rows(aggreg_all, .id = 'ele')


# simulate based on metrics -----------------------------------------------

dreisatz<-function(new, total, respective){
  
  factor<-respective/total
  new*factor
  
}

#trials

lowlands_land<-calculate_lsm(EG_habitat_low, what = c('lsm_l_ai', 'lsm_l_np'),
                             directions = 8)

mid_land<-calculate_lsm(EG_habitat_mid, what = c('lsm_l_ai', 'lsm_l_np'),
                             directions = 8)

high_land<-calculate_lsm(EG_habitat_high, what = c('lsm_l_ai', 'lsm_l_np'),
                        directions = 8)


climate_low<-nlm_randomcluster(50,50, resolution = 1,
                               p = dreisatz(91.9,100,40)/100, ai = c(0.92,0.08),
                               neighbourhood = 8)

climate_med<-nlm_randomcluster(50,50, resolution = 1,
                               p = dreisatz(88.2,100,40)/100, ai = c(0.87,0.13),
                               neighbourhood = 8)


climate_high<-nlm_randomcluster(50,50, resolution = 1,
                               p = dreisatz(85.3,100,40)/100, ai = c(0.28,0.72),
                               neighbourhood = 8)

#for 100ha

climate_low_100<-nlm_randomcluster(100,100, resolution = 1,
                               p = (dreisatz(67,100,40)/100)/2, ai = c(0.92,0.08),
                               neighbourhood = 8)


climate_med_100<-nlm_randomcluster(100,100, resolution = 1,
                               p = (dreisatz(76.5,100,40)/100)/2, ai = c(0.87,0.13),
                               neighbourhood = 8)


climate_high_100<-nlm_randomcluster(100,100, resolution = 1,
                                p = (dreisatz(82.3,100,40)/100)/2, ai = c(0.28,0.72),
                                neighbourhood = 8)



# combine with nutrition layers -------------------------------------------

#highlands

plot(highland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

plot(climate_high_100)

highlands_clim_nut<-mask(highland_random_large, climate_high_100, maskvalue=1, inverse = T)

highlands_clim_nut[is.na(highlands_clim_nut[])] <- 1.5 

plot(highlands_clim_nut, col = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'))

highlands_climate<-optimal_hr(highlands_clim_nut, 10, 2.6)

#lowlands

plot(lowland_random_large, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

plot(climate_low_100)

lowlands_clim_nut<-mask(lowland_random_large, climate_low_100, maskvalue=1, inverse = T)

lowlands_clim_nut[is.na(lowlands_clim_nut[])] <- 1.5 

plot(lowlands_clim_nut, col = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'))

lowlands_climate<-optimal_hr(lowlands_clim_nut, 10, 2.6)


# check climate suitability around plots ----------------------------------

#1000 ha - highlands

#dist for square buffer to reach 1000ha area - 1581.15

highland_plot<-plots%>%
  filter(waypoint == 'T3.5P1')%>%
  st_transform(crs = proj_utm)%>%
  st_buffer(dist = 1581.15, endCapStyle = 'SQUARE')%>%st_transform(crs = crs(EG_habitat))

#EG_habitat_high_re<-projectRaster(EG_habitat_high, crs = proj_utm)

plot(EG_habitat)
plot(highland_plot$geometry,add = T)

highland_rep<-crop(EG_habitat, highland_plot)

highland_rep[is.na(highland_rep[])] <- 0 

plot(highland_rep)

#for range

highland_rep_range<-crop(EG_habitat_pred, highland_plot)

plot(highland_rep_range)
plot(highland_plot$geometry,add = T)

# lowlands

lowland_plot<-plots%>%
  filter(waypoint == 'T1.5P5')%>%
  st_transform(crs = proj_utm)%>%
  st_buffer(dist = 1581.15, endCapStyle = 'SQUARE')%>%st_transform(crs = crs(EG_habitat))

plot(EG_habitat)
plot(lowland_plot$geometry,add = T)

lowland_rep<-crop(EG_habitat, lowland_plot)

lowland_rep[is.na(lowland_rep[])] <- 0 

plot(lowland_rep)

#for range

lowland_rep_range<-crop(EG_habitat_pred, lowland_plot)

plot(lowland_rep_range)
plot(lowland_plot$geometry,add = T)


#test with 0.25 climatic suitability

EG_habitat_range<-raster('data/C1 data/EG_full.tif')

EG_habitat_range_class<-reclassify(EG_habitat_range, c(-Inf,0.25, 0, 0.25, Inf , 1))

lowland_rep_25<-crop(EG_habitat_range_class, lowland_plot)

lowland_rep_25[is.na(lowland_rep_25[])] <- 0 

plot(lowland_rep_25)

# mid-elev.

mid_plot<-plots%>%
  filter(waypoint == 'T2P3')%>%
  st_transform(crs = proj_utm)%>%
  st_buffer(dist = 1581.15, endCapStyle = 'SQUARE')%>%st_transform(crs = crs(EG_habitat))

plot(EG_habitat)
plot(mid_plot$geometry,add = T)

mid_rep<-crop(EG_habitat, mid_plot)

mid_rep[is.na(mid_rep[])] <- 0 

plot(mid_rep)

#for range

mid_rep_range<-crop(EG_habitat_pred, mid_plot)

plot(mid_rep_range)
plot(mid_plot$geometry,add = T)

# with 0.25 clim suit

mid_rep_25<-crop(EG_habitat_range_class, mid_plot)

mid_rep_25[is.na(mid_rep_25[])] <- 0 

plot(mid_rep_25)


# 1000 ha nlms ---------------------------------------------------

#nitrogen

highland_random_1000<-nlm_randomcluster(32, 32, resolution = 100,
                                         p = (dreisatz(88.9,100,40)/100)/4, ai = c(0.65,0.27,0.09),
                                         neighbourhood = 8)

plot(highland_random_1000, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))

#climate

high_land_rep<-calculate_lsm(highland_rep, what = c('lsm_l_ai', 'lsm_l_np'),
                         directions = 8)

raster_stats(highland_rep)

highland_climate_1000<-nlm_randomcluster(32, 32, resolution = 100,
                                        p = 0.4, ai = c(0.1,0.9),
                                        neighbourhood = 8)

plot(highland_climate_1000)


#combine

highlands_clim_nut_1000<-mask(highland_random_1000, highland_climate_1000, maskvalue=1, inverse = T)

highlands_clim_nut_1000[is.na(highlands_clim_nut_1000[])] <- 1.5 

plot(highlands_clim_nut_1000, col = c('#29AF7FFF', '#FDE725FF', '#440154FF', '#E66101'))

# 3 high ele + 1 low ele --------------------------------------------------

lowland_random_1000<-nlm_randomcluster(32,32, resolution = 100,
                                        p = (dreisatz(88.85,100,40)/100)/3, ai = c(0.3,0.58,0.12),
                                        neighbourhood = 8)

highland_random_1000<-nlm_randomcluster(32, 32, resolution = 100,
                                        p = (dreisatz(88.9,100,40)/100)/3, ai = c(0.65,0.27,0.09),
                                        neighbourhood = 8)

plot(lowland_random_1000, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))
plot(highland_random_1000, col = c('#29AF7FFF', '#FDE725FF', '#440154FF'))


#check fracs

lowland_random_1000%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()
highland_random_1000%>%as.data.frame(xy = T, na.rm = T)%>%group_by(clumps)%>%tally()

