
# disturbance analysis ----------------------------------------------------

library(raster)
library(viridis)
library(RColorBrewer)
library(gt)
library(tidyverse)
library(patchwork)

## load data ##

hr_data_hulls<-read.csv('data/hr_data_hulls.csv')%>%
  mutate(transect = case_when(grepl('T1', plot) ~ 'lowele',
                              grepl('T2', plot) ~ 'midele',
                              grepl('T3', plot) ~ 'highele'),
         transect = factor(transect, levels = c('lowele', 'midele', 'highele')),
         perc_ret = factor(perc_ret, levels = c('100%', '90%','80%','70%','60%',
                                                '50%', '40%', '30%', '20%', '10%')),
         ret_strat = factor(ret_strat, levels = c('control', 'agg', 'disp')),
         functional = case_when(feed_area_ratio>=25 & area <=6 ~ 0,
                                TRUE ~ 1))

hr_data_areas<-read.csv('data/hr_data_areas.csv')%>%
  mutate(transect = case_when(grepl('T1', plot) ~ 'lowele',
                              grepl('T2', plot) ~ 'midele',
                              grepl('T3', plot) ~ 'highele'),
         transect = factor(transect, levels = c('lowele', 'midele', 'highele')),
         perc_ret = factor(perc_ret, levels = c('100%', '90%','80%','70%','60%',
                                                '50%', '40%', '30%', '20%', '10%')),
         ret_strat = factor(ret_strat, levels = c('control', 'agg', 'disp')))


#functions

get_listdata<-function(list, name, id = 'plot'){
  
  names<-names(list)
  flat<-lapply(list, `[`, c(name))%>%flatten()
  names(flat)<-names
  flat<-flat%>%bind_rows(.id = id)
  return(flat)
  
}

# plotting ----------------------------------------------------------------

transect.labs <- c('Low-', 'Mid-', 'High-elevation')
names(transect.labs) <- c("lowele", "midele", "highele")

HR.labs <- c('Feeding area = 1 ha', '2.6 ha', '4 ha', '7 ha')
names(HR.labs) <- c('1', '2.6', '4', '7')

## number of home-ranges ##

#one size only

ggplot(hr_data_areas%>%filter(HR_size == 2.6), aes(x = perc_ret, y = HRU, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(~transect)

#all HR sizes

ggplot(hr_data_areas, 
       aes(x = perc_ret, y = HRU, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(transect~HR_size,
             labeller = labeller(transect = transect.labs,
                                 HR_size = HR.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'No. of home ranges')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()

## HR area and feed-area ratio ##

#one size only

ggplot(hr_data_hulls%>%filter(HR_size == 2.6), 
       aes(x = perc_ret, y = area, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)

ggplot(hr_data_hulls%>%filter(HR_size == 2.6), 
       aes(x = perc_ret, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_wrap(~transect)

#all HR sizes

#feed-area ratio

ggplot(hr_data_hulls, 
       aes(x = perc_ret, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(transect~HR_size,
             labeller = labeller(transect = transect.labs,
                                 HR_size = HR.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'Feed area ratio [%]')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()

#HR area

ggplot(hr_data_hulls, 
       aes(x = perc_ret, y = area, color = as.factor(ret_strat)))+
  geom_hline(yintercept=20, color = 'blue', size = 1)+
  geom_boxplot()+
  facet_grid(transect~HR_size,
             labeller = labeller(transect = transect.labs,
                                 HR_size = HR.labs))+
  labs(color = 'Retention \nstrategy', x = '% retention', y = 'Home range area')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()

## line graphs ##

#feed area ratio

hr_data_hulls%>%filter(ret_strat %in% c('agg', 'disp'))%>%
ggplot(aes(perc_ret, feed_area_ratio, color = ret_strat))+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'Feed area ratio (%)')+
  theme_bw()

#HR size

hr_data_hulls%>%filter(ret_strat %in% c('agg', 'disp'))%>%
ggplot(aes(perc_ret, area, color = ret_strat))+
  geom_hline(yintercept=20, color = 'blue', size = 1)+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'Home range area (ha)')+
  theme_bw()

#No of HR

hr_data_areas%>%filter(ret_strat %in% c('agg', 'disp'))%>%
ggplot(aes(perc_ret, HRU, color = ret_strat))+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'No. of home ranges')+
  theme_bw()


# stats -------------------------------------------------------------------

summary(glm(feeding_total~as.integer(perc_ret), data = hr_data_hulls))

summary(glm(feed_area_ratio~as.numeric(perc_ret)+as.numeric(ret_strat), data = hr_data_hulls)) #

summary(glm(feed_area_ratio~ret_strat, data = hr_data_hulls)) #

summary(glm(HRU~ret_strat, data = hr_data_areas)) #

summary(glm(area~ret_strat, data = hr_data_hulls)) #

summary(glm(feed_ha~ret_strat, data = hr_data_hulls))

#separate models

summary(glm(area~as.integer(perc_ret), data = hr_data_hulls%>%filter(ret_strat == 'agg')))
summary(glm(area~as.integer(perc_ret), data = hr_data_hulls%>%filter(ret_strat == 'disp')))

## coefficient of variation ##

cv_analysis<-hr_data_hulls%>%
  filter(ret_strat %in% c('agg', 'disp'))%>%
  group_by(HR_size, ret_strat, perc_ret, transect)%>%
  summarise(cv_area = cv(area),
            cv_ratio = cv(feed_area_ratio))

#plot

# HR area

ggplot(hr_data_hulls, aes(perc_ret, area, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  stat_summary(geom = 'errorbar', fun = 'cv', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'CV HR area')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

# Feed area ratio

ggplot(hr_data_hulls, aes(perc_ret, feed_area_ratio, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  stat_summary(geom = 'errorbar', fun = 'cv', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'CV feed-area-ratio')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

#HR number

ggplot(hr_data_areas, aes(perc_ret, HRU, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  stat_summary(geom = 'errorbar', fun = 'cv', width = 0.8)+
  #facet_wrap(~HR_size)+
  facet_grid(transect~HR_size)+
  labs(x = 'Retention (%)', y = 'CV no. of HR')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()


# plots and stats for chapter (2.6ha only) --------------------------------

## plots ## 

#HR number

hr_data_areas%>%filter(HR_size == 2.6)%>%
ggplot(aes(x = perc_ret, y = HRU, fill = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'No. of home ranges')+
  scale_fill_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                    labels = c('Control', 'Aggregated', 'Dispersed'))+
  theme_bw()

hr_data_areas%>%filter(HR_size == 2.6)%>%
  ggplot(aes(perc_ret, HRU, color = ret_strat))+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention (%)', y = 'No. of home ranges')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  
  theme_bw()

hr_data_areas%>%filter(HR_size == 2.6)%>%
ggplot(aes(perc_ret, HRU, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention (%)', y = 'CV no. of HR', color = 'Retention \nstrategy')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

#HR area

hr_data_hulls%>%filter(HR_size == 2.6)%>%
ggplot(aes(x = perc_ret, y = area, fill = as.factor(ret_strat)))+
  geom_hline(yintercept=6, color = 'blue', size = 1)+
  geom_boxplot()+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(x = 'Retention [%]', y = 'Home range area [ha]')+
  scale_fill_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                    labels = c('Control', 'Aggregated', 'Dispersed'))+
  theme_bw()

hr_data_hulls%>%filter(HR_size == 2.6)%>%
  ggplot(aes(perc_ret, area, color = ret_strat))+
  geom_hline(yintercept=20, color = 'blue', size = 1)+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(x = 'Retention [%]', y = 'Home range area [ha]')+
  theme_bw()

hr_data_hulls%>%filter(HR_size == 2.6)%>%
ggplot(aes(perc_ret, area, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(x = 'Retention [%]', y = 'CV HR area')+
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = c(25, 50, 75, 100, 125))+
  theme_bw()

#feed area ratio

hr_data_hulls%>%filter(HR_size == 2.6)%>%
ggplot(aes(x = perc_ret, y = feed_area_ratio, fill = as.factor(ret_strat)))+
  geom_hline(yintercept=25, color = 'blue', size = 1)+
  geom_boxplot()+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(x = 'Retention [%]', y = 'Feed area ratio [%]')+
  scale_fill_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                    labels = c('Control', 'Aggregated', 'Dispersed'))+
  theme_bw()

hr_data_hulls%>%filter(HR_size == 2.6)%>%
  ggplot(aes(perc_ret, feed_area_ratio, color = ret_strat))+
  stat_summary(geom = 'line', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(x = 'Retention [%]', y = 'Feed area ratio [%]')+
  theme_bw()

hr_data_hulls%>%filter(HR_size == 2.6)%>%
ggplot(aes(perc_ret, feed_area_ratio, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(x = 'Retention (%)', y = 'CV feed-area-ratio')+
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = c(25, 50, 75, 100, 125))+
  theme_bw()

#scatter

hr_data_hulls%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_point()+
  geom_smooth(method = 'gam', color = 'black')+
  geom_hline(yintercept=25, color = 'blue', size = 1)+
  geom_vline(xintercept=6, color = 'blue', size = 1)+
  geom_vline(xintercept=18, color = 'red', size = 1)+
  facet_grid(~ret_strat)

hr_data_hulls%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_point()+
  geom_smooth(method ='gam')+
  facet_grid(~perc_ret)+
  theme_bw()


## stats ## 

summary(glm(feed_area_ratio~as.numeric(perc_ret)+as.numeric(ret_strat),
            data = hr_data_hulls%>%filter(HR_size == 2.6))) 

summary(glm(feed_area_ratio~ret_strat,
            data = hr_data_hulls%>%filter(HR_size == 2.6))) 

summary(glm(area~ret_strat,
            data = hr_data_hulls%>%filter(HR_size == 2.6))) 

summary(glm(HRU~ret_strat, 
            data = hr_data_areas%>%filter(HR_size == 2.6)))

summary(glm(as.factor(functional)~ret_strat, family = 'binomial',
            data = hr_data_hulls%>%filter(HR_size == 2.6)))


# functional HRs only -----------------------------------------------------

hr_functional_both<-hr_data_hulls%>%filter(area<=6 & feed_area_ratio>=25)

hr_functional_areas<-hr_functional_both%>%
  mutate(ID = 1)%>%
  group_by(plot, transect, HR_size, ret_strat, perc_ret)%>%
  count(ID, .drop = T)

# No. of HRs

hr_functional_areas%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = perc_ret, y = n, fill = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'No. of functional home ranges')+
  scale_fill_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                     labels = c('Control', 'Aggregated', 'Dispersed'))+
  theme_bw()


#no of func HRs:
sum(hr_functional_areas%>%filter(HR_size == 2.6)%>%ungroup()%>%select(n))

#no af all HRS

sum(hr_data_hulls%>%
  mutate(ID = 1)%>%
  group_by(plot, transect, HR_size, ret_strat, perc_ret)%>%
  count(ID, .drop = T)%>%filter(HR_size == 2.6)%>%ungroup()%>%select(n))
  

#all

hr_functional_areas%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = perc_ret, y = n, color = as.factor(ret_strat)))+
  geom_boxplot()+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'No. of functional home ranges')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()

# Feeding habitat

hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = perc_ret, y = feed_area_ratio, fill = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'Feed area ratio [%]')+
  scale_fill_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                     labels = c('Control', 'Aggregated', 'Dispersed'))+
  theme_bw()

#HR area

hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = perc_ret, y = area, fill = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'Home range area [ha]')+
  scale_fill_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                     labels = c('Control', 'Aggregated', 'Dispersed'))+
  theme_bw()

#scatter

hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_point()+
  geom_smooth(method ='gam')+
  scale_color_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                     labels = c('Control', 'Aggregated', 'Dispersed'))+
  facet_grid(~perc_ret)+
  labs(x = 'Home range area [ha]', y = 'Feed area ratio [%]')+
  theme_bw()

hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_point()+
  geom_smooth(method ='gam')+
  facet_grid(transect~perc_ret)+
  theme_bw()

#summary table

hr_functional_stats<-hr_functional_both%>%
  filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  group_by(perc_ret, ret_strat)%>%
  summarise_at(c('area', 'feed_area_ratio'), c(mean = mean,
                                               range = function(x){max(x)-min(x)},
                                               cv = cv,
                                               med = median, 
                                               sd = sd)) 

sum_tab<-hr_functional_stats%>%select('Area undistrubed' = perc_ret,
                                      'Spatial aggregation' = ret_strat,
                                      'Home range area range' = area_range,
                                      'SD home range area' = area_sd,
                                      'Feed-area-ratio range' = feed_area_ratio_range,
                                      'SD feed-area-ratio' = feed_area_ratio_sd)%>%
  gt()%>%
  fmt_number(columns = 3:6, decimals = 2)

#dir.create('tables')

gtsave(sum_tab, 'tables/dist_summary.rtf')


#CVs

hr_functional_both%>%
  filter(HR_size == 2.6, feed_area_ratio<=100)%>%
ggplot(aes(perc_ret, feed_area_ratio, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                     labels = c('Control', 'Aggregated', 'Dispersed'))+
  labs(x = 'Retention [%]', y = 'CV feed-area-ratio')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

hr_functional_both%>%
  filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(perc_ret, area, color = ret_strat))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = ret_strat, color = ret_strat))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_brewer(palette = 'Dark2', name = 'Retention \nstrategy', 
                     labels = c('Control', 'Aggregated', 'Dispersed'))+
  labs(x = 'Retention [%]', y = 'CV HR area')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()


#scatter slope and intercept

fitted_models <- hr_functional_both%>%
  filter(HR_size == 2.6, feed_area_ratio<=100)%>% 
  group_by(ret_strat, perc_ret)%>%
  do(model = lm(feed_area_ratio ~ area, data = .))%>%
  mutate(name = paste0(ret_strat, '_', perc_ret))

fitted_models_list<-fitted_models$model
names(fitted_models_list)<-fitted_models$name

get_listdata(fitted_models_list, 'coefficients', id = 'id')%>%
  select(id, intercept = 2, slope = 3)%>%
  mutate(start = fitted_models$ret_strat,
         perc = fitted_models$perc_ret)

# trials ------------------------------------------------------------------

#test with limiting total area to min 25 ha feeding

hr_data_hulls%>%filter(area<=6 & feeding_total>=25)%>%
  filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_point()+
  geom_smooth(method ='lm')+
  facet_grid(~perc_ret)+
  theme_bw()

#feeding_total is feeding area before disturbance!!

#color code feeding area

hr_functional_both<-hr_functional_both%>%
  mutate(feeding_code = case_when(feeding_total>=80  ~ '>80%',
                                  feeding_total<=80 & feeding_total>=70 ~ '>70%',
                                  feeding_total<=70 & feeding_total>=60 ~ '>60%',
                                  feeding_total<=60 & feeding_total>=50 ~ '>50%',
                                  feeding_total<=50 & feeding_total>=40 ~ '>40%',
                                  feeding_total<=40 & feeding_total>=30 ~ '>30%',
                                  feeding_total<=30 & feeding_total>=20 ~ '>20%',
                                  TRUE ~ '<20%'))

hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = area, y = feed_area_ratio, shape = as.factor(ret_strat), color = as.factor(feeding_code)))+
  geom_point()+
  facet_grid(~perc_ret)+
  scale_color_viridis(option = 'H', discrete=T)+
  theme_bw()

#calculate remaining feeding area

hr_functional_both_rem<-left_join(hr_functional_both, 
                              hr_functional_both%>%
                                group_by(plot, HR_size, ret_strat, perc_ret)%>%
                                summarise(feed_remaining = sum(feed_ha)),
                              by = c('plot', 'HR_size', 'ret_strat', 'perc_ret'))
  

hr_functional_both_rem%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = perc_ret, y = feed_remaining, color = as.factor(ret_strat)))+
  geom_boxplot()+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = 'Retention \nstrategy', x = 'Retention [%]', y = 'Total feeding habitat remaining [ha]')+
  scale_color_discrete(labels = c("Control", "Aggregated", 'Dispersed')) +
  theme_bw()



# figures -----------------------------------------------------------------

#unfiltered

a<-hr_data_areas%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = perc_ret, y = HRU, fill = as.factor(ret_strat)))+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_brewer(palette = 'Dark2', name = 'Spatial \naggregation', 
                    labels = c('Control', 'Clumped', 'Dispersed'))+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = '', x = 'Area undisturbed [%]', y = 'No. of home ranges')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.background = element_blank(),
        strip.text.y = element_blank())


b<-hr_data_hulls%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = perc_ret, y = area, fill = as.factor(ret_strat)))+
  geom_hline(yintercept=6, color = 'blue', size = 1)+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_brewer(palette = 'Dark2', name = 'Spatial \naggregation', 
                    labels = c('Control', 'Clumped', 'Dispersed'))+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = '', x = 'Area undisturbed [%]', y = 'Home range area [ha]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.background = element_blank(),
        strip.text.y = element_blank())


c<-hr_data_hulls%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = perc_ret, y = feed_area_ratio, fill = as.factor(ret_strat)))+
  geom_hline(yintercept=25, color = 'blue', size = 1)+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_brewer(palette = 'Dark2', name = 'Spatial \naggregation', 
                    labels = c('Control', 'Clumped', 'Dispersed'))+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = '', x = 'Area undisturbed [%]', y = 'Feed area ratio [%]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size = 15, face = 'bold', color = 'black'))

a + b + c + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('dist_raw.svg',path = 'figures/', device = 'svg', width = 45, height = 25, units = 'cm', dpi = 600)


#filtered

hr_functional_areas%>%filter(HR_size == 2.6)%>%
  ggplot(aes(x = perc_ret, y = n, fill = as.factor(ret_strat)))+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_brewer(palette = 'Dark2', name = 'Spatial \naggregation', 
                    labels = c('Control', 'Clumped', 'Dispersed'))+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = '', x = 'Area undisturbed [%]', y = 'No. of functional home ranges')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.position = c(0.9,0.15),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave('dist_nhr_filter.svg', 
       path = 'figures/', device = 'svg', width = 25, height = 20, units = 'cm', dpi = 600)


#HR area

a<-hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = perc_ret, y = area, fill = as.factor(ret_strat)))+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_brewer(palette = 'Dark2', name = 'Spatial \naggregation', 
                    labels = c('Control', 'Clumped', 'Dispersed'))+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = '', x = 'Area undisturbed [%]', y = 'Home range area [ha]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.background = element_blank(),
        strip.text.y = element_blank())


# Feeding habitat

b<-hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = perc_ret, y = feed_area_ratio, fill = as.factor(ret_strat)))+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_brewer(palette = 'Dark2', name = 'Spatial \naggregation', 
                    labels = c('Control', 'Clumped', 'Dispersed'))+
  facet_grid(rows = vars(transect),
             labeller = labeller(transect = transect.labs))+
  labs(color = '', x = 'Area undisturbed [%]', y = 'Feed area ratio [%]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size = 15, face = 'bold', color = 'black'))


a + b + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('dist_filter.svg',path = 'figures/', 
       device = 'svg', width = 35, height = 25, units = 'cm', dpi = 600)

# scatter filtered



hr_functional_both%>%filter(HR_size == 2.6, feed_area_ratio<=100)%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(ret_strat)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method ='gam', lwd = 1)+
  scale_color_brewer(palette = 'Dark2', name = 'Spatial \naggregation', 
                    labels = c('Control', 'Clumped', 'Dispersed'))+
  facet_grid(~perc_ret)+
  labs(x = 'Home range area [ha]', y = 'Feed-area-ratio [%]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        legend.position = c(0.9525,0.2),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave('dist_scatter.svg',path = 'figures/', 
       device = 'svg', width = 45, height = 20, units = 'cm', dpi = 600)
