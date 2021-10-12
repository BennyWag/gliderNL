
# climate analysis (1000ha) -----------------------------------------------

library(raster)
library(RColorBrewer)
library(tidyverse)
library(patchwork)

climate_analysis<-read.csv('data/climate_data_areas.csv')%>%
  mutate(perc = factor(perc, levels = c('100%', '90%','80%','70%','60%',
                                        '50%', '40%', '30%', '20%', '10%')))

# initial plots -----------------------------------------------------------

control_col<-brewer.pal(n = 1, name = "Dark2")

ggplot(climate_analysis, 
       aes(x = perc, y = density))+
  geom_boxplot(fill = control_col[1])+
  labs(x = '% area climatically suitable', y = 'Animals/ha')+

  theme_bw()

ggplot(climate_analysis, 
       aes(x = perc, y = areaha))+
  geom_boxplot()+
  labs(x = '% area', y = 'Accessible feeding area')+
  theme_bw()

ggplot(climate_analysis, 
       aes(x = perc, y = HRU))+
  geom_boxplot()+
  labs(x = '% area', y = 'Accessible feeding area')+
  theme_bw()

ggplot(climate_analysis, 
       aes(x = perc, y = density))+
  stat_summary(geom = 'line', lwd = 1, aes(group = 1))+
  stat_summary(geom = 'point', size = 2)+
  stat_summary(geom = 'errorbar', width = 0.8)+
  labs(x = '% area', y = 'Animals/ha')+
  theme_bw()

ggplot(climate_analysis, 
       aes(x = perc, y = density, color = plot))+
  geom_point()+
  geom_smooth(method = 'lm', aes(x = as.integer(perc), y = density))+
  labs(x = '% area', y = 'Animals/ha')+
  theme_bw()


ggplot(climate_analysis, 
       aes(x = perc, y = ed))+
  geom_boxplot()+
  labs(x = '% area', y = 'Edge density')+
  theme_bw()

ggplot(climate_analysis, 
       aes(x = perc, y = area_mn))+
  geom_boxplot()+
  labs(x = '% area', y = 'Mean patch area')+
  ylim(0,400)+
  theme_bw()

ggplot(climate_analysis, 
       aes(x = perc, y = clumpy))+
  geom_boxplot()+
  labs(x = '% area', y = 'Patch aggregation')+
  theme_bw()

ggplot(climate_analysis, 
       aes(x = perc, y = np))+
  geom_boxplot()+
  labs(x = '% area', y = 'Patch number')+
  theme_bw()


# summarry stats ----------------------------------------------------------


climate_stats<-climate_analysis%>%group_by(perc)%>%
  summarise_at(c('areaha', 'HRU', 'density'), c(mean = mean,
                                                min = min,
                                                max = max,
                                                med = median, 
                                                sd = sd))



# figures -----------------------------------------------------------------


ggplot(climate_analysis, aes(x = perc, y = density))+
  geom_boxplot(width = 0.5,  lwd = 1, fill = control_col[1])+
  labs(x = 'Fraction of total area climatically suitable', y = 'Animals per hectare')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

#dir.create('figures')

ggsave('climate_animalsha.svg',path = 'figures/', device = 'svg', width = 25, height = 15.6, units = 'cm', dpi = 600)

a<-ggplot(climate_analysis, aes(x = perc, y = ed))+
  geom_boxplot(width = 0.5,  lwd = 1, fill = control_col[1])+
  labs(x = NULL, y = 'Edge density [%]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

b<-ggplot(climate_analysis, aes(x = perc, y = np))+
  geom_boxplot(width = 0.5,  lwd = 1, fill = control_col[1])+
  labs(x = NULL, y = 'Number of patches')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

c<-ggplot(climate_analysis, aes(x = perc, y = clumpy*100))+
  geom_boxplot(width = 0.5,  lwd = 1, fill = control_col[1])+
  labs(x = 'Fraction of total area climatically suitable', y = 'Patch aggregation [%]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

a / b / c + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('climate_landscapemetrics.svg', 
       path = 'figures/', device = 'svg', width = 25, height = 25, units = 'cm', dpi = 600)
