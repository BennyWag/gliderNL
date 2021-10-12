
# feeding analysis --------------------------------------------------------

library(raster)
library(RColorBrewer)
library(tidyverse)
library(patchwork)

feeding_data_hulls<-read.csv('data/feeding_data_hulls.csv', stringsAsFactors = T)

feeding_data_areas<-read.csv('data/feeding_data_areas.csv')

# number of HR ------------------------------------------------------------

ggplot(feeding_data_areas, aes(reorder(perc, desc(perc)), HRU, color = agg))+
  stat_summary(geom = 'line', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', size = 2)+
  scale_color_discrete(labels = c("Aggregated", 'Dispersed')) +
  labs(x = '% feeding habitat', y = 'No. of home ranges', color = '')+
  theme_bw()

# HR area -----------------------------------------------------------------

ggplot(feeding_data_hulls, aes(x = perc, y = area, color = agg))+
  geom_hline(yintercept=6, color = 'blue', size = 1)+
  geom_boxplot()+
  scale_color_discrete(labels = c("Aggregated", 'Dispersed')) +
  labs(color = '', x = '% feeding habitat', y = 'Home range area [ha]')+
  theme_bw()

ggplot(feeding_data_hulls, aes(perc, area, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_discrete(labels = c("Aggregated", 'Dispersed')) +
  labs(x = '% feeding habitat', y = 'CV HR area')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

# feed area ratio ---------------------------------------------------------

ggplot(feeding_data_hulls, aes(x = perc, y = feed_area_ratio, color = agg))+
  geom_hline(yintercept=25, color = 'blue', size = 1)+
  geom_boxplot()+
  scale_color_discrete(labels = c("Aggregated", 'Dispersed')) +
  labs(color = '', x = '% feeding habitat', y = 'Feed-area-ratio [%]')+
  theme_bw()

ggplot(feeding_data_hulls, aes(perc, feed_area_ratio, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_discrete(labels = c("Aggregated", 'Dispersed')) +
  labs(x = '% feeding habitat', y = 'CV feed-area-ratio')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

#scatter

feeding_data_hulls%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(agg)))+
  geom_point()+
  geom_smooth(method ='lm', se = F)+
  facet_grid(~perc)+
  theme_bw()

#sign test

t.test(feeding_data_hulls%>%filter(agg == 'disp')%>%select(area), 
       feeding_data_hulls%>%filter(agg == 'agg')%>%select(area))

t.test(feeding_data_hulls%>%filter(agg == 'disp')%>%select(feed_area_ratio), 
       feeding_data_hulls%>%filter(agg == 'agg')%>%select(feed_area_ratio))

# functional home ranges --------------------------------------------------

feeding_functional_perc<-feeding_data_hulls%>%filter(feed_area_ratio>=25)
feeding_functional_area<-feeding_data_hulls%>%filter(area<=6)
feeding_functional_both<-feeding_data_hulls%>%filter(area<=6 & feed_area_ratio>=25)

## plot ##

colors<-my_colors <- RColorBrewer::brewer.pal(2, "Dark2")[2:3]

#area

ggplot(feeding_functional_both, aes(x = perc, y = area, fill = agg))+
  geom_boxplot()+
  scale_fill_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Aggregated', 'Dispersed'))+
  labs(color = '', x = '% feeding habitat', y = 'Home range area [ha]')+
  theme_bw()

ggplot(feeding_functional_both, aes(perc, area, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                    labels = c('Aggregated', 'Dispersed'))+  
  labs(x = '% feeding habitat', y = 'CV HR area')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

# feeding ratio

feeding_functional_both%>%
  mutate(feed_area_ratio = case_when(feed_area_ratio>100 ~ 100,
                                     TRUE ~ feed_area_ratio))%>%
ggplot(aes(x = perc, y = feed_area_ratio, fill = agg))+
  geom_boxplot()+
  scale_fill_manual(values = colors, name = 'Spatial \naggregation', 
                    labels = c('Aggregated', 'Dispersed'))+
  labs(color = '', x = '% feeding habitat', y = 'Feed-area-ratio [%]')+
  theme_bw()

feeding_functional_both%>%
  mutate(feed_area_ratio = case_when(feed_area_ratio>100 ~ 100,
                                     TRUE ~ feed_area_ratio))%>%
ggplot(aes(perc, feed_area_ratio, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Aggregated', 'Dispersed'))+  
  labs(x = '% feeding habitat', y = 'CV feed-area-ratio')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_bw()

#No of HRs

feeding_functional_both%>%mutate(ID = 1)%>%
  group_by(perc, agg)%>%
  count(ID, .drop = F)%>%
  ggplot(aes(reorder(perc, desc(perc)), n, color = agg))+
  stat_summary(geom = 'line', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Aggregated', 'Dispersed'))+ 
  labs(x = '% feeding habitat', y = 'No. of home ranges', color = '')+
  theme_bw()

#avg feed area ration + No Hr

feeding_functional_both%>%mutate(ID = 1)%>%
  group_by(perc, agg)%>%
  summarise(mean_fr = mean(feed_area_ratio),
         mean_area = mean(area),
         n_hr = sum(ID))%>%
  ggplot(aes(n_hr, mean_fr, color = agg))+
  stat_summary(geom = 'line', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Aggregated', 'Dispersed'))+ 
  labs(y = 'Average % feeding habitat', x = 'No. of home ranges', color = '')+
  theme_bw()

feeding_functional_both%>%mutate(ID = 1)%>%
  group_by(perc, agg)%>%
  summarise(mean_fr = mean(feed_area_ratio),
            mean_area = mean(area),
            n_hr = sum(ID))%>%
  ggplot(aes(n_hr, mean_area, color = agg))+
  stat_summary(geom = 'line', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Aggregated', 'Dispersed'))+ 
  labs(y = 'Average HR area', x = 'No. of home ranges', color = '')+
  theme_bw()

feeding_functional_both%>%mutate(ID = 1)%>%
  group_by(perc, agg)%>%
  summarise(mean_fr = mean(feed_area_ratio),
            mean_area = mean(area),
            n_hr = sum(ID))%>%
  ggplot(aes(mean_area, mean_fr, color = agg))+
  stat_summary(geom = 'line', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Aggregated', 'Dispersed'))+ 
  labs(y = 'Average % feeding habitat', x = 'Average HR area', color = '')+
  theme_bw()

#scatter

feeding_functional_both%>%
  mutate(feed_area_ratio = case_when(feed_area_ratio>100 ~ 100,
                                     TRUE ~ feed_area_ratio))%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(agg)))+
  geom_point()+
  geom_smooth(method ='lm')+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Aggregated', 'Dispersed'))+ 
  facet_grid(~perc)+
  theme_bw()


# figures -----------------------------------------------------------------

#not filtered

a<-ggplot(feeding_data_areas, aes(reorder(perc, desc(perc)), HRU, color = agg))+
  stat_summary(geom = 'line', lwd = 2, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', size = 4)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  labs(x = NULL, y = 'No. of home ranges', color = '')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.position = 'none')


b<-ggplot(feeding_data_hulls, aes(x = reorder(perc, desc(perc)), y = area, fill = agg))+
  geom_hline(yintercept=6, color = 'blue', size = 1)+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  labs(color = '', x = NULL, y = 'Home range area [ha]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))


c<-ggplot(feeding_data_hulls, aes(x = reorder(perc, desc(perc)), y = feed_area_ratio, fill = agg))+
  geom_hline(yintercept=25, color = 'blue', size = 1)+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_manual(values = colors, name = 'Spatial \naggregation', 
                    labels = c('Clumped', 'Dispersed'))+ 
  labs(color = '', x = 'Fraction of feeding habitat', y = 'Feed-area-ratio [%]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

a / b / c + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('feeding_raw.svg',path = 'figures/', device = 'svg', width = 25, height = 25, units = 'cm', dpi = 600)

#CVs raw

a<-ggplot(feeding_data_hulls, aes(reorder(perc, desc(perc)), area, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  labs(x = NULL, y = 'CV home range area [%]', color = '')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))


b<-ggplot(feeding_data_hulls, aes(reorder(perc, desc(perc)), feed_area_ratio, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  labs(x = 'Fraction of feeding habitat', y = 'CV feed-area-ratio [%]', color = '')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

a / b + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('feeding_raw_CVs.svg', path = 'figures/', device = 'svg', width = 25, height = 25, units = 'cm', dpi = 600)

#CVs filtered

a<-ggplot(feeding_functional_both, aes(reorder(perc, desc(perc)), area, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  labs(x = NULL, y = 'CV home range area [%]', color = '')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

b<-feeding_functional_both%>%
  mutate(feed_area_ratio = case_when(feed_area_ratio>100 ~ 100,
                                     TRUE ~ feed_area_ratio))%>%
  ggplot(aes(reorder(perc, desc(perc)), feed_area_ratio, color = agg))+
  stat_summary(geom = 'line', fun = 'cv', lwd = 1, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', fun = 'cv', size = 2)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  labs(x = 'Fraction of feeding habitat', y = 'CV feed-area-ratio [%]', color = '')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

a / b + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('feeding_filtered_CVs.svg', path = 'figures/', device = 'svg', width = 25, height = 25, units = 'cm', dpi = 600)

#filtered for functionality

a<-feeding_functional_both%>%mutate(ID = 1)%>%
  group_by(perc, agg)%>%
  count(ID, .drop = F)%>%
  ggplot(aes(reorder(perc, desc(perc)), n, color = agg))+
  stat_summary(geom = 'line', lwd = 2, aes(group = agg, color = agg))+
  stat_summary(geom = 'point', size = 4)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  labs(x = NULL, y = 'No. of home ranges', color = '')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.position = 'none')


b<-feeding_functional_both%>%
  mutate(feed_area_ratio = case_when(feed_area_ratio>100 ~ 100,
                                     TRUE ~ feed_area_ratio))%>%
  ggplot(aes(x = reorder(perc, desc(perc)), y = area, fill = agg))+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_manual(values = colors, name = 'Spatial \naggregation', 
                    labels = c('Clumped', 'Dispersed'))+ 
  labs(color = '', x = NULL, y = 'Home range area [ha]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

c<-feeding_functional_both%>%
  mutate(feed_area_ratio = case_when(feed_area_ratio>100 ~ 100,
                                     TRUE ~ feed_area_ratio))%>%
  ggplot(aes(x = reorder(perc, desc(perc)), y = feed_area_ratio, fill = agg))+
  geom_boxplot(width = 0.8,  lwd = 1)+
  scale_fill_manual(values = colors, name = 'Spatial \naggregation', 
                    labels = c('Clumped', 'Dispersed'))+ 
  labs(color = '', x = 'Fraction of feeding habitat', y = 'Feed-area-ratio [%]')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15))

a / b / c + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')

ggsave('feeding_filtered.svg',path = 'figures/', device = 'svg', width = 25, height = 25, units = 'cm', dpi = 600)

#scatter

feeding_functional_both%>%
  mutate(feed_area_ratio = case_when(feed_area_ratio>100 ~ 100,
                                     TRUE ~ feed_area_ratio))%>%
  ggplot(aes(x = area, y = feed_area_ratio, color = as.factor(agg)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method ='lm', lwd = 1)+
  scale_color_manual(values = colors, name = 'Spatial \naggregation', 
                     labels = c('Clumped', 'Dispersed'))+ 
  facet_grid(~reorder(perc, desc(perc)))+
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
        legend.position = c(0.94,0.2),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave('feeding_scatter.svg',path = 'figures/', 
       device = 'svg', width = 35, height = 15, units = 'cm', dpi = 600)
