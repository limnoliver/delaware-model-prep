library(ggplot2)
library(dplyr)

# figures to demonstrate the ability of the models
# to capture the timing of the annual maximum temperature

# read in max metrics
# filter to relevant mods
max_met <- read.csv('4_evaluation/out/max_metrics_by_seg_year.csv') %>%
  filter(model %in% c('sntemp_full', 'rgcn2_full_full'))

# create overlapping density plots
p <- ggplot(max_met, aes(x = error_max_timing)) +
  geom_density(aes(group = model, fill = model), alpha = 0.5, color = 'gray30') +
  scale_fill_manual(values = c('#7570b3','#1b9e77'), labels = c('PGDL', 'UPB')) +
  scale_x_continuous(breaks = c(-60, -30, -7,  7, 30, 60),
                     labels = c('-2 mo', '-1 mo', '-1 wk',  '1 wk', '1 mo', '2 mo')) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = NA, color = NA),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14)) +
  labs(x = expression(T[max]~timing~error), y = 'Density', fill = '')

ggsave('8_visualize/out/max_timing_density.png', p, height = 5, width = 7)

# create cumulative distribution plot
p <- ggplot(max_met, aes(x = abs(error_max_timing))) +
  stat_ecdf(aes(group = model, color = model), size = 1.2, alpha = 0.7) +
  scale_x_continuous(breaks = c(1, 7, 14, 30, 60),
                     labels = c('1d', '1wk', '2wk', '1mo', '2mo')) +
  scale_color_manual(values = c('#7570b3','#1b9e77'), labels = c('PGDL', 'UPB')) +
  theme_minimal() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.8, 0.3),
        legend.background = element_rect(fill = NA, color = NA),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 14))+
  labs(x = expression(Absolute~T[max]~timing~error),
       y = 'Proportion of site-years', color = '')

ggsave('8_visualize/out/cumulative_dist_max_timing_error.png', p, height = 4, width = 4)

