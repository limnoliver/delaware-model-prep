library(dplyr)
library(ggplot2)

# create process vs hybrid RMSE plots, similar to those made for Jared
compare <- feather::read_feather(sc_retrieve('3_predictions/out/compare_predictions_obs.feather.ind', 'getters.yml'))

scmake('4_evaluation/out/gen_metrics_by_seg.csv')

metrics <- read_csv('4_evaluation/out/gen_metrics_by_seg.csv') %>%
  filter(model %in% c('rgcn2_full_full', 'sntemp_full')) %>%
  tidyr::gather(key = 'metric', value = 'metric_value', -model, -seg_id_nat, -n)

sntemp <- filter(metrics, model %in% 'sntemp_full') %>%
  select(seg_id_nat, n, metric, sntemp_metric_value = metric_value)

rgcn <- filter(metrics, model %in% 'rgcn2_full_full') %>%
  select(seg_id_nat, n, metric, rgcn_metric_value = metric_value)

diff <- left_join(sntemp, rgcn) %>%
  mutate(metric_diff = sntemp_metric_value - rgcn_metric_value)

# now need to find out how much data each site had for training
# how much data did the model "see" at each site?

training_obs <- feather::read_feather('3_predictions/out/compare_predictions_obs.feather') %>%
  filter(seg_id_nat %in% diff$seg_id_nat) %>%
  filter(date <= as.Date('2004-09-30')) %>%
  filter(!is.na(temp_c)) %>%
  group_by(seg_id_nat) %>%
  summarize(n_training_obs = n())

diff <- left_join(mutate(diff, seg_id_nat = as.character(seg_id_nat)), training_obs) %>%
  mutate(diff_prop = metric_diff/sntemp_metric_value) %>%
  mutate(n_training_obs = ifelse(is.na(n_training_obs), 0, n_training_obs))


p_rmse <- ggplot(filter(diff, metric %in% 'rmse'), aes(x = sntemp_metric_value, y = rgcn_metric_value)) +
  geom_point(aes(size = n_training_obs), alpha = 0.3) +
  scale_size_continuous(breaks = c(5, 50, 500, 5000)) +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian(xlim = c(0,8.5), ylim = c(0,8.5)) +
  theme_bw() +
  labs(x = 'uncalibrated PB RMSE', y = 'PGDL RMSE', size = 'Number of\ntraining obs.') +
  theme(panel.grid = element_blank())

ggsave('8_visualize/out/pgdl_upb_rmse_comparison.png', p_rmse, height = 4, width = 6)

# difference vs training obs
p_diff <- ggplot(filter(diff, metric %in% 'rmse'), aes(x = n_training_obs, y = metric_diff)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, alpha = 0.5) +
  scale_x_log10() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y = 'RMSE Improvement (UPB - PGDL)', x = 'Number of training obs.')

ggsave('8_visualize/out/rmse_improvement_vs_ntraining.png', p_diff, height = 3, width = 4)

# relative difference vs training obs
p_diff <- ggplot(filter(diff, metric %in% 'rmse'), aes(x = n_training_obs, y = diff_prop)) +
  geom_point(alpha = 0.5, aes(size = n)) +
  scale_size_continuous(breaks = c(10, 100, 1000)) +
  geom_smooth(method = 'lm', se = FALSE, alpha = 0.5) +
  scale_x_log10() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y = 'rRMSE Improvement\n(UPB - PGDL/UPB)', x = 'Number of training obs.')

ggsave('8_visualize/out/rmse_prop_improvement_vs_ntraining.png', p_diff, height = 3, width = 5)

p_diff_test <- ggplot(filter(diff, metric %in% 'rmse'), aes(x = n_training_obs, y = metric_diff)) +
  geom_point(alpha = 0.5, aes(size = n)) +
  scale_size_continuous(breaks = c(10, 100, 1000)) +
  geom_smooth(method = 'lm', se = FALSE, alpha = 0.5) +
  scale_x_log10() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y = 'RMSE Improvement (UPB - PGDL)', x = 'Number of training obs.', size = 'Number of\ntest obs.')

p_diff_test <- ggplot(filter(diff, metric %in% 'rmse'), aes(x = n_training_obs, y = metric_diff)) +
  geom_point(alpha = 0.5, aes(color = n)) +
  scale_color_gradient(low = '#9ecae1', high = '#08306b') +
  geom_smooth(method = 'lm', se = FALSE, color = 'gray20') +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_log10() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y = 'RMSE Improvement (UPB - PGDL)', x = 'Number of training obs.', color = 'Number of\ntest obs.')

p_diff_test <- ggplot(filter(diff, metric %in% 'rmse'), aes(x = n_training_obs, y = metric_diff)) +
  geom_point(alpha = 0.5, aes(size = n, color = metric_diff < 0)) +
  scale_size_continuous(breaks = c(10, 100, 1000)) +
  scale_color_manual(values = c('black', 'red'), guide = FALSE) +
  geom_smooth(method = 'lm', se = FALSE, alpha = 0.5, color = 'black') +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_log10() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y = 'RMSE Improvement (UPB - PGDL)', x = 'Number of training obs.', size = 'Number of\ntest obs.')

ggsave('8_visualize/out/rmse_improvement_vs_ntraining_ntest.png', p_diff_test, height = 3, width = 5)



p_diff2 <- ggplot(filter(diff, metric %in% 'rmse'), aes(x = sntemp_metric_value, y = metric_diff)) +
  geom_point(alpha = 0.3, aes(size = n_training_obs)) +
  geom_smooth(method = 'lm', se = FALSE, alpha = 0.5) +
  geom_hline(yintercept = 0, color = 'darkgray', linetype = 2) +
  scale_size_continuous(breaks = c(5, 50, 500, 5000)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y = 'RMSE Improvement (UPB - PGDL)', x = 'Uncalibrated PB RMSE', size = 'Number of\ntraining obs.')

ggsave('8_visualize/out/rmse_improvement_vs_upb.png', p_diff2, height = 3, width = 5.5)

# overlapping density plots of focal model + PB0 RMSE
# long diff
metrics <- group_by(metrics, model, metric) %>%
  mutate(median_val = median(metric_value)) %>% ungroup()

p_density <- ggplot(filter(metrics, metric %in% 'rmse'), aes(x = metric_value)) +
  geom_density(aes(fill = model, group = model), color = NA, alpha = 0.7) +
  scale_fill_manual(values = c('#7570b3','#1b9e77'), labels = c('PGDL', 'UPB')) +
  geom_vline(aes(xintercept = median_val, color = model), size = 1.5, linetype = 2) +
  scale_color_manual(values = c('#7570b3','#1b9e77'), labels = c('PGDL', 'UPB'), guide = FALSE) +
  coord_cartesian(ylim = c(0, 0.65), expand = FALSE) +
  theme(strip.background = element_blank(), strip.text = element_text(size = 14)) +
  theme_bw() +
  scale_x_continuous(breaks = 0:8) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = c(.8, .8)) +
  labs(x = 'RMSE', y = 'Density', fill = 'Model')

ggsave('8_visualize/out/rmse_density.png', p_density, height = 4, width = 6)


