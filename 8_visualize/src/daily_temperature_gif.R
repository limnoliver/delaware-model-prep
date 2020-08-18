# prediction gif
# read in network for all possible reach IDs
library(ggplot2)
library(dplyr)
library(gganimate)
library(RColorBrewer)
library(sf)
library(RColorBrewer)

network <- readRDS('1_network/out/network.rds')
preds <- feather::read_feather('3_predictions/out/compare_predictions_obs.feather')

# plot theme
# modified from "black theme" so might not need everything below
white_theme <- theme(
  # get rid of panel grids
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change plot and panel background
  plot.background=element_rect(fill = "white", color = 'white'),
  panel.background = element_rect(fill = 'white', color = 'white'),
  # Change legend
  legend.position = 'right',
  legend.title.align = 0.5,
  legend.direction = "vertical",
  legend.justification = 'right',
  legend.key.height = unit(.4, 'inches'),
  legend.key = element_rect(color = "white", fill = "white"),
  legend.title = element_text(color = "black", size = 22),
  legend.text = element_text(color = "black", size = 18),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank(),
  plot.caption = element_text(color = 'grey30', size = 20, hjust = 1.1, vjust = 70, face = 'italic')
)

# filter to single year
dat <- filter(preds, lubridate::year(date) %in% 2009) %>%
  mutate(rgcn2_full_temp_c = ifelse(rgcn2_full_temp_c < 0, 0, rgcn2_full_temp_c))

# merge network with data
# needs to be the length of the data in order to flip through dates
plot_dat <- network$edges %>%
  select(seg_id_nat, geometry) %>%
  mutate(seg_id_nat = as.character(seg_id_nat)) %>%
  right_join(select(dat, date, seg_id_nat, temp_c = rgcn2_full_temp_c)) %>%
  mutate(date2 = format(date, "%b %d")) %>%
  arrange(date)

# created a differently formatted date, but it wasn't
# being used in the right order in the gif
# tried making it an ordered factor, but that made the gif fail
# in the end, went back to using normal date format
plot_dat$date2 <- factor(plot_dat$date2, levels = unique(plot_dat$date2))

# plot!
p <- ggplot(data = plot_dat) +
  #observed line
  geom_sf(aes(color = temp_c), size = 0.1) +
  scale_color_gradientn(colours = rev(brewer.pal(11, 'RdYlBu'))[c(1:4, 7:11)],
                        limits = c(0, 28), breaks = c(5, 10, 15, 20, 25)) +
  white_theme +
  guides(color = guide_colorbar(title = 'T (deg C)')) +
  gganimate::transition_states(date) +
  labs(caption = '{closest_state}')

gganimate::animate(p, width = 1000, height = 1000, bg = 'transparent', nframes = 1000, fps = 30)
anim_save('8_visualize/out/daily_temp_color_mod.gif')

# for testing
#gganimate::animate(p, width = 1000, height = 1000, bg = 'transparent', nframes = 10, fps = 20)
#anim_save('test_temp.gif')
