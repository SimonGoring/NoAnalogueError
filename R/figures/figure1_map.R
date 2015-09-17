# Figure 1 - Map


map <- map_data('world')

map <- subset(map, map$long > -160 & map$long < -30)
map <- subset(map, map$lat > 20 & map$lat < 83)

map_plot <- ggplot(data = data.frame(map), aes(x, y)) + 
  geom_polygon(aes(group=group), color='blue', alpha=0.7) +
  geom_point(data = data.frame(climate), aes(x = x, y = y), alpha = 0.5) +
  theme_bw() +
  xlab('') + ylab('') +
  theme(axis.text=element_text(size=12, family='serif'),
        panel.background = element_rect(fill = 'lightblue')) +
  coord_map(projection='albers', lat0=35, lat1=55, xlim=c(-100, -60), ylim=c(35, 80))
