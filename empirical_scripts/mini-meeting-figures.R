library(dplyr)
library(urbnmapr)
library(ggplot2)
obs_by_cell <- read.csv("Emperical_Results/obs10_by-cell-spp-year.csv", stringsAsFactors = FALSE)  
obs_by_cell <- obs_by_cell %>% 
  mutate(lat_bin = lat_bin - 0.5, lon_bin = lon_bin - 0.5)
te <- read.csv("Emperical_Results/emperical_tenth_estimates.csv", stringsAsFactors = FALSE) 
te <- left_join(te, obs_by_cell)
fe <- read.csv("Emperical_Results/emperical_fiftieth_estimates.csv", stringsAsFactors = FALSE)
fe <- left_join(fe, obs_by_cell)

p_tharos_10 <- te %>% 
  dplyr::filter(scientificName == "Phyciodes tharos")

p_tharos_50 <- fe %>% 
  dplyr::filter(scientificName == "Phyciodes tharos")

extent <- filter(states, long>= -97 & long <=  -68,
                 lat>= 36 & lat <= 43)

ggplot2::ggplot(data = p_tharos_50, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75), limits = c(20,75)) +
  coord_map() +
  theme_bw() +
  scale_color_viridis_c() +
  facet_wrap(~year)

p_glaucus_10 <- te %>% 
  dplyr::filter(scientificName == "Papilio glaucus")

ggplot2::ggplot(data = p_tharos_10, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  theme_bw() +
  scale_color_viridis_c() +
  facet_wrap(~year)


## most sampled species

threespp <- fe %>% 
  dplyr::filter(scientificName == "Phyciodes tharos" | scientificName == "Papilio glaucus"|
                  scientificName == "Epargyreus clarus")

ggplot(threespp) +
  geom_point(aes(x = lat_bin, y = estimate, color = scientificName)) +
  geom_smooth(aes(x = lat_bin, y = estimate, color = scientificName),se = FALSE, method = "glm")

# 2017 
p_tharos_10_2017 <- te %>% 
  dplyr::filter(scientificName == "Phyciodes tharos", year == 2017)

p_tharos_50_2017 <- fe %>% 
  dplyr::filter(scientificName == "Phyciodes tharos", year == 2017)

pt_50_2017 <- ggplot2::ggplot(data = p_tharos_50_2017, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Phyciodes Tharos - 2017") +
  theme_bw() +
  scale_color_viridis_c(limits = c(160,250),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

pt_10_2017 <- ggplot2::ggplot(data = p_tharos_10_2017, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Phyciodes Tharos - 2017") +
  theme_bw() +
  scale_color_viridis_c(limits = c(90,215),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))


p_glaucus_10_2017 <- te %>% 
  dplyr::filter(scientificName == "Papilio glaucus", year == 2017)

p_glaucus_50_2017 <- fe %>% 
  dplyr::filter(scientificName == "Papilio glaucus", year == 2017)

pg_50_2017 <- ggplot2::ggplot(data = p_glaucus_50_2017, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Papilio glaucus - 2017") +
  theme_bw() +
  scale_color_viridis_c(limits = c(160,250),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

pg_10_2017 <- ggplot2::ggplot(data = p_glaucus_10_2017, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Papilio glaucus - 2017") +
  theme_bw() +
  scale_color_viridis_c(limits = c(90,215),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

e_clarus_10_2017 <- te %>% 
  dplyr::filter(scientificName == "Epargyreus clarus", year == 2017)

e_clarus_50_2017 <- fe %>% 
  dplyr::filter(scientificName == "Epargyreus clarus", year == 2017)

ec_50_2017 <- ggplot2::ggplot(data = e_clarus_50_2017, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Epargyreus clarus - 2017") +
  theme_bw() +
  scale_color_viridis_c(limits = c(160,250),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

ec_10_2017 <- ggplot2::ggplot(data = e_clarus_10_2017, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Epargyreus clarus - 2017") +
  theme_bw() +
  scale_color_viridis_c(limits = c(90,215),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))


est_50_2017 <- gridExtra::grid.arrange(pg_50_2017, pt_50_2017, ec_50_2017)

est_10_2017


## most sampled species

threespp <- te %>% 
  dplyr::filter(scientificName == "Phyciodes tharos" | scientificName == "Papilio glaucus"|
                  scientificName == "Epargyreus clarus")

ggplot(threespp) +
  geom_point(aes(x = lat_bin, y = estimate, color = scientificName)) +
  geom_smooth(aes(x = lat_bin, y = estimate, color = scientificName),se = FALSE, method = "glm")

# 2018
p_tharos_10_2018 <- te %>% 
  dplyr::filter(scientificName == "Phyciodes tharos", year == 2018)

p_tharos_50_2018 <- fe %>% 
  dplyr::filter(scientificName == "Phyciodes tharos", year == 2018)

pt_50_2018 <- ggplot2::ggplot(data = p_tharos_50_2018, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Phyciodes Tharos - 2018") +
  theme_bw() +
  scale_color_viridis_c(limits = c(160,250),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

pt_10_2018 <- ggplot2::ggplot(data = p_tharos_10_2018, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Phyciodes Tharos - 2018") +
  theme_bw() +
  scale_color_viridis_c(limits = c(90,215),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

p_glaucus_10_2018 <- te %>% 
  dplyr::filter(scientificName == "Papilio glaucus", year == 2018)

p_glaucus_50_2018 <- fe %>% 
  dplyr::filter(scientificName == "Papilio glaucus", year == 2018)

pg_50_2018 <- ggplot2::ggplot(data = p_glaucus_50_2018, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Papilio glaucus - 2018") +
  theme_bw() +
  scale_color_viridis_c(limits = c(160,250),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

pg_10_2018 <- ggplot2::ggplot(data = p_glaucus_10_2018, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Papilio glaucus - 2018") +
  theme_bw() +
  scale_color_viridis_c(limits = c(90,215),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

e_clarus_10_2018 <- te %>% 
  dplyr::filter(scientificName == "Epargyreus clarus", year == 2018)

e_clarus_50_2018 <- fe %>% 
  dplyr::filter(scientificName == "Epargyreus clarus", year == 2018)

ec_50_2018 <- ggplot2::ggplot(data = e_clarus_50_2018, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Epargyreus clarus - 2018") +
  theme_bw() +
  scale_color_viridis_c(limits = c(160,250),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))

ec_10_2018 <- ggplot2::ggplot(data = e_clarus_10_2018, mapping = aes(x = lon_bin, y = lat_bin, color = estimate)) +
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group), fill = "lightgrey",
               fill = "white", color = "black", alpha = 1) +
  geom_point(aes(size = count), limits = c(20,75)) +
  coord_map() +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Epargyreus clarus - 2018") +
  theme_bw() +
  scale_color_viridis_c(limits = c(90,215),direction = -1) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2))


est_2018 <- gridExtra::grid.arrange(pg_2018, pt_2018, ec_2018)


maps_50 <- ggpubr::ggarrange(pg_50_2017, pt_50_2017, ec_50_2017,
                  pg_50_2018, pt_50_2018, ec_50_2018, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

maps_10 <-  ggpubr::ggarrange(pg_10_2017, pt_10_2017, ec_10_2017,
                              pg_10_2018, pt_10_2018, ec_10_2018, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

ggsave(maps_50, filename = "empirical_figures/mini-meet-updatefig.png", width = 15, height = 6, dpi = 300)

ggsave(maps_10, filename = "empirical_figures/mini-meet-updatefig_10.png", width = 15, height = 6, dpi = 300)


