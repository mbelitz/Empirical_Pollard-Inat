library(dplyr)
library(ggplot2)

but <- read.csv("data/total_buts.csv", stringsAsFactors = FALSE)
cc <- read.csv("data/comparable_cells.csv", stringsAsFactors = FALSE)
sp <- unique(cc$scientific_name)
addit_spp <- c(
                "Callophys augustinus",
                "Callophys niphon",
                "Poanes hobomok",
                "Satyrium calanus",
                "Satyrium titus"
               )


tot_sp <- c(sp, addit_spp)

but_40spp <- but %>% 
  filter(scientific_name %in% tot_sp)

spatial_cov <- but_40spp %>% 
  filter(latitude >= 36 & latitude <= 42) %>% 
  filter(longitude <= -75 & longitude >= -94) %>%
  filter(year >= 2015) %>% 
  mutate(lat_bin = 1*floor(latitude/1) + 1/1,
         lon_bin = 1*floor(longitude/1) + 1/1)

spatial_cov <- spatial_cov %>% 
  mutate(lat_bin = lat_bin - 0.5) %>% 
  mutate(lon_bin = lon_bin -0.5)

write.csv(spatial_cov, file = "data/toElise/raw_40spp.csv", row.names = FALSE)

spatial_group <- spatial_cov %>% 
  group_by(lat_bin, lon_bin, scientific_name, year) %>% 
  summarise(num_obs = n()) %>% 
  filter(num_obs >= 10)

write.csv(spatial_group, file = "data/toElise/observations_per_yearXcell_2015plus.csv", row.names = FALSE)

spp_cell_year <- spatial_group %>% 
  group_by(scientific_name, year) %>% 
  summarise(cells_with_enough_obs = n())

write.csv(spp_cell_year, file = "data/toElise/cells_with_enough_observations_yearxspecies_2015plus.csv", row.names = FALSE)

spp_enough <- spatial_group %>% 
  group_by(scientific_name) %>% 
  summarise(cells_with_enough_obs = n())

write.csv(spp_enough, file = "data/toElise/cells_that_are_comparable_per_species_2015plus.csv", row.names = FALSE)

