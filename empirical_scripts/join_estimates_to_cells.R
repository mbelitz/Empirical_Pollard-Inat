tenth <- read.csv("Emperical_Results/naive_tenth_estimates_BCAsCIs.csv", stringsAsFactors = FALSE)
fiftieth <- read.csv("Emperical_Results/naive_fiftieth_estimates.csv", stringsAsFactors = FALSE)
ccels <- compare_cells %>% 
  rename(scientificName = scientific_name)


tenth_compare <- left_join(tenth, ccels, by = c("scientificName", "year", "lat_bin", "lon_bin"))

tenth_compare <- tenth_compare %>% 
  dplyr::select(- combined_obs, -n) %>% 
  dplyr::rename(obs = count)

fiftieth_compare <- left_join(fiftieth, ccels, by = c("scientificName", "year", "lat_bin", "lon_bin"))

fiftieth_compare <- fiftieth_compare %>% 
  dplyr::select(- combined_obs, -n) %>% 
  dplyr::rename(obs = count)

write.csv(fiftieth_compare, file = "Emperical_Results/naive_fiftieth_iNat_estimates.csv")
write.csv(tenth_compare, file = "Emperical_Results/naive_tenth_inat_estimates.csv")
