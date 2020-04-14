library(dplyr)
library(ggplot2)

but <- read.csv("data/total_buts.csv", stringsAsFactors = FALSE)

dat <- but %>% 
  filter(scientific_name == "Speyeria cybele" | 
           scientific_name == "Megisto cymela" |
           scientific_name == "Phyciodes tharos" |
           scientific_name == "Vanessa atalanta") %>% 
  filter(longitude >= - 88 & longitude <= -87) %>% 
  filter(latitude >= 41 & latitude <= 42) %>% 
  filter(year == 2017 | year == 2018)


# explore data

il <- read.csv("data/pollard_flightcurves_IL.csv", stringsAsFactors = FALSE)

scybele_il <- il %>% 
  select(S.cybele, surveyN, DAY)

ggplot() + 
  geom_line(scybele_il, mapping = aes(x = DAY, y = S.cybele/nrow(scybele_il)), color = "purple") + 
  geom_density(sc, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 191.79, color = "purple") + 
  geom_vline(xintercept = 190.33, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Speyeria cybele") + 
  theme_classic()


va1 <- dat %>% 
  filter(scientific_name == "Vanessa atalanta") %>% 
  filter(year == 2018)

write.csv(x = dat, file = "data/toElise/illinois_2017-2018_4spp.csv", row.names = FALSE)
