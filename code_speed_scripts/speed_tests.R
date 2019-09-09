tests <- read.csv("speed_tests.csv")

boot <- tests %>% 
  dplyr::filter(parallellization == "boot")

ggplot(boot) +
  geom_line(aes(x = ncore, y = sec_elapsed)) +
  ggtitle("Using Boot Parrallelization")


mclapply <- tests %>% 
  dplyr::filter(parallellization == "mclapply") %>% 
  dplyr::filter(nlists == 7)


ggplot(mclapply) +
  geom_line(aes(x = ncore, y = sec_elapsed)) +
  ggtitle("Using mclapply Parrallelization")

