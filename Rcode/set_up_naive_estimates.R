# load packages
library(dplyr)
library(stringr)
library(phenesse)

# import data
total_buts <- read.csv(file = "data/total_incidental_bfly_data.csv", 
                       stringsAsFactors = FALSE, header = TRUE)

total_buts <- total_buts %>% 
  dplyr::filter(latitude <= 42 & latitude >=36) %>% 
  dplyr::filter(longitude <= -75 & longitude >= -94) %>% 
  dplyr::filter(year >= 2012)

compare_cells <- read.csv(file = "data/comparable_cells.csv", 
                          stringsAsFactors = FALSE, header = TRUE)

# read in the scored adult images

files <- list.files(path = "iNat_image_annotation/", pattern = "*.csv", full.names = TRUE)
files_list <- lapply(files, read.csv)

annotated_df <- do.call(rbind, files_list)
annotated_df <- annotated_df %>% 
  dplyr::filter(adults != "U") %>% 
  filter(adults != "F")
annotated_df$file <- as.character(annotated_df$file)
annotated_df$adults <- as.character(annotated_df$adults)
annotated_df$adults <- as.numeric(annotated_df$adults)

m <- gregexpr('[0-9]+',annotated_df$file)
mm <- str_extract(annotated_df$file, "[[:digit:]]+") %>% 
  as.numeric()

annotated_df <- annotated_df %>% 
  mutate( id  =mm)

cats <- filter(annotated_df, adults == 0)

# antijoin not adults

total_adult_buts <- anti_join(total_buts, cats, by = "id")

total_adult_buts <- total_adult_buts %>% 
  mutate(lat_bin = 1*floor(latitude/1) + 1/1,
         lon_bin = 1*floor(longitude/1) + 1/1)

total_adult_buts <- total_adult_buts %>% 
  mutate(lat_bin = lat_bin - 0.5) %>% 
  mutate(lon_bin = lon_bin -0.5)

# functions to get pheno-estimates

estimate_sp_pheno_apply <- function(binomial){
  
  sel_fun <- function(x){
    as.vector(dplyr::select(x, day))
  }
  
  sp_df <- total_adult_buts %>% 
    filter(scientific_name == binomial)
  
  spp_count_10 <- sp_df %>% 
    group_by(year, lon_bin, lat_bin) %>% 
    summarise(yearly_obs = n()) %>% 
    filter(yearly_obs >= 10)
  
  sp_df_10 <- left_join(sp_df, spp_count_10, by = c("year", "lat_bin", "lon_bin"))
  
  compare_df <- left_join(sp_df_10, compare_cells, by = c("scientific_name", "year",
                                                          "lat_bin", "lon_bin")) %>% 
    dplyr::filter(!is.na(combined_obs))
  
  lists <- split(compare_df, list(compare_df$year, compare_df$lat_bin, compare_df$lon_bin)) 
  lists2 <- lists[sapply(lists, nrow)>0]
  
  applylist <- lapply(lists2, sel_fun) 
  
  return(applylist)
}

# functions to add data together as we run these

results_to_df <- function(result_list, binomial){
  result_df <- as.data.frame(split(unlist(result_list), 1:3))
  
  result_df <- result_df %>% 
    dplyr::rename(estimate = X1,low_ci = X2, high_ci = X3) %>% 
    dplyr::mutate(year = as.numeric(substring(row.names(result_df), first = 1, last =4)),
                  lat_bin = as.numeric(substring(row.names(result_df), first = 6, last =9)),
                  lon_bin = as.numeric(substring(row.names(result_df), first = 11, last =15))) %>% 
    dplyr::mutate(scientificName = binomial)
  
  return(result_df)
}

# lapplyfun

fiftiethfun <- function(x){
  
  fiftieth <- phenesse::quantile_ci(observations = x$day, percentile = 0.5,bootstraps = 100000)
  return(fiftieth)
}

tenthfun <- function(x){
  
 tenth <- phenesse::quantile_ci(observations = x$day, percentile = 0.1,
                                bootstraps = 100000, type = "bca")
  return(tenth)
}

tenthfunperc<- function(x){
  
  tenth <- phenesse::quantile_ci(observations = x$day, percentile = 0.1,
                                 bootstraps = 100000, type = "perc")
  return(tenth)
}
# function to get data frame and estimate value and make results to dataframe

obs_to_estimate <- function(binomial, lapplyfun){
  list <- estimate_sp_pheno_apply(binomial = binomial)
  est <- lapply(list, FUN = lapplyfun)
  est_df <- results_to_df(est, binomial = binomial)
  
  return(est_df)
}

rm(m)
rm(mm)
rm(files)
rm(annotated_df,cats,files_list)