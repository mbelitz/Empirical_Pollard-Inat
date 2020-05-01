library(tidyverse)

tb <- read.csv("data/total_buts.csv", stringsAsFactors = FALSE)

# read in the scored adult images

files <- list.files(path = "annotated_csvs/", pattern = "d_plexip*", full.names = TRUE)
files_list <- lapply(files, read.csv)

annotated_df <- do.call(rbind, files_list)
annotated_df <- annotated_df 
annotated_df$file <- as.character(annotated_df$file)
annotated_df$adults <- as.character(annotated_df$adults)
annotated_df$adults <- as.numeric(annotated_df$adults)

m <- gregexpr('[0-9]+',annotated_df$file)
mm <- str_extract(annotated_df$file, "[[:digit:]]+") %>% 
  as.numeric()

annotated_df <- annotated_df %>% 
  mutate(id=mm)

# join annotated_df to see life stage

adult_monarchs <-left_join(monarchs_2018, annotated_df, by = "id")

adult_monarchs_studyarea <- filter(adult_monarchs, !is.na(adults))

write.csv('monarchs_annotated_inat.csv', row.names = FALSE)
