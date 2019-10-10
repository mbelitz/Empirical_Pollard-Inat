library(dplyr)

## Read in annotated files

files <- list.files(path = "annotated_csvs/", pattern = "*.csv", full.names = TRUE)
files_list <- lapply(files, read.csv)

annotated_df <- do.call(rbind, files_list)

annotated_df$life_stage <- as.character(annotated_df$life_stage)

annotated_df <- annotated_df %>% 
  mutate(lifeStage = ifelse(test = adults == "1", yes = "A", 
                            no = life_stage)) %>% 
  filter(lifeStage != "", lifeStage != "-") %>% 
  filter(!is.na(lifeStage))

counted_df <- annotated_df %>% 
  count(annotated_df$lifeStage) %>% 
  mutate(perc = (n / sum(n)) * 100)

####### Now by individual species ########

annotated_df <- annotated_df %>% 
  mutate(spCode = gsub('[0-9]+', '', file)) %>% 
  mutate(spCode = gsub('.jpg', '', spCode))
  
# A is adult, E is egg, L is larvae, P is pupae, U is unknown/unclear

sp_counted_df <- annotated_df %>% 
  group_by(spCode) %>% 
  summarise(A = sum(lifeStage == "A"),
            E = sum(lifeStage == "E"),
            L = sum(lifeStage == "L"),
            P = sum(lifeStage == "P"),
            U = sum(lifeStage == "U"))

sp_counted_df <- sp_counted_df %>% 
  filter(spCode != "") %>% 
  mutate(total = A + E + L + P +U) %>% 
  mutate(percA = (A/total)*100,
         percE = (E/total)*100,
         percL = (L/total)*100,
         percP = (P/total)*100,
         percU = (U/total)*100)


