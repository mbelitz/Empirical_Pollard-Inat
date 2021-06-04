#Project:  butterfly flight phenology drivers from incidental and survey data
#Survey data: NABMN surveys (pollardbase.org, Ohio)
#Incidental data: iNaturalist & eButterfly (metrics provided by M Belitz, UF)
#Elise Larsen, 2020-06 Updated through 2021-5
#Georgetown University 
#MS Figures
##Here: Code to estimate phenometrics from survey data

library(tidyverse)
library(lubridate)
library(boot)

#load project functions
source("Rcode/project_functions.R")

#load survey data
#load("data/survey_data.RData")
#Speyeria data as example 
survey_data<-read_csv("data/survey_speyeriadata.csv")
#Set parameters
nreps=1000 # number of reps for bootstrapping

#Summarize data: each row represents the data for one set of phenometrics
survey.group<-survey_data %>%
  group_by(SPECIES, YEAR, lat_bin,lon_bin) %>%
  tally()

#Create dataframe for survey phenometrics
survey.pheno<-data.frame("scientificName"=character(),"year"=numeric(),"lat_bin"=numeric(),"lon_bin"=numeric(),
                         "nsites"=numeric(),"nvisits"=numeric(),"ncounts"=numeric(),"zeros"=numeric(),"ci.repN"=numeric(),
                      "estimate10"=numeric(),"ci10.low95"=numeric(),"ci10.high95"=numeric(),
                      "estimate50"=numeric(),"ci50.low95"=numeric(),"ci50.high95"=numeric(), stringsAsFactors = FALSE)

#Calculate phenometrics
for (i in 1:nrow(survey.group)) {
  data.i<-merge(survey_data, survey.group[i,], by=c("lat_bin","lon_bin", "YEAR", "SPECIES"))
  gam.i<-try(boot(data.i, pheno_bootstrap,R=nreps))
  stat.i<-c("10%","50%")
  tenth.ci<-quantile(gam.i$t[,1], probs=c(0.025,0.975))
  fiftieth.ci<-quantile(gam.i$t[,2], probs=c(0.025,0.975))
  pheno.i<-data.i %>% group_by(SPECIES, YEAR, lat_bin,lon_bin) %>%
    summarize(nsites=length(unique(SITE_ID)), 
                              nvisits=length(unique(paste(SITE_ID, DATE))),
                              ncounts=n(),
                              zeros=nvisits-ncounts) %>%
    rename(scientificName=SPECIES, year=YEAR)
  metrics.i<-data.frame("nreps"=nreps, "estimate10"=gam.i$t0[1],t(tenth.ci[c(1,2)]),
                        "estimate50"=gam.i$t0[2],t(fiftieth.ci[c(1,2)]))

  survey.pheno<-rbind(survey.pheno, setNames(cbind(pheno.i, metrics.i), names(survey.pheno)))

}
#save(survey.pheno, file="data/pollard_indices.RData")     
#write.csv(filter(survey.pheno, stat="10%"),file="pollard10.csv")
#write.csv(filter(survey.pheno, stat="50%"),file="pollard50.csv")

