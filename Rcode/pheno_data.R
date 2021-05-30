#Project:  butterfly flight phenology drivers from incidental and survey data
#Survey data: NABMN surveys (pollardbase.org, Ohio)
#Incidental data: iNaturalist & eButterfly (metrics provided by M Belitz, UF)
#Elise Larsen, 2020-06 Updated through 2021-5
#Georgetown University 

#Here: phenometrics & explanatory variables combined for analysis


#libraries
library(MASS)
library(tidyverse) #tidyverse to keep things tidy
library(lubridate) #lubridate to manipulate dates
library(readxl)    #readxl to read excel files
library(corrplot)  #corrplot to check correlations among explanatory variables
library(lme4)      #lme4 to run mixed effects models
library(MuMIn)     #MuMIn to calculate r2 for mixed models

#library(ggplot2)

#library(ggeffects)
#library(leaps)
#library(gridExtra)


## DATA
#load 10% incidental phenometrics, select fields, add width of confidence int.
inat10<-read_csv("Emperical_Results/naive_tenth_inat_estimates.csv") %>% 
  rename(i.est=estimate, ilow=low_ci, ihigh=high_ci, i.npresence=obs) %>%
  mutate(i.ci=ihigh-ilow, metric=10)

#load 50% incidental phenometrics, select fields, add width of confidence int.
inat50<-read_csv("Emperical_Results/naive_fiftieth_iNat_estimates.csv") %>% 
  rename(i.est=estimate, ilow=low_ci, ihigh=high_ci, i.npresence=obs) %>%
  mutate(i.ci=ihigh-ilow, metric=50)


#load pollard phenometrics
load("data/pollard_indices.RData")
#select fields for 10% metrics, add width of confidence int.
pollard10<-pollardM %>%
  rename(p.nsites=nsites, p.nsurveys=nvisits, p.npresence=ncounts, 
         p.est=estimate10, plow=ci10.low95p, phigh=ci10.high95p) %>%
  dplyr::select(scientificName:phigh) %>%
  mutate(p.ci=phigh-plow, metric=10) 
#select fields for 50% metrics, add width of confidence int.
pollard50<-pollardM %>%
  rename( p.nsites=nsites, p.nsurveys=nvisits, p.npresence=ncounts, 
         p.est=estimate50, plow=ci50.low95p, phigh=ci50.high95p) %>%
  dplyr::select(scientificName:ci.repN,p.est:phigh) %>%
  mutate(p.ci=phigh-plow, metric=50) 

#remove raw survey metrics table
rm(pollardM)

#load species traits
#species traits from excel, filter to species with clear overwinter stage, set factors
#wing.mean, wing.max, canopy.open, canopy.mixed, canopy.closed, host.breadth.index, voltinism, egg.clusters,
traits<-read_excel("data/LarsenEtAl_Appendix1b.xlsx", sheet="spp_TRAITS_el") %>%
  rename(scientificName=scientific_name, local.abund=local.abundance.description, ows=overwinter.stage, mobility=Mobility_code, detect=detectability.index, confus = Confusability) %>%
  filter(ows %in% c("adult","pupa","larva","migrant")) %>%
  mutate(ows=as.factor(ows),
         local.abund=as.factor(local.abund)) %>%
  dplyr::select(scientificName,mobility:local.abund,host.breadth.index,voltinism,ows,egg.clusters,detect,confus)
  
#refactor levels for overwinter stage, local abundance
traits$ows<-fct_relevel(traits$ows,c("adult","pupa","larva","migrant"))
traits$local.abund<-fct_relevel(traits$local.abund,c("Uncommon","Common"))

#look at correlations among species traits
p.mat <- cor(traits[,c(2:7,9:10,12:14)])
colnames(p.mat) <- rownames(p.mat) <- colnames(traits)[c(2:7,9:10,12:14)]
corrplot(p.mat, method="number", sig.level = 0.05)
#wing mean and wing max are highly correlated, detectability is also somewhat correlated

##load gdd, select fields
source('Rcode/gdd.R')
gdd<-gdd %>% 
  #select(lat_bin, lon_bin, year, sumgdd, log.gdd) %>%
  #mutate(dv=1) %>% group_by(dv) %>%
  #mutate(mgd=mean(sumgdd, na.rm=T)) %>%
  #group_by(lat_bin, lon_bin, year) %>%   #mutate(gdd.dev=(log.gdd-mgd)/10) %>%
  dplyr::select(lat_bin, lon_bin, year, log.gdd)

##Combine variables in data frames for analysis

#10% phenometric dataset (emergence) 
#combine metrics from surveys and incidental data in one dataframe
pheno10<-merge(pollard10, inat10, by=intersect(names(inat10), names(pollard10))) 

#add gdd (by year-grid cell) to dataframe
pheno10<-merge(pheno10, gdd, by=intersect(names(pheno10), names(gdd)))
#add traits (by species) to dataframe
pheno10<-merge(pheno10,traits, by.x="scientificName", by.y="scientificName")

##Review stats of overlapping dataset
#n species with overlap
length(unique(pheno10$scientificName))
#n grid cells with overlap
length(unique(paste(pheno10$lat_bin, pheno10$lon_bin)))
#years with overlap
sort(unique(pheno10$year))


#50% dataset (mid-season)
#combine metrics from surveys and incidental data in one dataframe
pheno50<-merge(pollard50, inat50, by=intersect(names(inat50), names( pollard50)))
#add gdd (by year-grid cell) to dataframe
pheno50<-merge(pheno50, gdd, by=intersect(names(pheno50), names(gdd)))
#add traits (by species) to dataframe
pheno50<-merge(pheno50,traits, by.x="scientificName", by.y="scientificName")

rm(gdd, inat10, inat50, p.mat, pollard10, pollard50, traits)
survey10<-pheno10 %>% dplyr::select(scientificName,p.est,log.gdd:confus) %>% mutate(wing.mean=wing.mean/10, wing.max=wing.max/10)
survey50<-pheno50 %>% dplyr::select(scientificName,p.est,log.gdd:confus) %>% mutate(wing.mean=wing.mean/10, wing.max=wing.max/10)
incid10<-pheno10 %>% dplyr::select(scientificName,i.est,log.gdd:confus) %>% mutate(wing.mean=wing.mean/10, wing.max=wing.max/10)
incid50<-pheno50 %>% dplyr::select(scientificName,i.est,log.gdd:confus) %>% mutate(wing.mean=wing.mean/10, wing.max=wing.max/10)
(rm(pheno10, pheno50))
datasets<-list(incid10, survey10, incid50, survey50)

save(datasets, file="data/input_for_models.RData")