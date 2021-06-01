#Project: Parallel analyses of phenometrics from incidental & survey data
#Survey data: NABMN surveys (pollardbase.org, Ohio)
#Incidental data: iNaturalist & eButterfly (metrics provided by M Belitz, UF)
#Elise Larsen, 2020-06 Updated through 2021-5
#Georgetown University 

#Here: Statistical summary and models of confidence intervals


#libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)


#Load all calculated phenometrics
load("data/combined_phenometrics.RData")

#summary and analysis of 10% confidence intervals
summary(pheno10$i.ci)
i10.conf.lmer<-lmer(i.ci~i.npresence+detect+confus+(1|scientificName), data=pheno10)
i10.conf<-get_model(step(i10.conf.lmer))
summary(i10.conf)
i10.conf.lmer<-lmer(i.ci~i.npresence+wing.max+confus+(1|scientificName), data=pheno10)
i10.conf<-get_model(step(i10.conf.lmer))
summary(i10.conf)
i10.conf.lmer<-lmer(i.ci~i.npresence+wing.mean+confus+(1|scientificName), data=pheno10)
i10.conf<-get_model(step(i10.conf.lmer))
  summary(i10.conf)
r.squaredGLMM(i10.conf)

summary(pheno10$p.ci)
s10.conf.lmer<-lmer(p.ci~p.nsites+p.nsurveys+p.npresence+detect+confus+(1|scientificName), data=pheno10)
s10.conf<-get_model(step(s10.conf.lmer))
summary(s10.conf)
s10.conf.lmer<-lmer(p.ci~p.nsites+p.nsurveys+p.npresence+wing.max+confus+(1|scientificName), data=pheno10)
s10.conf<-get_model(step(s10.conf.lmer))
summary(s10.conf)
s10.conf.lmer<-lmer(p.ci~p.nsites+p.nsurveys+p.npresence+wing.mean+confus+(1|scientificName), data=pheno10)
s10.conf<-get_model(step(s10.conf.lmer))
s10.conf.lmer<-lmer(p.ci~p.nsites+p.nsurveys+p.npresence+wing.max+confus+(1|scientificName), data=pheno10)
s10.conf<-get_model(step(s10.conf.lmer))
summary(s10.conf)
r.squaredGLMM(s10.conf)

#summary and analysis of 50% confidence intervals
summary(pheno50$i.ci)
i50.conf.lmer<-lmer(i.ci~i.npresence+detect+confus+(1|scientificName), data=pheno50)
i50.conf<-get_model(step(i50.conf.lmer))
summary(i50.conf)
i50.conf.lmer<-lmer(i.ci~i.npresence+wing.max+confus+(1|scientificName), data=pheno50)
i50.conf<-get_model(step(i50.conf.lmer))
summary(i50.conf)
i50.conf.lmer<-lmer(i.ci~i.npresence+wing.mean+confus+(1|scientificName), data=pheno50)
i50.conf<-get_model(step(i50.conf.lmer))
summary(i50.conf)
r.squaredGLMM(i50.conf)

summary(pheno50$p.ci)
s50.conf.lmer<-lmer(p.ci~p.nsites+p.nsurveys+p.npresence+detect+confus+(1|scientificName), data=pheno50)
s50.conf<-get_model(step(s50.conf.lmer))
summary(s50.conf)
s50.conf.lmer<-lmer(p.ci~p.nsites+p.nsurveys+p.npresence+wing.max+confus+(1|scientificName), data=pheno50)
s50.conf<-get_model(step(s50.conf.lmer))
summary(s50.conf)
s50.conf.lmer<-lmer(p.ci~p.nsites+p.nsurveys+p.npresence+wing.mean+confus+(1|scientificName), data=pheno50)
s50.conf<-get_model(step(s50.conf.lmer))
summary(s50.conf)
r.squaredGLMM(s50.conf)


#load phenometrics and explanatory variables used in final analysis
source("Rcode/pheno_data.R")
analysis.domain<-survey10 %>%
  group_by(lat_bin, lon_bin, year, scientificName) %>%
  tally()
