#Project: Parallel analyses of phenometrics from incidental & survey data
#Survey data: NABMN surveys (pollardbase.org, Ohio)
#Incidental data: iNaturalist & eButterfly (metrics provided by M Belitz, UF)
#Elise Larsen, 2020-06 Updated through 2021-5
#Georgetown University 

#Here: Statistical models of phenological patterns


#load phenometrics and explanatory variables
source("Rcode/pheno_data.R")


#load project functions
source("Rcode/project_functions.R")



#wing.mean and wing.max are highly correlated, so remove wing.mean
incid10<-incid10 %>% dplyr::select(!wing.mean)
survey10<-survey10 %>% dplyr::select(!wing.mean)
incid50<-incid50 %>% dplyr::select(!wing.mean)
survey50<-survey50 %>% dplyr::select(!wing.mean)
######################################################################
#### PHENOLOGY ANALYSIS + MODEL SELECTION FOR EACH SET OF PHENOMETRICS
library(lmerTest)

## SURVEY 10%
# Full model formula: interaction of log.gdd and ows (overwinter stage); then all other parameters
#log.gdd is column 3; ows is column 12
formula.surv<-paste("p.est ~ -1 + log.gdd*ows",sep="")
for(i in c(4:11,13:15)) {formula.surv<-paste(formula.surv," + ",names(survey10)[i], sep="")}
formula.surv<-formula(formula.surv)
#run full mixed effects model
full.lm <- lm(formula.surv, data = survey10)
step.lm <- stepAIC(full.lm, direction = "both",  trace = FALSE)
(vars<-row.names(summary(step.lm)$coefficients))

#Fit Mixed effects model
full.model<-lmer(p.est~ -1 + log.gdd*ows + mobility + wing.max + canopy.open + canopy.mixed + canopy.closed + egg.clusters + local.abund + voltinism + confus + (0 + log.gdd|scientificName), data= survey10)
f1fin<-get_model(step(full.model))
extractAIC(f1fin)
r.squaredGLMM(f1fin)
ranova(f1fin)
#log.gdd slope in random effect does not improve model
##boundary fit:full.model2<-lmer(p.est~ -1 + log.gdd*ows + local.abund + (1|detect) + (1|scientificName), data= survey10)
full.model2<-lmer(p.est~ -1 + log.gdd*ows + local.abund + (1|confus/scientificName), data= survey10)
full.model2<-lmer(p.est~ -1 + log.gdd*ows + local.abund + (1|confus)  + (1|scientificName), data= survey10)
f2fin<-get_model(step(full.model2))
extractAIC(f2fin)
r.squaredGLMM(f2fin)
ranova(f2fin)

best.s10<-f2fin

rm(full.lm, step.lm, full.model, f1fin, full.model2, f2fin)

### INCIDENTAL 10%
# Fit fixed effect model
formula.inc<-paste("i.est ~ -1 + log.gdd*ows",sep="")
for(i in c(4:11,13:15)) {formula.inc<-paste(formula.inc," + ",names(incid10)[i], sep="")}
formula.inc<-formula(formula.inc)
full.lm <- lm(formula.inc, data = incid10)
step.lm <- stepAIC(full.lm, direction = "both",  trace = FALSE)
(vars<-row.names(summary(step.lm)$coefficients))

#Mixed effects model
full.model<-lmer(i.est~ -1 + log.gdd*ows + wing.max + canopy.mixed + canopy.closed + host.breadth.index + detect + (0 + log.gdd|scientificName), data= incid10)
f1fin<-get_model(step(full.model))
extractAIC(f1fin)
r.squaredGLMM(f1fin)
ranova(f1fin)
#log.gdd slope in random effect does not improve model

#Failed to converge: model2<-lmer(i.est~ -1  + log.gdd*ows +  (1|detect/scientificName), data= incid10)
#Boundary fit: model2<-lmer(i.est~ -1  + log.gdd*ows +  (1|detect) , data= incid10)
#Boundary fit: model2<-lmer(i.est~ -1  + log.gdd*ows + (1|detect) + (1|scientificName) , data= incid10)
#Boundary fit: model2<-lmer(i.est~ -1  + log.gdd*ows + (1|confus) + (1|scientificName) , data= incid10)
model2<-lmer(i.est~ -1  + log.gdd*ows + (1|scientificName) , data= incid10)

f2fin<-get_model(step(model2))
extractAIC(f2fin)
r.squaredGLMM(f2fin)
ranova(f2fin)
f2fin<-lmer(i.est~ -1  + log.gdd + ows + (1|scientificName) , data= incid10)

best.i10<-f2fin

rm(full.lm, step.lm, full.model, f1fin, model2, f2fin)

## SURVEY 50%
# Fit fixed effect model
full.lm <- lm(formula.surv, data = survey50)
step.lm <- stepAIC(full.lm, direction = "both",  trace = FALSE)
(vars<-row.names(summary(step.lm)$coefficients))


#Mixed effects model
full.model<-lmer(p.est~ -1 + log.gdd*ows  + canopy.open + canopy.closed + local.abund + host.breadth.index + voltinism + egg.clusters +  (0 + log.gdd|scientificName), data= survey50)
f1fin<-get_model(step(full.model))
extractAIC(f1fin)
r.squaredGLMM(f1fin)
ranova(f1fin)
#log.gdd slope in random effect does not improve model
#Boundary fit: full.model2<-lmer(p.est~ -1 + log.gdd*ows  + canopy.open + canopy.closed + local.abund + host.breadth.index + voltinism + egg.clusters + ( 1|confus), data= survey50)
model2<-lmer(p.est~ -1 + log.gdd*ows  + canopy.open + canopy.closed + local.abund + host.breadth.index + voltinism + egg.clusters + ( 1|scientificName), data= survey50)
f2fin<-get_model(step(model2))
extractAIC(f2fin)
r.squaredGLMM(f2fin)
ranova(f2fin)
model2<-lmer(p.est~ -1 + log.gdd + ows + local.abund + (1|confus) +  ( 1|scientificName), data= survey50)
f2fin<-get_model(step(model2))
extractAIC(f2fin)
r.squaredGLMM(f2fin)

best.s50<-f2fin
rm(full.lm, step.lm, full.model, f1fin, full.model2, f2fin)

### INCIDENTAL 50%
# Fit fixed effect model
full.lm <- lm(formula.inc, data = incid50)
step.lm <- stepAIC(full.lm, direction = "both",  trace = FALSE)
(vars<-row.names(summary(step.lm)$coefficients))

#Mixed effects model
full.model<-lmer(i.est~ -1 + log.gdd*ows + canopy.closed + local.abund + host.breadth.index + voltinism + egg.clusters +(0 + log.gdd|scientificName), data= incid50)
f1fin<-get_model(step(full.model))
extractAIC(f1fin)
r.squaredGLMM(f1fin)
ranova(f1fin)
#log.gdd slope in random effect does not improve model
model2a<-lmer(i.est ~ -1 + log.gdd*ows + host.breadth.index + (1|detect/scientificName), data= incid50)
r.squaredGLMM(model2a)
model2b<-lmer(i.est ~ -1 + log.gdd*ows + host.breadth.index + (1|confus/scientificName), data= incid50)
r.squaredGLMM(model2b)
ranova(model2a)
ranova(model2b)
anova(model2a, model2b)
#model2b SLIGHTLY better
model2<-model2b
f2fin<-get_model(step(model2))
extractAIC(f2fin)
r.squaredGLMM(f2fin)
ranova(f2fin)
f3fin<-lmer(i.est ~ -1 + ows + host.breadth.index + (1|scientificName), data= incid50)
r.squaredGLMM(f3fin)
ranova(f3fin)
extractAIC(f3fin) #lower AIC

best.i50<-f3fin


##Table 1: Best model parameters
params.10<-c("log.gdd","adult diapause (0/1)","pupal diapause (0/1)","larval diapause (0/1)","migrant (0/1)","locally common (0/1)","marginal r2","conditional r2")
params.50<-c("adult diapause (0/1)","pupal diapause (0/1)","larval diapause (0/1)","migrant (0/1)","host breadth index","marginal r2","conditional r2")

#Use getparamests function from project_functions.R
#Create tables with model output
(table10<-getparamests(best.s10,best.i10))
(table50<-getparamests(best.s50,best.i50))

write.csv(table10, file="Larsen_etal_Table1.csv")
write.csv(table50, file="Larsen_etal_Table5.csv")

save(best.i10, best.s10, best.i50, best.s50, file="Rcode/finalmodels.RData" )

