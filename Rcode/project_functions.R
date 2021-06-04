#Project: Parallel analyses of phenometrics from incidental & survey data
#Survey data: NABMN surveys (pollardbase.org, Ohio)
#Incidental data: iNaturalist & eButterfly (metrics provided by M Belitz, UF)
#Elise Larsen, 2020-06 Updated through 2021-5
#Georgetown University 

#Here: Defining functions used in analysis

#Phenometric estimation is based on rbms package
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("RetoSchmucki/rbms")
library(tidyverse)
library(lubridate)
library(rbms)

#### CALCULATING PHENOMETRICS FOR SURVEY DATA
#Function pheno_bootstrap adjusts the rbms regional GAM code to include incides which allows for bootstrapping.
#Function is designed to be called by boot()
#Function pheno_bootstrap returns the daily cumulative flight curve and the 10% and 50% DOY phenometrics 
pheno_bootstrap<-function( mydata,              #survey data for specific species-region
                           indices,             #indices for bootstrapping
                           StartMonth = 4,      #rbms parameter: default start month is April
                           EndMonth = 10,       #rbms parameter: default end month is October
                           StartDay = 1,        #rbms parameter: default startday is 1
                           EndDay = NULL,       #rbms parameter: no default end day
                           CompltSeason = TRUE, #rbms parameter: only run years with sufficient sampling
                           Anchor = TRUE,       #rbms parameter: set anchor with relative abundance=0 at ends of sampling
                           AnchorLength = 7,    #rbms parameter: default length of anchor is 7 days
                           AnchorLag = 14) {    #rbms parameter: default lag for anchor on each end of survey season
  #Filter data by indices for bootstrap confidence intervals
  mydata<-mydata %>% mutate(DATE=as.Date(DATE, format="%m/%d/%Y"))
  mydata<-mydata[indices,]
  visit<-mydata %>%
    group_by(SITE_ID, DATE) %>%
    select(SITE_ID, DATE)
  count<-mydata %>%
    filter(COUNT>0)

  #Assign year, species parameters from data
  this_year<-as.numeric(year(mydata$DATE[1]))
  this_species<-count$SPECIES[1]
  
  #Format data for GAM
  ts_date <- rbms::ts_dwmy_table(InitYear = this_year, LastYear = this_year, WeekDay1 = 'monday')

  ts_season <- rbms::ts_monit_season(ts_date, StartMonth = StartMonth, EndMonth = EndMonth, StartDay = StartDay, EndDay = EndDay,
                                     CompltSeason = CompltSeason, Anchor = Anchor, AnchorLength = AnchorLength, AnchorLag = AnchorLag)
  ts_season_visit <- rbms::ts_monit_site(visit, ts_season)
  
  ts_season_count <- rbms::ts_monit_count_site(ts_season_visit, count, sp = this_species)
  
  ## compute the flight curve, using the rbms regionalGAM method
  ##=========================================
  dataset_y <- ts_season_count[, .(SPECIES, SITE_ID, DATE, WEEK, WEEK_DAY, DAY_SINCE, M_YEAR, M_SEASON, COUNT, ANCHOR)]
  dataset_y[, trimDAYNO := DAY_SINCE]

  #Because data is subset for bootstrapping, we've lowered the data thresholds in the GAM.  
  system.time(
    ts_flight_curve <- rbms::flight_curve(ts_season_count, NbrSample = 200, MinVisit = 2, MinOccur = 1, MinNbrSite = 1,
                                          MaxTrial = 3, FcMethod = 'regionalGAM', GamFamily = 'nb', SpeedGam = FALSE, CompltSeason = T)
  )
  #Returns NM relative abundance metric for each day of season, sums to 1.

  #Calculate cumulative proportion of flight curve abundance by day (accumulated area under the flight curve)
  test1<-ts_flight_curve$pheno %>%
    group_by(M_YEAR) %>%
    filter(M_YEAR %in% count$YEAR) %>%
    mutate(bd=cumsum(NM))
  
  #Calculate phenometric DOYs for 10% and 50% percentiles
  #10% area under the curve
  p10<-test1[(test1$bd/max(test1$bd))>=0.1,]$trimDAYNO[1]
  #50% area under the curve
  p50<-test1[(test1$bd/max(test1$bd))>=0.5,]$trimDAYNO[1]
  #return daily cumulative proportion of butterflies and selected phenometrics
  return(c(p10,p50,test1$bd[1:365]))
  
}
##end bootstrapped phenometrics


##### RESULTS TABLES

##Function getparamests extracts parameters of interest from best survey and incidental models
#Function getparamests returns summary results tables
getparamests<-function(model.survey,  #best statistical model for survey data phenometric
                       model.inc) {   #best statistical model for incidental data phenometric
  surv<-summary(model.survey)$coefficients
  inc<-summary(model.inc)$coefficients
  ms1<-cbind(round(surv[,1],1),round(surv[,2],1),surv[,5])
  mi1<-cbind(round(inc[,1],1),round(inc[,2],1),inc[,5])
  if(nrow(ms1)>nrow(mi1)) {
    nparam<-nrow(ms1)
    while(nrow(mi1)< nparam) {mi1<-rbind(mi1,c(rep(NA,3))) }
    row.names(mi1)<-row.names(ms1)
  } else {
    nparam<-nrow(mi1)
    while(nrow(ms1)< nparam) {ms1<-rbind(ms1,c(rep(NA,3))) }
    row.names(ms1)<-row.names(mi1)
  }
  r2s<-cbind(round(t(r.squaredGLMM(model.survey)),2),c(NA,NA),c(NA,NA))
  r2i<-cbind(round(t(r.squaredGLMM(model.inc)),2),c(NA,NA),c(NA,NA))
  
  ms1<-rbind(ms1,r2s) 
  mi1<-rbind(mi1,r2i)
  table.out<-as.data.frame(cbind(ms1,mi1))
  names(table.out)<-c("Survey Estimate","Surv. SE","Surv. p value","Incidental Estimate","Inc. SE","Inc. p value")
  return(table.out)  
}


