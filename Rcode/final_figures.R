#Project:  butterfly flight phenology drivers from incidental and survey data
#Survey data: NABMN surveys (pollardbase.org, Ohio)
#Incidental data: iNaturalist & eButterfly (metrics provided by M Belitz, UF)
#Elise Larsen, 2020-06 Updated through 2021-5
#Georgetown University 
#MS Figures
##Here: All code needed to produce the figures in submitted manuscript.


library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(grid)
library(ggpubr)
library(boot)
library(viridis)
#Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
#Please cite ggmap if you use it! See citation("ggmap") for details.

#species list
splist<-read_csv("data/species_list.csv")

#### FIGURE 1
#Load & summarize incidental data
incid<-read_csv("data/obs10_by-cell-spp-year.csv") %>%
  filter( between(lat_bin,36,42), between( lon_bin, -94, -76 )) %>%
  filter(scientificName %in% splist$scientificName, between(year, 2012,2018)) %>%
  group_by(lat_bin, lon_bin, scientificName) %>%
  tally(name="nyears") %>%
  group_by(lat_bin, lon_bin, nyears) %>%
  tally(name="nsp") %>%
  arrange(-nyears) %>%
  group_by(lat_bin, lon_bin) %>%
  mutate(NSP=cumsum(nsp)) %>%
  arrange(lat_bin, lon_bin)

#Load & Summarize survey data

survey<-read_csv(file="data/survey_sp_summary.csv") %>%
  select(lat_bin, lon_bin, scientificName, year) %>%
  filter(scientificName %in% splist$scientificName) %>%
  group_by(lat_bin, lon_bin, scientificName, year) %>%
  summarize(n=1) %>%
  group_by(lat_bin, lon_bin, scientificName, n) %>%
  summarize(nyears=sum(n, na.rm=T)) %>%
  group_by(lat_bin, lon_bin, nyears) %>%
  summarize(nsp=sum(n)) %>%
  arrange(-nyears) %>%
  group_by(lat_bin, lon_bin) %>%
  mutate(NSP=cumsum(nsp)) %>%
  arrange(lat_bin, lon_bin)


#Load map data
load("data/regionalMap.RData")


p<-ggmap(map2) + 
  theme_void() + xlim(-95,-70) + ylim(35,45) + 
  ggtitle("") + 
  theme(
    plot.title = element_text(colour = "black"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )

survey.map<-p + geom_polygon(fill = "gray", color = "darkgray") + 
  geom_point(data=filter(survey, nyears==7), aes(lon_bin,lat_bin, size=nyears, color=NSP), inherit.aes=F) +
  geom_point(data=filter(survey, nyears==6), aes(lon_bin,lat_bin, size=nyears, color=NSP), inherit.aes=F) +
  geom_point(data=filter(survey, nyears==5), aes(lon_bin,lat_bin, size=nyears, color=NSP), inherit.aes=F) +
  geom_point(data=filter(survey, nyears==4), aes(lon_bin,lat_bin, size=nyears, color=NSP), inherit.aes=F) +
  geom_point(data=filter(survey, nyears==3), aes(lon_bin,lat_bin, size=nyears, color=NSP), inherit.aes=F) +
  geom_point(data=filter(survey, nyears==2), aes(lon_bin,lat_bin, size=nyears, color=NSP), inherit.aes=F) +
  geom_point(data=filter(survey, nyears==1), aes(lon_bin,lat_bin, size=nyears, color=NSP), inherit.aes=F) + 
  ggtitle("Survey data density") + xlim(-95,-73) + ylim(35,44) + 
  scale_color_gradient(name="N. Species", limits=c(1,40), low="deepskyblue", high="black") + 
  theme(legend.position="bottom", panel.border = element_rect(colour = "black", fill=NA),
        axis.text = element_text(colour = 1, size = 11),
        legend.background = element_blank(), legend.margin=margin(c(5,5,5,5)),
        legend.box.background = element_rect(colour = "black")) +  scale_size_continuous(name="N. Years")  

survey.map
# Extract the legend. Returns a gtable
leg <- as_ggplot(get_legend(survey.map))
survey.map <- survey.map + theme(legend.position='none')+labs(tag="A")


incid.map<-p + geom_polygon(fill = "gray", color = "darkgray") + 
  geom_point(data=filter(incid, nyears==7), aes(lon_bin,lat_bin, color=NSP), size=4, inherit.aes=F) +
  geom_point(data=filter(incid, nyears==6), aes(lon_bin,lat_bin, color=NSP), size=3.5, inherit.aes=F) +
  geom_point(data=filter(incid, nyears==5), aes(lon_bin,lat_bin, color=NSP), size=3, inherit.aes=F) +
  geom_point(data=filter(incid, nyears==4), aes(lon_bin,lat_bin, color=NSP), size=2.5, inherit.aes=F) +
  geom_point(data=filter(incid, nyears==3), aes(lon_bin,lat_bin, color=NSP), size=2, inherit.aes=F) +
  geom_point(data=filter(incid, nyears==2), aes(lon_bin,lat_bin, color=NSP), size=1.5, inherit.aes=F) +
  geom_point(data=filter(incid, nyears==1), aes(lon_bin,lat_bin, color=NSP), size=1, inherit.aes=F) + 
  ggtitle("Incidental data density") + xlim(-95,-73) + ylim(35,44) + 
  scale_color_gradient(name="N. Species", limits=c(0,40), low="deepskyblue", high="black") + 
  theme(legend.position="bottom", panel.border = element_rect(colour = "black", fill=NA),
        axis.text = element_text(colour = 1, size = 11),
        legend.background = element_blank(), legend.margin=margin(c(5,5,5,5)),
        legend.box.background = element_rect(colour = "black")) +  scale_size_continuous(name="N. Years")  + 
        theme(legend.position="none") +labs(tag="B")

incid.map

##Combine panels into Figure 1:
figure <- grid.arrange(survey.map, incid.map, leg, ncol = 1, nrow = 3, widths=c(5), heights=c(4,4,1.5))
figure
ggsave("figs/Larsen_etal_Fig1.png", plot=figure, width = 7, height = 7, dpi = 300, units = "in", device='png')



#### Figure 2

#Filter for data
lat.f2<-41.5
lon.f2<-(-83.5)
year.f2<-2017
sp.f2<-"Speyeria cybele"
#Survey data
load("data/survey_speyeria.RData")
tenth.ci<-quantile(boot.out$t[,1], probs=c(0.025,0.975))
fiftieth.ci<-quantile(boot.out$t[,2], probs=c(0.025,0.975))

survey_samp<-read_csv("data/survey_speyeriadata.csv") %>%
  mutate(npres=ifelse((COUNT==0),0,1))


surv.speyeria<-data.frame(yday=numeric(),est=numeric(),lowci=numeric(),highci=numeric())
for(i in 1:365) { #loop through days of year
  result.ci<-boot.ci(boot.out, index=i+2, conf = 0.95, type = "perc")
  surv.speyeria[nrow(surv.speyeria)+1,]<-c(i,boot.out$t0[i+2], round(result.ci$percent[4],4), round(result.ci$percent[5],4))
}

surv.speyeria<-surv.speyeria %>%
  mutate(dailyprob=est*1000) %>%
  arrange(yday)

for(i in c(2:365)) {surv.speyeria$dailyprob[i]<-(surv.speyeria$est[i]-surv.speyeria$est[i-1])*6000}

(FIG2survey<-ggplot(data=filter(surv.speyeria,yday<361), aes(x=yday,y=dailyprob)) +
    geom_rect(aes(xmin=tenth.ci[1],xmax=tenth.ci[2], ymin=-Inf, ymax=Inf), fill="gold1", alpha=0.25) +
    geom_rect(aes(xmin=fiftieth.ci[1],xmax=fiftieth.ci[2], ymin=-Inf, ymax=Inf), fill="cadetblue1", alpha=0.25) +
    geom_line(data=filter(surv.speyeria,yday<361), aes(x=yday,y=dailyprob)) +
    #  geom_vline(xintercept=round(tenth.ci$percent[4]),color="black", linetype=3) +
    #  geom_vline(xintercept=round(tenth.ci$percent[5]),color="black", linetype=2) +
    geom_vline(xintercept=boot.out$t0[1], linetype=3) +
    geom_vline(xintercept=boot.out$t0[2], linetype=2) +
    xlim(100,320) + theme_classic() + theme(axis.text.y = element_blank()) +
    geom_histogram(data=survey_samp, aes(x=yday, fill = as.factor(npres)), colour="black", binwidth=7,  inherit.aes=F) + #xlim(190, 230) +
    scale_fill_manual(values=c("white","black"), guide=F) +
    labs(x="Day of year", y="Relative abundance") +
    labs(tag="A")  )
FIG2survey

#Incidental data
incid_sample<-read_csv("data/incid_speyeria.csv") 


incid10<-read_csv("data/naive_tenth_iNat_estimates.csv") %>%
  filter(lat_bin==lat.f2,lon_bin==lon.f2, year==year.f2, scientificName==sp.f2)
incid50<-read_csv("data/naive_fiftieth_iNat_estimates.csv") %>%
  filter(lat_bin==lat.f2,lon_bin==lon.f2, year==year.f2, scientificName==sp.f2)

incid.hist<-hist(incid_sample$day, breaks=c(10:50)*7)
inat.h<-(incid.hist$breaks + ((max(incid.hist$breaks)-min(incid.hist$breaks))/(length(incid.hist$breaks)-1)/2))[c(1:length(incid.hist$counts))]
inat.c<-incid.hist$counts/2500

FIG2incid<- ggplot() +
  xlim(100,300) + 
  geom_rect(aes(xmin=incid10[1,2][[1]],xmax=incid10[1,3][[1]],ymin=-Inf,ymax=Inf), alpha=0.6, fill="gold1") + 
  geom_rect(aes(xmin=incid50[1,2][[1]],xmax=incid50[1,3][[1]],ymin=-Inf,ymax=Inf), alpha=0.6, fill="cadetblue1") + 
  geom_density(incid_sample, mapping = aes(day)) +
  geom_vline(xintercept = incid10[1,1][[1]], color = "black", linetype=3) +
  geom_vline(xintercept = incid50[1,1][[1]], color = "black", linetype=2) +
  labs(x = "Day of year", y = "Data density") +
  theme_classic() + theme(axis.text.y = element_blank()) + 
  geom_col(aes(x=(inat.h), y=inat.c), color="black") + labs(tag="B")

FIG2incid


##Combine panels into Figure 1:
figure2 <- grid.arrange(FIG2survey, FIG2incid,  ncol = 2, nrow = 1)
figure2
ggsave("figs/Larsen_etal_Fig2.png", plot=figure2, width = 6, height = 2.5, dpi = 300, units = "in", device='png')



###########################################
## FIGURE 3: Model predictions
#### PLOTS FOR GDD MODEL FIT BY OVERWINTER STRATEGY

library(merTools)
#load pheno data
load("data/combined_phenometrics.RData")

#load paramterized models
datasets<-list(pheno10, pheno10, pheno50, pheno50)
finalmodels<-list(best.i10, best.s10, best.i50, best.s50)
titles<-c("B. 10% metrics from incidental data","A. 10% metrics from survey data",
          "D. 50% metrics from incidental data", "C. 50% metrics from survey data")
ylim.i<-list(c(90,220),c(90,220),c(150,260),c(150,260))
fig3panels<-list()
for(i in 1:4) {
  if(i %in% c(1,3)) {
    data.i<-datasets[[i]] %>% rename(est=i.est)
    } else {
      data.i<-datasets[[i]] %>% rename(est=p.est)
    }
  fm1 <- lm(formula=est~log.gdd+ows, data=data.i)
  fm1<-stepAIC(fm1)
  df3 = cbind(data.i, predict(fm1,interval = "confidence"))
   
  fig3panels[[i]]<- ggplot(data=df3, mapping=aes(x=exp(log.gdd), y=est, color=ows)) +  
    geom_ribbon(data=df3, aes(x=exp(log.gdd), ymin=lwr, ymax=upr, group=ows), color="gray", size=0, fill="gray", alpha=0.6) + 
    geom_line(mapping=aes(y=fit, color=ows), size=1.5) + 
    scale_color_viridis(discrete=TRUE) + ylim(ylim.i[[i]]) +
    scale_x_continuous(name='GDD (Jan-Jun)', trans="identity", labels=waiver()) + 
    theme_classic()+labs(y="DOY prediction", title=titles[i]) + theme(legend.position = "none")
  
}
#combine plots

# Extract the legend. Returns a gtable
forlegend<-ggplot(data=df3, mapping=aes(x=exp(log.gdd), y=est, color=ows)) +  
  geom_ribbon(data=df3, aes(x=exp(log.gdd), ymin=lwr, ymax=upr, group=ows), color="gray", size=0, fill="gray", alpha=0.6) + 
  geom_line(mapping=aes(y=fit, color=ows), size=2) +
  scale_color_viridis(discrete=TRUE) + labs(color="Overwinter stage:") +
  scale_x_continuous(name='GDD (Jan-Jun)', trans="identity", labels=waiver()) + 
  theme_classic()+labs(y="DOY prediction", title=titles[i], legend="Overwinter stage:") + 
  theme(legend.position = "bottom", legend.text=element_text(size=13), legend.title=element_text(size=13))

leg3 <- ggpubr::as_ggplot(ggpubr::get_legend(forlegend))

(fig3<-grid.arrange(fig3panels[[2]],fig3panels[[1]],fig3panels[[4]],fig3panels[[3]],leg3, nrow=3, layout_matrix = rbind(c(1,2),c(3,4),c(5,5)),heights=c(4,4,1)))
 

ggsave("figs/Larsen_etal_Figure3.png",fig3,width = 8, height = 8, dpi = 300, units = "in")


###########################################
##  SUPPLEMENTAL FIGURE 1: Compare emergences to "day 0"

day0pheno<-read_csv("data/day0_combined.csv")
day0pheno<-merge(day0pheno, splist[,c(1:2)], by="scientificName")


(day0plot<-ggplot(data=day0pheno, aes(x=scientificName, y=emerg.lag, fill=datasource)) + 
    geom_boxplot() + theme_light() + geom_hline(yintercept=7) + 
    labs(title="", x="Species", y="# days (estimate - day0)") + 
    scale_fill_manual(name="Data source:", labels=c("Incidental","Survey"), values=c("palegreen","dodgerblue3"),guide = guide_legend(reverse = FALSE)) + 
    theme(legend.position='top', axis.text.x = element_text(size=8,face="italic",angle = 75, vjust = 1, hjust=1)) + 
    scale_x_discrete(labels=sort(unique(day0pheno$sciname))) )

ggsave("figs/Larsen_etal_S2Fig1.png", day0plot, width=6, height=5, units="in")




###########################################
##  SUPPLEMENTAL FIGURE 2: Phenometrics by overwinter stage
pheno.long<-pheno10 %>%
  pivot_longer(c(p.est,i.est), names_to="datasource", values_to="est.10")
pheno.long$ows<-fct_relevel(pheno.long$ows,c("adult","pupa","larva","migrant"))


(fig.owsa<-ggplot(data=pheno.long, aes(x=ows,y=est.10, fill=datasource) ) + 
    geom_boxplot() + ylim(60,300) + 
    scale_fill_manual(name="Data source:", labels=c("Incidental","Survey"), values=c("palegreen","dodgerblue3"),guide = FALSE)  + 
    theme_classic() + labs(x="Overwinter strategy", y="10% Estimate", tag="A") )

pheno50.long<-read_csv("data/midseason_indices.csv")  %>%
  dplyr::select(scientificName,p.est,i.est,ows) %>%
  pivot_longer(c(p.est,i.est), names_to="datasource", values_to="est.50")
pheno50.long$ows<-fct_relevel(pheno50.long$ows,c("adult","pupa","larva","migrant"))


(fig.owsb<-ggplot(data=pheno50.long, aes(x=ows,y=est.50, fill=datasource) ) + 
    geom_boxplot() + ylim(60,300) + 
    scale_fill_manual(name="Data source:", labels=c("Incidental","Survey"), values=c("palegreen","dodgerblue3"),guide = guide_legend(reverse = FALSE) )  + 
    theme_classic() + labs(x="Overwinter strategy", y="50% Estimate", tag="B") )

# Extract the legend. Returns a gtable
leg.ows <- as_ggplot(get_legend(fig.owsb))
fig.owsb <- fig.owsb + theme(legend.position='none')


##Combine panels into Figure 4:
figures22 <- grid.arrange(fig.owsa, fig.owsb, leg.ows,  ncol = 3, nrow = 1, widths=c(3,3,1))
figures22
ggsave("figs/Larsen_etal_S2Fig2.png", plot=figures22, width = 6, height = 2.5, dpi = 300, units = "in", device='png')


