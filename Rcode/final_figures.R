#MS Figures

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(grid)
library(ggpubr)
library(boot)

#Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
#Please cite ggmap if you use it! See citation("ggmap") for details.

#species list
splist<-read_csv("data/species_list.csv")

#### FIGURE 1
#Load & summarize incidental data
incid<-read_csv("Emperical_Results/obs10_by-cell-spp-year.csv") %>%
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
  scale_color_gradient(name="N. Species", limits=c(0,40), low="deepskyblue", high="black") + 
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
ggsave("figs/Larsen_etal_Fig1.png", plot=figure, width = 6, height = 5, dpi = 300, units = "in", device='png')



#### Figure 2

#Filter for data
lat.f2<-41.5
lon.f2<-(-83.5)
year.f2<-2017
sp.f2<-"Speyeria cybele"
#Survey data
load("data/survey_speyeria.RData")
tenth.ci<-quantile(boot.out$t[,366], probs=c(0.025,0.975))
fiftieth.ci<-quantile(boot.out$t[,367], probs=c(0.025,0.975))

survey_samp<-read_csv("data/survey_speyeriadata.csv") %>%
  mutate(npres=ifelse((COUNT==0),0,1))


surv.speyeria<-data.frame(yday=numeric(),est=numeric(),lowci=numeric(),highci=numeric())
for(i in 1:365) { #loop through days of year
  result.ci<-boot.ci(boot.out, index=i, conf = 0.95, type = "perc")
  surv.speyeria[nrow(surv.speyeria)+1,]<-c(i,boot.out$t0[i], round(result.ci$percent[4],4), round(result.ci$percent[5],4))
}

surv.speyeria<-surv.speyeria %>%
  mutate(dailyprob=est*1000) %>%
  arrange(yday)

for(i in c(2:365)) {surv.speyeria$dailyprob[i]<-(surv.speyeria$est[i]-surv.speyeria$est[i-1])*10000}

(FIG2survey<-ggplot(data=filter(surv.speyeria,yday<361), aes(x=yday,y=dailyprob)) +
    geom_rect(aes(xmin=tenth.ci[1],xmax=tenth.ci[2], ymin=-Inf, ymax=Inf), fill="gold1", alpha=0.25) +
    geom_rect(aes(xmin=fiftieth.ci[1],xmax=fiftieth.ci[2], ymin=-Inf, ymax=Inf), fill="cadetblue1", alpha=0.25) +
    geom_line(data=filter(surv.speyeria,yday<361), aes(x=yday,y=dailyprob)) +
    #  geom_vline(xintercept=round(tenth.ci$percent[4]),color="black", linetype=3) +
    #  geom_vline(xintercept=round(tenth.ci$percent[5]),color="black", linetype=2) +
    geom_vline(xintercept=boot.out$t0[366], linetype=3) +
    geom_vline(xintercept=boot.out$t0[367], linetype=2) +
    xlim(100,320) + theme(axis.text.y = element_blank()) +
    geom_histogram(data=survey_samp, aes(x=yday, fill = as.factor(npres)), colour="black", binwidth=7,  inherit.aes=F) + #xlim(190, 230) +
    scale_fill_manual(values=c("white","black"), guide=F) +
    labs(x="Day of year", y="Relative abundance") +
    theme_classic()  +labs(tag="A")  )
FIG2survey

#Incidental data
incid_sample<-read_csv("data/incid_speyeria.csv") 


incid10<-read_csv("Emperical_Results/naive_tenth_iNat_estimates.csv") %>%
  filter(lat_bin==lat.f2,lon_bin==lon.f2, year==year.f2, scientificName==sp.f2)
incid50<-read_csv("Emperical_Results/naive_fiftieth_iNat_estimates.csv") %>%
  filter(lat_bin==lat.f2,lon_bin==lon.f2, year==year.f2, scientificName==sp.f2)

incid.hist<-hist(incid_sample$day, breaks=c(10:30)*10)
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
  geom_col(aes(x=inat.h, y=inat.c), color="black") + labs(tag="B")

FIG2incid


##Combine panels into Figure 1:
figure2 <- grid.arrange(FIG2survey, FIG2incid,  ncol = 2, nrow = 1)
figure2
ggsave("figs/Larsen_etal_Fig2.png", plot=figure2, width = 6, height = 2.5, dpi = 300, units = "in", device='png')

