##################################################################################
## Daniel Anstett
## Generate climate focused Fig 1
## Using Climate Moisture Deficit (CMD) and Cliamte Moisture Deficit Anomaly (CMDA)
##
## Last Modified January 22, 2020
###################################################################################


###################################################################################
#Import libraries
library(tidyverse)
library(cowplot)
library(ggmap)

###################################################################################
###################################################################################
# Fig 1A, make map with sites

#Import California & Oregon Map
cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))
#Import Site Data
site.lat.long <- read.csv("Data/sites.csv", header=T)
site.lat.long <- site.lat.long %>% filter(Site!=12)

#Map Making
base_map <- ggplot(cali_or) + 
  geom_polygon(aes(x=long,y=lat,group = group), colour="black", fill="white") + coord_fixed(1.3) +
  geom_point(data=site.lat.long, aes(x=Longitude, y=Latitude, fill=Region),size=4, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333"))+
  theme_nothing()
base_map


###################################################################################
###################################################################################
# Fig 1B, make CMDA plot per site.lat 2010 - 2016

#Imports 30 years of Mimulus climate data
m_year<-read.csv('Data/m_year.csv', header=T) 
#Set up dataframe with correct region information
colnames(m_year)[2]<-"Site"
m_year<-m_year %>% filter(Site!="S11")
m_year<-m_year %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                            ifelse((Latitude >35) & (Latitude <40), "Centre","South"))) %>%
  mutate(Site.Lat=paste(Latitude,Site,sep="_")) #Set up Site.Lat
m_year$Region<-as.factor(m_year$Region) #Make Region appear in logical order
m_year$Region<-factor(m_year$Region,levels=c("North","Centre","South"))
Site.lable<-c("32.89928_S02"="1","34.28425_S07"="2","36.20081_S10"="3","36.69096_S08"="4", 
              "37.539_S32"="5","39.39442_S29"="6","39.74298_S18"="7", "41.66546_S17"="8", 
              "41.80979_S16"="9","42.27411_S36"="10","43.37876_S15"="11")

#Use ggplot to generate plot with all required formating
CMD_Lat<-ggplot(m_year, aes(x=Site.Lat, y=CMD,fill=Region))+ 
  geom_boxplot()+
  scale_y_continuous(name="CMD", limits=c(0,1250))+
  xlab("Site")+
  coord_flip()+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
CMD_Lat
CMD_Lat<-CMD_Lat + theme(legend.position = "none",
                axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                #axis.ticks.x = element_blank(),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=14,face="bold"),
                axis.title.y = element_text(size=16,vjust = 1, face="bold"))+
  scale_x_discrete(labels=Site.lable)
CMD_Lat

###################################################################################
###################################################################################
# Fig 1C, make CMD CV plot per site.lat 2010 - 2016 

#Imports the mean of 30 years of Mimulus climate data
CV_climate<-read.csv('Data/climate.csv', header=T)

#Clean up data set
colnames(CV_climate)[2]<-"Site" ; CV_climate<-CV_climate %>% filter(Site!="S11") 
CV_climate<-CV_climate %>% select(Site,ID2,Latitude,Longitude)

# calculate coefficient of variation
for (i in 1:11){ 
site.select<-m_year %>% filter(Site==CV_climate$Site[i])
CV_climate[i,5]<-sd(site.select$CMD)/mean(site.select$CMD)
}
colnames(CV_climate)[5]<-"CMD.CV"

#Set up dataframe with correct region information
CV_climate<-CV_climate %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                            ifelse((Latitude >35) & (Latitude <40), "Centre","South"))) %>%
  mutate(Site.Lat=paste(Latitude,Site,sep="_")) #Set up Site.Lat
CV_climate$Region<-as.factor(CV_climate$Region) #Make Region appear in logical order
CV_climate$Region<-factor(CV_climate$Region,levels=c("North","Centre","South"))
Site.lable<-c("32.89928_S02"="1","34.28425_S07"="2","36.20081_S10"="3","36.69096_S08"="4", 
              "37.539_S32"="5","39.39442_S29"="6","39.74298_S18"="7", "41.66546_S17"="8", 
              "41.80979_S16"="9","42.27411_S36"="10","43.37876_S15"="11")


#Use ggplot to generate plot with all required formatting
CV_Lat<-ggplot(CV_climate, aes(x=Site.Lat, y=CMD.CV))+ 
  geom_point(aes(fill=Region),size=5, shape=21,color="black")+
  scale_y_continuous(name="CMD CV",limits=c(0,0.30))+
  xlab("Site")+
  coord_flip()+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
CV_Lat<-CV_Lat + theme(legend.position = "none",
                axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                #axis.ticks.x = element_blank(),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=14,face="bold",hjust=0.5),
                axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5))+
  scale_x_discrete(labels=Site.lable)
CV_Lat

###################################################################################
###################################################################################
# Fig 1D, make Cumulate Moisture Deficite Anomaly (CMDA) plot per site for 2010 to 2016

#Imports CMDA for all site/year cominbations
wna.all<-read.csv('Data/wna_all.csv', header=T)

#Clean up dataset and set regions and lables
wna.all<-wna.all %>% filter(Site!="S11")
wna.all<-wna.all %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Centre","South"))) %>%
  mutate(Site.Lat=paste(Latitude,Site,sep="_")) #Set up Site.Lat
wna.all$Region<-as.factor(wna.all$Region) #Make Region appear in logical order
wna.all$Region<-factor(wna.all$Region,levels=c("North","Centre","South"))
Site.lable<-c("32.89928_S02"="1","34.28425_S07"="2","36.20081_S10"="3","36.69096_S08"="4", 
              "37.539_S32"="5","39.39442_S29"="6","39.74298_S18"="7", "41.66546_S17"="8", 
              "41.80979_S16"="9","42.27411_S36"="10","43.37876_S15"="11")


#Use ggplot to generate plot with all required formating
CMDA_Lat<-ggplot(wna.all, aes(x=Site.Lat, y=CMD.anom, shape=factor(Year), col=factor(Region)))+ 
  geom_point(aes(fill=Region), size =3)+
  scale_shape_manual(values =c(48:54))+
  scale_y_continuous(name="CMDA",limits=c(-200,200))+
  xlab("Site")+
  geom_hline(yintercept = 0, color="black", size=0.8)+
  coord_flip()+
  scale_color_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
CMDA_Lat<-CMDA_Lat + theme(legend.position = "none",
                           axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                           #axis.ticks.x = element_blank(),
                           axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                           axis.text.y = element_text(size=14,face="bold"),
                           axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5))+
  scale_x_discrete(labels=Site.lable)
CMDA_Lat

###################################################################################
#Place all four figures into one output using Cowplot. Export at  6 X 10 inches
plot_grid(base_map,CMD_Lat,CV_Lat,CMDA_Lat,ncol = 4,labels = "AUTO", 
          rel_widths = c(1.07, 1, 1, 1.5), rel_heights = c(1, 0.1, 0.1, 0.1))

###################################################################################
