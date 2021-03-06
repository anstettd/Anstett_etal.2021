##################################################################################
## Daniel Anstett
## Generate Fig S1 using Temperature and Precipitation data
## MAT = Mean Annual Temperature ,MAP = Mean Annual Precipitation
## 
##
## Last Modified January 22, 2020
###################################################################################


###################################################################################
#Import libraries
library(tidyverse)
library(cowplot)



###################################################################################
# Mean Annual Temperature (MAT)

###################################################################################
###################################################################################
# Fig S1A, make MAT plot for 1979 to 2009

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
MAT_Lat<-ggplot(m_year, aes(x=Site.Lat, y=MAT,fill=Region))+ 
  geom_boxplot()+
  scale_y_continuous(name="MAT",limits=c(5,20))+
  xlab("Site")+
  coord_flip()+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
MAT_Lat
MAT_Lat<-MAT_Lat + theme(legend.position = "none",
                         axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                         #axis.ticks.x = element_blank(),
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.y = element_text(size=16,vjust = 1, face="bold"))+
  scale_x_discrete(labels=Site.lable)
MAT_Lat

###################################################################################
###################################################################################
# Fig S1B, make MAT CV plot for 1979 to 2009

#Imports the mean of 30 years of Mimulus climate data
CV_climate<-read.csv('Data/climate.csv', header=T) 

#Set up dataframe with correct region information
colnames(CV_climate)[2]<-"Site" ; CV_climate<-CV_climate %>% filter(Site!="S11")

# Calculate coefficient of variation
CV_climate<-CV_climate %>% select(Site,ID2,Latitude,Longitude)
m_year<-m_year %>% mutate(MAT_K=MAT+273.15) #Convert Temperature to Kelvin
for (i in 1:11){
  site.select<-m_year %>% filter(Site==CV_climate$Site[i])
  CV_climate[i,5]<-sd(site.select$MAT_K)/mean(site.select$MAT_K)
}
colnames(CV_climate)[5]<-"MAT.CV"

#Add in Region and Site.Lat
CV_climate<-CV_climate %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                                  ifelse((Latitude >35) & (Latitude <40), "Centre","South"))) %>%
  mutate(Site.Lat=paste(Latitude,Site,sep="_")) #Set up Site.Lat
CV_climate$Region<-as.factor(CV_climate$Region) #Make Region appear in logical order
CV_climate$Region<-factor(CV_climate$Region,levels=c("North","Centre","South"))
Site.lable<-c("32.89928_S02"="1","34.28425_S07"="2","36.20081_S10"="3","36.69096_S08"="4", 
              "37.539_S32"="5","39.39442_S29"="6","39.74298_S18"="7", "41.66546_S17"="8", 
              "41.80979_S16"="9","42.27411_S36"="10","43.37876_S15"="11")

#Use ggplot to generate plot with all required formating
MAT_CV_Lat<-ggplot(CV_climate, aes(x=Site.Lat, y=MAT.CV))+ 
  geom_point(aes(fill=Region),size=5, shape=21,color="black")+
  scale_y_continuous(name="MAT CV")+
  xlab("Site")+
  coord_flip()+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
MAT_CV_Lat<-MAT_CV_Lat + theme(legend.position = "none",
                       axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                       #axis.ticks.x = element_blank(),
                       axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                       axis.text.y = element_text(size=14,face="bold",hjust=0.5),
                       axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5))+
  scale_x_discrete(labels=Site.lable)
MAT_CV_Lat

###################################################################################
###################################################################################
# Fig S1C, make Mean Annual Temperature Anomaly (MATA) plot per site.lat 2010 - 2016 

#Imports MATA for all sites across each year
wna.all<-read.csv('Data/wna_all.csv', header=T) 

#Set up dataframe with correct region information
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
MATA_Lat<-ggplot(wna.all, aes(x=Site.Lat, y=MAT.anom, shape=factor(Year), col=factor(Region)))+ 
  geom_point(aes(fill=Region), size =5)+
  scale_shape_manual(values =c(48:54))+
  scale_y_continuous(name="MATA",limits=c(-2,2))+
  xlab("Site")+
  geom_hline(yintercept = 0, color="black", size=0.8)+
  coord_flip()+
  scale_color_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
MATA_Lat<-MATA_Lat + theme(legend.position = "none",
                           axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                           #axis.ticks.x = element_blank(),
                           axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                           axis.text.y = element_text(size=14,face="bold"),
                           axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5))+
  scale_x_discrete(labels=Site.lable)
MATA_Lat


###################################################################################
# Mean Annual Precipitation (MAP)

###################################################################################
###################################################################################
# Fig S1D, make MAP plot for 1979 to 2009

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
MAP_Lat<-ggplot(m_year, aes(x=Site.Lat, y=MAP,fill=Region))+ 
  geom_boxplot()+
  scale_y_continuous(name="MAP", limits=c(1,2000))+
#  scale_y_continuous(name="MAP", trans='log10', limits=c(10,3000))+
  xlab("Site")+
  coord_flip()+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
MAP_Lat
MAP_Lat<-MAP_Lat + theme(legend.position = "none",
                         axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                         #axis.ticks.x = element_blank(),
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.y = element_text(size=16,vjust = 1, face="bold"))+
  scale_x_discrete(labels=Site.lable)
MAP_Lat





###################################################################################
###################################################################################
### Fig S1E, make MAP CV plot for 1979 to 2009 ###

#Imports the mean of 30 years of Mimulus climate data
CV_climate<-read.csv('Data/climate.csv', header=T)

#Set up dataframe with correct region information
colnames(CV_climate)[2]<-"Site" ; CV_climate<-CV_climate %>% filter(Site!="S11") #Clean up data set
CV_climate<-CV_climate %>% select(Site,ID2,Latitude,Longitude)

# Calculate coefficient of variation
for (i in 1:11){ 
  site.select<-m_year %>% filter(Site==CV_climate$Site[i])
  CV_climate[i,5]<-sd(site.select$MAP)/mean(site.select$MAP)
}
colnames(CV_climate)[5]<-"MAP.CV"

#Add in Region and Site.Lat
CV_climate<-CV_climate %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                                  ifelse((Latitude >35) & (Latitude <40), "Centre","South"))) %>%
  mutate(Site.Lat=paste(Latitude,Site,sep="_")) #Set up Site.Lat
CV_climate$Region<-as.factor(CV_climate$Region) #Make Region appear in logical order
CV_climate$Region<-factor(CV_climate$Region,levels=c("North","Centre","South"))
Site.lable<-c("32.89928_S02"="1","34.28425_S07"="2","36.20081_S10"="3","36.69096_S08"="4", 
              "37.539_S32"="5","39.39442_S29"="6","39.74298_S18"="7", "41.66546_S17"="8", 
              "41.80979_S16"="9","42.27411_S36"="10","43.37876_S15"="11")


#Use ggplot to generate plot with all required formating
MAP_CV_Lat<-ggplot(CV_climate, aes(x=Site.Lat, y=MAP.CV))+ 
  geom_point(aes(fill=Region),size=5, shape=21,color="black")+
  scale_y_continuous(name="MAP CV",limits=c(0.2,0.5))+
  xlab("Site")+
  coord_flip()+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
MAP_CV_Lat<-MAP_CV_Lat + theme(legend.position = "none",
                               axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                               #axis.ticks.x = element_blank(),
                               axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                               axis.text.y = element_text(size=14,face="bold",hjust=0.5),
                               axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5))+
  scale_x_discrete(labels=Site.lable)
MAP_CV_Lat


###################################################################################
###################################################################################
### Fig S1F, make MAPA plot per site.lat 2010 - 2016 ###

#Imports MATA for all sites across each year
wna.all<-read.csv('Data/wna_all.csv', header=T) 



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
MAPA_Lat<-ggplot(wna.all, aes(x=Site.Lat, y=MAP.anom, shape=factor(Year), col=factor(Region)))+ 
  geom_point(aes(fill=Region),size=5)+
  scale_shape_manual(values =c(48:54))+
  scale_y_continuous(name="MAPA")+
  xlab("Site")+
  geom_hline(yintercept = 0, color="black", size=0.8)+
  coord_flip()+
  scale_color_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
MAPA_Lat<-MAPA_Lat + theme(legend.position = "none",
                           axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                           #axis.ticks.x = element_blank(),
                           axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                           axis.text.y = element_text(size=14,face="bold"),
                           axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5))+
  scale_x_discrete(labels=Site.lable)
MAPA_Lat

###################################################################################
#Place all six figures into one output using Cowplot. Export at  8 X 12 inches
plot_grid(MAT_Lat,MAT_CV_Lat,MATA_Lat,MAP_Lat,MAP_CV_Lat,MAPA_Lat, ncol = 3,
          labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 16)
###################################################################################


