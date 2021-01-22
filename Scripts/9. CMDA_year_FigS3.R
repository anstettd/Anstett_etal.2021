##################################################################################
## Daniel Anstett
## Generate Fig S2 
## Plots Climate Moisture Deficit Anomaly (CMDA) from 2010 to 2016
## Used to select site/year data included in the study
##
## Last Modified January 22, 2020
###################################################################################


###################################################################################
#Import libraries
library(tidyverse)

###################################################################################
# #Imports weather dataset for each site and year
wna_anom <- read.csv("Data/wna_graph.csv", header=T)

#Reorganize and lable data
wna_anom<-wna_anom %>% mutate(Site.Lat=paste(Latitude, Site, sep="_"))
wna_anom
Site_Labs<-c("32.89928_S02"="Site 1", "34.28425_S07"="Site 2", "36.69096_S08"="Site 4", "36.20081_S10"="Site 3", 
             "34.07808_S11"="Site 12", "43.37876_S15"="Site 11", "41.80979_S16"="Site 9", "41.66546_S17"="Site 8",
             "39.74298_S18"="Site 7", "39.39442_S29"="Sites 6", "37.539_S32"="Site 5",   "42.27411_S36"="Site 10")

#Plot CMDA against year per site
weath.year <- ggplot(wna_anom, aes(Year,CMD.anom))+
  geom_point(size=3, aes(colour=Decision))+
  geom_line()+
  theme_minimal()
# Format points to include decision information
weath.year <- weath.year + facet_wrap( ~ Site.Lat, ncol=4, labeller=labeller(Site.Lat=Site_Labs))+
  scale_color_manual(values= c("Included"="black", "Remove"="red", "Unavailable"="blue"))
weath.year + theme(legend.text = element_text(size = 12, face = "bold"),
                   legend.title = element_text(size=14, face="bold"),
                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                     axis.text.y = element_text(size=14,face="bold"),
                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"),
                   strip.text = element_text(size = 14, face="bold")) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Climatic Moisture Deficit Anomaly")

###################################################################################
