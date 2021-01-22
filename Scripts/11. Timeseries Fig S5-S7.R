##################################################################################
## Daniel Anstett
## Timeseries graphs of weather from 1980 to 2016
## MAT = Mean Annual Temp, CMD = Climate Moisture Deficit, MAP = Mean Annual Precipitation
##
##
## Last Modified January 21, 2020
###################################################################################


###################################################################################
#Import libraries
library(tidyverse)
library(cowplot)

###################################################################################
#Imports 1980-2009 climate data
m_year<-read.csv('Data/m_year.csv', header=T) 
m_year<- m_year %>% select(ID,ID2,hist_year,Latitude,CMD,MAT,MAP) #Select relevant data
colnames(m_year)[3]<-"Year"

#Imports 2009-2016 climate data
weather<-read.csv('Data/weather.csv', header=T) 
weather<- weather %>% select(ID,ID2,Year,Latitude,CMD,MAT,MAP) #Select relevant data

#Bind the data set and rename sites
clim_time<-bind_rows(m_year,weather)
clim_time<-clim_time %>% filter(ID!="S11")
clim_time<-clim_time %>% mutate(Site = ifelse(ID =="S02", 1, 
  ifelse((ID=="S07"), 2, 
         ifelse((ID=="S10"), 3,
                ifelse((ID=="S08"), 4,
                       ifelse((ID=="S32"), 5,
                              ifelse((ID=="S29"), 6,
                                     ifelse((ID=="S18"), 7,
                                            ifelse((ID=="S17"), 8,
                                                   ifelse((ID=="S16"), 9,
                                                          ifelse((ID=="S36"), 10,11)))))))))))

###################################################################################
###################################################################################
# Make weather timeseries graphics

# Mean Annual Temp vs Year
all_MAT <-ggplot(clim_time , aes(x=Year, y=MAT))+ 
  geom_line()+
  facet_wrap(.~Site)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))
  all_MAT
ggsave("Fig S5.pdf", width = 12, height = 7, units = "in")

# Climate Moisture Deficit, vs Year
all_CMD <-ggplot(clim_time , aes(x=Year, y=CMD))+ 
  geom_line()+
  facet_wrap(.~Site)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))
all_CMD 
ggsave("Fig S6.pdf", width = 12, height = 7, units = "in")

# Mean Annual Precipitation vs Year
all_MAP <-ggplot(clim_time , aes(x=Year, y=MAP))+ 
  geom_line()+
  facet_wrap(.~Site)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))
  all_MAP
ggsave("Fig S7.pdf", width = 12, height = 7, units = "in")
###################################################################################

