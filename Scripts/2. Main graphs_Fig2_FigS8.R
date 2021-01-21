##################################################################################
## Daniel Anstett
## Plots of Site*Year*Drought Mixed Models using the visreg package
## Generates Fig 2 and Fig S8
## 3-way interactions are visualized regardless of significance to allow for easier visual comparison
##
## Last Modified January 21, 2020
###################################################################################


###################################################################################
#Import libraries
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)
library(cowplot)

###################################################################################
#Imports main dataset
y5 <- read.csv("Data/y5.csv", header=T) 

#Set factors
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factors

#Define regions
y5<-y5 %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Center","South")))


###################################################################################
# Generate Fig 2
###################################################################################

######################################################################################################################
# SLA
######################################################################################################################

fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5) # Run 3-way model
vis_flower_D<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))



######################################################################################################################
# Date of Flowering
######################################################################################################################

finalmod.exp <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5) # Run 3-way model
vis_flower_D<-visreg(finalmod.exp, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.exp, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="D (North)", "Center"="E (Centre)", "South"="F (South)")
#Use ggplot to generate plot with all required formating
Flower_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Flower_plot <-Flower_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
Flower_plot <-Flower_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))

## Cowplot export at 8 X 7 inches
plot_grid(SLA_plot,Flower_plot,ncol = 1)



###################################################################################
# Generate Fig S8
###################################################################################


######################################################################################################################
# Water_Content 
######################################################################################################################

finalmod.wc <- lmer(Water_Content ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), # Run 3-way model
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
vis_flower_D<-visreg(finalmod.wc, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.wc, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
WC_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Water Content")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
WC_plot <-WC_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=18,vjust = 2, face="bold",hjust=0.5))
WC_plot <-WC_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))


######################################################################################################################
# Assimilation
######################################################################################################################

finalmod.A <- lmer(Assimilation ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), # Run 3-way model
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
vis_flower_D<-visreg(finalmod.A, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.A, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#Set up site lables equating names to codes
Site_Labs<-c("North"="D (North)", "Center"="E (Centre)", "South"="F (South)")
#Use ggplot to generate plot with all required formating
A_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Assimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
A_plot <-A_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=18,vjust = 2, face="bold",hjust=0.5))
A_plot <-A_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))


######################################################################################################################
# Stomatal Conductance
######################################################################################################################

finalmod.gs <- lmer(Stomatal_Conductance ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)  # Run 3-way model
vis_flower_D<-visreg(finalmod.gs, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.gs, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#Set up site lables equating names to codes
Site_Labs<-c("North"="G (North)", "Center"="H (Centre)", "South"="I (South)")
#Use ggplot to generate plot with all required formating
gs_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Stomatal Conductance")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
gs_plot <-gs_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=18,vjust = 2, face="bold",hjust=0.5))
gs_plot <-gs_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))

## Cowplot export at 11 X 7 inches
plot_grid(WC_plot,A_plot,gs_plot,ncol = 1)


