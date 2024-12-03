################################################################
####### de León et al. 2024
#######    Figures   
################################################################

library(ggplot2)
library(scico)
library(paletteer)
library(ggpubr)
library(viridis)
library(sf)
library(ggspatial)

######################################################################################

colnames(coord.river)<- c("X","Y")
colnames(coord.just.river)<- c("X","Y")
multipoint_sf_river <- st_sfc(st_multipoint(as.matrix(coord.river[, c("X", "Y")])))
multipoint_sf_just_river <- st_sfc(st_multipoint(as.matrix(coord.just.river[, c("X", "Y")])))
multipoint_sf_river <- st_sf(geometry = multipoint_sf_river, crs = 3857)
multipoint_sf_just_river <- st_sf(geometry = multipoint_sf_just_river, crs = 3857)
Wetland <- st_cast(multipoint_sf_river, "POINT")
Stream <- st_cast(multipoint_sf_just_river, "POINT")

#This elements come from the other script, so CHECK SCRIPT_SIMULATIONS FIRST
c( "Fixed node (upstream diversity)",Filter.river$Water.body,"Fixed node (outlet diversity)")-> Wetland$Water.body
c( "Fixed node (upstream diversity)",rep("River (a3-a4)",515),"Fixed node (outlet diversity)")-> Stream$Water.body
ifelse(Wetland$Water.body=="River (1)", "River (a2)", Wetland$Water.body)-> Wetland$Water.body
ifelse(Wetland$Water.body=="Wetland (2-3)", "Wetland (a3-a4)", Wetland$Water.body)-> Wetland$Water.body

Wetland$clos<- Filter.river$clos
Stream$clos<- Filter.just.river$clos

######################################################################################
######################################################################################
#Figure 1A
Wetland$size <- ifelse(Wetland$Water.body %in% c("Fixed node (outlet diversity)", "Fixed node (upstream diversity)"), 1, 0.9)
Wetland.landscape<-ggplot() +
  geom_sf(data = Wetland, aes(fill = Water.body, size = size), shape = 21, color = "black", stroke = 0.05) + 
  scale_fill_manual(values = c("orange", "red3", "darkblue", "steelblue1")) +
  guides(fill = guide_legend(title = "Environments", , 
                             keywidth = 2, keyheight = 4, 
                             override.aes = list(size = 5)), size = "none") +
  scale_size_continuous(range = c(3.5, 5)) + 
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true") +
  theme_pubr(border = TRUE) +
  theme(legend.text = element_text(size = 30), legend.title = element_text(size = 35, face = "bold", hjust = 0.5), legend.position = "right",
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),  
        axis.text.y = element_text(size = 20, angle = 45, hjust = 1))  
Stream$size <- ifelse(Stream$Water.body %in% c("Fixed node (outlet diversity)", "Fixed node (upstream diversity)"), 1, 0.9)
Stream.landscape <- ggplot() +
  geom_sf(data = Stream, aes(fill = Water.body, size = size), shape = 21, color = "black", stroke = 0.05) + 
  scale_fill_manual(values = c("orange", "red3", "darkblue")) +
  guides(fill = guide_legend(title = "Environments", , 
                             keywidth = 2, keyheight = 4, 
                             override.aes = list(size = 5)), size = "none") +
  scale_size_continuous(range = c(3.5, 5)) + 
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true") +
  theme_pubr(border = TRUE) +
  theme(legend.text = element_text(size = 30), legend.title = element_text(size = 35, face = "bold", hjust = 0.5), legend.position = "right",
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),  
        axis.text.y = element_text(size = 20, angle = 45, hjust = 1)) 

##################################################################
#Create vectors for the images, allow easier manipulation of data
#Wetland landscape
#Alpha diversity vectors
as.vector(result.35.mean)-> alfa_35_solo
as.vector(result.85.mean)-> alfa_85_solo
as.vector(result.200.mean)-> alfa_200_solo
as.vector(result.J2250.35.mean)-> alfa_35_2250J
as.vector(result.J3500.35.mean)-> alfa_35_3500J
as.vector(result.J5000.35.mean)-> alfa_35_5000J
as.vector(result.J2250.85.mean)-> alfa_85_2250J
as.vector(result.J3500.85.mean)-> alfa_85_3500J
as.vector(result.J5000.85.mean)-> alfa_85_5000J
as.vector(result.J2250.200.mean)-> alfa_200_2250J
as.vector(result.J3500.200.mean)-> alfa_200_3500J
as.vector(result.J5000.200.mean)-> alfa_200_5000J
as.vector(result.F2250.35.mean)-> alfa_35_2250F
as.vector(result.F3500.35.mean)-> alfa_35_3500F
as.vector(result.F5000.35.mean)-> alfa_35_5000F
as.vector(result.F2250.85.mean)-> alfa_85_2250F
as.vector(result.F3500.85.mean)-> alfa_85_3500F
as.vector(result.F5000.85.mean)-> alfa_85_5000F
as.vector(result.F2250.200.mean)-> alfa_200_2250F
as.vector(result.F3500.200.mean)-> alfa_200_3500F
as.vector(result.F5000.200.mean)-> alfa_200_5000F
#Log-ratio between impacted and unimpacted simulations, adding the result to our matrix
as.vector(log(alfa_35_2250J/alfa_35_solo)) -> Wetland$Ratio_35_2250J
as.vector(log(alfa_85_2250J/alfa_85_solo)) -> Wetland$Ratio_85_2250J
as.vector(log(alfa_200_2250J/alfa_200_solo)) -> Wetland$Ratio_200_2250J
as.vector(log(alfa_35_3500J/alfa_35_solo)) -> Wetland$Ratio_35_3500J
as.vector(log(alfa_85_3500J/alfa_85_solo)) -> Wetland$Ratio_85_3500J
as.vector(log(alfa_200_3500J/alfa_200_solo)) -> Wetland$Ratio_200_3500J
as.vector(log(alfa_35_5000J/alfa_35_solo)) -> Wetland$Ratio_35_5000J
as.vector(log(alfa_85_5000J/alfa_85_solo)) -> Wetland$Ratio_85_5000J
as.vector(log(alfa_200_5000J/alfa_200_solo)) -> Wetland$Ratio_200_5000J
as.vector(log(alfa_35_2250F/alfa_35_solo)) -> Wetland$Ratio_35_2250F
as.vector(log(alfa_85_2250F/alfa_85_solo)) -> Wetland$Ratio_85_2250F
as.vector(log(alfa_200_2250F/alfa_200_solo)) -> Wetland$Ratio_200_2250F
as.vector(log(alfa_35_3500F/alfa_35_solo)) -> Wetland$Ratio_35_3500F
as.vector(log(alfa_85_3500F/alfa_85_solo)) -> Wetland$Ratio_85_3500F
as.vector(log(alfa_200_3500F/alfa_200_solo)) -> Wetland$Ratio_200_3500F
as.vector(log(alfa_35_5000F/alfa_35_solo)) -> Wetland$Ratio_35_5000F
as.vector(log(alfa_85_5000F/alfa_85_solo)) -> Wetland$Ratio_85_5000F
as.vector(log(alfa_200_5000F/alfa_200_solo)) -> Wetland$Ratio_200_5000F

#The same but for stream landscape
as.vector(result.river.35.mean)-> alfa_river_35_solo
as.vector(result.river.85.mean)-> alfa_river_85_solo
as.vector(result.river.200.mean)-> alfa_river_200_solo
as.vector(result.river.J2250.35.mean)-> alfa_river_35_2250J
as.vector(result.river.J3500.35.mean)-> alfa_river_35_3500J
as.vector(result.river.J5000.35.mean)-> alfa_river_35_5000J
as.vector(result.river.J2250.85.mean)-> alfa_river_85_2250J
as.vector(result.river.J3500.85.mean)-> alfa_river_85_3500J
as.vector(result.river.J5000.85.mean)-> alfa_river_85_5000J
as.vector(result.river.J2250.200.mean)-> alfa_river_200_2250J
as.vector(result.river.J3500.200.mean)-> alfa_river_200_3500J
as.vector(result.river.J5000.200.mean)-> alfa_river_200_5000J
as.vector(result.river.F2250.35.mean)-> alfa_river_35_2250F
as.vector(result.river.F3500.35.mean)-> alfa_river_35_3500F
as.vector(result.river.F5000.35.mean)-> alfa_river_35_5000F
as.vector(result.river.F2250.85.mean)-> alfa_river_85_2250F
as.vector(result.river.F3500.85.mean)-> alfa_river_85_3500F
as.vector(result.river.F5000.85.mean)-> alfa_river_85_5000F
as.vector(result.river.F2250.200.mean)-> alfa_river_200_2250F
as.vector(result.river.F3500.200.mean)-> alfa_river_200_3500F
as.vector(result.river.F5000.200.mean)-> alfa_river_200_5000F
as.vector(log(alfa_river_35_2250J/alfa_river_35_solo)) -> Stream$Ratio_35_2250J
as.vector(log(alfa_river_85_2250J/alfa_river_85_solo)) -> Stream$Ratio_85_2250J
as.vector(log(alfa_river_200_2250J/alfa_river_200_solo)) -> Stream$Ratio_200_2250J
as.vector(log(alfa_river_35_3500J/alfa_river_35_solo)) -> Stream$Ratio_35_3500J
as.vector(log(alfa_river_85_3500J/alfa_river_85_solo)) -> Stream$Ratio_85_3500J
as.vector(log(alfa_river_200_3500J/alfa_river_200_solo)) -> Stream$Ratio_200_3500J
as.vector(log(alfa_river_35_5000J/alfa_river_35_solo)) -> Stream$Ratio_35_5000J
as.vector(log(alfa_river_85_5000J/alfa_river_85_solo)) -> Stream$Ratio_85_5000J
as.vector(log(alfa_river_200_5000J/alfa_river_200_solo)) -> Stream$Ratio_200_5000J
as.vector(log(alfa_river_35_2250F/alfa_river_35_solo)) -> Stream$Ratio_35_2250F
as.vector(log(alfa_river_85_2250F/alfa_river_85_solo)) -> Stream$Ratio_85_2250F
as.vector(log(alfa_river_200_2250F/alfa_river_200_solo)) -> Stream$Ratio_200_2250F
as.vector(log(alfa_river_35_3500F/alfa_river_35_solo)) -> Stream$Ratio_35_3500F
as.vector(log(alfa_river_85_3500F/alfa_river_85_solo)) -> Stream$Ratio_85_3500F
as.vector(log(alfa_river_200_3500F/alfa_river_200_solo)) -> Stream$Ratio_200_3500F
as.vector(log(alfa_river_35_5000F/alfa_river_35_solo)) -> Stream$Ratio_35_5000F
as.vector(log(alfa_river_85_5000F/alfa_river_85_solo)) -> Stream$Ratio_85_5000F
as.vector(log(alfa_river_200_5000F/alfa_river_200_solo)) -> Stream$Ratio_200_5000F


####################################################################
##### Figure 1c. Reference community assembly: Stream landscape
### d50 = 35 ####
Stream$diversidad.35<- (alfa_river_35_solo)
alfa_river_35<- ggplot(data=Stream, 
                       aes(colour=diversidad.35)) +
  geom_sf() +
  scale_colour_gradientn(colours=c("darkblue", "steelblue1", "yellow", "orange3", "red4"), limits=c(130,300))+
  guides(col= guide_colourbar(title= "Alpha diversity"))+
  ggtitle(NULL)+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
### d50 = 85 ####
Stream$diversidad.85<- (alfa_river_85_solo)
alfa_river_85<- ggplot(data=Stream, 
                       aes(colour=diversidad.85)) +
  geom_sf() +
  scale_colour_gradientn(colours=c("darkblue", "steelblue1", "yellow", "orange3", "red4"), limits=c(130,300))+
  guides(col= guide_colourbar(title= "Alpha diversity"))+
  ggtitle(NULL)+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
### d50 = 200 ####
Stream$diversidad.200<- (alfa_river_200_solo)
alfa_river_200<- ggplot(data=Stream, 
                        aes(colour=diversidad.200)) +
  geom_sf() +
  scale_colour_gradientn(colours=c("darkblue", "steelblue1", "yellow", "orange3", "red4"), limits=c(130,300))+
  guides(col= guide_colourbar(title= "Alpha diversity"))+
  ggtitle(NULL)+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) #, legend.key.size = unit(10, "cm")
ggpubr::ggarrange(alfa_river_35, alfa_river_85, alfa_river_200,
                  labels= c("c1", "c2", "c3"), 
                  ncol=3, nrow=1, common.legend= T, legend= "right")

######################################################################################
######################################################################################
#Extract community abundance in each scenario is possible, we will use it for the next figures
Wetland$Efecto_J_2250<- 3000 - WWTP.impact.J2250
Wetland$Efecto_J_3500<- 3000 - WWTP.impact.J3500
Wetland$Efecto_J_5000<- 3000 - WWTP.impact.J5000
Stream$Efecto_J_2250<- 3000 - River.impact.J2250
Stream$Efecto_J_3500<- 3000 - River.impact.J3500
Stream$Efecto_J_5000<- 3000 - River.impact.J5000
#Extract community filter value in each scenario is possible, we will also use it for the next figures
Wetland$Efecto_F_2250<- 1- WWTP.impact.F2250
Wetland$Efecto_F_3500<- 1- WWTP.impact.F3500
Wetland$Efecto_F_5000<- 1- WWTP.impact.F5000
Stream$Efecto_F_2250<- 1- River.impact.F2250
Stream$Efecto_F_3500<- 1- River.impact.F3500
Stream$Efecto_F_5000<- 1- River.impact.F5000

######################################################################################
######################################################################################
#Figure 2: 
Imagen_ef_3500J_river<- ggplot(data=Stream, 
                               aes(colour=Efecto_J_3500)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  guides(col= guide_colourbar(title= "Pollutant concentration"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_Ratio_200_3500J_river<- ggplot(data=Stream, 
                                      aes(colour= Ratio_200_3500J)) +
  geom_sf() +
  viridis::scale_colour_viridis(option= "turbo")+#, limits=c(-0.05,0.035))+
  guides(col= guide_colourbar(title= "logRatio Richness"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_Ratio_200_3500F_river<- ggplot(data=Stream, 
                                      aes(colour= Ratio_200_3500F)) +
  geom_sf() +
  viridis::scale_colour_viridis(option= "turbo")+#, limits=c(-0.05,0.035))+
  guides(col= guide_colourbar(title= "logRatio Richness"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggarrange(Imagen_Ratio_200_3500J_river, Imagen_Ratio_200_3500F_river,
          common.legend = F, legend = "left", ncol = 1, nrow = 2)

par(mfrow=c(2,1), mar=c(5,5,4,4))
plot(Stream$Ratio_200_3500J~(Stream$Efecto_J_3500),  
     ylab = "log-ratio of Richness ", xlab = "Pollutant concentration",xaxt="n",
     cex.lab=2, cex.axis=1.3, cex.main=3.5, cex.sub=2, pch=19,
     main= "Abundance effect", font.main=4)
plot(Stream$Ratio_200_3500F~(Stream$Efecto_F_3500),  
     ylab = "log-ratio of Richness ", xlab = "Pollutant concentration",xaxt="n",
     cex.lab=2, cex.axis=1.3, cex.main=3.5, cex.sub=2, pch=19,
     main= "Filter effect", font.main=4)

######################################################################################
######################################################################################
#Figure 3: 
# Pollution propagation:
Imagen_ef_2250J_river<- ggplot(data=Stream, 
                               aes(colour=Efecto_J_2250)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  
  guides(col= guide_colourbar(title= "Community abundance"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_ef_3500J_river<- ggplot(data=Stream, 
                               aes(colour=Efecto_J_3500)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  guides(col= guide_colourbar(title= "Community abundance"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_ef_5000J_river<- ggplot(data=Stream, 
                               aes(colour=Efecto_J_5000)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  guides(col= guide_colourbar(title= "Community abundance"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggpubr::ggarrange(Imagen_ef_2250J_river, Imagen_ef_3500J_river, Imagen_ef_5000J_river,
                  labels= c("A", "B", "C"), 
                  ncol=1, nrow=3, common.legend= T, legend= "top")
#GAMs
DF.River=NULL
DF.River<- matrix(data = Stream$clos, ncol=1, nrow = 515)
colnames(DF.River)<- c("clos")
DF.River<- cbind(DF.River, (Stream$Efecto_F_2250))
DF.River<- cbind(DF.River, (Stream$Efecto_F_3500))
DF.River<- cbind(DF.River, (Stream$Efecto_F_5000))
DF.River<- cbind(DF.River, (Stream$Efecto_J_2250))
DF.River<- cbind(DF.River, (Stream$Efecto_J_3500))
DF.River<- cbind(DF.River, (Stream$Efecto_J_5000))
colnames(DF.River)<- c("clos", "Efecto_F_2250", "Efecto_F_3500", "Efecto_F_5000",
                       "Efecto_J_2250", "Efecto_J_3500","Efecto_J_5000")
#Create vectors that will be used as response variables in GAMs
as.vector(log(alfa_river_35_2250J/alfa_river_35_solo)) -> ratio_35_2250J; cbind(DF.River, ratio_35_2250J) -> DF.River
as.vector(log(alfa_river_85_2250J/alfa_river_85_solo)) -> ratio_85_2250J; cbind(DF.River, ratio_85_2250J)-> DF.River
as.vector(log(alfa_river_200_2250J/alfa_river_200_solo)) -> ratio_200_2250J; cbind(DF.River, ratio_200_2250J)-> DF.River
as.vector(log(alfa_river_35_3500J/alfa_river_35_solo)) -> ratio_35_3500J; cbind(DF.River, ratio_35_3500J)-> DF.River
as.vector(log(alfa_river_85_3500J/alfa_river_85_solo)) -> ratio_85_3500J; cbind(DF.River, ratio_85_3500J)-> DF.River
as.vector(log(alfa_river_200_3500J/alfa_river_200_solo)) -> ratio_200_3500J; cbind(DF.River, ratio_200_3500J)-> DF.River
as.vector(log(alfa_river_35_5000J/alfa_river_35_solo)) -> ratio_35_5000J; cbind(DF.River, ratio_35_5000J)-> DF.River
as.vector(log(alfa_river_85_5000J/alfa_river_85_solo)) -> ratio_85_5000J; cbind(DF.River, ratio_85_5000J)-> DF.River
as.vector(log(alfa_river_200_5000J/alfa_river_200_solo)) -> ratio_200_5000J; cbind(DF.River, ratio_200_5000J)-> DF.River

as.vector(log(alfa_river_35_2250F/alfa_river_35_solo)) -> ratio_35_2250F; cbind(DF.River, ratio_35_2250F)-> DF.River
as.vector(log(alfa_river_85_2250F/alfa_river_85_solo)) -> ratio_85_2250F; cbind(DF.River, ratio_85_2250F)-> DF.River
as.vector(log(alfa_river_200_2250F/alfa_river_200_solo)) -> ratio_200_2250F; cbind(DF.River, ratio_200_2250F)-> DF.River
as.vector(log(alfa_river_35_3500F/alfa_river_35_solo)) -> ratio_35_3500F; cbind(DF.River, ratio_35_3500F)-> DF.River
as.vector(log(alfa_river_85_3500F/alfa_river_85_solo)) -> ratio_85_3500F; cbind(DF.River, ratio_85_3500F)-> DF.River
as.vector(log(alfa_river_200_3500F/alfa_river_200_solo)) -> ratio_200_3500F; cbind(DF.River, ratio_200_3500F)-> DF.River
as.vector(log(alfa_river_35_5000F/alfa_river_35_solo)) -> ratio_35_5000F; cbind(DF.River, ratio_35_5000F)-> DF.River
as.vector(log(alfa_river_85_5000F/alfa_river_85_solo)) -> ratio_85_5000F; cbind(DF.River, ratio_85_5000F)-> DF.River
as.vector(log(alfa_river_200_5000F/alfa_river_200_solo)) -> ratio_200_5000F; cbind(DF.River, ratio_200_5000F)-> DF.River
######################################################
as.data.frame(DF.River)-> DF.River
par(mfrow=c(2,3), mar=c(4,4.3,2.5,5))
######################################################
#e50= 2250
gam(ratio_35_2250J~ clos* Efecto_J_2250, data= DF.River)-> gam.River.35.2250J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.35.2250J, view=c("Efecto_J_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=2250, d50=35", zlim = c(-0.8,0.01), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_2250J~ clos* Efecto_J_2250, data= DF.River)-> gam.River.85.2250J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.85.2250J, view=c("Efecto_J_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=2250, d50=85", zlim = c(-0.8,0.01), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_2250J~ clos* Efecto_J_2250, data= DF.River)-> gam.River.200.2250J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.200.2250J, view=c("Efecto_J_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=2250, d50=200", zlim = c(-0.8,0.01), rm.ranef=F, hide.label=T, add.color.legend=F)
gam((ratio_35_2250F) ~ clos* Efecto_F_2250, data= DF.River)-> gam.River.35.2250F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.35.2250F, view=c("Efecto_F_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=2250, d50=35", zlim = c(-5.3,-0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_2250F~ clos* Efecto_F_2250, data= DF.River)-> gam.River.85.2250F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.85.2250F, view=c("Efecto_F_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=2250, d50=85", zlim = c(-5.3,-0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_2250F~ clos* Efecto_F_2250, data= DF.River)-> gam.River.200.2250F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.200.2250F, view=c("Efecto_F_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=2250, d50=200", zlim = c(-5.3,-0.1), rm.ranef=F, hide.label=T, add.color.legend=F)

# e50=3500
gam(ratio_35_3500J~ clos* Efecto_J_3500, data= DF.River)-> gam.River.35.3500J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.35.3500J, view=c("Efecto_J_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=3500, d50=35", zlim = c(-0.9,0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_3500J~ clos* Efecto_J_3500, data= DF.River)-> gam.River.85.3500J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.85.3500J, view=c("Efecto_J_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=3500, d50=85", zlim = c(-0.9,0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_3500J~ clos* Efecto_J_3500, data= DF.River)-> gam.River.200.3500J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.200.3500J, view=c("Efecto_J_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=3500, d50=200", zlim = c(-0.9,0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam((ratio_35_3500F) ~ clos* Efecto_F_3500, data= DF.River)-> gam.River.35.3500F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.35.3500F, view=c("Efecto_F_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=3500, d50=35", zlim = c(-3.4,-0.8), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_3500F~ clos* Efecto_F_3500, data= DF.River)-> gam.River.85.3500F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.85.3500F, view=c("Efecto_F_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=3500, d50=85", zlim = c(-3.4,-0.8), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_3500F~ clos* Efecto_F_3500, data= DF.River)-> gam.River.200.3500F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.200.3500F, view=c("Efecto_F_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=200", zlim = c(-3.4,-0.8), rm.ranef=F, hide.label=T, add.color.legend=F)

# e50=5000 #####
gam(ratio_35_5000J~ clos* Efecto_J_5000, data= DF.River)-> gam.River.35.5000J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.35.5000J, view=c("Efecto_J_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=5000, d50=35", zlim = c(-1.2,0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_5000J~ clos* Efecto_J_5000, data= DF.River)-> gam.River.85.5000J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.85.5000J, view=c("Efecto_J_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=5000, d50=85", zlim = c(-1.2,0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_5000J~ clos* Efecto_J_5000, data= DF.River)-> gam.River.200.5000J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.200.5000J, view=c("Efecto_J_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=5000, d50=200", zlim = c(-1.2,0.1), rm.ranef=F, hide.label=T, add.color.legend=F)
gam((ratio_35_5000F) ~ clos* Efecto_F_5000, data= DF.River)-> gam.River.35.5000F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.35.5000F, view=c("Efecto_F_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=35", zlim = c(-2.3,-2), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_5000F~ clos* Efecto_F_5000, data= DF.River)-> gam.River.85.5000F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.85.5000F, view=c("Efecto_F_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=85", zlim = c(-2.3,-2), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_5000F~ clos* Efecto_F_5000, data= DF.River)-> gam.River.200.5000F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.River.200.5000F, view=c("Efecto_F_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=200", zlim = c(-2.3,-2), rm.ranef=F, hide.label=T, add.color.legend=F)



######################################################################################
######################################################################################
########################     SUPPLEMENTARY      ######################################
######################################################################################
######################################################################################

##### Figure S1. Reference community assembly: Wetland landscape
### d50 = 35 ####
Wetland$diversidad.35<- (alfa_35_solo)
alfa_35<- ggplot(data=Wetland, 
                 aes(colour=diversidad.35)) +
  geom_sf(size=2, pch=19) +
  scale_colour_gradientn(colours=c("darkblue", "steelblue1", "yellow", "orange3", "red4"), limits=c(50,200))+
  guides(col= guide_colourbar(title= "Alpha diversity"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
### d50 = 85 ####
Wetland$diversidad.85<- (alfa_85_solo)
alfa_85<- ggplot(data=Wetland, 
                 aes(colour=diversidad.85)) +
  geom_sf(size=2, pch=19) +
  scale_colour_gradientn(colours=c("darkblue", "steelblue1", "yellow", "orange3", "red4"), limits=c(50,200))+
  guides(col= guide_colourbar(title= "Alpha diversity"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
### d50 = 200 ####
Wetland$diversidad.200<- (alfa_200_solo)
alfa_200<- ggplot(data=Wetland, 
                  aes(colour=diversidad.200)) +
  geom_sf(size=2, pch=19) +
  scale_colour_gradientn(colours=c("darkblue", "steelblue1", "yellow", "orange3", "red4"), limits=c(50,200))+
  guides(col= guide_colourbar(title= "Alpha diversity"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggpubr::ggarrange(alfa_35, alfa_85, alfa_200,
                  labels= c("A", "B", "C"), 
                  ncol=3, nrow=1, common.legend= T, legend= "right")

######################################################################################
######################################################################################
#Figure S2
Imagen_ef_3500J<- ggplot(data=Wetland, 
                         aes(colour=Efecto_J_3500)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  guides(col= guide_colourbar(title= "Pollutant concentration"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggarrange(Imagen_ef_3500J, legend = "left")
Imagen_Ratio_200_3500J<- ggplot(data=Wetland, 
                                aes(colour= Ratio_200_3500J)) +
  geom_sf() +
  viridis::scale_colour_viridis(option= "turbo")+#, limits=c(-0.05,0.035))+
  guides(col= guide_colourbar(title= "logRatio Richness"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_Ratio_200_3500F<- ggplot(data=Wetland, 
                                aes(colour= Ratio_200_3500F)) +
  geom_sf() +
  viridis::scale_colour_viridis(option= "turbo")+#, limits=c(-0.05,0.035))+
  guides(col= guide_colourbar(title= "logRatio Richness"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggarrange(Imagen_Ratio_200_3500J, Imagen_Ratio_200_3500F,
          ncol=1, nrow=2, common.legend= F, legend= "left")
par(mfrow=c(2,1), mar=c(5,5,4,4))
plot(Wetland$Ratio_200_3500J~(Wetland$Efecto_J_3500),  
      ylab = "log-ratio of Richness ", xlab = "Pollutant concentration",xaxt="n",
      cex.lab=2, cex.axis=1.3, cex.main=3.5, cex.sub=2, pch=19,
      main= "Abundance effect", font.main=4)
plot( Wetland$Ratio_200_3500F~(Wetland$Efecto_F_3500),  
      ylab = "log-ratio of Richness ", xlab = "Pollutant concentration",xaxt="n",
      cex.lab=2, cex.axis=1.3, cex.main=3.5, cex.sub=2, pch=19,
      main= "Filter effect", font.main=4)

######################################################################################
######################################################################################
#Figure S3
Imagen_ef_2250J<- ggplot(data=Wetland, 
                         aes(colour=Efecto_J_2250)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  guides(col= guide_colourbar(title= "Community abundance"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_ef_3500J<- ggplot(data=Wetland, 
                         aes(colour=Efecto_J_3500)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  guides(col= guide_colourbar(title= "Community abundance"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_ef_5000J<- ggplot(data=Wetland, 
                         aes(colour=Efecto_J_5000)) +
  geom_sf() +
  scale_colour_gradientn(colors=c( "steel blue 1","orange 2","orange red" ), limits=c(0,3000))+
  guides(col= guide_colourbar(title= "Community abundance"))+
  theme_pubr(border=TRUE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggarrange(Imagen_ef_2250J, Imagen_ef_3500J, Imagen_ef_5000J,
          labels= c("A", "B", "C"), 
          ncol=1, nrow=3, common.legend= T, legend= "top")
#GAMs
library(tidyverse)
library(sjPlot)
library(lme4)
library(mgcv)
DF=NULL
library(itsadug)
DF<- matrix(data = clos, ncol=1, nrow = 1888)
colnames(DF)<- c("clos")
DF<- cbind(DF, (Wetland$Efecto_F_2250))
DF<- cbind(DF, (Wetland$Efecto_F_3500))
DF<- cbind(DF, (Wetland$Efecto_F_5000))
DF<- cbind(DF, (Wetland$Efecto_J_2250))
DF<- cbind(DF, (Wetland$Efecto_J_3500))
DF<- cbind(DF, (Wetland$Efecto_J_5000))
colnames(DF)<- c("clos", "Efecto_F_2250", "Efecto_F_3500", "Efecto_F_5000",
                 "Efecto_J_2250", "Efecto_J_3500","Efecto_J_5000")

#Create vectors that will be used as response variables in GAMs
as.vector(log(alfa_35_2250J/alfa_35_solo)) -> ratio_35_2250J; cbind(DF, ratio_35_2250J) -> DF
as.vector(log(alfa_85_2250J/alfa_85_solo)) -> ratio_85_2250J; cbind(DF, ratio_85_2250J)-> DF
as.vector(log(alfa_200_2250J/alfa_200_solo)) -> ratio_200_2250J; cbind(DF, ratio_200_2250J)-> DF
as.vector(log(alfa_35_3500J/alfa_35_solo)) -> ratio_35_3500J; cbind(DF, ratio_35_3500J)-> DF
as.vector(log(alfa_85_3500J/alfa_85_solo)) -> ratio_85_3500J; cbind(DF, ratio_85_3500J)-> DF
as.vector(log(alfa_200_3500J/alfa_200_solo)) -> ratio_200_3500J; cbind(DF, ratio_200_3500J)-> DF
as.vector(log(alfa_35_5000J/alfa_35_solo)) -> ratio_35_5000J; cbind(DF, ratio_35_5000J)-> DF
as.vector(log(alfa_85_5000J/alfa_85_solo)) -> ratio_85_5000J; cbind(DF, ratio_85_5000J)-> DF
as.vector(log(alfa_200_5000J/alfa_200_solo)) -> ratio_200_5000J; cbind(DF, ratio_200_5000J)-> DF

as.vector(log(alfa_35_2250F/alfa_35_solo)) -> ratio_35_2250F; cbind(DF, ratio_35_2250F)-> DF
as.vector(log(alfa_85_2250F/alfa_85_solo)) -> ratio_85_2250F; cbind(DF, ratio_85_2250F)-> DF
as.vector(log(alfa_200_2250F/alfa_200_solo)) -> ratio_200_2250F; cbind(DF, ratio_200_2250F)-> DF
as.vector(log(alfa_35_3500F/alfa_35_solo)) -> ratio_35_3500F; cbind(DF, ratio_35_3500F)-> DF
as.vector(log(alfa_85_3500F/alfa_85_solo)) -> ratio_85_3500F; cbind(DF, ratio_85_3500F)-> DF
as.vector(log(alfa_200_3500F/alfa_200_solo)) -> ratio_200_3500F; cbind(DF, ratio_200_3500F)-> DF
as.vector(log(alfa_35_5000F/alfa_35_solo)) -> ratio_35_5000F; cbind(DF, ratio_35_5000F)-> DF
as.vector(log(alfa_85_5000F/alfa_85_solo)) -> ratio_85_5000F; cbind(DF, ratio_85_5000F)-> DF
as.vector(log(alfa_200_5000F/alfa_200_solo)) -> ratio_200_5000F; cbind(DF, ratio_200_5000F)-> DF
######################################################
as.data.frame(DF)-> DF
par(mfrow=c(2,3), mar=c(4,4.3,2.5,5))
######################################################
# e50=2250 #####
gam(ratio_35_2250J~ clos* Efecto_J_2250, data= DF)-> gam.WWTP.35.2250J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.35.2250J, view=c("Efecto_J_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=2250, d50=35", zlim = c(-0.56,0.013), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_2250J~ clos* Efecto_J_2250, data= DF)-> gam.WWTP.85.2250J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.85.2250J, view=c("Efecto_J_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=2250, d50=85", zlim = c(-0.56,0.013), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_2250J~ clos* Efecto_J_2250, data= DF)-> gam.WWTP.200.2250J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.200.2250J, view=c("Efecto_J_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=2250, d50=200", zlim = c(-0.56,0.013), rm.ranef=F, hide.label=T, add.color.legend=F)
gam((ratio_35_2250F) ~ clos* Efecto_F_2250, data= DF)-> gam.WWTP.35.2250F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.35.2250F, view=c("Efecto_F_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=2250, d50=35", zlim = c(-2.2,0.04), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_2250F~ clos* Efecto_F_2250, data= DF)-> gam.WWTP.85.2250F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.85.2250F, view=c("Efecto_F_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=2250, d50=85", zlim = c(-2.2,0.04), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_2250F~ clos* Efecto_F_2250, data= DF)-> gam.WWTP.200.2250F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.200.2250F, view=c("Efecto_F_2250", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=2250, d50=200", zlim = c(-2.2,0.04), rm.ranef=F, hide.label=T, add.color.legend=F)

# e50=3500
gam(ratio_35_3500J~ clos* Efecto_J_3500, data= DF)-> gam.WWTP.35.3500J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.35.3500J, view=c("Efecto_J_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=3500, d50=35", zlim = c(-0.72,0.084), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_3500J~ clos* Efecto_J_3500, data= DF)-> gam.WWTP.85.3500J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.85.3500J, view=c("Efecto_J_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=3500, d50=85", zlim = c(-0.72,0.084), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_3500J~ clos* Efecto_J_3500, data= DF)-> gam.WWTP.200.3500J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.200.3500J, view=c("Efecto_J_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=3500, d50=200", zlim = c(-0.72,0.084), rm.ranef=F, hide.label=T, add.color.legend=F)
gam((ratio_35_3500F) ~ clos* Efecto_F_3500, data= DF)-> gam.WWTP.35.3500F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.35.3500F, view=c("Efecto_F_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=3500, d50=35", zlim = c(-1.4,0.17), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_3500F~ clos* Efecto_F_3500, data= DF)-> gam.WWTP.85.3500F
fvisgam(color= c( "red4", "gold2", "darkgreen"), gam.WWTP.85.3500F, view=c("Efecto_F_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=3500, d50=85", zlim = c(-1.4,0.17), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_3500F~ clos* Efecto_F_3500, data= DF)-> gam.WWTP.200.3500F
fvisgam(color= c( "red4", "gold2", "darkgreen"), gam.WWTP.200.3500F, view=c("Efecto_F_3500", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=200", zlim = c(-1.4,0.17), rm.ranef=F, hide.label=T, add.color.legend=F)

# e50=5000 #####
gam(ratio_35_5000J~ clos* Efecto_J_5000, data= DF)-> gam.WWTP.35.5000J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.35.5000J, view=c("Efecto_J_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=5000, d50=35", zlim = c(-0.93,0.197), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_5000J~ clos* Efecto_J_5000, data= DF)-> gam.WWTP.85.5000J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.85.5000J, view=c("Efecto_J_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=5000, d50=85", zlim = c(-0.93,0.197), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_5000J~ clos* Efecto_J_5000, data= DF)-> gam.WWTP.200.5000J
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.200.5000J, view=c("Efecto_J_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Community abundance", ylab="Closeness", dec=3, main="Abundance effect, e50=5000, d50=200", zlim = c(-0.93,0.197), rm.ranef=F, hide.label=T, add.color.legend=F)
gam((ratio_35_5000F) ~ clos* Efecto_F_5000, data= DF)-> gam.WWTP.35.5000F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.35.5000F, view=c("Efecto_F_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=35", zlim = c(-0.7,-0.6), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_85_5000F~ clos* Efecto_F_5000, data= DF)-> gam.WWTP.85.5000F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.85.5000F, view=c("Efecto_F_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=85", zlim = c(-0.7,-0.6), rm.ranef=F, hide.label=T, add.color.legend=F)
gam(ratio_200_5000F~ clos* Efecto_F_5000, data= DF)-> gam.WWTP.200.5000F
fvisgam(color= c("red4", "gold2", "darkgreen"), gam.WWTP.200.5000F, view=c("Efecto_F_5000", "clos"), n.grid = 100, alpha.diff = c(0.8), zlab="LogRatio diversity loss", xlab="Pollution filter", ylab="Closeness", dec=3, main="Filter effect, e50=5000, d50=200", zlim = c(-0.7,-0.6), rm.ranef=F, hide.label=T, add.color.legend=F)



######################################################################
#Figure S4. Communities centrality
Imagen_clos<- ggplot(data=Wetland, 
                     aes(colour=clos)) +
  geom_sf() +
  scale_colour_gradientn(colors=c("yellow2" ,"orange3", "red4"))+
  ggtitle("Wetland landscape")+
  guides(col= guide_colourbar(title= "Clossenes"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
Imagen_clos_river<- ggplot(data=Stream, 
                           aes(colour=clos)) +
  geom_sf() +
  scale_colour_gradientn(colors=c("yellow2" ,"orange3", "red4"))+
  ggtitle("Stream landscape")+
  guides(col= guide_colourbar(title= "Clossenes"))+
  theme_pubr(border=TRUE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggpubr::ggarrange(Imagen_clos, Imagen_clos_river,
                  labels= c("A", "B"), 
                  ncol=2, nrow=1, common.legend= T, legend= "right")
