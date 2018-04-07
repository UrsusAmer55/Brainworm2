library(HATOPO)
library(chron)
library(spatstat)
library(raster)
library(ggplot2)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(maptools)
library(rgeos)
library(geosphere)
library(rgdal)
library(chron)
library(plyr)
library(ggmap)
library(mapproj)
library(ctmm)
library(move)
library(dplyr)



deerW<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/AmandaRSF/deer files-20170822T165945Z-001/deer files/s3.winter.all.b.csv") 
head(deerW)
deerW$Hsd<-"NA"

deerS<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/AmandaRSF/deer files-20170822T165945Z-001/deer files/s3.summer.all.b.csv") 
head(deerS)
deerS$X.1<-NULL
colnames(deerS)[2]<-c("animalID")
names(deerW)
deerS$sndpt<-"NA"

deerA<-rbind(deerS,deerW)

D2SP<-SpatialPointsDataFrame(coords=deerA[c("easting","northing")],proj4string= CRS("+proj=utm +zone=15 ellps=WGS84"),data=deerA)
#plot(D2SP,col=D2SP$y)
table(D2SP$y)
#get proper lat long
D2SPlatlong <- spTransform(D2SP, CRS("+proj=longlat +datum=WGS84"))
Dcoords<-as.data.frame(D2SPlatlong@coords)
head(Dcoords)
colnames(Dcoords)<-c("long","lat")
deerA<-cbind(deerA,Dcoords)



head(deerA)
unique(deerA$season)


#assign a season
deerA$dtL<-strptime(deerA$dt, "%m/%d/%Y %H:%M",tz="America/Chicago")
deerA$dtL<-as.POSIXct(deerA$dtL)
head(deerA)
deerA$month<-as.numeric(format(deerA$dtL,"%m"))
table(deerA$month)
deerA$monthday<-format(deerA$dtL, format="%m-%d")

deerA$season4<-NA
deerA$season4[deerA$monthday>"03-15" & deerA$monthday<="06-15"]<- "Spring"
deerA$season4[deerA$monthday>"06-15" & deerA$monthday<="09-15"]<- "Summer"
deerA$season4[deerA$monthday>"09-15" & deerA$monthday<="12-15"]<- "Fall"
deerA$season4[deerA$monthday>"12-15" | deerA$monthday<="03-15"]<- "Winter"

table(deerA$season4)
table(deerA$y)
deerAY<-deerA[deerA$y==1,]

hab<-deerAY %>% 
  group_by(animalID,cover,season4) %>% 
  summarise(Frequency = sum(y))

tot<-deerAY %>% 
  group_by(animalID,season4) %>% 
  summarise(Frequency = sum(y))

hab<-as.data.frame(hab)
tot<-as.data.frame(tot)

per<-merge(hab,tot,by=c("animalID","season4"),all.x=TRUE)

colnames(per)<-c("animalID","season4","cover","use","total")
per$percent<-per$use/per$total
head(per)

boxplot(percent~cover+season4,data=per)


aveAMhabuse_season4<-aggregate( percent~cover+season4,data=per, FUN = "mean")
colnames(aveAMhabuse_season4)<-c("cover","season4","mean per")

aveAMhabuse_season4sd<-aggregate( percent~cover+season4,data=per, FUN = "sd")
colnames(aveAMhabuse_season4sd)<-c("cover","season4","sd per")
aveAMhabuse_season4n<-aggregate( percent~cover+season4,data=per, function(x) length(unique(x)))
colnames(aveAMhabuse_season4n)<-c("cover","season4","sample size")

aveAMhabuse_season4_byindiv<-cbind(aveAMhabuse_season4,aveAMhabuse_season4sd,aveAMhabuse_season4n)

aveAMhabuse_season4_byindiv[,c(4:5,7:8)]<-NULL

aveAMhabuse_season4_byindiv$LOCI<-aveAMhabuse_season4_byindiv$`mean per`-((aveAMhabuse_season4_byindiv$`sd per`/sqrt(aveAMhabuse_season4_byindiv$`sample size`)*1.96))
aveAMhabuse_season4_byindiv$HICI<-aveAMhabuse_season4_byindiv$`mean per`+((aveAMhabuse_season4_byindiv$`sd per`/sqrt(aveAMhabuse_season4_byindiv$`sample size`)*1.96))


write.csv(aveAMhabuse_season4,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData//HabUse_AMcat_season4_032318.csv")

#repeat all of this but for the NLCD lidar data!
#first need to extract the values
head(deerAY)

#LID is LiDAR from James, NLCD is actually LIdAR from Knight Group
AllHab<-raster("E:/ZooLaptop_062817/DeerData/for_Zoo/GIS_files/GIS_Ditmer/Deer_NE_LiDAR-20170523T184449Z-001/Deer_NE_LiDAR/MN_LandCover_2014_Arrowhead.img")
AllHabC<-crop(AllHab, under)
plot(AllHabC)
system.time(deerAY$NLCDLID<-extract(AllHabC, deerAY[,8:9]))

table(deerAY$season4)

hab<-deerAY %>% 
  group_by(animalID,NLCDLID,season4) %>% 
  summarise(Frequency = sum(y))

tot<-deerAY %>% 
  group_by(animalID,season4) %>% 
  summarise(Frequency = sum(y))

hab<-as.data.frame(hab)
tot<-as.data.frame(tot)

head(hab)

per<-merge(hab,tot,by=c("animalID","season4"),all.x=TRUE)

colnames(per)<-c("animalID","season4","NLCDLIDcover","use","total")
per$percent<-per$use/per$total
head(per)

boxplot(percent~NLCDLIDcover,data=per)

#ave<-aggregate(x=per,by=list())
aveNLCDuse_season4<-aggregate( percent~NLCDLIDcover+season4,data=per, FUN = "mean")
colnames(aveNLCDuse_season4)<-c("NLCDLIDcover","season4","mean per")

aveNLCDuse_season4sd<-aggregate( percent~NLCDLIDcover+season4,data=per, FUN = "sd")
colnames(aveNLCDuse_season4sd)<-c("NLCDLIDcover","season4","sd per")
aveNLCDuse_season4n<-aggregate( percent~NLCDLIDcover+season4,data=per, function(x) length(unique(x)))
colnames(aveNLCDuse_season4n)<-c("NLCDLIDcover","season4","sample size")

aveNLCDuse_season4_byindiv<-cbind(aveNLCDuse_season4,aveNLCDuse_season4sd,aveNLCDuse_season4n)

aveNLCDuse_season4_byindiv[,c(4:5,7:8)]<-NULL

aveNLCDuse_season4_byindiv$LOCI<-aveNLCDuse_season4_byindiv$`mean per`-((aveNLCDuse_season4_byindiv$`sd per`/sqrt(aveNLCDuse_season4_byindiv$`sample size`)*1.96))
aveNLCDuse_season4_byindiv$HICI<-aveNLCDuse_season4_byindiv$`mean per`+((aveNLCDuse_season4_byindiv$`sd per`/sqrt(aveNLCDuse_season4_byindiv$`sample size`)*1.96))


write.csv(aveNLCDuse_season4_byindiv,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData//HabUse_NLCDLIDcat_season4_032318.csv")








deerSum<-deerA[deerA$season=="summer",]
head(deerSum)


hab<-deerSum %>% 
  group_by(animalID,cover) %>% 
  summarise(Frequency = sum(y))

tot<-deerSum %>% 
  group_by(animalID) %>% 
  summarise(Frequency = sum(y))

hab<-as.data.frame(hab)
tot<-as.data.frame(tot)

per<-merge(hab,tot,by="animalID",all.x=TRUE)

colnames(per)<-c("animalID","cover","use","total")
per$percent<-per$use/per$total
head(per)

boxplot(percent~cover,data=per)

ave<-aggregate(x=per,by=list())
aggregate( percent~cover,data=per, FUN = "mean")
aggregate( percent~cover,data=per, FUN = "sd")



###re-do but also by region
unique(deerSum$region)

deerSum$region[deerSum$region=="ely"]<-"Ely"
deerSum$region[deerSum$region=="isabella"]<-"Isa"
deerSum$region<-droplevels(deerSum$region)
unique(deerSum$region)

hab2<-deerSum %>% 
  group_by(region,animalID,cover) %>% 
  summarise(Frequency = sum(y))

tot2<-deerSum %>% 
  group_by(region,animalID) %>% 
  summarise(Frequency = sum(y))

hab2<-as.data.frame(hab2)
tot2<-as.data.frame(tot2)

head(hab2)
head(tot2)

per2<-merge(hab2,tot2,by="animalID",all.x=TRUE)
head(per2)
per2$region.y<-NULL
colnames(per2)<-c("animalID","region","cover","use","total")
per2$percent<-per2$use/per2$total
head(per2)

boxplot(percent~region+cover,data=per2)

###re-do but also by migrator
unique(deerSum$migrators)

hab3<-deerSum %>% 
  group_by(migrators,animalID,cover) %>% 
  summarise(Frequency = sum(y))

tot3<-deerSum %>% 
  group_by(migrators,animalID) %>% 
  summarise(Frequency = sum(y))

hab3<-as.data.frame(hab3)
tot3<-as.data.frame(tot3)

head(hab3)
head(tot3)

per3<-merge(hab3,tot3,by="animalID",all.x=TRUE)
head(per3)
per3$migrators.y<-NULL
colnames(per3)<-c("animalID","migrators","cover","use","total")
per3$percent<-per3$use/per3$total
head(per3)

boxplot(percent~migrators+cover,data=per3)


deer<-deerA[deerA$season=="summer",]
deer$animalID<-as.factor(deer$animalID)


#elev_P75_0p15plus_30METERS_UTM15.tif = 75th percentile for canopy height to help decrease influence from super-canopy trees
canheight<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/elev_P75_0p15plus_30METERS_UTM15.tif")
plot(canheight)
#Canopy_Prop3m_UTM.tif = returns above 3-m for % canopy cover
cancover<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/Canopy_Prop3m_UTM.tif")
#Prop_1_3m = non-ground returns between 1 and 3 m above ground (proxy for understory cover).
under<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/Prop_1_3m")
plot(under)


lidar<-stack(canheight,cancover,under)
str(lidar)
plot(lidar)

#predictions from Amanda's RSF

NLCD<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/NLCD 2011 - Land Cover1.tif")

NLCDC<-crop(NLCD, under)

under
canheight
cancover
AllHabC

underZ<-scale(under,center = TRUE, scale = TRUE)
plot(underZ)


cancover100<-cancover*100
cancover100Z<-scale(cancover100,center = TRUE, scale = TRUE)
plot(cancover100Z)

plot(canheight)
canheightZ<-scale(canheight,center = TRUE, scale = TRUE)
plot(canheightZ)


ext<-extent(habC)
CTI2<-setExtent(CTI, ext)
CTI2 <- resample(CTI2, FrstHab)

#create a separate raster for each cover type
##from the metadata: 11 Open Water, 21:24 Developed, 31 Barren, 41	Deciduous Forest, 42	Evergreen Forest, 43	Mixed Forest, 52	Shrub/Scrub,
#71	Grassland/Herbaceous, 81	Pasture/Hay, 82	Cultivated Crops, 90	Woody Wetlands, 95	Emergent Herbaceous Wetlands
unique(NLCDC@data@values)
table(NLCDC@data@values)

#woody wetland
NLCDC90<-NLCDC==90

#deciduous forest
NLCDC41<-NLCDC==41

#mixed forest
NLCDC43<-NLCDC==43

#conifer forest
NLCDC42<-NLCDC==42

#shrub
NLCDC52<-NLCDC==52

#other
NLCDC21<-NLCDC==21
NLCDC22<-NLCDC==22
NLCDC23<-NLCDC==23
NLCDC24<-NLCDC==24
NLCDC95<-NLCDC==95
NLCDC81<-NLCDC==81
NLCDC82<-NLCDC==82
NLCDC71<-NLCDC==71

NLCDCother<-NLCDC21+NLCDC22+NLCDC23+NLCDC24+NLCDC95+NLCDC81+NLCDC82+NLCDC71
plot(NLCDCother)



#non habitat
NLCDC0<-NLCDC==0
NLCDC11<-NLCDC==11
NLCDC31<-NLCDC==31

NLCDCnon<-NLCDC0+NLCDC11+NLCDC31

plot(NLCDC42)



#non-snow - non-migratory coeff other than hab categories
canheightCOEF<--0.01
cancoverCOEF<--0.12
underCOEF<--0.03

#calculate values for each habitat category (add in coeff for habitat below), should be a value of 0 if not in a given habitat types
#other
AMpredictNoSnow_NLCDCother<-((NLCDCother*.67)+((underZ*underCOEF)*NLCDCother)+((canheightZ*canheightCOEF)*NLCDCother)+((cancover100Z*cancoverCOEF)*NLCDCother))
plot(AMpredictNoSnow_NLCDCother)
#woody wet reference (=0)
AMpredictNoSnow_NLCDC90<-((NLCDC90*0)+((underZ*underCOEF)*NLCDC90)+((canheightZ*canheightCOEF)*NLCDC90)+((cancover100Z*cancoverCOEF)*NLCDC90))
plot(AMpredictNoSnow_NLCDC90)
#conifer forest
AMpredictNoSnow_NLCDC42<-((NLCDC42*.2)+((underZ*underCOEF)*NLCDC42)+((canheightZ*canheightCOEF)*NLCDC42)+((cancover100Z*cancoverCOEF)*NLCDC42))
plot(AMpredictNoSnow_NLCDC42)
#deciduous forest
AMpredictNoSnow_NLCDC41<-(NLCDC41*.61+((underZ*underCOEF)*NLCDC41)+((canheightZ*canheightCOEF)*NLCDC41)+((cancover100Z*cancoverCOEF)*NLCDC41))
plot(AMpredictNoSnow_NLCDC41)
#mixed forest
AMpredictNoSnow_NLCDC43<-((NLCDC43*.40)+((underZ*underCOEF)*NLCDC43)+((canheightZ*canheightCOEF)*NLCDC43)+((cancover100Z*cancoverCOEF)*NLCDC43))
plot(AMpredictNoSnow_NLCDC43)
#shrubs
AMpredictNoSnow_NLCDC52<-((NLCDC52*.61)+((underZ*underCOEF)*NLCDC52)+((canheightZ*canheightCOEF)*NLCDC52)+((cancover100Z*cancoverCOEF)*NLCDC52))
plot(AMpredictNoSnow_NLCDC52)
#non-habitat
AMpredictNoSnow_NLCDCnon<-((NLCDCnon*0)+((underZ*underCOEF)*NLCDCnon)+((canheightZ*canheightCOEF)*NLCDCnon)+((cancover100Z*cancoverCOEF)*NLCDCnon))
plot(AMpredictNoSnow_NLCDCnon)



#add up all of the rasters
AMpredictNoSnow_habC_ALL<-AMpredictNoSnow_NLCDCother+AMpredictNoSnow_NLCDC90+AMpredictNoSnow_NLCDC42+AMpredictNoSnow_NLCDC41+AMpredictNoSnow_NLCDC43+
  AMpredictNoSnow_NLCDC52+ AMpredictNoSnow_NLCDCnon
# 
# ?overlay
# 
# BR1<-brick(AMpredictNoSnow_NLCDCother,AMpredictNoSnow_NLCDC90,AMpredictNoSnow_NLCDC42,AMpredictNoSnow_NLCDC41,AMpredictNoSnow_NLCDC43,
#                                           AMpredictNoSnow_NLCDC52, AMpredictNoSnow_NLCDCnon)
# 
# BR1O<-overlay(BR1,fun=sum)
# plot(BR1O)
# hist(BR1O)


par(mfrow=c(2,1))
hist(BR1O)
hist(AMpredictNoSnow_habC_ALL)
#rna <- overlay(r, otherband, fun=function(x,y){if(is.na(y)) return(NA) else return(x)})


plot(AMpredictNoSnow_habC_ALL)
hist(AMpredictNoSnow_habC_ALL)
summary(AMpredictNoSnow_habC_ALL)

NAcheck<-is.na(AMpredictNoSnow_habC_ALL)
plot(NAcheck)
NAcheck_NLCD<-NLCDC*NAcheck
hist(NAcheck_NLCD)
freq(NAcheck_NLCD)

writeRaster(AMpredictNoSnow_habC_ALL,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/AmandaRSF/AMpredictNoSnow_habC_ALL.tif")






#non-snow - MIGRATORY coeff other than hab categories
canheightCOEF<-0.14
cancoverCOEF<--0.12
underCOEF<-0.11

#calculate values for each habitat category (add in coeff for habitat below), should be a value of 0 if not in a given habitat types
#other
AMpredictNoSnow_NLCDCotherM<-((NLCDCother*.69)+((underZ*underCOEF)*NLCDCother)+((canheightZ*canheightCOEF)*NLCDCother)+((cancover100Z*cancoverCOEF)*NLCDCother))
plot(AMpredictNoSnow_NLCDCotherM)
#woody wet reference (=0)
AMpredictNoSnow_NLCDC90M<-((NLCDC90*0)+((underZ*underCOEF)*NLCDC90)+((canheightZ*canheightCOEF)*NLCDC90)+((cancover100Z*cancoverCOEF)*NLCDC90))
plot(AMpredictNoSnow_NLCDC90M)
#conifer forest
AMpredictNoSnow_NLCDC42M<-((NLCDC42*.7)+((underZ*underCOEF)*NLCDC42)+((canheightZ*canheightCOEF)*NLCDC42)+((cancover100Z*cancoverCOEF)*NLCDC42))
plot(AMpredictNoSnow_NLCDC42M)
#deciduous forest
AMpredictNoSnow_NLCDC41M<-(NLCDC41*.9+((underZ*underCOEF)*NLCDC41)+((canheightZ*canheightCOEF)*NLCDC41)+((cancover100Z*cancoverCOEF)*NLCDC41))
plot(AMpredictNoSnow_NLCDC41M)
#mixed forest
AMpredictNoSnow_NLCDC43M<-((NLCDC43*.97)+((underZ*underCOEF)*NLCDC43)+((canheightZ*canheightCOEF)*NLCDC43)+((cancover100Z*cancoverCOEF)*NLCDC43))
plot(AMpredictNoSnow_NLCDC43M)
#shrubs
AMpredictNoSnow_NLCDC52M<-((NLCDC52*1.35)+((underZ*underCOEF)*NLCDC52)+((canheightZ*canheightCOEF)*NLCDC52)+((cancover100Z*cancoverCOEF)*NLCDC52))
plot(AMpredictNoSnow_NLCDC52M)
#non-habitat
AMpredictNoSnow_NLCDCnonM<-((NLCDCnon*0)+((underZ*underCOEF)*NLCDCnon)+((canheightZ*canheightCOEF)*NLCDCnon)+((cancover100Z*cancoverCOEF)*NLCDCnon))
plot(AMpredictNoSnow_NLCDCnonM)




#add up all of the rasters
AMpredictNoSnow_habC_ALLM<-AMpredictNoSnow_NLCDCotherM+AMpredictNoSnow_NLCDC90M+AMpredictNoSnow_NLCDC42M+AMpredictNoSnow_NLCDC41M+AMpredictNoSnow_NLCDC43M+
  AMpredictNoSnow_NLCDC52M+ AMpredictNoSnow_NLCDCnonM
  
plot(AMpredictNoSnow_habC_ALLM)
hist(AMpredictNoSnow_habC_ALLM)
summary(AMpredictNoSnow_habC_ALLM)

NAcheck<-is.na(AMpredictNoSnow_habC_ALL)
plot(NAcheck)
NAcheck_NLCD<-NLCDC*NAcheck
hist(NAcheck_NLCD)
freq(NAcheck_NLCD)

writeRaster(AMpredictNoSnow_habC_ALLM,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/AmandaRSF/AMpredictNoSnow_habC_ALL_migrators.tif")




setExtent(habC)


#habitat - amanda class - NLCD_RCL_AM
hab<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/NLCD_RCL_AM")
plot(hab)



habC<-crop(hab, under)
habCpts<-rasterToPoints(habC, fun=NULL, spatial=TRUE)
habCptsDF<-as.data.frame(habCpts)
head(habCptsDF)
table(habCptsDF$NLCD_RCL_AM)
habCptsDF2<-habCptsDF[habCptsDF$NLCD_RCL_AM!=4,] 

habCpts <- SpatialPointsDataFrame(habCptsDF2[,c("x", "y")], habCptsDF2)
str(habCpts)

system.time(habCptsDF2$lidar<-extract(lidar, habCpts))
str(habCptsDF2)
summary(habCptsDF2$lidar[,1])


#LID is LiDAR from James, NLCD is actually LIdAR from Knight Group
AllHab<-raster("E:/ZooLaptop_062817/DeerData/for_Zoo/GIS_files/GIS_Ditmer/Deer_NE_LiDAR-20170523T184449Z-001/Deer_NE_LiDAR/MN_LandCover_2014_Arrowhead.img")
AllHabC<-crop(AllHab, under)
plot(AllHabC)
system.time(habCptsDF2$AllHab<-extract(AllHabC, habCpts))
summary(habCptsDF2$AllHab)

head(habCptsDF2)
habCptsDF2C<-na.omit(habCptsDF2, cols="lidar[,1]")

saveRDS(habCptsDF2C,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/LandscapePts_032018.R")
habCptsDF2C<-readRDS("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/LandscapePts_032018.R")

str(habCptsDF2C)

habCptsDF2C$elev_P75_0p15plus_30METERS_UTM15<-habCptsDF2C$lidar[,1]
habCptsDF2C$Canopy_Prop3m_UTM<-habCptsDF2C$lidar[,2]
habCptsDF2C$Prop_1_3m<-habCptsDF2C$lidar[,3]

habCptsDF2C$lidar<-NULL
str(habCptsDF2C)

habCptsDF2C2<-habCptsDF2C[,1:7]

habCptsDF2CDF<-as.data.frame(habCptsDF2C2)
str(habCptsDF2C2)

D1<-as.data.frame(habCptsDF2C2$NLCD_RCL_AM)
habCptsDF2C_AM <- SpatialPointsDataFrame(habCptsDF2C2[,c("x", "y")], data=D1)

D2<-as.data.frame(habCptsDF2C2$AllHab)
habCptsDF2C_AllH <- SpatialPointsDataFrame(habCptsDF2C2[,c("x", "y")], data=D2)

D3<-as.data.frame(habCptsDF2C2$elev_P75_0p15plus_30METERS_UTM15)
habCptsDF2C_CanH <- SpatialPointsDataFrame(habCptsDF2C2[,c("x", "y")], data=D3)

D4<-as.data.frame(habCptsDF2C2$Canopy_Prop3m_UTM)
habCptsDF2C_CanP <- SpatialPointsDataFrame(habCptsDF2C2[,c("x", "y")], data=D4)

D5<-as.data.frame(habCptsDF2C2$Prop_1_3m)
habCptsDF2C_GrCov <- SpatialPointsDataFrame(habCptsDF2C2[,c("x", "y")], data=D5)


str(habCptsDF2C)
#create a raster from the points

Landrast_AM<-rasterFromXYZ(habCptsDF2C_AM)
plot(Landrast_AM)

Landrast_AllH<-rasterFromXYZ(habCptsDF2C_AllH)
plot(Landrast_AllH)

Landrast_CanH<-rasterFromXYZ(habCptsDF2C_CanH)
plot(Landrast_CanH)

Landrast_CanP<-rasterFromXYZ(habCptsDF2C_CanP)
plot(Landrast_CanP)

Landrast_GrCov<-rasterFromXYZ(habCptsDF2C_GrCov)
plot(Landrast_GrCov)

Landrast<-stack(Landrast_AM,Landrast_AllH,Landrast_CanH,Landrast_CanP,Landrast_GrCov)
plot(Landrast)

str(Landrast)
plot(Landrast,4)
str(Landrast)

Landrast_sp<-as(Landrast,"SpatialPixelsDataFrame")
class(Landrast_sp)

Landrast_ras<-brick(Landrast_sp)


#write that raster! (two types here)
writeRaster(Landrast_ras,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/Landrast.tif")

AllR<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/Landrast.tif")
plot(AllR,5)


#writeRaster(Landrast,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/Landrast.asc",format="raster")
str(Landrast_ras)


####Deer Use V.1 - % on Amanda Groups
#instead of making new rasters for each could just extract from moose HR and multiply by the values....

####Summer moose HR
#Read in all shapefiles individually
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_summer_poly/")
shps <- dir(getwd(), "*.shp")
shps <- sub('\\.shp$',"", shps)
shps
for (shp in shps) assign(shp, readOGR('.',layer=shp))

plot(X178_2015_Summer.X178_BB90_poly,add=TRUE)
plot(X192_2015_Summer.X192_BB90_poly,add=TRUE)

summerMooseBB90HR<-rbind(X11_2013_Summer.X11_BB90_poly,X13_2013_Summer.X13_BB90_poly,X15_2013_Summer.X15_BB90_poly,  
                         X151_2014_Summer.X151_BB90_poly,X156_2014_Summer.X156_BB90_poly,X157_2016_Summer.X157_BB90_poly,
                         X158_2016_Summer.X158_BB90_poly,X161_2015_Summer.X161_BB90_poly,X178_2014_Summer.X178_BB90_poly,
                         X178_2015_Summer.X178_BB90_poly,X181_2015_Summer.X181_BB90_poly,X181_2016_Summer.X181_BB90_poly,
                         X19_2013_Summer.X19_BB90_poly,X192_2015_Summer.X192_BB90_poly,X200_2015_Summer.X200_BB90_poly,
                         X202_2015_Summer.X202_BB90_poly,X205_2015_Summer.X205_BB90_poly,X31_2013_Summer.X31_BB90_poly,  
                         X58_2013_Summer.X58_BB90_poly)
plot(summerMooseBB90HR,col="red")
head(summerMooseBB90HR)
str(summerMooseBB90HR)

####Spring moose HR
#Read in all shapefiles individually
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_spring_poly/")
shps <- dir(getwd(), "*.shp")
shps <- sub('\\.shp$',"", shps)
shps
for (shp in shps) assign(shp, readOGR('.',layer=shp))

plot(X178_2015_Spring.X178_BB90_poly,add=TRUE)
plot(X192_2015_Spring.X192_BB90_poly,add=TRUE)

springMooseBB90HR<-rbind(X11_2013_Spring.X11_BB90_poly,X13_2013_Spring.X13_BB90_poly,X13_2014_Spring.X13_BB90_poly,X15_2013_Spring.X15_BB90_poly,  
                         X15_2014_Spring.X15_BB90_poly,X151_2014_Spring.X151_BB90_poly,X156_2014_Spring.X156_BB90_poly,X156_2015_Spring.X156_BB90_poly,X157_2016_Spring.X157_BB90_poly,
                         X157_2017_Spring.X157_BB90_poly,X158_2016_Spring.X158_BB90_poly,X161_2015_Spring.X161_BB90_poly,
                         X178_2015_Spring.X178_BB90_poly,X181_2016_Spring.X181_BB90_poly,
                         X19_2013_Spring.X19_BB90_poly,X192_2015_Spring.X192_BB90_poly,X200_2015_Spring.X200_BB90_poly,X200_2016_Spring.X200_BB90_poly,
                         X202_2016_Spring.X202_BB90_poly,X205_2015_Spring.X205_BB90_poly,X31_2013_Spring.X31_BB90_poly,  
                         X58_2013_Spring.X58_BB90_poly,X90_2013_Spring.X90_BB90_poly)
unique(springMooseBB90HR$ID)
plot(springMooseBB90HR,col="red")
head(springMooseBB90HR)
str(springMooseBB90HR)

####Fall moose HR
#Read in all shapefiles individually
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_fall_poly/")
shps <- dir(getwd(), "*.shp")
shps <- sub('\\.shp$',"", shps)
shps
for (shp in shps) assign(shp, readOGR('.',layer=shp))

plot(X200_2015_Fall.X200_BB90_poly)
plot(X192_2015_Fall.X192_BB90_poly,add=TRUE)

fallMooseBB90HR<-rbind(X13_2013_Fall.X13_BB90_poly,X15_2013_Fall.X15_BB90_poly,  
                         X156_2014_Fall.X156_BB90_poly,X157_2016_Fall.X157_BB90_poly,
                         X158_2016_Fall.X158_BB90_poly,
                         X178_2014_Fall.X178_BB90_poly,X181_2015_Fall.X181_BB90_poly,
                         X19_2013_Fall.X19_BB90_poly,X192_2015_Fall.X192_BB90_poly,X200_2015_Fall.X200_BB90_poly,
                         X202_2015_Fall.X202_BB90_poly,X205_2015_Fall.X205_BB90_poly,X31_2013_Fall.X31_BB90_poly,  
                         X58_2013_Fall.X58_BB90_poly)
unique(fallMooseBB90HR$ID)
plot(fallMooseBB90HR,col="red")
head(fallMooseBB90HR)
str(fallMooseBB90HR)



AMpredictNoSnow_habC_ALL
newproj<-projection(summerMooseBB90HR)
newproj2<-projection(RecentCutF)

#Transformations
plot(habC)
#habC<- projectRaster(habC, crs=newproj)
fallMooseBB90HR2 <- spTransform(fallMooseBB90HR, crs(newproj2))
springMooseBB90HR2 <- spTransform(springMooseBB90HR, crs(newproj2))
summerMooseBB90HR2 <- spTransform(summerMooseBB90HR, crs(newproj2))
plot(summerMooseBB90HR2,col="red")

summary(AMpredictNoSnow_habC_ALL)
plot(AMpredictNoSnow_habC_ALL)
#Amanda RSF 4/1/2018 - UPDATED VERSION
# Extract raster values to polygons                             
( v <- extract(AMpredictNoSnow_habC_ALLM, summerMooseBB90HR) )
# Get class counts for each polygon
v.counts <- lapply(v,table)
# Calculate class percentages for each polygon
( v.mean <- lapply(v, FUN=mean,na.rm = TRUE) )
( v.median <- lapply(v, FUN=median,na.rm = TRUE) )
( v.max <- lapply(v, FUN=max,na.rm = TRUE) )
( v.sd <- lapply(v, FUN=sd,na.rm = TRUE) )
( v.n <- lapply(v, FUN=function(x){ length(x) } ) )

class.df.mean <- as.data.frame(t(sapply(v.mean,'[',1:length(unique(summerMooseBB90HR@data$ID))))) 
class.df.median <- as.data.frame(t(sapply(v.median,'[',1:length(unique(summerMooseBB90HR@data$ID))))) 
class.df.max <- as.data.frame(t(sapply(v.max,'[',1:length(unique(summerMooseBB90HR@data$ID))))) 
class.df.sd <- as.data.frame(t(sapply(v.sd,'[',1:length(unique(summerMooseBB90HR@data$ID))))) 
class.df.n <- as.data.frame(t(sapply(v.n,'[',1:length(unique(summerMooseBB90HR@data$ID))))) 
#class.dfraw <- as.data.frame(t(sapply(v.raw,'[',1:length(unique(CTIforSTAN60)))))  
# Replace NA's with 0 and add names
#class.df[is.na(class.df)] <- 0  
#class.dfraw[is.na(class.dfraw)] <- 0   

class.df<-cbind(class.df.mean[,1],class.df.median[,1],class.df.max[,1],class.df.sd[,1],class.df.n[,1])
colnames(class.df) <- c("mean.mig","median.mig","max.mig","sd.nomig","n.mig")
#names(class.dfraw) <- paste(c("CTIforSTAN50_dryraw","CTIforSTAN50_wetraw"))
AM_RSF_Mig<-class.df
AM_RSF_Mig
#CTIforSTAN50r<-class.dfraw
# Add back to polygon data
summerMooseBB90HR@data <- data.frame(summerMooseBB90HR@data, AM_RSF_Mig)
head(summerMooseBB90HR@data)

summerMooseBB90HR$season<-"summer"

write.csv(summerMooseBB90HR,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/summer_AM_RSFupdt_Migrators_040218.csv")



#Amanda deer habitat layer  ---OLD VERSION
# Extract raster values to polygons                             
( v <- extract(habC, springMooseBB90HR2) )
# Get class counts for each polygon
v.counts <- lapply(v,table)
# Calculate class percentages for each polygon
( v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )
( v.raw <- lapply(v.counts, FUN=function(x){ x } ) )
# Create a data.frame where missing classes are NA
class.df <- as.data.frame(t(sapply(v.pct,'[',1:length(unique(habC)))))  
class.dfraw <- as.data.frame(t(sapply(v.raw,'[',1:length(unique(habC)))))  
# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0  
class.dfraw[is.na(class.dfraw)] <- 0   
head(class.df)
head(class.dfraw)
names(class.df) <- paste(c("FrgH","PrH","WtH","aHab"))
names(class.dfraw) <- paste(c("FrgHC","PrHC","WtHC","aHabC"))
AMclassDeer<-class.df
AMclassDeer2<-class.dfraw
# Add back to polygon data
springMooseBB90HR2@data <- data.frame(springMooseBB90HR2@data, AMclassDeer,AMclassDeer2)
head(springMooseBB90HR2@data)



#LIDAR NLCD deer habitat layer
# Extract raster values to polygons                             
( v <- extract(AllHabC, springMooseBB90HR2) )
# Get class counts for each polygon
v.counts <- lapply(v,table)
# Calculate class percentages for each polygon
( v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )
( v.raw <- lapply(v.counts, FUN=function(x){ x } ) )
# Create a data.frame where missing classes are NA
class.df <- as.data.frame(t(sapply(v.pct,'[',1:length(unique(AllHabC)))))
class.dfraw <- as.data.frame(t(sapply(v.raw,'[',1:length(unique(AllHabC)))))  
# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0   
class.dfraw[is.na(class.dfraw)] <- 0   
head(class.dfraw)

# Developed	100
# Emergent Wetlands 	101
# Forested and Shrub Wetlands 	102
# Open Water	103
# Extraction	104
# Coniferous Forest	105
# Deciduous Forest	106
# Mixed Forest	107
# Regenerated Forest	108
# Grassland 	109
# Hay and Pasture	110


names(class.df) <- paste(c("Devel","EmWet","WoodyWet","OpenWater","ConFrst","DecidFrst","MixFrst","RegenFrst","Grass"))
names(class.dfraw) <- paste(c("DevelC","EmWetC","WoodyWetC","OpenWaterC","ConFrstC","DecidFrstC","MixFrstC","RegenFrstC","GrassC"))
AllHabclassDeer<-class.df
AllHabclassDeer2<-class.dfraw
# Add back to polygon data
springMooseBB90HR2@data <- data.frame(springMooseBB90HR2@data, AllHabclassDeer,AllHabclassDeer2)
head(springMooseBB90HR2@data)

springMooseBB90HR2@data$NA..3<-NULL

write.csv(springMooseBB90HR2,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/Spring_MooseHR_HabPer_032218.csv")

#s <- calc(r, fun=function(x){ x[x < 4] <- NA; return(x)} )


aveAMhabuse_season4<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/DeerUse_PercentMeth/HabUse_AMcat_season4.csv")
plot(Landrast_ras[[1]])
str(Landrast_ras[[1]])
table(Landrast_ras[[1]]@data@values)
SpringDeerAMPer<-Landrast_ras[[1]]

SpringDeerAMPer<-with(Landrast_ras[[1]], ifelse(Landrast_ras[[1]] < 1, Spr$percent, ifelse(x > 0.15 & dif < 0, 2, 1)))


#Disturbance hypothesis

#extract for distruabnce time (CSV contains table for value matchup)
disturb<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/disturb09clsUTM.tif")
dist_tab<-read.csv("E:/ZooLaptop_062817/NE/Wolf/OutsideSpatial/RDS-2016-0001/Data/DISTURB_table.csv")

plot(disturb)
disturb2000<-disturb>5
plot(disturb2000)

newproj<-projection(fallMooseBB90HR)
disturb2000p<-projectRaster(disturb2000,crs=newproj)


#disturbed layer
# Extract raster values to polygons                             
( v <- extract(disturb2000, fallMooseBB90HR) )
# Get class counts for each polygon
v.counts <- lapply(v,table)
# Calculate class percentages for each polygon
( v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )
( v.raw <- lapply(v.counts, FUN=function(x){ x } ) )
# Create a data.frame where missing classes are NA
class.df <- as.data.frame(t(sapply(v.pct,'[',1:length(unique(disturb2000)))))  
class.dfraw <- as.data.frame(t(sapply(v.raw,'[',1:length(unique(disturb2000)))))  
# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0  
class.dfraw[is.na(class.dfraw)] <- 0   
head(class.df)
head(class.dfraw)
names(class.df) <- paste(c("undisturbed2000","disturbed2000"))
names(class.dfraw) <- paste(c("undisturbed2000","disturbed2000"))
disturbed0009<-class.df
disturbed0009r<-class.dfraw
# Add back to polygon data
fallMooseBB90HR@data <- data.frame(fallMooseBB90HR@data, disturbed0009,disturbed0009r)
head(fallMooseBB90HR@data)

fallMooseBB90HR$season<-"fall"

write.csv(fallMooseBB90HR,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/fall_Disturb2000_2009_032618.csv")


###deer permit areas
#use a centroud points to extract

str(fallMooseBB90HR)
fallcent <- getSpPPolygonsLabptSlots(fallMooseBB90HR)
springcent <- getSpPPolygonsLabptSlots(springMooseBB90HR)
summercent <- getSpPPolygonsLabptSlots(summerMooseBB90HR)

plot(fallMooseBB90HR)
fallcentSP<-SpatialPointsDataFrame(coords=fallcent[,1:2],data=as.data.frame(fallcent),proj4string=CRS("+proj=utm +zone=15 ellps=WGS84"))

plot(fallcentSP,add=TRUE,col="red")

fallcent<-cbind(as.data.frame(fallMooseBB90HR@data$ID),fallcent)
springcent<-cbind(as.data.frame(springMooseBB90HR@data$ID),springcent)
summercent<-cbind(as.data.frame(summerMooseBB90HR@data$ID),summercent)

colnames(fallcent)<-c("ID","X","Y")
colnames(springcent)<-c("ID","X","Y")
colnames(summercent)<-c("ID","X","Y")


allcent<-rbind(fallcent,springcent,summercent)

write.csv(fallcent,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/fall_BBridge_ventroids_032618.csv")
write.csv(springcent,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/spring_BBridge_ventroids_032618.csv")
write.csv(summercent,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/summer_BBridge_ventroids_032618.csv")
write.csv(allcent,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/allcentroids_BBridge_ventroids_032618.csv")

allcentSP<-SpatialPointsDataFrame(coords=allcent[,2:3],data=as.data.frame(allcent),proj4string=CRS("+proj=utm +zone=15 ellps=WGS84"))


deerharv14<-readOGR(dsn="C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_env_mn_deer_harvest",layer="deer_harvest_2014")
plot(deerharv14)
plot(allcentSP,add=TRUE)

deerproj<-projection(deerharv14)
allcentSP2<-spTransform(allcentSP, crs(deerproj))

allcentSP_deer14<-extract(deerharv14,allcentSP2)
names(allcentSP_deer14)
allcentSP_deer14<-allcentSP_deer14[,14]
allcentSP_deer14<-as.data.frame(allcentSP_deer14)
colnames(allcentSP_deer14)<-c("deerharvSQMI2014")


deerharv13<-readOGR(dsn="C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_env_mn_deer_harvest",layer="deer_harvest_2013")
allcentSP_deer13<-extract(deerharv13,allcentSP2)
names(allcentSP_deer13)
allcentSP_deer13<-allcentSP_deer13[,14]
allcentSP_deer13<-as.data.frame(allcentSP_deer13)
colnames(allcentSP_deer13)<-c("deerharvSQMI2013")


deerharv12<-readOGR(dsn="C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_env_mn_deer_harvest",layer="deer_harvest_2012")
allcentSP_deer12<-extract(deerharv12,allcentSP2)
names(allcentSP_deer12)
allcentSP_deer12<-allcentSP_deer12[,14]
allcentSP_deer12<-as.data.frame(allcentSP_deer12)
colnames(allcentSP_deer12)<-c("deerharvSQMI2012")

deerharv<-cbind(as.data.frame(allcentSP),allcentSP_deer12,allcentSP_deer13,allcentSP_deer14)
deerharv$harvestAVE<-(deerharv$deerharvSQMI2012+deerharv$deerharvSQMI2013+deerharv$deerharvSQMI2014)/3
hist(deerharv$harvestAVE)

write.csv(deerharv,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/deerharvest_centroids_032618.csv")


#Recent cuts/disturbance of state and national lands
RecentCut<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_biota_dnr_forest_stand_inventory/FedNStateCut0918.tif")
RecentCut
plot(RecentCut)
plot(fallMooseBB90HR,add=TRUE)

#disturbed layer
# Extract raster values to polygons                             
( v <- extract(RecentCut, summerMooseBB90HR) )
# Get class counts for each polygon
v.counts <- lapply(v,table)
# Calculate class percentages for each polygon
( v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )
( v.raw <- lapply(v.counts, FUN=function(x){ x } ) )
# Create a data.frame where missing classes are NA
#removed the transpose ("t()")
class.df <- as.data.frame(sapply(v.pct,'[',1:length(unique(RecentCut)))) 
class.dfraw <- as.data.frame(sapply(v.raw,'[',1:length(unique(RecentCut))))  
# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0  
class.dfraw[is.na(class.dfraw)] <- 0   
head(class.df)
head(class.dfraw)
names(class.df) <- paste(c("RecentCut2009_2018"))
names(class.dfraw) <- paste(c("RecentCut2009_2018_raw"))
RecentCut2009_2018<-class.df
RecentCut2009_2018r<-class.dfraw
# Add back to polygon data
summerMooseBB90HR@data <- data.frame(summerMooseBB90HR@data, RecentCut2009_2018,RecentCut2009_2018r)
head(summerMooseBB90HR@data)

#get cut area by multiplying count of raw cells by cell size
summerMooseBB90HR$CutArea<-summerMooseBB90HR$RecentCut2009_2018_raw*(30*30)

summerMooseBB90HR$season<-"summer"

write.csv(summerMooseBB90HR,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/summer_Disturb2009_2018_032818.csv")

#Recent cuts/disturbance of state and national lands - Updated BY YEAR
RecentCutF<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_biota_dnr_forest_stand_inventory/fedcut0918_v2")
RecentCutS<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_biota_dnr_forest_stand_inventory/stcut0918_v2")
names(MB4.dat.move)
head(MB4.dat.move)

MB4.dat.moveDF<-as.data.frame(MB4.dat.move)
MooseL<-SpatialPointsDataFrame(coords=MB4.dat.moveDF[c("utm.easting","utm.northing")],data=MB4.dat.moveDF,proj4string=CRS("+proj=utm +zone=15 ellps=WGS84"))
MooseL$FedCutValue<-extract(RecentCutF,MooseL)
hist(MooseL$FedCutValue)
table(test)

#2012
FCut2012<-FCut2012==1
SCut2012<-RecentCutS==2012
SCut2012 <- projectRaster(SCut2012,FCut2012,method = 'ngb')
BCut2012<-merge(FCut2012,SCut2012)
BCut2012 <- reclassify(BCut2012, cbind(0,NA)) 
plot(BCut2012,col="black")

#2016
FCut2016<-RecentCutF==3
SCut2016<-RecentCutS==2016
SCut2016 <- projectRaster(SCut2016,FCut2016,method = 'ngb')
BCut2016<-merge(FCut2016,SCut2016)
BCut2016 <- reclassify(BCut2016, cbind(0,NA)) 
plot(BCut2016,col="black")
#2014
FCut2014<-RecentCutF==4
SCut2014<-RecentCutS==2014
SCut2014 <- projectRaster(SCut2014,FCut2014,method = 'ngb')
BCut2014<-merge(FCut2014,SCut2014)
BCut2014 <- reclassify(BCut2014, cbind(0,NA)) 
plot(BCut2014,col="black")
#2015
FCut2015<-RecentCutF==5
SCut2015<-RecentCutS==2015
SCut2015 <- projectRaster(SCut2015,FCut2015,method = 'ngb')
BCut2015<-merge(FCut2015,SCut2015)
BCut2015 <- reclassify(BCut2015, cbind(0,NA)) 
plot(BCut2015,col="black")
#2011
FCut2011<-RecentCutF==6
SCut2011<-RecentCutS==2011
SCut2011 <- projectRaster(SCut2011,FCut2011,method = 'ngb')
BCut2011<-merge(FCut2011,SCut2011)
BCut2011 <- reclassify(BCut2011, cbind(0,NA)) 
plot(BCut2011,col="black")

#2010
FCut2010<-RecentCutF==7
SCut2010<-RecentCutS==2010
SCut2010 <- projectRaster(SCut2010,FCut2010,method = 'ngb')
BCut2010<-merge(FCut2010,SCut2010)
BCut2010 <- reclassify(BCut2010, cbind(0,NA)) 
plot(BCut2010,col="black")

#2013
FCut2013<-RecentCutF==8
SCut2013<-RecentCutS==2013
SCut2013 <- projectRaster(SCut2013,FCut2013,method = 'ngb')
BCut2013<-merge(FCut2013,SCut2013)
BCut2013 <- reclassify(BCut2013, cbind(0,NA)) 
plot(BCut2013,col="black")

#disturbed layer
# Extract raster values to polygons                             
( v2010 <- extract(BCut2010, springMooseBB90HR) )
# Get class counts for each polygon
v2010.counts <- lapply(v2010,table)
# Calculate class percentages for each polygon
#( v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )
( v2010.raw <- lapply(v2010.counts, FUN=function(x){ x } ) )
# Create a data.frame where missing classes are NA
class.dfraw.2010 <- as.data.frame(t(sapply(v2010.raw,'[',1:length(unique(FCut2010)))))
# Replace NA's with 0 and add names
class.dfraw.2010[is.na(class.dfraw.2010)] <- 0  
class.dfraw.2010$V2<-NULL
names(class.dfraw.2010) <- paste(c("RecentCut2010"))
# Add back to polygon data
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, class.dfraw.2010)
head(springMooseBB90HR@data)
#get cut area by multiplying count of raw cells by cell size
springMooseBB90HR$CutArea2010<-springMooseBB90HR$RecentCut2010*(30*30)

#disturbed layer 2011
( v2011 <- extract(BCut2011, springMooseBB90HR) )
v2011.counts <- lapply(v2011,table)
( v2011.raw <- lapply(v2011.counts, FUN=function(x){ x } ) )
class.dfraw.2011 <- as.data.frame(t(sapply(v2011.raw,'[',1:length(unique(FCut2011)))))
class.dfraw.2011[is.na(class.dfraw.2011)] <- 0  
class.dfraw.2011$V2<-NULL
names(class.dfraw.2011) <- paste(c("RecentCut2011"))
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, class.dfraw.2011)
head(springMooseBB90HR@data)
springMooseBB90HR$CutArea2011<-springMooseBB90HR$RecentCut2011*(30*30)


#disturbed layer 2012
( v2012 <- extract(BCut2012, springMooseBB90HR) )
v2012.counts <- lapply(v2012,table)
( v2012.raw <- lapply(v2012.counts, FUN=function(x){ x } ) )
class.dfraw.2012 <- as.data.frame(t(sapply(v2012.raw,'[',1:length(unique(FCut2012)))))
class.dfraw.2012[is.na(class.dfraw.2012)] <- 0  
class.dfraw.2012$V2<-NULL
names(class.dfraw.2012) <- paste(c("RecentCut2012"))
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, class.dfraw.2012)
head(springMooseBB90HR@data)
springMooseBB90HR$CutArea2012<-springMooseBB90HR$RecentCut2012*(30*30)


#disturbed layer 2013
( v2013 <- extract(BCut2013, springMooseBB90HR) )
v2013.counts <- lapply(v2013,table)
( v2013.raw <- lapply(v2013.counts, FUN=function(x){ x } ) )
class.dfraw.2013 <- as.data.frame(t(sapply(v2013.raw,'[',1:length(unique(FCut2013)))))
class.dfraw.2013[is.na(class.dfraw.2013)] <- 0  
class.dfraw.2013$V2<-NULL
names(class.dfraw.2013) <- paste(c("RecentCut2013"))
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, class.dfraw.2013)
springMooseBB90HR$CutArea2013<-springMooseBB90HR$RecentCut2013*(30*30)
head(springMooseBB90HR@data)
#disturbed layer 2014
( v2014 <- extract(BCut2014, springMooseBB90HR) )
v2014.counts <- lapply(v2014,table)
( v2014.raw <- lapply(v2014.counts, FUN=function(x){ x } ) )
class.dfraw.2014 <- as.data.frame(t(sapply(v2014.raw,'[',1:length(unique(FCut2014)))))
class.dfraw.2014[is.na(class.dfraw.2014)] <- 0  
class.dfraw.2014$V2<-NULL
names(class.dfraw.2014) <- paste(c("RecentCut2014"))
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, class.dfraw.2014)
springMooseBB90HR$CutArea2014<-springMooseBB90HR$RecentCut2014*(30*30)
head(springMooseBB90HR@data)

#disturbed layer 2015
( v2015 <- extract(BCut2015, springMooseBB90HR) )
v2015.counts <- lapply(v2015,table)
( v2015.raw <- lapply(v2015.counts, FUN=function(x){ x } ) )
class.dfraw.2015 <- as.data.frame(t(sapply(v2015.raw,'[',1:length(unique(FCut2015)))))
class.dfraw.2015[is.na(class.dfraw.2015)] <- 0  
class.dfraw.2015$V2<-NULL
names(class.dfraw.2015) <- paste(c("RecentCut2015"))
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, class.dfraw.2015)
springMooseBB90HR$CutArea2015<-springMooseBB90HR$RecentCut2015*(30*30)
head(springMooseBB90HR@data)

#disturbed layer 2016
( v2016 <- extract(BCut2016, springMooseBB90HR) )
v2016.counts <- lapply(v2016,table)
( v2016.raw <- lapply(v2016.counts, FUN=function(x){ x } ) )
class.dfraw.2016 <- as.data.frame(t(sapply(v2016.raw,'[',1:length(unique(FCut2016)))))
class.dfraw.2016[is.na(class.dfraw.2016)] <- 0  
class.dfraw.2016$V2<-NULL
names(class.dfraw.2016) <- paste(c("RecentCut2016"))
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, class.dfraw.2016)
springMooseBB90HR$CutArea2016<-springMooseBB90HR$RecentCut2016*(30*30)
head(springMooseBB90HR@data)

springMooseBB90HR@data$season<-"spring"
write.csv(springMooseBB90HR,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/spring_Disturb2010_2018_040418.csv")



########wet forests
FrstHab<-AllHabC>=105&AllHabC<=108
plot(FrstHab)

RegenHab<-AllHabC==108
plot(RegenHab)

CTI<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/Soil/CTI_v1")

ext<-extent(RegenHab)
CTI2<-setExtent(CTI, ext)
CTI2 <- resample(CTI2, RegenHab)
CTIreg<-CTI2*RegenHab
plot(CTIreg)
CTIregSTAN<-CTIfor/20.40229
CTIregSTAN.1<-CTIregSTAN
CTIregSTAN.1[CTIregSTAN.1 < 0.000001] <- NA

plot(CTIregSTAN.1)
plot(summerMooseBB90HR,add=TRUE,col="red")

#wet regen forest

# Extract raster values to polygons                             
( v <- extract(CTIregSTAN.1, springMooseBB90HR) )
# Get class counts for each polygon
v.counts <- lapply(v,table)
# Calculate class percentages for each polygon
( v.sum <- lapply(v, FUN=function(x){ mean(x,na.rm=TRUE) }) )
( v.raw <- lapply(v.counts, FUN=function(x){ sum(x) } ) )
# Create a data.frame where missing classes are NA
#removed the transpose ("t()")
class.df <- as.data.frame(t(sapply(v.sum,'[',1:length(unique(springMooseBB90HR@polygons))))) 
class.dfraw <- as.data.frame(t(sapply(v.raw,'[',1:length(unique(springMooseBB90HR@polygons)))))  
# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0  
class.dfraw[is.na(class.dfraw)] <- 0   
head(class.df)
head(class.dfraw)
class.df<-class.df[,1]
class.dfraw<-class.dfraw[,1]
class.df<-as.data.frame(class.df)
class.dfraw<-as.data.frame(class.dfraw)
names(class.df) <- paste(c("RegenWetCTIMean"))
names(class.dfraw) <- paste(c("RegenWetCTITotalCount"))
CTIfregSTAN50<-class.df
CTIfregSTAN50r<-class.dfraw
CTIfregSTAN50r$RegenWetCTITotalArea<- CTIfregSTAN50r$RegenWetCTITotalCount *(15*15)
# Add back to polygon data
springMooseBB90HR@data <- data.frame(springMooseBB90HR@data, CTIfregSTAN50,CTIfregSTAN50r)
head(springMooseBB90HR@data)

springMooseBB90HR$season<-"spring"

write.csv(springMooseBB90HR,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/spring_wetREGENforest50_040618.csv")



#wet forest

# Extract raster values to polygons                             
( v <- extract(CTIforSTAN60, fallMooseBB90HR) )
# Get class counts for each polygon
v.counts <- lapply(v,table)
# Calculate class percentages for each polygon
( v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )
( v.raw <- lapply(v.counts, FUN=function(x){ x } ) )
# Create a data.frame where missing classes are NA
#removed the transpose ("t()")
class.df <- as.data.frame(t(sapply(v.pct,'[',1:length(unique(CTIforSTAN60))))) 
class.dfraw <- as.data.frame(t(sapply(v.raw,'[',1:length(unique(CTIforSTAN60)))))  
# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0  
class.dfraw[is.na(class.dfraw)] <- 0   
head(class.df)
head(class.dfraw)
names(class.df) <- paste(c("CTIforSTAN50_drypercent","CTIforSTAN50_wetpercent"))
names(class.dfraw) <- paste(c("CTIforSTAN50_dryraw","CTIforSTAN50_wetraw"))
CTIforSTAN50<-class.df
CTIforSTAN50r<-class.dfraw
# Add back to polygon data
fallMooseBB90HR@data <- data.frame(fallMooseBB90HR@data, CTIforSTAN50,CTIforSTAN50r)
head(fallMooseBB90HR@data)

fallMooseBB90HR$season<-"fall"

write.csv(fallMooseBB90HR,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/fall_wetforest50_032818.csv")

########wet areas in general in home range....


CTI<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/Soil/CTI_v1")

#standardize it
CTISTAN<-(CTI-2.058521)/(23.69904-2.058522)


# Extract raster values to polygons                             
( v <- extract(CTISTAN, summerMooseBB90HR) )
# Calculate class percentages for each polygon
( v.pct <- lapply(v, FUN=mean ) )

# Create a data.frame where missing classes are NA
#removed the transpose ("t()")
class.df <- as.data.frame(t(sapply(v.pct,'[',1:length(unique(summerMooseBB90HR@data$ID))))) 
#class.dfraw <- as.data.frame(t(sapply(v.raw,'[',1:length(unique(CTIforSTAN60)))))  
# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0  
#class.dfraw[is.na(class.dfraw)] <- 0   
head(class.df)
#head(class.dfraw)
class.df<-class.df[,1]
names(class.df) <- paste(c("CTISTANWetmeant"))
#names(class.dfraw) <- paste(c("CTIforSTAN50_dryraw","CTIforSTAN50_wetraw"))
CTISTANWetmeant<-class.df
#CTIforSTAN50r<-class.dfraw
# Add back to polygon data
summerMooseBB90HR@data <- data.frame(summerMooseBB90HR@data, CTISTANWetmeant)
head(summerMooseBB90HR@data)

summerMooseBB90HR$season<-"summer"

write.csv(summerMooseBB90HR,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/summer_wetforest50_allwetmean_033018.csv")




#calculate BB seasonal HR sizes

fallarea<-as.data.frame(sapply(slot(fallMooseBB90HR, "polygons"), slot, "area"))
colnames(fallarea)<-"area_m2"
fallarea$area_km2<-fallarea$area_m2/100000
fallarea<-cbind(fallMooseBB90HR@data$ID,fallarea)
colnames(fallarea)[1]<-"ID"

springarea<-as.data.frame(sapply(slot(springMooseBB90HR, "polygons"), slot, "area"))
colnames(springarea)<-"area_m2"
springarea$area_km2<-springarea$area_m2/100000
springarea<-cbind(springMooseBB90HR@data$ID,springarea)
colnames(springarea)[1]<-"ID"

summerarea<-as.data.frame(sapply(slot(summerMooseBB90HR, "polygons"), slot, "area"))
colnames(summerarea)<-"area_m2"
summerarea$area_km2<-summerarea$area_m2/100000
summerarea<-cbind(summerMooseBB90HR@data$ID,summerarea)
colnames(summerarea)[1]<-"ID"

allarea<-rbind(summerarea,fallarea,springarea)

write.csv(allarea,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/allarea_032818.csv")


#pull in outside moose data especially date of death
MooseInfo<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/MooseInfo.csv")

###issue of snow out date
snow<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/TS2-7819730596981797434/TS2-7819730596981797434.csv")
head(snow)
str(snow)
snow$timestampL<-strptime(snow$timestamp, "%Y-%m-%d %H:%M:%S",tz="America/Chicago")
snow$animalID<-snow$individual.local.identifier

summary(snow$MODIS.Snow.Terra.Snow.500m.Daily.NDSI.Snow.Cover)
summary(snow$MODIS.Snow.Terra.Snow.500m.8d.Maximum.Snow.Extent)
summary(snow$MODIS.Snow.Terra.Snow.500m.8d.Snow.Cover)

table(snow$animalID,snow$MODIS.Snow.Terra.Snow.500m.8d.Snow.Cover)

p <- ggplot(snow, aes(x=timestampL,y=MODIS.Snow.Terra.Snow.500m.8d.Snow.Cover,group=as.factor(animalID),colour=as.factor(animalID))) + geom_line(size=2)
p

MB4.dat<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Moose_Movebank_Alter_110817.csv")
head(MB4.dat)
str(MB4.dat)
unique(MB4.dat$deathtime)

MB4.dat$timestampL<-strptime(MB4.dat$timestamp, "%Y-%m-%d %H:%M:%S",tz="America/Chicago")
MB4.dat$animalID<-MB4.dat$individual.local.identifier

head(MB4.dat)
MB4.dat$deathtime<-paste(MB4.dat$Date.of.death,MB4.dat$time.of.death,sep=" ")
MB4.dat$deathtime<-strptime(MB4.dat$deathtime, "%m/%d/%Y %H:%M:%S",tz="America/Chicago")
MB4.dat$year<-as.numeric(format(MB4.dat$timestampL,"%Y"))
MB4.dat$julian<-as.numeric(format(MB4.dat$timestampL,"%j"))
MB4.dat$DEATHyear<-as.numeric(format(MB4.dat$deathtime,"%Y"))
head(MB4.dat)
MB4.dat$IDyear<-paste(MB4.dat$animalID,MB4.dat$year,sep="_")

head(snow)
str(snow)
snow$year<-as.numeric(format(snow$timestampL,"%Y"))
snow$julian<-as.numeric(format(snow$timestampL,"%j"))
snow$month<-as.numeric(format(snow$timestampL,"%m"))
snow$month<-as.numeric(snow$month)
snow$timestampL<-as.POSIXct(snow$timestampL)

#take death time minus 2 years
#unique year in each
#calculate first 0 recording (min 0 recording by Julian), and last 0 recording (maybe cut data from jan to July and get max julian)

samplehold<-NULL

for (i in 1:length(unique(MB4.dat$animalID))){
  
  subdat<-subset(MB4.dat,MB4.dat$animalID==unique(MB4.dat$animalID)[i]) 
  endGPS<-unique(subdat$DEATHyear)
  startGPS<-unique(subdat$DEATHyear)-1
  #timediff<-as.numeric(max(subdat$timestampL)-unique(subdat$deathtime))
  subSNOW<-snow[snow$animalID==unique(subdat$animalID)&snow$year<=endGPS&snow$year>=startGPS,]
  unique(subSNOW$year)
  p <- ggplot(subSNOW, aes(x=timestampL,y=MODIS.Snow.Terra.Snow.500m.8d.Snow.Cover)) + geom_line(size=2)+ggtitle(paste(unique(subSNOW$animalID)))
  print(p)
  subSNOWZ<-subSNOW[subSNOW$MODIS.Snow.Terra.Snow.500m.8d.Snow.Cover==0,]
  head(subSNOWZ)

  FirstNoSnow<-subSNOWZ %>%
    group_by(year) %>%
    summarize(min.snow.date = min(timestampL, na.rm = TRUE))
  
  FirstNoSnow<- as.data.frame(FirstNoSnow)
  FirstNoSnow$ID<-NA
  FirstNoSnow$ID<-unique(subSNOWZ$animalID)
  
  lastSNOWZ<-subSNOW[subSNOW$month<7&subSNOW$MODIS.Snow.Terra.Snow.500m.8d.Snow.Cover>0,]
  head(lastSNOWZ)
  
  LastSnow<-lastSNOWZ %>%
    group_by(year) %>%
    summarize(max.snow.date = max(timestampL, na.rm = TRUE))
  LastSnow<- as.data.frame(LastSnow)
  
  snow.dates<-cbind(FirstNoSnow,LastSnow)
  
  samplehold<-rbind(samplehold,snow.dates)
  print(i)
}

snowtimes<-samplehold

#an issue is if the moose died during winter - see moose #19 in 2014 as example
#pull in outside moose data especially date of death
MooseInfo<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/MooseInfo.csv")
head(MooseInfo)
MooseInfo$deathtime<-paste(MooseInfo$Date.of.death,MooseInfo$time.of.death,sep=" ")
MooseInfo$deathtime<-strptime(MooseInfo$deathtime, "%m/%d/%Y %H:%M:%S",tz="America/Chicago")
str(MooseInfo)
str(snowtimes)
snowtimesM<-merge(MooseInfo,snowtimes,by.x="Moose_ID",by.y="ID",all.x=TRUE)

snowtimes$deathtime<-MooseInfo$deathtime[match(snowtimes$ID,MooseInfo$Moose_ID)]


snowtimes$min.death.diff.weeks<-difftime(snowtimes$deathtime,snowtimes$min.snow.date,units="weeks")
snowtimes$max.death.diff.weeks<-difftime(snowtimes$deathtime,snowtimes$max.snow.date,units="weeks")

#remove observation 31 as min is same as death (within week)
snowtimes<-snowtimes[-31,]
#turn max values of less than 5 weeks into "NA"
snowtimes$max.snow.date[]
str(snowtimes$max.death.diff.weeks)

snowtimes$max.snow.date[snowtimes$max.death.diff.weeks<=5]<-NA
snowtimes$max.snow.date.jul<-as.numeric(format(snowtimes$max.snow.date,"%j"))
snowtimes$min.snow.date.jul<-as.numeric(format(snowtimes$min.snow.date,"%j"))

write.csv(snowtimes,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/snowtimes.csv")


p <- ggplot(snow, aes(x=timestampL,y=MODIS.Snow.Terra.Snow.500m.8d.Snow.Cover,group=as.factor(animalID),colour=as.factor(animalID))) + geom_line(size=2)
print(p)


#Distance from centroid to Lake Superior
#read in a polygon of LS
LS<-readOGR("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_water_lake_superior_basin",layer="lake_superior_basin",)
head(str(LS))
plot(LS)


fallcent <- getSpPPolygonsLabptSlots(fallMooseBB90HR)
springcent <- getSpPPolygonsLabptSlots(springMooseBB90HR)
summercent <- getSpPPolygonsLabptSlots(summerMooseBB90HR)

plot(fallMooseBB90HR)
fallcentSP<-SpatialPointsDataFrame(coords=fallcent[,1:2],data=as.data.frame(fallcent),proj4string=CRS("+proj=utm +zone=15 ellps=WGS84"))
plot(fallcentSP,add=TRUE,col="red")


str(fallMooseBB90HR)
fallcent <- getSpPPolygonsLabptSlots(fallMooseBB90HR)
springcent <- getSpPPolygonsLabptSlots(springMooseBB90HR)
summercent <- getSpPPolygonsLabptSlots(summerMooseBB90HR)

plot(fallMooseBB90HR)
fallcentSP<-SpatialPointsDataFrame(coords=fallcent[,1:2],data=as.data.frame(fallcent),proj4string=CRS("+proj=utm +zone=15 ellps=WGS84"))
plot(fallcentSP,add=TRUE,col="red")

fallcent<-cbind(as.data.frame(fallMooseBB90HR@data$ID),fallcent)
springcent<-cbind(as.data.frame(springMooseBB90HR@data$ID),springcent)
summercent<-cbind(as.data.frame(summerMooseBB90HR@data$ID),summercent)

colnames(fallcent)<-c("ID","X","Y")
colnames(springcent)<-c("ID","X","Y")
colnames(summercent)<-c("ID","X","Y")


allcent<-rbind(fallcent,springcent,summercent)

allcentSP<-SpatialPointsDataFrame(coords=allcent[,2:3],data=as.data.frame(allcent),proj4string=CRS("+proj=utm +zone=15 ellps=WGS84"))
plot(allcentSP,add=TRUE,col="blue")
#distances to lake superior

allcentSP
LS

newproj_LS<-projection(LS)
#Transformations
plot(habC)
#habC<- projectRaster(habC, crs=newproj)
allcentSP2 <- spTransform(allcentSP, crs(newproj_LS))
distLS<-as.vector(apply(gDistance(allcentSP2, LS,byid=TRUE),2,min))
str(distLS)
allcentSP2DF<-as.data.frame(allcentSP2)
allcentSP2DF<-cbind(allcentSP2DF,distLS)

write.csv(allcentSP2DF,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData/centroid_dist_LakeSuperior_040218.csv")











# FITTING A GENERALISED FUNCTIONAL RESPONSE MODEL (see Matthiopoulos et al. 2011)
# View the columns of the 'habitatDat' data frame
head(habitatDat)
str(habitatDat)
table(habitatDat$id)
# NOTE! The rows are sorted by id. This is necessary for later stages.
# You can make sure this is the case as follows:

deer<-deer[order(deer$animalID),]

# The columns contain
# id: the sampling instance
# food, water: covariate values that vary by location within a sampling instance
# N: the population density associated with the entire sampling instance
# use: the count of animals in each grid cell in the data

# Specification of a Generalised Functional Response model
# The user is required to specify two components:

# The first is a formula involving the main effects
# (i.e. those variable that change at different spatial locations within a sampling instance)
# Here, food and temperature are of interest.
# Note that a quadratic main effect for temperature is also included to capture the optimal
# temperature if discernible within the data (see Matthiopoulos et al 2015 for a more extensive
# discussion on resourses, risks and conditions)

#####Amanda's top model for summer and winter: y ~ cover + h75 + st1 + st2 + (1 | AnimalID)* **
formula<-y~cover + h75 + st1 + st2

# The second component is a vector of text strings,
# the names of the instance-specific covariates
addexp="N"

# The gfrModel can now be fit to the habitatDat. An order 2 GFR will incorporate
# 1st and 2nd expectations of covariates within eack sampling instance
str(deer)
str(habitatDat)
deer$coverN<-as.numeric(deer$cover)
formula<-y~coverN + h75 + st1 + st2
deer$blk<-1
deer$blk<-as.integer(deer$blk)

gfrModel<-gfr(formula=formula,
              data=deer, order=2,family=gaussian,
              blockName=deer$blk,step=FALSE)

# Inspect quality of fit to observed usage (consider refitting the gfr with order 3 or higher)
smoothScatter(habitatDat$use,fitted(gfrModel$model), xlab="Observed usage", ylab="Fitted usage")
abline(0,1)


# FITTING A POPULATION MODEL TO DEMOGRAPHIC DATA USING SPATIAL INFORMATION
# Step 1: Generate a Gaussian mixture approximation of the habitat composition
# of different instances. This is the parametric habitat availability summary
# collected in the object fa. A total of G=30 mixture components are used to
# describe habitat availability in each instance (more is better - and slower).
fa<-favail(habitatDat[,2:3], blocking=habitatDat$id, G=5)
head(habitatDat)
unique(habitatDat$id)
unique(habitatDat$use)
# Step 2: Extract the combined coefficients of the model's main effects
# for each sampling instance from the GFR
ga<-ga.gfr(gfrModel$model, gfrModel$expectations)

# Step 3: Calculate the constructed covariate dataframe for the population model
pop.frame<-pop.covariates(fa, ga)

# Step 4: Combine the constructed covariates
# (describing the effects of the habitat composition)
# with the population data for each sampling instance
datPop<-cbind(populationDat, pop.frame)
# Perhaps, exclude very small population sizes, to avoid influencing the model by
# landscapes that were not used due to low overall density
datPop<-na.omit(datPop[datPop$N1>100,])

# Step 5: Fit the population model using constructed covariates for growth
# and density dependence (See Matthiopoulos et al. 2015)
spatial<-glm(N2~food+temp+temp_2+dens_dep:N1, family=poisson(), offset=log(N1), data=datPop)
# For comparison, a non-spatial version of the model is fitted
# (using just the averages of covariates)
meanfield<-glm(N2~AvFood+AvTemp+AvTemp2+N1, family=poisson(), offset=log(N1), data=datPop)

# Step 6: Some quality of fit plots
xax<-log(datPop$N2/datPop$N1)
yrange<-c(min(xax),max(xax))
par(mfrow=c(2,2))
# Plotting predictions for population growth rate
smoothScatter(xax, log(fitted(spatial)/datPop$N1), ylim=yrange, ylab="Predicted",
              xlab="Observed log(r)", main="Growth Rate (spatial)")
abline(0,1)
smoothScatter(xax, log(fitted(meanfield)/datPop$N1), ylim=yrange, ylab="Predicted",
              xlab="Observed log(r)", main="Growth Rate (mean field)")
abline(0,1)
# Plotting predictions for population carrying capacity
datPopK<-datPop[datPop$year>10,]
KKspatial<-(cbind(datPopK[,1]*0+1, datPopK$food, datPopK$temp, datPopK$temp_2)%*%
              spatial$coefficients[1:4])/(-spatial$coefficients[5]*datPopK$dens_dep)
KKmean<-(cbind(datPopK[,1]*0+1, datPopK$AvFood, datPopK$AvTemp, datPopK$AvTemp^2)%*%
           meanfield$coefficients[1:4])/(-meanfield$coefficients[5])
yrange<-c(0.9*min(datPopK$N2),1.1*max(datPopK$N2))
smoothScatter(datPopK$N2, KKspatial,  nrpoints=500, bandwidth=40, ylim=yrange, xlim=yrange,
              xlab="Observed carrying capacity (K)", ylab="K predicted", main="Carrying capacity (spatial)")
abline(0,1)
smoothScatter(datPopK$N2, KKmean,  nrpoints=500, bandwidth=40, ylim=yrange, xlim=yrange,
              xlab="Observed carrying capacity (K)", ylab="K predicted", main ="Carrying capacity (mean field)")
abline(0,1)
par(mfrow=c(1,1))
