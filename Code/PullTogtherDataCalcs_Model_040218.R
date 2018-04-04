
#overall (ignoring winter severity) - %68 of adult females were migrators

setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData")

#ID, and home range size
HomeRange<-read.csv("allarea_032818.csv")
head(HomeRange)
str(HomeRange)

D1<-HomeRange

#centroid XY and dist to Lake Superior
LakeSup<-read.csv("centroid_dist_LakeSuperior_040218.csv")
head(LakeSup)
str(LakeSup)

D1$CentX<-LakeSup$X[match(D1$ID,LakeSup$ID)]
D1$CentY<-LakeSup$Y[match(D1$ID,LakeSup$ID)]
D1$distLS<-LakeSup$distLS[match(D1$ID,LakeSup$ID)]

#amanda orig categories and LIDAR NLCD - rax count and percentage
FallHabRaw<-read.csv("Fall_MooseHR_HabPer_032218.csv")
head(FallHabRaw)
names(FallHabRaw)
SpringHabRaw<-read.csv("Spring_MooseHR_HabPer_032218.csv")
names(SpringHabRaw)
SummerHabRaw<-read.csv("Summer_MooseHR_HabPer_032218.csv")
names(SummerHabRaw)

sort(names(FallHabRaw))
sort(names(SummerHabRaw))
colnames(SummerHabRaw)[12]<-c("DecidFrst")
colnames(SummerHabRaw)[21]<-c("DecidFrstC")
Lid_OldAM_Raw<-rbind(FallHabRaw,SummerHabRaw,SpringHabRaw)
D1<-merge(D1,Lid_OldAM_Raw,by="ID",all.X=TRUE)
head(D1)

D1$X.x<-NULL
D1$X.y<-NULL


#disturbed sights (raw and percent) from 2000-2009
FallDIST0009<-read.csv("Fall_Disturb2000_2009_032618.csv")
head(FallDIST0009)
names(FallDIST0009)
SpringDIST0009<-read.csv("Spring_Disturb2000_2009_032618.csv")
names(SpringHabRaw)
SummerDIST0009<-read.csv("Summer_Disturb2000_2009_032618.csv")
names(SummerHabRaw)

DIST0009<-rbind(FallDIST0009,SummerDIST0009,SpringDIST0009)
D1<-merge(D1,DIST0009,by="ID",all.X=TRUE)
head(D1)
D1$season<-NULL


####THIS NEEDS TO BE RE-DONE
#disturbed sights (raw and percent) from 2010-2018
FallDIST0918<-read.csv("fall_Disturb2010_2018_040418.csv")
head(FallDIST0918)
SpringDIST0918<-read.csv("spring_Disturb2010_2018_040418.csv")
names(SpringDIST0918)
SpringDIST0918$NA.<-NULL
SpringDIST0918$season<-"spring"
SummerDIST0918<-read.csv("summer_Disturb2010_2018_040418.csv")
names(SummerDIST0918)

DIST0918<-rbind(FallDIST0918,SummerDIST0918,SpringDIST0918)
D1<-merge(D1,DIST0918,by="ID",all.X=TRUE)
head(D1)
D1$season<-NULL

#DEER HARVEST VALUES
DeerHarv<-read.csv("deerharvest_centroids_032618.csv")
head(DeerHarv)

D1$deerharvSQMI2012<-DeerHarv$deerharvSQMI2012[match(D1$ID,LakeSup$ID)]
D1$deerharvSQMI2013<-DeerHarv$deerharvSQMI2013[match(D1$ID,LakeSup$ID)]
D1$deerharvSQMI2014<-DeerHarv$deerharvSQMI2014[match(D1$ID,LakeSup$ID)]
D1$harvestAVE<-DeerHarv$harvestAVE[match(D1$ID,LakeSup$ID)]

#wetness!!!!!
FALLwetarea<-read.csv("fall_wetforest50_allwetmean_033018.csv")
head(FALLwetarea)
SPRwetarea<-read.csv("spring_wetforest50_allwetmean_033018.csv")
SUMwetarea<-read.csv("summer_wetforest50_allwetmean_033018.csv")

ALLwetarea<-rbind(FALLwetarea,SPRwetarea,SUMwetarea)
D1<-merge(D1,ALLwetarea,by="ID",all.X=TRUE)
head(D1)
D1$season<-NULL
D1$X.y<-NULL

#RSF AM update Non-Migrators
RSFnonmig_fall<-read.csv("fall_AM_RSFupdt_NonMig_040218.csv")
head(RSFnonmig_fall)
RSFnonmig_spring<-read.csv("spring_AM_RSFupdt_NonMig_040218.csv")
RSFnonmig_summer<-read.csv("summer_AM_RSFupdt_NonMig_040218.csv")
RSFnonmig_summer$mean.nomig.1<-NULL
RSFnonmig_summer$median.nomig.1<-NULL
RSFnonmig_summer$max.nomig.1<-NULL
RSFnonmig_summer$sd.nomig.1<-NULL
RSFnonmig_summer$n.nomig.1<-NULL

RSFnonmig<-rbind(RSFnonmig_fall,RSFnonmig_summer,RSFnonmig_spring)
D1<-merge(D1,RSFnonmig,by="ID",all.X=TRUE)
head(D1)
D1$season<-NULL

#RSF AM update Migrators
RSFMigrators_fall<-read.csv("fall_AM_RSFupdt_Migrators_040218.csv")
head(RSFMigrators_fall)
RSFMigrators_spring<-read.csv("spring_AM_RSFupdt_Migrators_040218.csv")
RSFMigrators_summer<-read.csv("summer_AM_RSFupdt_Migrators_040218.csv")

RSFMigrators<-rbind(RSFMigrators_fall,RSFMigrators_summer,RSFMigrators_spring)
D1<-merge(D1,RSFMigrators,by="ID",all.X=TRUE)
head(D1)
D1$season<-NULL
D1$X.x<-NULL
D1$X.y<-NULL


#deconstruct the D1$ID into parts
head(D1)
test<-unlist(strsplit(as.character(D1$ID), "[_]|[.]|[X]")) 
head(test,15)
D1$year <- test[seq(3, length(test), 7)]
D1$MooseID <- test[seq(2, length(test), 7)]
D1$Season2 <- test[seq(4, length(test), 7)]

D1$IDyear<-paste(D1$MooseID,D1$year,sep="_")
D1$IDseas<-paste(D1$MooseID,D1$Season2,sep="_")

TOTALarea_km2<-aggregate(D1$area_km2,list(D1$IDseas),FUN=sum)
colnames(TOTALarea_km2)<-c("IDseas","TOTALarea_km2")
D1<-merge(D1,TOTALarea_km2,by="IDseas",all.X=TRUE)

#snowtimes
#merge by ID
snow<-read.csv("snowtimes.csv")
head(snow)
snow$IDyear<-paste(snow$ID,snow$year,sep="_")
D1$max.snow.date.jul<-snow$max.snow.date.jul[match(D1$IDyear,snow$IDyear)]
D1$min.snow.date.jul<-snow$min.snow.date.jul[match(D1$IDyear,snow$IDyear)]


require(dplyr)
D1roll<-D1%>%
  group_by(IDseas)%>%
  summarise(distLSW= weighted.mean(distLS,area_km2),CentXW= weighted.mean(CentX,area_km2),CentYW= weighted.mean(CentY,area_km2),TOTALarea_km2=sum(area_km2),
            FrgHW=weighted.mean(FrgH,area_km2),PrW=weighted.mean(PrH,area_km2),WtHW=weighted.mean(WtH,area_km2),aHabW=weighted.mean(aHab,area_km2), 
            DevelW=weighted.mean(Devel,area_km2),EmWetW=weighted.mean(EmWet,area_km2),WoodyWetW=weighted.mean(WoodyWet,area_km2),OpenWaterW=weighted.mean(OpenWater,area_km2),ConFrstW=weighted.mean(ConFrst,area_km2),DecidFrstW=weighted.mean(DecidFrst,area_km2),MixFrstW=weighted.mean(MixFrst,area_km2),RegenFrstW=weighted.mean(RegenFrst,area_km2),GrassW=weighted.mean(Grass,area_km2),
            disturbed2000W=weighted.mean(disturbed2000,area_km2),deerharvSQMI2012W=weighted.mean(deerharvSQMI2012,area_km2),deerharvSQMI2032W=weighted.mean(deerharvSQMI2013,area_km2),deerharvSQMI2014W=weighted.mean(deerharvSQMI2014,area_km2),harvestAVEW=weighted.mean(harvestAVE,area_km2),
            CTIforSTAN50_wetpercentW=weighted.mean(CTIforSTAN50_wetpercent,area_km2),CTISTANWetmeantW=weighted.mean(CTISTANWetmeant,area_km2),
            mean.nomigW=weighted.mean(mean.nomig,area_km2),median.nomigW=weighted.mean(median.nomig,area_km2),max.nomigW=weighted.mean(max.nomig,area_km2),sd.nomigW=weighted.mean(sd.nomig.x,area_km2),mean.migW=weighted.mean(mean.mig,area_km2),median.migW=weighted.mean(median.mig,area_km2),max.migW=weighted.mean(max.mig,area_km2),sd.migW=weighted.mean(sd.nomig.y,area_km2),
            CutArea2010W=sum(CutArea2010/(area_km2*1000000)),CutArea2011W=sum(CutArea2011/(area_km2*1000000)),CutArea2012W=sum(CutArea2012/(area_km2*1000000)),CutArea2013W=sum(CutArea2013/(area_km2*1000000)),CutArea2014W=sum(CutArea2014/(area_km2*1000000)),CutArea2015W=sum(CutArea2015/(area_km2*1000000)),CutArea2016W=sum(CutArea2016/(area_km2*1000000)),
            max.snow.date.jul=mean(max.snow.date.jul),min.snow.date.jul=mean(min.snow.date.jul)
            )

D2<-as.data.frame(D1roll)

test<-unlist(strsplit(as.character(D2$IDseas), "[_]")) 
D2$MooseID <- test[seq(1, length(test), 2)]
D2$Season <- test[seq(2, length(test), 2)]


#Moose Demo Info
MooseInfo<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/MooseInfo.csv")
head(MooseInfo)
#MB3.dat<-merge(MB3.dat,MooseInfo,by.x="animalID",by.y="Moose_ID",all.x=TRUE)
MooseInfo$deathtime<-paste(MooseInfo$Date.of.death,MooseInfo$time.of.death,sep=" ")
MooseInfo$deathtime<-strptime(MooseInfo$deathtime, "%m/%d/%Y %H:%M:%S",tz="America/Chicago")
MooseInfo$deathyear<-format(MooseInfo$deathtime,"%Y")

head(D2)
D2$deathyear<-MooseInfo$deathyear[match(as.numeric(D2$MooseID),MooseInfo$Moose_ID)]
D2$sex<-MooseInfo$Sex[match(as.numeric(D2$MooseID),MooseInfo$Moose_ID)]
D2$PT<-MooseInfo$PT[match(as.numeric(D2$MooseID),MooseInfo$Moose_ID)]
D2$death.age<-MooseInfo$Age.at.Death[match(as.numeric(D2$MooseID),MooseInfo$Moose_ID)]


#disturb by year 2010-2018
head(D2)
#add in death year and do iselse statements
summary(as.numeric(D2$deathyear))
D2$RecentDisturb<-NA
D2$RecentDisturb<-ifelse(D2$deathyear=="2013",D2$CutArea2010W+D2$CutArea2011W+D2$CutArea2012W,D2$RecentDisturb)
D2$RecentDisturb<-ifelse(D2$deathyear=="2014",D2$CutArea2010W+D2$CutArea2011W+D2$CutArea2012W+D2$CutArea2013W,D2$RecentDisturb)
D2$RecentDisturb<-ifelse(D2$deathyear=="2015",D2$CutArea2010W+D2$CutArea2011W+D2$CutArea2012W+D2$CutArea2013W+D2$CutArea2014W,D2$RecentDisturb)
D2$RecentDisturb<-ifelse(D2$deathyear=="2016",D2$CutArea2010W+D2$CutArea2011W+D2$CutArea2012W+D2$CutArea2013W+D2$CutArea2014W+D2$CutArea2015W,D2$RecentDisturb)
D2$RecentDisturb<-ifelse(D2$deathyear=="2017",D2$CutArea2010W+D2$CutArea2011W+D2$CutArea2012W+D2$CutArea2013W+D2$CutArea2014W+D2$CutArea2015W+D2$CutArea2016W,D2$RecentDisturb)
summary(D2$RecentDisturb)

D2$AllDisturb<-D2$RecentDisturb+D2$disturbed2000W
summary(D2$AllDisturb)

#Lidar hab use (Deer) percent by season and cover
DeerHabPer<-read.csv("HabUse_NLCDLIDcat_season4_032318.csv")
head(D2)
Clnames<-c("Devel","EmWet","WoodyWet","OpenWater","ConFrst","DecidFrst","MixFrst","RegenFrst","Grass")
DeerHabPer$HabNames<-rep(Clnames,4)

#Fall
D2$DeerHabLidWt_fall<-NA
D2$DeerHabLidWt_fall.devel<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==100]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.emwet<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==101]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.woodywet<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==102]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.openwater<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==103]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.confrst<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==105]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.decidfrst<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==106]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.mixfrst<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==107]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.regenfrst<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==108]*D2$DevelW),D2$DeerHabLidWt_fall)
D2$DeerHabLidWt_fall.grass<-ifelse(D2$Season=="Fall",(DeerHabPer$mean.per[DeerHabPer$season4=="Fall"&DeerHabPer$NLCDLIDcover==109]*D2$DevelW),D2$DeerHabLidWt_fall)
#Spring
D2$DeerHabLidWt_spring<-NA
D2$DeerHabLidWt_spring.devel<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==100]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.emwet<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==101]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.woodywet<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==102]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.openwater<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==103]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.confrst<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==105]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.decidfrst<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==106]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.mixfrst<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==107]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.regenfrst<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==108]*D2$DevelW),D2$DeerHabLidWt_spring)
D2$DeerHabLidWt_spring.grass<-ifelse(D2$Season=="Spring",(DeerHabPer$mean.per[DeerHabPer$season4=="Spring"&DeerHabPer$NLCDLIDcover==109]*D2$DevelW),D2$DeerHabLidWt_spring)
#Summer
D2$DeerHabLidWt_summer<-NA
D2$DeerHabLidWt_summer.devel<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==100]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.emwet<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==101]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.woodywet<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==102]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.openwater<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==103]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.confrst<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==105]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.decidfrst<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==106]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.mixfrst<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==107]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.regenfrst<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==108]*D2$DevelW),D2$DeerHabLidWt_summer)
D2$DeerHabLidWt_summer.grass<-ifelse(D2$Season=="Summer",(DeerHabPer$mean.per[DeerHabPer$season4=="Summer"&DeerHabPer$NLCDLIDcover==109]*D2$DevelW),D2$DeerHabLidWt_summer)

#ALL

names(D2)
head(D2)
D2$DeerHabLidWt.fallTOT<-rowSums(D2[,52:60],na.rm=TRUE)
D2$DeerHabLidWt.springTOT<-rowSums(D2[,62:70],na.rm=TRUE)
D2$DeerHabLidWt.summerTOT<-rowSums(D2[,62:80],na.rm=TRUE)

D2$DeerHabLidWt.TOTAL<-rowSums(D2[,81:83],na.rm=TRUE)

hist(D2$DeerHabLidWt.TOTAL)

saveRDS(D2,"D2_040418.R")
write.csv(D2,"D2_040418.csv")
saveRDS(D1,"D1_040418.R")
write.csv(D1,"D1_040418.csv")


