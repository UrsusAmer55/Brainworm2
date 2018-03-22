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

M1a<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/DNR_moose/moose_locs_Ditmer_3missing/181DEAD_Collar13782_GPS_Locs_CLEAN.csv")
M1b<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/DNR_moose/moose_locs_Ditmer_3missing/156DEAD_Collar13819_GPS_Locs_CLEAN.csv")
head(M1b)

setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/DNR_moose/moose_locs_Ditmer_3missing")

# way to read in and add the file name as a column (need to extract MooseID number)
read.data <- function(file){
  dat <- read.table(file,header=T,sep=",")
  dat$fname <- file
  return(dat)
}
M1 <- do.call(rbind.fill, lapply(list.files(pattern="csv$"),read.data))
M1$MooseID<-substr(M1$fname,1,3)
head(M1)



plot(M1$Easting.fixed,M1$Northing,col=as.factor(M1$MooseID))

unique(M1$MooseID)
hist(M1$velocity.)


#dealing with the time. apparently not local - just UTC -6
# dr2$TimeUTC <- chron(times=as.character(dr2$UTC_Time))
# dr2$DateUTC<- as.POSIXct(dr2$UTC_Date, format= "%m/%d/%Y ")
# dr2$dtmUTC <- as.factor(paste(dr2$UTC_Date, dr2$UTC_Time, sep = " "))
# head(dr2$dtmUTC)
str(M1$date.timeGMToff)



head(M1$date.timeGMToff)
M1$dtL<-strptime(M1$date.timeGMToff, "%Y-%m-%d %H:%M:%S",tz = "Etc/GMT+6")
head(M1$dtL)
str(M1$dtL)
M1$dtL<-as.POSIXct(format(M1$dtL, tz="America/Chicago"))
head(M1$dtL)

M1$dtUTC <- strptime(x=M1$date.timeGMToff, format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT+6")
M1$dtUTC <- as.POSIXct(x=M1$dtUTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
M1$dtUTC<-M1$dtUTC+(6*(60*60))
head(M1$dtUTC)
head(M1$dtL)

M1$MooseID<-as.numeric(M1$MooseID)

#read in and merge the moose stats (sex, brain worm?, age)
Mstat<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/DNRmooseStatus.csv")

M2<-merge(M1,Mstat,by="MooseID",all.x=TRUE)
head(M2)

summary(M2$Bworm)
table(M2$Bworm)
plot(M2$Easting.fixed,M2$Northing,col=M2$Bworm)


#convert to spatial points DF
M2SP<-SpatialPointsDataFrame(coords=M2[c("Easting.fixed","Northing")],proj4string= CRS("+proj=utm +zone=15 ellps=WGS84"),data=M2)
#get proper lat long
M2SPlatlong <- spTransform(M2SP, CRS("+proj=longlat +datum=WGS84"))
test<-as.data.frame(M2SPlatlong@coords)
head(test)
colnames(test)<-c("long","lat")
#create new M2 with proper lat/long
M2<-cbind(M2,test)
head(M2)


plot(M2SPlatlong,col="blue")
plot(dr2SP,col="red",add=TRUE)

#plot points with google maps
map <- get_map(location = c(lon = -91.3, lat = 47.75), zoom = 8)
ggmap(map)
ggmap(map)+
  geom_point(aes(x = long, y = lat,color=as.factor(Bworm)), data = M2,
             alpha = .5, size = 3)
unique(M2$MooseID)



head(M3)
M81<-M2[M2$MooseID==181,]
M56<-M2[M2$MooseID==156,]
head(M56)
str(M56$date.timeGMToff)
#moose 156 is a pain in the ass
M56<-M2[M2$MooseID=="156",]
head(M56)
M56$date.timeGMToff<-strptime(M56$date.timeGMToff, "%m/%d/%Y %H:%M:%S")
M56$date.timeGMToff<-format(M56$date.timeGMToff,"%Y-%m-%d %H:%M:%S")
M56$LMTconvert<-strptime(M56$LMTconvert, "%m/%d/%Y %H:%M:%S")
M56$LMTconvert<-format(M56$LMTconvert,"%Y-%m-%d %H:%M:%S")
head(M56)
M56$dtL<-strptime(M56$date.timeGMToff, "%Y-%m-%d %H:%M:%S",tz = "Etc/GMT+6")
head(M56$dtL)
str(M56$dtL)
M56$dtL<-as.POSIXct(format(M56$dtL, tz="America/Chicago"))
head(M56$dtL)

M56$dtUTC <- strptime(x=M56$date.timeGMToff, format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT+6")
M56$dtUTC <- as.POSIXct(x=M56$dtUTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
M56$dtUTC<-M56$dtUTC+(6*(60*60))
head(M56$dtUTC)

M81<-M2[M2$MooseID=="181",]
head(M81)
M81$date.timeGMToff<-strptime(M81$date.timeGMToff, "%m/%d/%Y %H:%M:%S")
M81$date.timeGMToff<-format(M81$date.timeGMToff,"%Y-%m-%d %H:%M:%S")
M81$LMTconvert<-strptime(M81$LMTconvert, "%m/%d/%Y %H:%M:%S")
M81$LMTconvert<-format(M81$LMTconvert,"%Y-%m-%d %H:%M:%S")
head(M81)
M81$dtL<-strptime(M81$date.timeGMToff, "%Y-%m-%d %H:%M:%S",tz = "Etc/GMT+6")
head(M81$dtL)
str(M81$dtL)
M81$dtL<-as.POSIXct(format(M81$dtL, tz="America/Chicago"))
head(M81$dtL)

M81$dtUTC <- strptime(x=M81$date.timeGMToff, format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT+6")
M81$dtUTC <- as.POSIXct(x=M81$dtUTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
M81$dtUTC<-M81$dtUTC+(6*(60*60))
head(M81$dtUTC)



M3<-M2[M2$MooseID!=156&M2$MooseID!=181,]
M3<-rbind(M3,M56,M81)
head(Mtest)


names(M3)
M3<-M3[,-c(38:61)]
head(M3)
names(M3)
M3<-M3[,-c(49:70)]

M3$fixID<-1:nrow(M3)

write.csv(M3,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Clean_092617.csv")
NAs<-is.na(M3$dtUTC)
table(NAs)

#just the 3 missing moose
Miss3<-M3[M3$MooseID==156|M3$MooseID==157|M3$MooseID==181,]
write.csv(Miss3,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Clean_092517_3missing.csv")

M3<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Clean_092617.csv")
head(M3)
length(unique(M3$MooseID))
table(M3$MooseID)
head(M3)
MovebankVers_091917
MB3<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Moose_Movebank_092717.csv")
head(MB3)
vignette("variogram")
data("buffalo")
Cilla <- buffalo$Cilla
head(buffalo)

MB3 <- as.telemetry(MB3)
head(MB3)


Cilla<-MB3$`11`
SVF <- variogram(Cilla)
level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # 0-12 hour window
plot(SVF,xlim=xlim,level=level)
title("zoomed in")
plot(SVF,fraction=0.65,level=level)
title("zoomed out")
#individual vario
variogram.fit(SVF)

#population vario
SVF4 <- lapply(MB3,variogram)
SVF4 <- mean(SVF4)
plot(SVF4,fraction=0.35,level=level)
title("Population variogram")

#movebank funcs
MB3<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Moose_Movebank_092717.csv")

testO<-move("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Moose_Movebank_092717.csv")
str(testO)
head(MB3)
str(test)
Move3<-move(MB3)

Amy<-test@trackId[1]
Amy<-test$`X11`
show(Amy)
plot(Amy)
head(timeLag(Amy, units="hours"))
leroy_df <- as(Amy, "data.frame")
head(leroy_df)

#' Create move object and look at functions in move package
#+warning=FALSE
head(MB3)
MB3.dat<-MB3[order(MB3$individual.local.identifier,MB3$timestamp),]
length(unique(MB3.dat$individual.local.identifier))
str(MB3.move)


###first split by season
head(MB3.dat)
str(MB3.dat)



MB3.dat$timestampL<-strptime(MB3.dat$timestamp, "%Y-%m-%d %H:%M:%S",tz="America/Chicago")
MB3.dat$animalID<-MB3.dat$individual.local.identifier

#pull in outside moose data especially date of death

MooseInfo<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/MooseInfo.csv")
head(MooseInfo)
MB3.dat<-merge(MB3.dat,MooseInfo,by.x="animalID",by.y="Moose_ID",all.x=TRUE)
head(MB3.dat)
MB3.dat$deathtime<-paste(MB3.dat$Date.of.death,MB3.dat$time.of.death,sep=" ")
MB3.dat$deathtime<-strptime(MB3.dat$deathtime, "%m/%d/%Y %H:%M:%S",tz="America/Chicago")

#create a loop to remove the last 2 months of GPS data for each moose

samplehold<-NULL

for (i in 1:length(unique(MB3.dat$animalID))){
  
  subdat<-subset(MB3.dat,MB3.dat$animalID==unique(MB3.dat$animalID)[i]) 
  endGPS<-unique(subdat$deathtime)-((60*24*60*60))
  #timediff<-as.numeric(max(subdat$timestampL)-unique(subdat$deathtime))
  subdatCUT<-subdat[subdat$timestampL<=endGPS,]
  OneYear<-max(subdatCUT$timestampL)-((365*24*60*60))
  subdatCUTyr<-subdatCUT[subdatCUT$timestampL>=OneYear,]
  samplehold<-rbind(samplehold,subdatCUTyr)
print(i)
}

table(samplehold$animalID)

MB4.dat<-samplehold

MB4.dat<-MB4.dat[!is.na(MB4.dat$location.long),]


str(MB4.dat)
summary(MB4.dat$location.long)

#assign a season
head(MB4.dat)
MB4.dat$month<-as.numeric(format(MB4.dat$timestampL,"%m"))
table(MB4.dat$month)
MB4.dat$monthday<-format(MB4.dat$timestampL, format="%m-%d")

str(MB4.dat)
MB4.dat$season<-NA
MB4.dat$season[MB4.dat$monthday>"03-15" & MB4.dat$monthday<="06-15"]<- "Spring"
MB4.dat$season[MB4.dat$monthday>"06-15" & MB4.dat$monthday<="09-15"]<- "Summer"
MB4.dat$season[MB4.dat$monthday>"09-15" & MB4.dat$monthday<="12-15"]<- "Fall"
MB4.dat$season[MB4.dat$monthday>"12-15" | MB4.dat$monthday<="03-15"]<- "Winter"
table(MB4.dat$season)
head(MB4.dat)
str(MB4.dat)
MB4.dat$season<-as.factor(MB4.dat$season)

str(MB4.dat)
unique(MB4.dat$season)
MB4.spr<-MB4.dat[MB4.dat$season=="Spring",]
head(MB4.spr$timestampL[1000:1200],50)


dups<-MB4.dat$timestamp[MB4.dat$individual.local.identifier==13]
x<-dups[duplicated(dups)==TRUE]
head(x)
dups2<-MB4.dat[MB4.dat$timestamp=="2013-01-30 00:46:08.000",]
summary(MB4.dat$event.id)
MB4.dat<-MB4.dat[!duplicated(MB4.dat$event.id),]



write.csv(MB4.dat,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Moose_Movebank_Alter_110817.csv")
test<-move("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Moose_Movebank_Alter_110817.csv")


# str(MB4.dat)
# MB4.dat$individual.local.identifier<-as.character(MB4.dat$individual.local.identifier)
# data2 <- move(x=MB4.dat$location.long, y=MB4.dat$location.lat,
#               time=as.POSIXct(MB4.dat$timestamp,format="%Y-%m-%d %H:%M:%S", tz="UTC"),
#               data=MB4.dat, proj=CRS("+proj=longlat +ellps=WGS84"),
#               animal=MB4.dat$individual.local.identifier)
# 



MB4.dat.move<-test
table(MB4.dat.move@trackId)

str(MB4.dat.move@trackId[1])
MB4.dat.move$year<-as.numeric(format(MB4.dat.move$timestamp,"%Y"))
head(MB4.dat.move)




MB4.spr.move<-MB4.dat.move[MB4.dat.move@data$season=="Spring",]
table(MB4.spr.move@trackId)
unique(MB4.spr.move@trackId)
MB4.spr.move$IDYrSeas<-paste(MB4.spr.move@trackId,MB4.spr.move$year,MB4.spr.move$season,sep="_")
unique(MB4.spr.move$IDYrSeas)
table(MB4.spr.move$IDYrSeas)
# MB4.spr.moveC<-MB4.spr.move[MB4.spr.move$IDYrSeas=="X13_2014_Spring"|MB4.spr.move$IDYrSeas=="X13_2013_Spring"|MB4.spr.move$IDYrSeas=="X15_2013_Spring"|MB4.spr.move$IDYrSeas=="X15_2014_Spring"|MB4.spr.move$IDYrSeas=="X156_2014_Spring"|
#                               MB4.spr.move$IDYrSeas=="X156_2015_Spring"|MB4.spr.move$IDYrSeas=="X157_2016_Spring"|MB4.spr.move$IDYrSeas=="X157_2017_Spring"|MB4.spr.move$IDYrSeas=="X200_2015_Spring"|MB4.spr.move$IDYrSeas=="X200_2016_Spring",]
# unique(MB4.spr.moveC$IDYrSeas)
# table(MB4.spr.moveC$IDYrSeas)

MB4.sum.move<-MB4.dat.move[MB4.dat.move@data$season=="Summer",]
table(MB4.sum.move@trackId)

unique(MB4.sum.move@trackId)
MB4.sum.move$IDYrSeas<-paste(MB4.sum.move@trackId,MB4.sum.move$year,MB4.sum.move$season,sep="_")
unique(MB4.sum.move$IDYrSeas)
table(MB4.sum.move$IDYrSeas)
MB4.sum.move<-MB4.sum.move[MB4.sum.move$IDYrSeas!="X202_2016_Summer",]


MB4.fall.move<-MB4.dat.move[MB4.dat.move@data$season=="Fall",]
table(MB4.fall.move@trackId)
summary(MB4.fall.move[1])

unique(MB4.fall.move@trackId)
MB4.fall.move$IDYrSeas<-paste(MB4.fall.move@trackId,MB4.fall.move$year,MB4.fall.move$season,sep="_")
unique(MB4.fall.move$IDYrSeas)
table(MB4.fall.move$IDYrSeas)
# remove X161_2015_Fall
MB4.fall.move<-MB4.fall.move[MB4.fall.move$IDYrSeas!="X161_2015_Fall",]

# MB4.dat.move<-move(x=MB4.dat$location.long, y=MB4.dat$location.lat, 
#                    time=as.POSIXct(MB4.dat$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#                    proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
#                    data=MB4.dat, animal=MB4.dat$individual.local.identifier)


# MB4.spr.move<-move(x=MB4.spr$location.long, y=MB4.spr$location.lat, 
#                   time=as.POSIXct(MB4.spr$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#                   proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
#                   data=MB4.spr, animal=MB4.spr$individual.local.identifier)

plot(MB4.sum.move)
show(MB4.sum.move)
summary(MB4.sum.move)
MB4.sum.moveU<-spTransform(MB4.sum.move, CRSobj="+proj=utm +zone=15 ellps=WGS84")
head(MB4.sum.moveCU)

?split
unstacked <- split(MB4.sum.moveU,f=MB4.sum.moveU$IDYrSeas)
head(unstacked)
str(unstacked)


#re-calc time between fixes
for (i in 1:length(unique(MB4.sum.moveU$IDYrSeas))){

t11<-unstacked[i]

t11<-as.data.frame(t11)
n<-nrow(t11)
t11$mooseID<-rep(unique(MB4.sum.move@trackId[i]),n)

t11 <- t11[order(t11[,3]),]
t11$time.diff<-c(0,diff(t11[,3],units="min"))

t11$time.diff<-c(0,as.numeric(diff(t11[,3]),units="mins"))

print(summary(t11$time.diff[2:n]))
print(hist(t11$time.diff[2:n],breaks = c(15,30,45,100,200,300,400,500,600,max(t11$time.diff))),plot=TRUE,xlim=c(0,800))
print(quantile(t11$time.diff[2:n],probs=seq(0,1,.05)))
print(i)
}

t11
unique(MB4.sum.moveU$IDYrSeas)
# X181_2015_Summer 15 second problem

for (i in 12:length(unique(MB4.sum.moveU$IDYrSeas))){

moveU1<-unstacked[i]
stack <- moveStack(moveU1)
BB<-spTransform(stack, CRSobj="+proj=utm +zone=15 ellps=WGS84")
unique(BB$IDYrSeas)
#check time intervals
#60/15

df<-as.data.frame(moveU1)
nobs<-1:nrow(df)

test<-c(as.numeric(difftime(df[nobs+1,3], df[nobs,3]),units="mins"))
print(summary(test))

df$time.diff<-c(as.numeric(difftime(df[nobs+1,3], df[nobs,3]),units="mins"))
head(df)
df<-df[df$time.diff>10,]


print(min(df[nobs+1,3]-df[nobs,3],na.rm=TRUE))
print(quantile(df[nobs+1,3]-df[nobs,3],probs=c(.01),na.rm=TRUE))

step<-min(as.numeric(difftime(df[nobs+1,3], df[nobs,3]), units="mins"),na.rm=TRUE)/15
step<-as.numeric(step)

#show(moveU1)
stack <- moveStack(moveU1)
BB<-spTransform(stack, CRSobj="+proj=utm +zone=15 ellps=WGS84")
BB11<-brownian.bridge.dyn(BB, raster=30,location.error=15,time.step=step,ext=3)


#plot(BB11)
#contour(BB11,level=.9)
#contour(BB11,level=.5)
head(BB)
plot(BB)



BB11cont50<-raster2contour(BB11,levels=c(.5))
plot(BB11cont50,add=TRUE,col="red")
# str(BB11)
# BB11cont50poly<-rasterToPolygons(BB11)
# vud <- getvolumeUD(BB11)
# y <- as(BB11, 'SpatialPolygons') 


BB11cont90<-raster2contour(BB11,levels=c(.9))
plot(BB11cont90,col="green",add=TRUE)

BB11cont95<-raster2contour(BB11,levels=c(.95))
plot(BB11cont95,col="orange",add=TRUE)


# BB11cont100<-raster2contour(BB11,levels=c(1))
# plot(BB11cont100,col="green",add=TRUE)


ID<-unique(BB@trackId)
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge50_summer/")
writeOGR(BB11cont50,paste(ID,"_BB50.shp",sep=""),"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge50_summer/",driver="ESRI Shapefile")
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_summer/")
writeOGR(BB11cont90,paste(ID,"_BB90.shp",sep=""),"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_summer/",driver="ESRI Shapefile")
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge95_summer/")
writeOGR(BB11cont95,paste(ID,"_BB95.shp",sep=""),"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge95_summer/",driver="ESRI Shapefile")


}


###read in the Brown Bridge home ranges
BB90<-readOGR("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_summer",layer="X181_2015_Summer.X181_BB90")
plot(BB90,col="red")
str(BB90)


#install.packages(pkgs=c("CircStats", "deSolve", "coda", "deldir", "igraph", "RandomFields", "ks"))
#must reproject to UTM for the shapefiles
newproj <- "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
BB90 <- spTransform(BB90, crs(newproj))

library(sp)
library(raster)
library(sf)
library(rgdal)
library(tools)   # unless already loaded, comes with base R
library(PBSmapping)
library(maptools)
#great way to read in a bunch of dataframes
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_fall/")
file_list <- list.files()
 file_list 
 filenames<-unique(file_path_sans_ext(file_list))

 setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_fall_poly/")
 for (i in 1:length(filenames)){
 test<-readOGR(dsn="C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_fall",layer=filenames[i])
 
 var_temp <- SpatialLines2PolySet(test)
 sp_try <- PolySet2SpatialPolygons(var_temp)
 p.df <- data.frame(ID=rep(filenames[i],length(sp_try)))
 p <- SpatialPolygonsDataFrame(sp_try, p.df) 
 plot(p,col="blue")
 p
 assign(paste(filenames[i],"poly",sep="_"), p )
 
 writeOGR(p,paste(filenames[i],"poly.shp",sep="_"),"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/BrownBridge90_fall_poly/",driver="ESRI Shapefile")
 print(i)
 }
 
#Read in all shapefiles individually
shps <- dir(getwd(), "*.shp")
shps <- sub('\\.shp$',"", shps)
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
plot(summerMooseBB90HR,add=TRUE,col="red")
head(summerMooseBB90HR)
str(summerMooseBB90HR)







#this works to convert lines to polygons
var_temp <- SpatialLines2PolySet(X151_2014_Summer.X151_BB90)
sp_try <- PolySet2SpatialPolygons(var_temp)
p.df <- data.frame(ID=rep(filenames[i],length(sp_try)))
p <- SpatialPolygonsDataFrame(sp_try, p.df) 
plot(p,col="blue")
p

?SpatialPolygonsDataFrame
SpatialPolygonsDataFrame()
plot(sp_try,col="red")



nor_coast_lines_PS <- (X151_2014_Summer.X151_BB90)
plot(nor_coast_lines_PS)
plotLines(nor_coast_lines_PS)
o3 <- PolySet2SpatialLines(nor_coast_lines_PS)
plot(o3, axes=TRUE)
str(o3)
o4<-map2SpatialPolygons(o3)


#Convert foreign object to an sf object
sf_BB90 <- st_as_sf(BB90) 
sf_BB90_polygons <- st_polygonize(sf_BB90)
plot(sf_BB90_polygons)
map2SpatialPolygons
test<-sf_BB90_polygons[1]
b <- spChFIDs(sf_BB90_polygons, paste("ID", row.names(sf_BB90_polygons), sep="."))
# test <- rbind(sf_BB90_polygons[1],sf_BB90_polygons[2], makeUniqueIDs = TRUE)  
shp_sf_BB90_polygons <- as(test, "Spatial") # If you want sp
class(sf_BB90_polygons)




NLCD<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/NLCD 2011 - Land Cover1.tif") #raster: NLCD habitat
nlcdtab<-read.csv("E:ZooLaptop_062817/Moose/Data/OutsideSpatial/NLCD11_Table.csv",header=TRUE) #table so you can see what raster value
nlcdtab[1:2]

str(NLCD)
plot(NLCD)

NLCDcut_BB90<-mask(NLCD,sf_BB90_polygons)
plot(BB50_hab)





BB50_habo<-extract(NLCDcut_BB50,BB50)
# ?extract
# vegRcut<-crop(vegR, extent(min(M1$easting)-300, max(M1$easting)+300,min(M1$northing)-300, max(M1$northing)+300))
# 

library(reshape2)
BB50_habodf<-melt(BB50_habo)
head(BB50_habodf)

y50 <- xtabs(~ L1 + value, BB50_habodf)
# x <- count(testEx2df, c('L1', 'value'))
y50<-as.data.frame(y50)
w50 <- reshape(y50, 
             timevar = "value",
             idvar = c("L1"),
             direction = "wide")
head(w50)
nlcdtab[1:2]
colnames(w50)<-c("ptid","OpenWater","Developed","DeciduousForest","EvergreenForest","MixedForest","ShrubScrub","Herbaceous","WoodyWet","EmergeWet")
#colnames(w)<-c("ptid","DeciduousForest","EvergreenForest","MixedForest","WoodyWet","HerbWet")
head(w50)

w50$sum<-rowSums(w[2:ncol(w)])
summary(w50$sum)

w50<-cbind(w50,BB50@data$indvd__)

w50$PerWater<-w50$OpenWater/w50$sum
w50$PerDevel<-w50$Developed/w50$sum
w50$PerWDecFor<-w50$DeciduousForest/w50$sum
w50$PerEverFor<-w50$EvergreenForest/w50$sum
w50$PerMixFor<-w50$MixedForest/w50$sum
w50$PerShrub<-w50$ShrubScrub/w50$sum
w50$PerWoodWet<-w50$WoodyWet/w50$sum
w50$PerHerbWet<-(w50$Herbaceous+w50$EmergeWet)/w50$sum

w50$FrgH<-w50$PerWDecFor+w50$PerShrub+w50$PerMixFor
w50$PrH<-w50$PerEverFor
w50$WtH<-w50$PerWoodWet
w50$aHab<-1-(w50$FrgH+w50$WtH+w50$PrH)
head(w)


?contour
#get moose HR
names(MB3.dat)
MB3.dat$individual.local.identifier<-as.factor(MB3.dat$individual.local.identifier)
HR<-SpatialPointsDataFrame(coords=MB3.dat[c("utm.easting","utm.northing")],data=MB3.dat[11],proj4string=CRS("+proj=utm +zone=15 ellps=WGS84"))
library(adehabitatHR)
?kernelUD
UD.apollo<-kernelUD(HR, h="href")
image(UD.apollo)

# Outer 50% contour
ver <- getverticeshr(UD.apollo, 50,unin=c("m"),unout=c("km2"))
plot(ver)
head(ver)
verD<-as.data.frame(ver)
centroids <- getSpPPolygonsLabptSlots(ver)
points(centroids, pch = 3, col = "Red")
centroids

#read in a polygon of LS
LS<-readOGR("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_water_lake_superior_basin",layer="lake_superior_basin",)
head(str(LS))
plot(LS)
points(centroids, pch = 3, col = "Red")
str(centroids)
cent<-as.data.frame(centroids)
head(cent)
colnames(cent)<-c("X","Y")
library(rgeos)
centS<-SpatialPoints(cent,proj=CRS("+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#centS<-spTransform(centS, CRSobj="+proj=utm +zone=15 ellps=WGS84")
#distances to lake superior
distLS<-as.vector(apply(gDistance(centS, LS,byid=TRUE),2,min))
str(distLS)
test<-cbind(verD,distLS,cent)
test
plot(test$X,test$Y)
text(test$X,test$Y,labels=test$id)

#read in moose status
Mstat<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/DNRmooseStatus.csv")
head(Mstat)

test1<-merge(test,Mstat,by.x="id",by.y="MooseID")
test1
test1$BwormB<-NA
test1$BwormB[test1$Bworm=="No"]<-0
test1$BwormB[test1$Bworm=="Yes"]<-1

hist(log(test1$distLS))
hist(log(test1$area))


m1<-lm(BwormB~log(distLS)+Sex+log(area),data=test1)
summary(m1)
boxplot(log(test1$distLS)~test1$Bworm,ylab="log(distance to L.Superior)",xlab="Brain worm present?")
#moose 19 and 161 are really far from superior...
test2<-test1[test1$id!=19&test1$id!=161,]
m2<-lm(BwormB~log(distLS)+Sex,data=test2)
summary(m2)
boxplot(log(test2$distLS)~test2$Bworm,ylab="log(distance to L.Superior)",xlab="Brain worm present?")
boxplot(test2$distLS~test2$Bworm,ylab="log(distance to L.Superior)",xlab="Brain worm present?")


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
#get proper lat long
D2SPlatlong <- spTransform(D2SP, CRS("+proj=longlat +datum=WGS84"))
Dcoords<-as.data.frame(D2SPlatlong@coords)
head(Dcoords)
colnames(Dcoords)<-c("long","lat")
deerA<-cbind(deerA,Dcoords)

#plot points with google maps
head(M2)

map <- get_map(location = c(lon = -91.3, lat = 47.75), zoom = 8)
ggmap(map)
ggmap(map)+
  geom_point(aes(x = long, y = lat,color=as.factor(Bworm)), data = M2,
             alpha = .5, size = 3)+
  geom_point(aes(x = long, y = lat), data = deerA,
             alpha = .5, size = 3,color="red")

test<-M3[is.na(M3$dtUTC),]
unique(test$MooseID)


summary(M2$long)



ggmap(map,add=TRUE)

mapPointsDA <- ggmap(map) + geom_point(aes(x = Longitude, y = Latitude), data = dr1, alpha = .5)

mapPointsDA + geom_point(aes(x = long, y = lat,color="red"), data = M22, alpha = .5)







head(M1$dtUTC)
# ####convert to local time
dr2$dtPm <- as.POSIXct(dr2$dtPmUTC, tz="GMT")
dr2$dtPmL<-format(dr2$dtPm, tz="America/Chicago",usetz=TRUE)
dr2$dtPmL <- as.POSIXct(dr2$dtPmL, tz="America/Chicago")
