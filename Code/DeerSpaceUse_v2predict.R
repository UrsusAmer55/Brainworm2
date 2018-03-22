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

hab<-deerA %>% 
  group_by(animalID,cover,season4) %>% 
  summarise(Frequency = sum(y))

tot<-deerA %>% 
  group_by(animalID,season4) %>% 
  summarise(Frequency = sum(y))

hab<-as.data.frame(hab)
tot<-as.data.frame(tot)

per<-merge(hab,tot,by=c("animalID","season4"),all.x=TRUE)

colnames(per)<-c("animalID","season4","cover","use","total")
per$percent<-per$use/per$total
head(per)

boxplot(percent~cover+season4,data=per)

ave<-aggregate(x=per,by=list())
aveAMhabuse_season4<-aggregate( percent~cover+season4,data=per, FUN = "mean")

write.csv(aveAMhabuse_season4,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/DeerUse_PercentMeth/HabUse_AMcat_season4.csv")

deerSum<-deerA[deerA$season=="summer",]
head(deerSum)

library(dplyr)
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
#writeRaster(Landrast,"C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/Landrast.asc",format="raster")
str(Landrast_ras)


####Deer Use V.1 - % on Amanda Groups
#instead of making new rasters for each could just extract from moose HR and multiply by the values....

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


newproj<-projection(summerMooseBB90HR)
newproj2<-projection(habC)

#Transformations
plot(habC)
#habC<- projectRaster(habC, crs=newproj)

summerMooseBB90HR2 <- spTransform(summerMooseBB90HR, crs(newproj2))
plot(summerMooseBB90HR2,col="red")
# summerMoosedeer<-mask(crop(habC,summerMooseBB90HR2),summerMooseBB90HR2)
# str(summerMoosedeer)
# plot(summerMoosedeer)




# Extract raster values to polygons                             
( v <- extract(habC, summerMooseBB90HR2) )

# Get class counts for each polygon
v.counts <- lapply(v,table)

# Calculate class percentages for each polygon
( v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )

# Create a data.frame where missing classes are NA
class.df <- as.data.frame(t(sapply(v.pct,'[',1:length(unique(habC)))))  

# Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0   
names(class.df) <- paste("class", names(class.df),sep="")

AMclassDeer<-class.df
# Add back to polygon data
summerMooseBB90HR2@data <- data.frame(summerMooseBB90HR2@data, AMclassDeer)
head(summerMooseBB90HR2@data)




aveAMhabuse_season4<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/RiskMaps/DeerUse_PercentMeth/HabUse_AMcat_season4.csv")
plot(Landrast_ras[[1]])
str(Landrast_ras[[1]])
table(Landrast_ras[[1]]@data@values)
SpringDeerAMPer<-Landrast_ras[[1]]

SpringDeerAMPer<-with(Landrast_ras[[1]], ifelse(Landrast_ras[[1]] < 1, Spr$percent, ifelse(x > 0.15 & dif < 0, 2, 1)))







ext<-extent(canheight)
habCE<-setExtent(habC, ext, keepres=TRUE)

str(habCE)
plot(habCE)

str(habCE@data@attributes)
habCE2 <- setValues(raster(habCE), habCE[])
plot(habCE2)

ncol(habCE)


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
